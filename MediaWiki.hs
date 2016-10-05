{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module MediaWiki (Doc(..), parse) where

import Debug.Trace
import Control.Monad (replicateM_, void)
import Data.Monoid
import Control.Applicative hiding (many)

import Text.Parsers.Frisby hiding ((<>))
import Text.Parsers.Frisby.Char

newtype PageName = PageName String
                 deriving (Show)
newtype Url = Url String
            deriving (Show)

data Doc = Text !Char
         | Comment !String
         | Heading !Int !String
         | InternalLink !PageName ![Doc]
         | ExternalLink !Url
         | Template !String [(Maybe String, String)]
         | XmlOpenClose String
         | XmlOpen String
         | XmlClose String
         | BoldItalic [Doc]
         | Bold [Doc]
         | Italic [Doc]
         | NumberedList !Int [Doc]
         | BulletList !Int [Doc]
         | CodeLine !String
         | NoWiki !String
         | NewPara
         deriving (Show)

parse :: String -> [Doc]
parse s = concatMap (runPeg doc) (lines s)

doc :: PM s (P s [Doc])
doc = fmap many doc'

manyBetween' :: P s start -> P s a -> P s end -> PM s (P s [a])
manyBetween' start thing end = do
    xs <- manyUntil end thing
    return $ start *> xs

manyBetween :: P s delim -> P s a -> PM s (P s [a])
manyBetween delim thing = manyBetween' delim thing delim

eol :: P s ()
eol = void $ char '\n' <> char '\r'

doc' :: forall s. PM s (P s Doc)
doc' = mdo
    -- headings
    headings <- mapM heading [6,5..1]

    -- formatting
    boldItalic <- fmap BoldItalic <$> manyBetween (text "'''''") aDoc
    bold <- fmap Bold <$> manyBetween (text "'''") aDoc
    italic <- fmap Italic <$> manyBetween (text "''") aDoc
    formatting <- newRule $ boldItalic // bold // italic

    -- other
    comment <- fmap Comment <$> manyBetween' (text "<!--") anyChar (text "-->")
    noWiki <-  fmap NoWiki  <$> manyBetween' (text "<nowiki>") anyChar (text "</nowiki>")

    -- lists
    let listLike :: (Int -> [Doc] -> Doc) -> Char -> PM s (P s Doc)
        listLike constr bullet = do
            body <- manyUntil eol aDoc
            let p = pure constr
                    <*  eol
                    <*> fmap length (some $ char bullet)
                    <*  spaces
                    <*> body
            return p
    numberedList <- listLike NumberedList '#'
    bulletList   <- listLike BulletList '*'
    let list = numberedList <> bulletList

    let blankLine = eol

    let template = mempty
        image = mempty
        table = mempty
        anythingElse = mempty
        link = mempty

    wikiText <- newRule
        $ noWiki // template // choice headings // list // formatting
        // image // link // table // anythingElse
        // codeLine // comment // (eol *> pure NewPara)

    para <- pure $ wikiText <* eol

    let aDoc = wikiText
    return aDoc

plainText :: P s Doc
plainText = Text <$> anyChar

heading :: Int -> PM s (P s Doc)
heading n =
    let marker = replicateM_ n (char '=')
    in fmap (Heading n) <$> manyBetween' (marker *> spaces) anyChar (spaces *> marker)

codeLine :: P s Doc
codeLine = bof *> char ' ' *> fmap CodeLine rest

spaces :: P s ()
spaces = void $ many space

{-}
xmlish :: P s Doc
xmlish = do
    char '<'
    closeTag <|> openTag
  where
    closeTag = do
        char '/' >> spaces
        tag <- some letter
        return $ XmlClose tag

    openTag = do
        spaces
        tag <- some letter
        spaces
        many attribute
        selfClosing tag <|> withContent tag
      where
        attribute = do
            some letter
            spaces
            char '='
            (spaces >> between' (char '"') (char '"')) <|> some $ noneOf "/> \t\n"
            spaces

        withContent tag = do
            char '>'
            return $ XmlOpen tag

        selfClosing tag = do
            text "/>"
            return $ XmlOpenClose tag

template :: P s Doc
template = do
    text "{{"
    title <- balancedText
    pairs <- many $ char '|' >> (try keyValuePair <|> onlyValue <|> emptyPair)
    text "}}"
    return $ Template title pairs
  where
    balancedText = named "balanced text" $ sliced content
      where
        content = some $  void template
                      <|> void (some $ noneOf "}|")
                      <|> void (notFollowedBy (text "}}") >> char '}')

    emptyPair = return (Nothing, mempty)

    onlyValue = do
      val <- balancedText
      return (Nothing, val)

    keyValuePair = do
      key <- sliced $ some $ noneOf "}|="
      char '='
      value <- balancedText
      return (Just key, value)

internalLink :: P s Doc
internalLink ctx = do
    text "[["
    page <- PageName <$> some (noneOf "|]" <|> singleClose)
    attrs <- many $ do
        char '|'
        many $ notFollowedBy (text "]]") >> doc' (ctx & insideInternalLink .~ True)
    let body = case attrs of [] -> []
                             xs -> last xs
    text "]]"
    return $ InternalLink page body
  where
    singleClose = notFollowedBy (text "]]") >> char ']'

url :: DeltaParsing m => m Url
url = fmap Url $ do
    method <- some $ asciiUpper <|> asciiLower
    text "://"
    rest <- some urlChars
    return $ concat [method, "://", rest]
-}

urlChar :: P s Char
urlChar = letter <|> digit <|> oneOf "-_.~!*'();:@&=+$,/?%#[]"
