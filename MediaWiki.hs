{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module MediaWiki (Doc(..), parse) where

import Debug.Trace
import qualified Control.Lens as L
import           Control.Lens ((&), (.~), (^.))
import           Data.Bits.Lens (bitAt)
import Control.Monad (replicateM_, void)
import Data.Monoid
import Control.Applicative hiding (many)

import Text.Parsers.Frisby
import Text.Parsers.Frisby.Char

newtype PageName = PageName String
                 deriving (Show)
newtype Url = Url String
            deriving (Show)

data Doc = Text !Char
         | Comment !String
         | Header !Int !String
         | InternalLink !PageName ![Doc]
         | ExternalLink !Url
         | Template !String [(Maybe String, String)]
         | XmlOpenClose String
         | XmlOpen String
         | XmlClose String
         | BoldItalic [Doc]
         | Bold [Doc]
         | Italic [Doc]
         | CodeLine !String
         | NoWiki !String
         deriving (Show)

parse :: String -> [Doc]
parse s = concatMap (runPeg doc) (lines s)

doc :: PM s (P s [Doc])
doc = mdo
    newRule $ many doc'

doc' :: P s (P s Doc)
doc' = mdo
    h1 <- header 1
    h2 <- header 2
    h3 <- header 3
    h4 <- header 4
    h5 <- header 5
    h6 <- header 6
    // noWiki // codeLine // comment
    // boldItalic // bold // italic // plainText

bold :: PM s (P s Doc)
bold = do
    text "'''"
    Bold <$> manyUntil (text "'''") doc

italic :: PM s (P s Doc)
italic = do
    text "''"
    Italic <$> manyUntil (text "''") doc

boldItalic :: PM s (P s Doc)
boldItalic = do
    text "'''''"
    BoldItalic <$> manyUntil (text "'''''") doc

plainText :: P s Doc
plainText = Text <$> anyChar

header :: Int -> P s Doc
header n = do
    replicateM_ n (char '=')
    manyUntil (replicateM_ n (char '=')) anyChar
    return $ Header n title

codeLine :: P s Doc
codeLine = do
    bof
    char ' '
    CodeLine <$> rest

noWiki :: P s Doc
noWiki = do
    text "<nowiki>"
    NoWiki <$> manyUntil (text "</nowiki>") anyChar

comment :: P s Doc
comment = do
    text "<!--"
    Comment <$> comment (text "-->") anyChar

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
