{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module MediaWiki (Doc(..), parse) where

import Debug.Trace
import Control.Monad (replicateM_, void)
import Data.Bifunctor
import Data.Monoid
import Control.Applicative hiding (many)

import Text.Parsers.Frisby hiding ((<>), optional)
import Text.Parsers.Frisby.Char

newtype PageName = PageName String
                 deriving (Show)
newtype Url = Url String
            deriving (Show)

data Doc = Text !String
         | Char !Char
         | Comment !String
         | Heading !Int !String
         | InternalLink !PageName ![[Doc]]
         | ExternalLink !Url
         | Template !String [(Maybe String, [Doc])]
         | XmlOpenClose String [(String, String)]
         | XmlOpen String [(String, String)]
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
    return $ start *> xs <* end

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
            body <- manyUntil (eol <> eof) aDoc
            let p = pure constr
                    <*  eol
                    <*> fmap length (some $ char bullet)
                    <*  spaces
                    <*> body
            return p
    numberedList <- listLike NumberedList '#'
    bulletList   <- listLike BulletList '*'
    let list = numberedList <> bulletList

    -- links
    internalLink <- do
        let linkEnd = void (char '|') <> void (text "]]")
        pageName <- manyUntil linkEnd anyChar
        attrValue <- manyUntil linkEnd aDoc
        attributes <- manyUntil (text "]]") (spaces *> char '|' *> attrValue)
        return $ pure InternalLink <*  text "[["
                                   <*> fmap PageName pageName
                                   <*> attributes <* text "]]"
    let link = internalLink <> externalLink
        externalLink = mempty

    -- code line
    codeLine <- do
        line <- manyUntil eol anyChar
        return $ eol *> char ' ' *> fmap CodeLine line

    -- templates
    template <- do
        let templateEnd = void (text "}}") <> void (char '|')
        templateName <- manyUntil templateEnd anyChar
        namedPart <- do
            key <- manyUntil (char '=') anyChar
            value <- manyUntil (templateEnd <> eol) aDoc
            return $ pure (,) <*  char '|' <* spaces
                              <*> optional (key <* char '=') <* spaces
                              <*> value <* optional eol
          :: PM s (P s (Maybe String, [Doc]))
        templateParts <- manyUntil (text "}}") namedPart
        return $ pure Template <* text "{{" <*> templateName <* optional eol <*> templateParts <* text "}}"

    -- XMLish
    xmlAttr <- do
        key <- manyUntil (char '=' <> space) anyChar
        value <- manyUntil (char '>' <> space) anyChar
        return $ pure (,) <*> key <* spaces <* char '='
                          <*> value <* spaces
    xmlAttrs <- manyUntil (char '>') xmlAttr
    tagName <- manyUntil (char '>' <> space) anyChar
    xmlOpen <-
        return $ pure XmlOpen <* char '<'
                              <*> tagName <* spaces
                              <*> xmlAttrs <* char '>'
    xmlClose <-
        return $ pure XmlClose <* text "</"
                               <*> tagName <* spaces <* char '>'
    xmlOpenClose <-
        return $ pure XmlOpenClose <* text "<"
                                   <*> tagName <* spaces
                                   <*> xmlAttrs <* text "/>"

    let xmlish = xmlClose <> xmlOpen <> xmlOpenClose

    let blankLine = eol

    let image = mempty
        table = mempty
        anythingElse = Char <$> anyChar

    wikiText <- newRule
        $ noWiki // template // choice headings // list // formatting
        // codeLine // comment
        // template // xmlish // image // link // table
        // (eol *> eol *> pure NewPara)
        // anythingElse

    para <- pure $ wikiText <* eol

    let aDoc = wikiText
    return aDoc

plainText :: P s Doc
plainText = Char <$> anyChar

heading :: Int -> PM s (P s Doc)
heading n =
    let marker = replicateM_ n (char '=')
    in fmap (Heading n) <$> manyBetween' (marker *> spaces) anyChar (spaces *> marker)

spaces :: P s ()
spaces = void $ many space

compressText :: [Doc] -> [Doc]
compressText = go []
  where
    go acc (Text s : xs)          = go (reverse s ++ acc) xs
    go acc (Char c : xs)          = go (c : acc) xs
    go []  (BoldItalic ds : xs)   = BoldItalic (go [] ds) : go [] xs
    go []  (Bold ds : xs)         = Bold (go [] ds) : go [] xs
    go []  (Italic ds : xs)       = Italic (go [] ds) : go [] xs
    go []  (BulletList n ds : xs) = BulletList n (compressText ds) : go [] xs
    go []  (Template n ds : xs)   = Template n (map (second compressText) ds) : go [] xs
    go []  (InternalLink n ds : xs) = InternalLink n (map compressText ds) : go [] xs
    go []  (other : xs)           = other : go [] xs
    go []  []                     = []
    go acc xs                     = Text (reverse acc) : go [] xs

{-}
url :: DeltaParsing m => m Url
url = fmap Url $ do
    method <- some $ asciiUpper <|> asciiLower
    text "://"
    rest <- some urlChars
    return $ concat [method, "://", rest]
-}

urlChar :: P s Char
urlChar = letter <|> digit <|> oneOf "-_.~!*'();:@&=+$,/?%#[]"
