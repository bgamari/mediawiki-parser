{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Data.MediaWiki.Markup
    ( Doc(..), parse
    , PageName(..), Url(..)
    ) where

import Control.Monad (replicateM_, void)
import Data.Bifunctor
import Data.Monoid
import qualified Data.Text as T
import Control.Applicative hiding (many, optional)
import GHC.Generics

import Text.Parsers.Frisby hiding ((<>))
import Text.Parsers.Frisby.Char

newtype PageName = PageName { getPageName :: T.Text }
                 deriving (Show, Eq, Ord, Generic)

newtype Url = Url String
            deriving (Show, Eq, Ord, Generic)

data Doc = Text !String
         | Char !Char
         | Comment !String
         | Heading !Int !String
         | InternalLink !PageName ![[Doc]]
         | ExternalLink !Url (Maybe String)
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
         | Table !String
         | NewPara
         deriving (Show, Eq, Ord, Generic)

parse :: String -> Either String [Doc]
parse = fmap cleanup . runPeg (withError doc)

doc :: PM s (P s [Doc])
doc = fmap many doc'

withError :: PM s (P s a) -> PM s (P s (Either String a))
withError = fmap f
  where
    f parser =
        fmap Right (parser <* eof) <|> fmap toLeft rest
    toLeft r = Left $ "Parser error with leftovers: "++r

manyBetween' :: P s start -> P s a -> P s end -> PM s (P s [a])
manyBetween' start thing end = do
    xs <- manyUntil end thing
    return $ start *> xs <* end

manyBetween :: P s delim -> P s a -> PM s (P s [a])
manyBetween delim thing = manyBetween' delim thing delim

eol :: P s ()
eol = void $ char '\n' <> char '\r'

text_ :: String -> P s ()
text_ = void . text

char_ :: Char -> P s ()
char_ = void . char

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
                    <*> fmap length (many1 $ char bullet)
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
                                   <*> fmap (PageName . T.pack) pageName
                                   <*> attributes <* text "]]"
    externalLink <- do
        let scheme = many1 alpha <* text "://"
        url <- manyUntil (char ']' <> space) anyChar
        let f proto r = ExternalLink $ Url $ proto++"://"++r
        anchor <- manyUntil (char ']') anyChar
        return $ pure f <*  char '['
                        <*> scheme
                        <*> url
                        <*> option Nothing (pure Just <* spaces <*> anchor)
                        <* char ']'
    let link = internalLink <> externalLink

    -- code line
    codeLine <- do
        line <- manyUntil eol anyChar
        return $ eol *> char ' ' *> fmap CodeLine line

    -- templates
    template <- do
        let templateEnd = text_ "}}"
                       <> char_ '|'
                       <> (eol *> spaces *> (text_ "}}"))
                       <> (eol *> spaces *> (char_ '|'))
        templateName <- manyUntil (templateEnd <> eol) anyChar
        value <- manyUntil templateEnd aDoc
        part <- do
            key <- manyUntil (text "=" <> text "|" <> text "}}") anyChar
            return $ pure (,) <*  spaces <* char '|' <* spaces
                              <*> option Nothing (Just <$> key <* char '=' <* spaces)
                              <*> value <* optional eol
          :: PM s (P s (Maybe String, [Doc]))
        templateParts <- manyUntil (spaces *> text "}}") part
        return $ pure Template <* text "{{"
                               <*> templateName <* optional eol
                               <*> templateParts <* spaces <* text "}}"

    -- XMLish
    xmlAttr <- do
        key <- manyUntil (char '=' <> space) anyChar
        unquotedValue <- manyUntil (char '>' <> space) anyChar
        quotedValue <- manyUntil (char '"') anyChar
        let value = (char '"' *> quotedValue <* char '"') <|> unquotedValue
        return $ pure (,) <*> key <* spaces <* char '='
                          <*> value <* spaces
    xmlAttrs <- manyUntil (void (char '>') <> void (text "/>")) xmlAttr
    tagName <- manyUntil (void (char '>') <> void (text "/>") <> void space) anyChar
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

    -- table
    table <- do
        tableBody <- manyUntil (text "|}") anyChar
        return $ pure Table <* text "{|" <*> tableBody <* text "|}"

    let blankLine = eol

    let image = mempty
        anythingElse = Char <$> anyChar

    -- See https://www.mediawiki.org/wiki/Parser_2011/Stage_1:_Formal_grammar
    wikiText <- newRule
        $ comment // noWiki // table
        // template // choice headings // list // formatting
        // codeLine
        // xmlish // image // link // table
        // (eol *> matches eol *> pure NewPara)
        // anythingElse

    para <- pure $ wikiText <* eol

    let aDoc = wikiText
    return aDoc

heading :: Int -> PM s (P s Doc)
heading n =
    let marker = replicateM_ n (char '=')
    in fmap (Heading n) . ((eol <> bof) *>) <$> manyBetween' (marker *> spaces) anyChar (spaces *> marker)

spaces :: P s ()
spaces = void $ many space

cleanup :: [Doc] -> [Doc]
cleanup = go []
  where
    go acc (Text s : xs)            = go (reverse s ++ acc) xs
    go acc (Char '\n' : xs)         = go acc xs
    go acc (Char c : xs)            = go (c : acc) xs
    go []  (BoldItalic ds : xs)     = BoldItalic (cleanup ds) : go [] xs
    go []  (Bold ds : xs)           = Bold (cleanup ds) : go [] xs
    go []  (Italic ds : xs)         = Italic (cleanup ds) : go [] xs
    go []  (BulletList n ds : xs)   = BulletList n (cleanup ds) : go [] xs
    go []  (NumberedList n ds : xs) = NumberedList n (cleanup ds) : go [] xs
    go []  (Template n ds : xs)     = Template n (map (second cleanup) ds) : go [] xs
    go []  (InternalLink n ds : xs) = InternalLink n (map cleanup ds) : go [] xs
    go []  (NewPara : NewPara : xs) = go [] (NewPara : xs)
    go []  (other : xs)             = other : go [] xs
    go []  []                       = []
    go acc xs                       = Text (reverse acc) : go [] xs
