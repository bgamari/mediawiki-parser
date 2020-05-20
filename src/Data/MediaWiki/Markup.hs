{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- A reasonably robust MediaWiki markup parser.
--
-- Note that bold and italics are parsed as start/end tokens since parsing
-- this format in one go in the presence of syntax errors (e.g. unpaired
-- symbols, which tend to be quite common in practice) is nearly impossible
-- in one pass.
--
module Data.MediaWiki.Markup
    ( Doc(..), parse
    , PageName(..), LinkTarget(..), Url(..)
    , ListType(..)
    ) where

import Control.Monad (replicateM_, void)
import Data.Bifunctor
import Data.Hashable
import Data.Monoid
import Data.String (IsString)
import Data.Char (toLower)
import qualified Data.Text as T
import Control.Applicative hiding (many, optional)
import GHC.Generics

import Text.Parsers.Frisby hiding ((<>))
import Text.Parsers.Frisby.Char

newtype PageName = PageName { getPageName :: T.Text }
                 deriving (Show, Generic, IsString)

-- | Respects Wikimedia title equality rules: first character is
-- case-insensitive, remaining title case-sensitive.
instance Eq PageName where
    PageName a == PageName b =
        T.toCaseFold (T.take 1 a) == T.toCaseFold (T.take 1 b)
        && T.drop 1 a == T.drop 1 b

instance Ord PageName where
    PageName a `compare` PageName b =
        case T.toCaseFold (T.take 1 a) `compare` T.toCaseFold (T.take 1 b) of
          EQ -> T.drop 1 a `compare` T.drop 1 b
          x -> x

instance Hashable PageName where
    hashWithSalt salt (PageName t)
      | T.null t  = salt
      | otherwise = hashWithSalt (hashWithSalt salt (T.drop 1 t))
                                 (toLower $ T.head t)

newtype Url = Url String
            deriving (Show, Eq, Ord, Generic)

type TagName = T.Text

-- | An internal link target.
data LinkTarget = LinkTarget { linkTargetPage   :: !PageName
                             , linkTargetAnchor :: !(Maybe T.Text)
                             }
                deriving (Show, Eq, Ord, Generic)

data BraceCount = DoubleBrace | TripleBrace
                deriving (Show, Eq, Ord, Generic)

data ListType = Numbered | Bulleted | Unmarked | Definition
              deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data Doc = Text !String
         | Char !Char
         | Comment !String
         | Heading !Int [Doc]
         | InternalLink !LinkTarget [[Doc]]
         | ExternalLink !Url (Maybe String)
         | Template [Doc] [(Maybe T.Text, [Doc])]
         | TemplateArg [Doc] [(Maybe T.Text, [Doc])]
         | MagicWord !T.Text ![(Maybe T.Text, [Doc])]
         | Math !T.Text
         | XmlOpenClose TagName [(String, String)]
         | XmlTag TagName [(String, String)] [Doc]
         | BoldItalic
         | Bold
         | Italic
         | List [ListType] [Doc]
         | CodeLine !String
         | NoWiki !String
         | Table !String
         | HRule
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

(<$*>) :: (a -> b) -> PM s (P s a) -> PM s (P s b)
(<$*>) f = fmap (fmap f)
infixl 4 <$*>

doc' :: forall s. PM s (P s Doc)
doc' = mdo
    -- headings
    let heading :: Int -> PM s (P s Doc)
        heading n = do
            let marker = replicateM_ n (char '=')
            fmap (Heading n) . ((eol <> bof) *>)
              <$> manyBetween' (marker *> spaces) wikiText (spaces *> marker)

    headings <- mapM heading [6,5..1]

    -- formatting
    let boldItalic = BoldItalic <$ text "'''''"
        bold       = Bold <$ text "'''"
        italic     = Italic <$ text "''"
    formatting <- newRule $ boldItalic // bold // italic

    -- other
    comment <- Comment <$*> manyBetween' (text "<!--") anyChar (text "-->")
    noWiki <-  NoWiki  <$*> manyBetween' (text "<nowiki>") anyChar (text "</nowiki>")

    -- lists
    let aList :: PM s (P s Doc)
        aList = do
            body <- manyUntil (text_ "}}" <> eol <> eof) aDoc
            let listChar =  Numbered <$ char '#'
                        <|> Bulleted <$ char '*'
                        <|> Unmarked <$ char ':'
                        <|> Definition <$ char ';'
                p = pure List
                    <*  eol
                    <*> many1 listChar
                    <*  spaces
                    <*> body
            return p
    list <- aList

    -- horizontal rule
    hrule <- newRule $ eol *> text "----" *> pure HRule

    -- links
    internalLink <- do
        let linkEnd = char_ '|' <> text_ "]]" <> char_ '#'
        pageName <- PageName . T.pack <$*> manyUntil linkEnd anyChar
        sectionName <- T.pack <$*> manyUntil linkEnd anyChar
        let linkTarget = LinkTarget <$> pageName
                                    <*> option Nothing (char_ '#' *> fmap Just sectionName)
        attrValue <- manyUntil linkEnd aDoc
        attributes <- manyUntil (text "]]") (spaces *> char '|' *> attrValue)
        return $ pure InternalLink <*  text "[[" <* spaces
                                   <*> linkTarget
                                   <*> attributes <* text "]]"
    externalLink <- do
        let scheme = many1 alpha <* text "://"
        url <- manyUntil (char ']' <> space) anyChar
        let f proto r = ExternalLink $ Url $ proto++"://"++r
        anchor <- manyUntil (char ']') anyChar
        return $ pure f <*  char '[' <* spaces
                        <*> scheme
                        <*> url
                        <*> option Nothing (pure Just <* spaces <*> anchor)
                        <*  char ']'
    link <- newRule $ internalLink <> externalLink

    -- code line
    codeLine <- do
        line <- manyUntil eol anyChar
        return $ eol *> char ' ' *> fmap CodeLine line

    -- magic words
    magicWord <- do
        parts <- templateParts DoubleBrace templateBody
        let theWord = T.pack <$> many alphaNum
        return $ pure MagicWord <*  text_ "{{" <* optional eol <* spaces
                                <*  optional (char_ '#')
                                <*> theWord <* char_ ':'
                                <*> parts
                                <*  spaces <*  text_ "}}"

    -- templates
    let template' f bc = do
          -- drop comments after template name
          let comments = void (comment *> eol *> comment) <|> (comment *> spaces)
          parts <- templateParts bc templateBody
          body <- manyUntil (templateEnd bc) templateBody
          return $ pure f
              <*  openBraces bc
              <*> body <* many comments <* optional eol <* many comments
              <*  spaces
              <*> option [] (char_ '|' *> spaces *> parts <* spaces)
              <*  closeBraces bc
    template2 <- template' Template DoubleBrace
    template3 <- template' TemplateArg TripleBrace
    -- Assume that last three open braces are grouped if possible; see
    -- https://meta.wikimedia.org/wiki/Help:Expansion#XML_parse_tree
    let tryTemplate2 = peek (text_ "{{{{{") *> template2
        template = tryTemplate2 <|> template3 <|> template2

    -- <math>
    -- We sadly can't handle this as XML since math can look like a heading. See TimeValueOfMoney.wiki
    math <- do
        let open = char '<' *> spaces *> text "math" *> spaces *> char '>'
            close = text "</"  *> spaces *> text "math" *> spaces *> char '>'
        body <- fmap T.pack <$> manyUntil close anyChar
        return $ fmap Math (open *> body <* close)

    -- XMLish
    xmlAttr <- do
        key <- newRule $ many1 $ alpha <|> char '-' <|> char '_'
        unquotedValue <- manyUntil (char_ '>' <> text_ "/>" <> void space) anyChar
        quotedValue <- manyUntil (char '"') anyChar
        let value = (char '"' *> quotedValue <* char '"') <|> unquotedValue
        let equalsValue = spaces *> char '=' *> spaces *> value
            optionalEqualsValue = equalsValue <|> pure []
        return $ pure (,) <*> key <*> optionalEqualsValue <* spaces
    xmlAttrs <- manyUntil (char_ '>' <> text_ "/>") xmlAttr
    -- Based on the XML 1.1 grammar
    tagNameChar <- newRule $ many $ alpha <|> char '-' <> oneOf ['0'..'9']
    tagName <- newRule $ (\x xs -> T.pack (x:xs)) <$> alpha <*> tagNameChar
    xml <- do
        let open :: P s (TagName, [(String, String)])
            open = pure (,) <* char '<'
                            <*> tagName <* spaces
                            <*> xmlAttrs <* char '>'
            close = text "</" *> spaces *> tagName <* spaces <* char '>'
        children <- manyUntil close aDoc
        let pair :: P s ((TagName, [(String, String)]), [Doc], TagName)
            pair = (,,) <$> open <*> children <*> close

            toXml ((oTag, attrs), cs, _cTag) = XmlTag oTag attrs cs
        return $ fmap toXml $ pair -- onlyIf pair (\((oTag, _), _, cTag) -> T.toCaseFold oTag == T.toCaseFold cTag)
    xmlOpenClose <-
        return $ pure XmlOpenClose <* char '<'
                                   <*> tagName <* spaces
                                   <*> xmlAttrs <* text "/>"
    brTag <- newRule $ pure (XmlOpenClose "br" []) <* text "<br>"
    xmlish <- newRule $ brTag <> xml <> xmlOpenClose

    -- table
    table <- do
        -- See 2PM testcase
        let end = text "|}" *> matches (doesNotMatch $ char '}')
        tableBody <- manyUntil end anyChar
        return $ pure Table <* text "{|" <*> tableBody <* text "|}"

    let blankLine = eol

    let image = mempty
        anythingElse = Char <$> anyChar

    -- See https://www.mediawiki.org/wiki/Parser_2011/Stage_1:_Formal_grammar
    wikiText <- newRule
        $ comment // math // noWiki // table
        // magicWord // template
        // choice headings // list // hrule // formatting
        // codeLine
        // xmlish // image // link // table
        // (eol *> matches eol *> pure NewPara)
        // anythingElse

    -- Templates can have indented elements
    templateBody <- newRule
        $ comment // noWiki // table
        // magicWord // template
        // choice headings // list // hrule -- // formatting
           -- not codeLine
        // xmlish // image // link // table
        // (eol *> matches eol *> pure NewPara)
        // anythingElse

    para <- pure $ wikiText <* eol

    let aDoc = wikiText
    return aDoc

-- | Eat whitespace (but not newlines!)
spaces :: P s ()
spaces = void $ many $ char ' ' // char '\t'

openBraces, closeBraces :: BraceCount -> P s ()
openBraces DoubleBrace  = text_ "{{"
openBraces TripleBrace  = text_ "{{{"
closeBraces DoubleBrace = text_ "}}"
closeBraces TripleBrace = text_ "}}}"

templateEnd :: BraceCount -> P s ()
templateEnd bc =
       closeBraces bc
    <> char_ '|'
    <> (eol *> spaces *> closeBraces bc)
    <> (eol *> spaces *> char_ '|')

templateParts :: forall s. BraceCount -> P s Doc -> PM s (P s [(Maybe T.Text, [Doc])])
templateParts bc templateBody = mdo
    value <- manyUntil (templateEnd bc) templateBody
    part <- do
        key <- T.strip . T.pack <$*> manyUntil (text_ "=" <> text_ "|" <> closeBraces bc) anyChar
        return $ pure (,) <*> option Nothing (Just <$> key <* char '=' <* spaces)
                          <*> value <* optional eol
      :: PM s (P s (Maybe T.Text, [Doc]))
    parts <- newRule $
                ((:) <$> part <* spaces <* char '|' <* spaces <*> parts)
            <|> ((:[]) <$> part)
    return parts

cleanup :: [Doc] -> [Doc]
cleanup = go []
  where
    go acc (Text s : xs)            = go (reverse s ++ acc) xs
    go acc (Char '\n' : xs)         = go acc xs
    go acc (Char c : xs)            = go (c : acc) xs
    go []  (Heading n ds : xs)      = Heading n (cleanup ds) : go [] xs
    go []  (BoldItalic : xs)        = BoldItalic : go [] xs
    go []  (Bold : xs)              = Bold : go [] xs
    go []  (Italic : xs)            = Italic : go [] xs
    go []  (List tys ds : xs)       = List tys (cleanup ds) : go [] xs
    go []  (Template n ds : xs)     = Template (cleanup n) (map (second cleanup) ds) : go [] xs
    go []  (TemplateArg n ds : xs)  = TemplateArg (cleanup n) (map (second cleanup) ds) : go [] xs
    go []  (MagicWord t ds : xs)    = MagicWord t (map (fmap cleanup) ds) : go [] xs
    go []  (InternalLink t ds : xs) = InternalLink t (map cleanup ds) : go [] xs
    go []  (NewPara : NewPara : xs) = go [] (NewPara : xs)
    go []  (XmlTag tag attrs cs : xs) = XmlTag tag attrs (go [] cs) : go [] xs
    go []  (other : xs)             = other : go [] xs
    go []  []                       = []
    go acc xs                       = Text (reverse acc) : go [] xs
