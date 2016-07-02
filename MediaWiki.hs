{-# LANGUAGE OverloadedStrings #-}

module MediaWiki where

import Control.Monad (replicateM_)
import Data.Monoid
import Control.Applicative

import Text.Trifecta hiding (doc)
import Text.Trifecta.Util.It
import qualified Data.CharSet as CS
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

newtype Namespace = Namespace ByteString
                  deriving (Show)
newtype PageName = PageName ByteString
                 deriving (Show)
newtype Url = Url ByteString
            deriving (Show)

data Doc = Text !ByteString
         | Header !Int !ByteString
         | InternalLink !(Maybe Namespace) !PageName ![Doc]
         | ExternalLink !Url
         | Template !ByteString [(ByteString, Maybe ByteString)]
         | Bold !ByteString
         | Italic !ByteString
         | BoldItalic !ByteString
         | CodeLine !ByteString
         | NoWiki !ByteString
         deriving (Show)

named = flip (<?>)

doc :: Parser Doc
doc = named "document element"
    $ header 6 <|> header 5 <|> header 4 <|> header 3 <|> header 2 <|> header 1
   <|> internalLink <|> template <|> boldItalic <|> bold <|> italic <|> text_
  where
    boldItalic = fmap BoldItalic $ try $ between' (text "'''''") (try $ text "'''''")
    bold       = fmap Bold       $ try $ between' (text "'''") (try $ text "'''")
    italic     = fmap Italic     $ try $ between' (text "''") (try $ text "''")
    text_      = Text <$> sliced (anyChar >> many (noneOf "[]{}*&|\\<\"':\n"))
    header n   = named "header" $ try $ fmap (Header n) $ do
      newline
      replicateM_ n (char '=')
      spaces
      title <- sliced $ some $ noneOf "="
      replicateM_ n (char '=')
      return title

template :: Parser Doc
template = named "template" $ do
    text "{{"
    title <- balancedText
    pairs <- many $ char '|' >> pair
    text "}}"
    return $ Template title pairs
  where
    -- FIXME
    balancedText = sliced $ many $ noneOf "{}|"
    pair = do
      key <- sliced $ many $ noneOf "{}|="
      value <- optional $ char '=' >> balancedText
      return (key, value)

internalLink :: Parser Doc
internalLink = named "internal link" $ do
    text "[["
    (namespace, page) <- try targetWithNamespace <|> target
    body <- option [] $ do
        char '|'
        some $ notFollowedBy (char ']') >> doc
    text "]]"
    return $ InternalLink namespace page body
  where
    targetWithNamespace = do
      namespace <- Namespace <$> sliced (some $ noneOf ":|]")
      char ':'
      pageName <- PageName <$> sliced (some $ noneOf ":|]")
      return (Just namespace, pageName)

    target = do
      optional $ char ':'
      pageName <- PageName <$> sliced (some $ noneOf ":|]")
      return (Nothing, pageName)
       
between' :: Parser bra -> Parser ket -> Parser ByteString
between' bra ket = do
    bra
    start <- mark
    let go = end <|> (anyChar >> go)
        end = do d <- mark
                 ket
                 release d
    ret <- sliced go
    ket
    return ret

url :: DeltaParsing m => m Url
url = fmap Url $ sliced $ do
    some $ oneOfSet asciiLetters
    text "://"
    some $ oneOfSet urlChars

asciiLetters :: CS.CharSet
asciiLetters = CS.range 'a' 'z' <> CS.range 'A' 'Z'

urlChars :: CS.CharSet
urlChars = asciiLetters <> CS.range '0' '9' <> CS.fromList "-_.~!*'();:@&=+$,/?%#[]"
