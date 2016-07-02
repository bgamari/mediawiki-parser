{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MediaWiki where

import qualified Control.Lens as L
import           Control.Lens ((&), (.~), (^.))
import           Data.Bits.Lens (bitAt)
import Debug.Trace
import Control.Monad (replicateM_, void)
import Data.Monoid
import Control.Applicative

import Text.Trifecta hiding (doc)
import Text.Trifecta.Util.It
import qualified Data.CharSet as CS
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

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
         | Template !ByteString [(Maybe ByteString, ByteString)]
         | XmlOpenClose String
         | XmlOpen String
         | XmlClose String
         | BoldItalic [Doc]
         | Bold [Doc]
         | Italic [Doc]
         | CodeLine !ByteString
         | NoWiki !ByteString
         deriving (Show)

named = flip (<?>)

data Context = Context { _ctxFlags :: !Int }

L.makeLenses ''Context

insideBold, insideItalic, insideBoldItalic :: L.Lens' Context Bool
insideBold = ctxFlags . bitAt 0
insideItalic = ctxFlags . bitAt 1
insideBoldItalic = ctxFlags . bitAt 2

doc :: Parser Doc
doc = doc' (Context 0)

doc' :: Context -> Parser Doc
doc' ctx = named "document element"
    $ header <|> codeLine <|> try noWiki <|> try comment <|> try xmlish
   <|> internalLink <|> template -- <|> boldItalic <|> bold <|> italic
   <|> text_
  where
    boldItalic
      | ctx ^. insideBoldItalic = empty
      | otherwise  = named "bold italic"
                   $ do let sym = text "'''''"
                        fmap BoldItalic $ between sym sym $ some $ doc' (ctx & insideBoldItalic .~ True)

    bold    
      | ctx ^. insideBold = empty
      | otherwise  = named "bold"
                   $ do let sym = notFollowedBy (text "''''") >> text "'''"
                        fmap Bold $ between sym sym $ some $ doc' (ctx & insideBold .~ True)

    italic    
      | ctx ^. insideItalic = empty
      | otherwise  = named "italic"
                   $ do let sym = notFollowedBy (text "'''") >> text "''"
                        fmap Italic $ between sym sym $ some $ doc' (ctx & insideItalic .~ True)

    codeLine   = fmap CodeLine   $ try $ newline >> space >> restOfLine <* newline
    noWiki     = fmap NoWiki     $ try $ between' (text "<nowiki>") (text "</nowiki>")
    comment    = do
      between' (text "<!--") (text "-->")
      return $ Text mempty
    text_      = Text <$> sliced (anyChar >> many (noneOf "[]{}*&|\\<\"':\n"))
      
    header     = named "header" $ try $ do
      newline
      n <- length <$> some (char '=')
      spaces
      title <- sliced $ some $ noneOf "="
      replicateM_ n (char '=')
      spaces
      newline
      return $ Header n title

xmlish :: Parser Doc
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
            (spaces >> between' (char '"') (char '"')) <|> sliced (some $ noneOf "/> \t\n")
            spaces

        withContent tag = do
            char '>'
            return $ XmlOpen tag

        selfClosing tag = do
            text "/>" 
            return $ XmlOpenClose tag

template :: Parser Doc
template = named "template" $ do
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

internalLink :: Parser Doc
internalLink = named "internal link" $ do
    text "[["
    (namespace, page) <- try targetWithNamespace <|> target
    body <- option [] $ do
        char '|'
        some $ notFollowedBy (text "]]") >> doc
    text "]]"
    return $ InternalLink namespace page body
  where
    singleClose = notFollowedBy (text "]]") >> char ']'
    targetWithNamespace = do
      namespace <- Namespace <$> sliced (some $ noneOf ":|]" <|> singleClose)
      char ':'
      pageName <- PageName <$> sliced (some $ noneOf "|]" <|> singleClose)
      return (Just namespace, pageName)

    target = do
      optional $ char ':'
      pageName <- PageName <$> sliced (some $ noneOf "|]" <|> singleClose)
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

