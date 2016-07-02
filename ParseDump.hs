{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ParseDump 
    ( parseWikiDocs
    , WikiDoc(..)
    , Format(..)
    , PageId(..)
    , NamespaceId(..)
    ) where

import Data.Maybe
import Text.XML.Expat.SAX
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL

newtype NamespaceId = NamespaceId Int
                    deriving (Eq, Ord, Enum, Show)

newtype PageId = PageId Int
               deriving (Eq, Ord, Enum, Show)

data Format = XWiki
            | OtherFormat Text
            deriving (Show)

data WikiDoc = WikiDoc { docTitle     :: Text
                       , docNamespace :: NamespaceId
                       , docPageId    :: PageId
                       , docRedirects :: [Text]
                       , docFormat    :: Format
                       , docText      :: Text
                       }
             deriving (Show)

entities :: HM.HashMap Text Text
entities = HM.fromList
    [ ("gt", ">")
    , ("lt", "<")
    , ("amp", "&")
    , ("quot", "\"")
    ]

parseWikiDocs :: BSL.ByteString -> [WikiDoc]
parseWikiDocs = parseWikiDocs' . parse parseOpts
  where
    parseOpts = defaultParseOptions { entityDecoder = Just (`HM.lookup` entities) }

parseWikiDocs' :: [SAXEvent Text Text] -> [WikiDoc]
parseWikiDocs' = go . dropWhile (not . isEndTag "siteinfo")
  where
    go [] = []
    go xs = 
      let (page, rest) = break (isEndTag "page") $ dropWhile (not . isStartTag "page") xs
      in parsePage emptyDoc page : go rest

    emptyDoc = WikiDoc { docTitle = ""
                       , docNamespace = NamespaceId 0
                       , docPageId = PageId 0
                       , docRedirects = []
                       , docFormat = OtherFormat ""
                       , docText = ""
                       }

    parsePage :: WikiDoc -> [SAXEvent Text Text] -> WikiDoc
    parsePage doc [] = doc
    parsePage doc (x:xs)
      | isStartTag "title" x =
          let (content, xs') = break (isEndTag "title") xs
              doc' = doc {docTitle = getContent content}
          in parsePage doc' xs'
      | isStartTag "ns" x =
          let (content, xs') = break (isEndTag "ns") xs
              doc' = doc {docNamespace = toEnum $ read $ T.unpack $ getContent content}
          in parsePage doc' xs'
      | isStartTag "id" x =
          let (content, xs') = break (isEndTag "id") xs
              doc' = doc {docPageId = toEnum $ read $ T.unpack $ getContent content}
          in parsePage doc' xs'
      | isStartTag "redirect" x =
          let StartElement _ attrs = x
              Just title = lookup "title" attrs
              doc' = doc {docRedirects = title : docRedirects doc}
          in parsePage doc' xs
      | isStartTag "format" x =
          let (content, xs') = break (isEndTag "format") xs
              doc' = doc {docFormat = parseFormat $ getContent content}
          in parsePage doc' xs'
      | isStartTag "text" x =
          let (content, xs') = break (isEndTag "text") xs
              doc' = doc {docText = getContent content}
          in parsePage doc' xs'
      | isStartTag "page" x = parsePage doc xs
      | otherwise           = parsePage doc xs
      where
        parseFormat "text/x-wiki" = XWiki
        parseFormat other         = OtherFormat other

        getContent :: [SAXEvent Text Text] -> Text
        getContent =
            TL.toStrict . foldMap getCharData
          where
            getCharData (CharacterData text) = TL.fromStrict text
            getCharData _                    = TL.empty

isStartTag :: Eq tag => tag -> SAXEvent tag text -> Bool
isStartTag tag (StartElement tag' _) = tag == tag'
isStartTag _   _ = False

isEndTag :: Eq tag => tag -> SAXEvent tag text -> Bool
isEndTag tag (EndElement tag') = tag == tag'
isEndTag _   _ = False

