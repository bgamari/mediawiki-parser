{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

import Debug.Trace
import Data.Char
import Data.List (intersperse, isPrefixOf)
import Data.Binary
import Data.Default
import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as HM
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ

import Text.Trifecta
import MediaWiki

import ParseDump

main :: IO ()
main = do
    docs <- parseWikiDocs <$> BSL.getContents
    let links = 
            concat
          $ withStrategy (parBuffer 80 rseq)
          [ [ (ParseDump.docTitle doc, linkTarget, linkNamespace, linkAnchor)
            | Link{..} <- docLinks' doc
            , not ("http://" `BS.isPrefixOf` linkTarget)
            , not ("https://" `BS.isPrefixOf` linkTarget)
            ]
          | doc <- docs
          ]
 
    let showLink (a,b,c,d) = mconcat $ intersperse (BB.char8 '\t') [escape a, escape b, maybe "" escape c, escape d]
    BSL.writeFile "links.out" $ BB.toLazyByteString $ mconcat $ intersperse (BB.char8 '\n') $ map showLink links

    --let ci = defaultConnectInfo { connectHost = "localhost"
    --                            , connectUser = "ldietz"
    --                            , connectPassword = "mudpie"
    --                            , connectDatabase = "wikipedia"
    --                            }
    --conn <- connect ci
    --execute_ conn [sql| CREATE TABLE IF NOT EXISTS links
    --                       ( source_title text NOT NULL
    --                       , dest_title text NOT NULL
    --                       , dest_namespace text
    --                       , anchor text) |]
    --flip traverse_ links $ \x ->
    --    execute conn [sql| INSERT INTO links VALUES (?,?,?,?) |] x
    return ()

data Link = Link { linkTarget :: !ByteString
                 , linkNamespace :: !(Maybe ByteString)
                 , linkAnchor :: !ByteString
                 }
          deriving (Show)

escape :: ByteString -> BB.Builder
escape = go
  where
    go bs
      | BS.null bs = mempty
      | '\\' <- BS.head bs = "\\\\" <> go (BS.tail bs)
      | '\t' <- BS.head bs = "\\t" <> go (BS.tail bs)
      | '\n' <- BS.head bs = " " <> go (BS.tail bs)
      | otherwise =
        let (taken, rest) = BS.span (not . badChar) bs
        in BB.byteString taken <> go rest
      where
        badChar '\t' = True
        badChar '\n' = True
        badChar '\\' = True
        badChar _    = False

docLinks' :: WikiDoc -> [Link]
docLinks' doc = 
    case parseByteString (many MediaWiki.doc) mempty $ docText doc of
      Success doc' -> foldMap findLinks doc'
      Failure err  -> trace ("dropped "++show (ParseDump.docTitle doc)++"\n"++show err) []  -- error $ show err
  where
    findLinks (InternalLink namespace (PageName name) body) =
        [Link { linkAnchor = foldMap getText body
              , linkNamespace = fmap (\(Namespace ns) -> ns) namespace
              , linkTarget = name
              }]
    findLinks _ = []

    getText :: Doc -> ByteString
    getText (Text s)        = s
    --getText (Bold s)        = s
    --getText (Italic s)      = s
    --getText (BoldItalic s)  = s
    getText _               = mempty

