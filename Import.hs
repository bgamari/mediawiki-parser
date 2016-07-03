{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

import Debug.Trace
import Data.Char
import Data.List (intersperse, isPrefixOf)
import Data.List.Split (chunksOf)
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

import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent as PC

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ

import Text.Trifecta
import MediaWiki

import ParseDump

main :: IO ()
main = do
    (namespaces, docs) <- parseWikiDocs <$> BSL.getContents
    let links = 
            concat
          $ withStrategy (parBuffer 80 rseq)
          [ [ (ParseDump.docTitle doc, linkTarget, linkNamespace, linkAnchor)
            | Link{..} <- docLinks (map snd namespaces) doc
            , not ("http://" `BS.isPrefixOf` linkTarget)
            , not ("https://" `BS.isPrefixOf` linkTarget)
            ]
          | doc <- docs
          ]
 
    print namespaces
    let showLink (a,b,c,d) = mconcat $ intersperse (BB.char8 '\t') [escape a, escape b, maybe "" escape c, escape d]
    --BSL.writeFile "links.out" $ BB.toLazyByteString $ mconcat $ intersperse (BB.char8 '\n') $ map showLink links

    let ci = defaultConnectInfo { connectHost = "localhost"
                                , connectUser = "ldietz"
                                , connectPassword = "mudpie"
                                , connectDatabase = "wikipedia"
                                }
    conn <- connect ci
    execute_ conn [sql| CREATE TABLE IF NOT EXISTS links
                           ( source_title text NOT NULL
                           , dest_title text NOT NULL
                           , dest_namespace text
                           , anchor text) |]
    (sq, rq, seal) <- PC.spawn' PC.unbounded
    writer <- async $ runEffect $ for (PC.fromInput rq) $ \xs ->
        void $ liftIO $ executeMany conn [sql| INSERT INTO links (source_title, dest_title, dest_namespace, anchor)
                                               VALUES (?,?,?,?) |] xs
    link writer
    mapM_ (atomically . PC.send sq) (chunksOf 10000 links)
    atomically seal
    wait writer
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

stripWhitespace :: BS.ByteString -> BS.ByteString
stripWhitespace = fst . BS.spanEnd isSpace . BS.dropWhile isSpace

docLinks :: [Namespace] -> WikiDoc -> [Link]
docLinks namespaces doc = 
    case parseByteString (many MediaWiki.doc) mempty $ docText doc of
      Success doc' -> foldMap findLinks doc'
      Failure err  -> trace ("dropped "++show (ParseDump.docTitle doc)++"\n"++show err) []  -- error $ show err
  where
    namespaceNames = [ map toLower $ BS.unpack name | Namespace name <- namespaces ]
    findLinks (InternalLink (PageName name) body) =
      let (page, namespace)
            | Just page <- ":" `BS.stripPrefix` name = (page, Nothing)

            | (ns, page) <- BS.span (/= ':') name
            , map toLower (BS.unpack ns) `elem` namespaceNames
            , not (BS.null page)
            = (stripWhitespace $ BS.tail page, Just $ stripWhitespace ns)

            | otherwise 
            = (name, Nothing)
      in [Link { linkAnchor = foldMap getText body
               , linkNamespace = namespace
               , linkTarget = page
               }]
    findLinks _ = []

    getText :: Doc -> ByteString
    getText (Text s)        = s
    --getText (Bold s)        = s
    --getText (Italic s)      = s
    --getText (BoldItalic s)  = s
    getText _               = mempty

