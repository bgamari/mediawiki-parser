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
import qualified Data.HashSet as HS
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
import qualified Data.Text.Lazy.Encoding as TLE

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
            , not ("http://" `T.isPrefixOf` linkTarget)
            , not ("https://" `T.isPrefixOf` linkTarget)
            ]
          | doc <- docs
          ]
 
    print namespaces
    let showLink (src, dest, ns, anchor) = mconcat $ intersperse (BB.char8 '\t')
            $ map TLE.encodeUtf8Builder
            [ escape $ TE.decodeUtf8 src
            , escape dest
            , maybe "" escape ns
            , escape anchor]
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

data Link = Link { linkTarget :: !T.Text
                 , linkNamespace :: !(Maybe T.Text)
                 , linkAnchor :: !TL.Text
                 }
          deriving (Show)

escape :: Text -> TL.Text
escape = TB.toLazyText . go
  where
    go bs
      | T.null bs = mempty
      | '\\' <- T.head bs = "\\\\" <> go (T.tail bs)
      | '\t' <- T.head bs = "\\t" <> go (T.tail bs)
      | '\n' <- T.head bs = " " <> go (T.tail bs)
      | otherwise =
        let (taken, rest) = T.span (not . badChar) bs
        in TB.fromText taken <> go rest
      where
        badChar '\t' = True
        badChar '\n' = True
        badChar '\\' = True
        badChar _    = False

docLinks :: [Namespace] -> WikiDoc -> [Link]
docLinks namespaces doc = 
    case parseByteString (many MediaWiki.doc) mempty $ docText doc of
      Success doc' -> foldMap findLinks doc'
      Failure err  -> trace ("dropped "++show (ParseDump.docTitle doc)++"\n"++show err) []  -- error $ show err
  where
    namespaceNames = HS.fromList [ T.toCaseFold $ TE.decodeUtf8 name
                                 | Namespace name <- namespaces ]
    findLinks (InternalLink (PageName name') body) =
      let name = TE.decodeUtf8 name'
          (page, namespace)
            | Just page <- ":" `T.stripPrefix` name = (page, Nothing)

            | (ns, page) <- T.span (/= ':') name
            , T.toCaseFold ns `HS.member` namespaceNames
            , not (T.null page)
            = (T.strip $ T.tail page, Just $ T.strip ns)

            | otherwise 
            = (name, Nothing)
      in [Link { linkAnchor = TLE.decodeUtf8 $ BB.toLazyByteString $ foldMap getText body
               , linkNamespace = namespace
               , linkTarget = page
               }]
    findLinks _ = []

    getText :: Doc -> BB.Builder
    getText (Text s)        = BB.byteString s
    --getText (Bold s)        = s
    --getText (Italic s)      = s
    --getText (BoldItalic s)  = s
    getText _               = mempty

