{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

import Debug.Trace
import Data.List (intersperse)
import Data.Binary
import Data.Default
import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as HM
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
          [ [ (ParseDump.docTitle doc, T.toCaseFold linkAnchor, linkTarget)
            | Link{..} <- docLinks' doc
            , not ("http://" `T.isPrefixOf` linkTarget)
            , not ("https://" `T.isPrefixOf` linkTarget)
            ]
          | doc <- docs
          ]
 
    let showLink (a,b,c) = mconcat $ intersperse (TB.singleton '\t') [TB.fromText a, TB.fromText b, TB.fromText c]
    TL.writeFile "links.out" $ TB.toLazyText $ mconcat $ intersperse (TB.singleton '\n') $ map showLink links

    --let ci = defaultConnectInfo { connectHost = "localhost"
    --                            , connectUser = "ldietz"
    --                            , connectPassword = "mudpie"
    --                            , connectDatabase = "wikipedia"
    --                            }
    --conn <- connect ci
    --execute_ conn [sql| CREATE TABLE IF NOT EXISTS links ( sourceTitle text, destTitle text, anchor text) |]
    --flip traverse_ links $ \x ->
    --    execute conn [sql| INSERT INTO links VALUES (?,?,?) |] x
    return ()

data Link = Link { linkTarget :: !Text 
                 , linkAnchor :: !Text
                 }
          deriving (Show)

docLinks' :: WikiDoc -> [Link]
docLinks' doc = 
    case parseByteString (many MediaWiki.doc) mempty $ TE.encodeUtf8 $ docText doc of
      Success doc' -> foldMap findLinks doc'
      Failure err  -> trace ("dropped "++show (ParseDump.docTitle doc)++"\n"++show err) []  -- error $ show err
  where
    findLinks (InternalLink ns (PageName name) body) =
      [Link { linkAnchor = foldMap getText body
            , linkTarget = TE.decodeUtf8 name
            }]
    findLinks _ = []

    getText :: Doc -> T.Text
    getText (Text s)        = TE.decodeUtf8 s
    --getText (Bold s)        = TE.decodeUtf8 s
    --getText (Italic s)      = TE.decodeUtf8 s
    --getText (BoldItalic s)  = TE.decodeUtf8 s
    getText _               = mempty

