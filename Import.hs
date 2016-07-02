{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Default
import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Parallel.Strategies
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL

import qualified Text.Pandoc as P
import Text.Pandoc.Walk
import Text.Pandoc.Readers.MediaWiki

import Text.Trifecta
import MediaWiki

import ParseDump

main :: IO ()
main = do
    docs <- parseWikiDocs <$> BSL.getContents
    let forms = foldl' (HM.unionWith mappend) mempty
          $ withStrategy (parBuffer 80 rseq)
          [ HM.fromListWith (HM.unionWith mappend)
            [ (T.toCaseFold linkAnchor, HM.singleton (T.toCaseFold linkTarget) (Sum 1))
            | Link{..} <- docLinks' doc
            , not ("http://" `T.isPrefixOf` linkTarget)
            , not ("https://" `T.isPrefixOf` linkTarget)
            ]
          | doc <- docs
          ]
    print forms

readerOpts = def

data Link = Link { linkTarget :: !Text 
                 , linkAnchor :: !Text
                 }
          deriving (Show)

docLinks :: WikiDoc -> [Link]
docLinks doc =
    let doc' = readMediaWiki readerOpts $ T.unpack $ docText doc
    in query f doc'
  where
    f (P.Link _ anchor (url, title)) =
      [Link { linkAnchor = getInlineText anchor
            , linkTarget = T.pack url
            }]
    f _ = []

    getInlineText :: [P.Inline] -> T.Text
    getInlineText = TL.toStrict . foldMap go
      where
        go (P.Str s)    = TL.pack s
        go (P.Emph x)   = foldMap go x
        go (P.Strong x) = foldMap go x
        go P.Space      = " "
        go _            = mempty

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
    getText (Bold s)        = TE.decodeUtf8 s
    getText (Italic s)      = TE.decodeUtf8 s
    getText (BoldItalic s)  = TE.decodeUtf8 s
    getText _               = mempty

