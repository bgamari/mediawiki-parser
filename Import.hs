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
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import ParseDump
import qualified Text.Pandoc as P
import Text.Pandoc.Walk
import Text.Pandoc.Readers.MediaWiki

main :: IO ()
main = do
    docs <- parseWikiDocs <$> BSL.getContents
    let forms = foldl' (HM.unionWith mappend) mempty
          $ withStrategy (parBuffer 80 rseq)
          [ HM.fromListWith (HM.unionWith mappend)
            [ (T.toCaseFold linkAnchor, HM.singleton (T.toCaseFold linkTarget) (Sum 1))
            | Link{..} <- docLinks doc
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


