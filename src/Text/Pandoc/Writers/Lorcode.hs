{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.Lorcode 
  ( writeLorcode
  )
  where

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Data.List (intersperse)
import Data.Maybe (listToMaybe)

-- | Convert Pandoc representation to Lorcode text
writeLorcode :: WriterOptions -> Pandoc -> String
writeLorcode _ (Pandoc _ blocks) = 
  render Nothing $ prettyBlocks blocks

prettyBlocks :: [Block] -> Doc
prettyBlocks = prettyList . map prettyBlock                                

prettyBlock :: Block -> Doc
prettyBlock HorizontalRule = empty
prettyBlock (Plain inlines) = prettyInlines inlines
prettyBlock (BlockQuote blocks) = tag Nothing "quote" $ prettyBlocks blocks
prettyBlock (BulletList items) = tag Nothing "list" $ prettyItems items
prettyBlock (OrderedList _ items) = tag (Just "1") "list" $ prettyItems items
prettyBlock (Para inlines) = prettyInlines inlines
prettyBlock (CodeBlock (_, cls, _) code) =
  tag (listToMaybe cls) "code" $ text code
prettyBlock rest = notImplemented rest

prettyItem :: [Block] -> Doc
prettyItem = (text "[*]" <>) . prettyBlocks

prettyItems :: [[Block]] -> Doc
prettyItems = mconcat . map prettyItem

prettyInline :: Inline -> Doc
prettyInline (Strikeout inlines) = tag Nothing "s" $ prettyInlines inlines
prettyInline (Str str) = text str
prettyInline (Link title (url, _)) = link (prettyInlines title) url
prettyInline (Image title (url, _)) = link (prettyInlines title) url
prettyInline (Strong inlines) = tag Nothing "strong" $ prettyInlines inlines
prettyInline (Emph inlines) = tag Nothing "em" $ prettyInlines inlines
prettyInline (Code _ code) = tag Nothing "inline" $ text code
prettyInline Space = space
prettyInline LineBreak = text "[br]"
prettyInline rest = notImplemented rest

notImplemented :: (Show a) => a -> b
notImplemented x = error $ "cannot handle: " ++ show x

prettyList :: [Doc] -> Doc
prettyList = mconcat . intersperse blankline

prettyInlines :: [Inline] -> Doc
prettyInlines = mconcat . map prettyInline

tag :: Maybe String -> String -> Doc -> Doc
tag opt name body = start <> body <> end
  where start    = text $ "[" <> name <> maybeOpt <> "]"
        end      = text $ "[/" <> name <> "]"
        maybeOpt = case opt of
                     Nothing -> ""
                     Just x  -> "=" <> x



link :: Doc -> String -> Doc
link title url = tag (Just url) "url" title
