{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Lordown 
  ( module Text.Pandoc.Writers.Lorcode
  , markdownOptions
  , markdownExtensions
  , toLorcode
  , fromMarkdown
  , convert
  ) 
  where

import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Lorcode
import qualified Data.Set as S

-- | Converts from Markdown to Pandoc
fromMarkdown :: String -> Pandoc
fromMarkdown = either (error . show) id . readCommonMark markdownOptions

-- | Markdown reader options
markdownOptions :: ReaderOptions
markdownOptions = def { readerSmart = True
                      , readerExtensions = markdownExtensions }

-- | Markdown reader extensions                     
markdownExtensions :: S.Set Extension
markdownExtensions = S.fromList
  [Ext_backtick_code_blocks
  ,Ext_strikeout
  ,Ext_fancy_lists]

-- | Converts from Pandoc to Lorcode
toLorcode :: Pandoc -> String
toLorcode = writeLorcode def

-- | Replaces headers with strong text
behead :: Block -> Block
behead (Header _ _ xs) = Para [Strong xs]
behead x               = x
  
-- | StackOverflow-like user casts
handleCast :: Inline -> Inline
handleCast (Str ('@' : user)) = Str $ "[user]" ++ user ++ "[/user]"
handleCast x                  = x

-- | Converts text from Markdown to Lorcode
convert :: String -> String
convert = toLorcode . (walk handleCast) . (walk behead) . fromMarkdown
