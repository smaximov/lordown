module Main where

import Test.Hspec
import Text.Pandoc.Lordown
import Control.Monad

main :: IO ()
main = hspec $ do
  describe "convert" $ do
    it "drops horizontal rules" $ do
      let hrules = ["***"
                   ,"---"
                   ,"___"]
      forM_ hrules $ \hrule ->
        convert hrule `shouldBe` "" 

    it "replaces headers with strong text" $ do
      let headers = ["# foo"
                    ,"## foo"
                    ,"### foo"
                    ,"#### foo"
                    ,"##### foo"
                    ,"###### foo"
                    ,"foo\n---"
                    ,"foo\n==="]
      forM_ headers $ \header ->
        convert header `shouldBe` "[strong]foo[/strong]"
        
    describe "code blocks" $ do
      it "handles indented code blocks" $ do
        let block  = "    foo\n\
                     \    bar\n\
                     \    baaz"
            result = "[code]foo\n\
                     \bar\n\
                     \baaz\n[/code]"
        convert block `shouldBe` result

      it "handles fenced code blocks" $ do
        let block  = "```\n\
                     \foo\n\
                     \bar\n\
                     \baaz\n\
                     \```"
            result = "[code]foo\n\
                     \bar\n\
                     \baaz\n[/code]"
        convert block `shouldBe` result

      it "handles fenced code blocks with info strings" $ do
        let block  = "``` ruby\n\
                     \def inc(x)\n\
                     \  x + 1\n\
                     \end\n\
                     \```"
            result = "[code=ruby]def inc(x)\n\
                     \  x + 1\n\
                     \end\n[/code]"
        convert block `shouldBe` result  

    it "handles multiple paragraphs" $ do
      let par = "aaa\n\
                \\n\
                \bbb"
      convert par `shouldBe` par
      
    describe "blockquotes" $ do
      it "handles simple blockquotes" $ do
        let bquote = "> this\n\
                     \> is quote"
            result = "[quote]this is quote[/quote]"
        convert bquote `shouldBe` result
        
      it "handles multiple paragraphs inside blockquotes" $ do
        let bquote = "> first para\n\
                     \>\n\
                     \> second para"
            result = "[quote]first para\n\
                     \\n\
                     \second para[/quote]"
        convert bquote `shouldBe` result
        
      it "handles multiple blockquotes separated by blank lines" $ do
        let bquote = "> first quote\n\
                     \\n\
                     \> second quote"
            result = "[quote]first quote[/quote]\n\
                     \\n\
                     \[quote]second quote[/quote]"
        convert bquote `shouldBe` result
        
    describe "bullet lists" $ do
      it "handles bullet lists with plain text" $ do
        let blist  = "- first item\n\
                     \- second item\n\
                     \- third item"
            result = "[list][*]first item[*]second item[*]third item[/list]"
        convert blist `shouldBe` result

    describe "ordered lists" $ do
      it "handles ordered lists marked by arabic numbers" $ do
        let olist  = "1. first\n\
                     \2. second"
            result = "[list=1][*]first[*]second[/list]"
        convert olist `shouldBe` result
        
    describe "nested lists" $ do
      it "handle nested lists" $ do
        let list   = "1. first\n\
                     \2. second\n\
                     \   - nested first\n\
                     \   - nested second"
            result = "[list=1][*]first[*]second[list][*]nested first[*]nested second[/list][/list]"
        convert list `shouldBe` result

    it "handles user casts" $ do
      convert "cast @user" `shouldBe` "cast [user]user[/user]"
      convert "cast @@user" `shouldBe` "cast [user]@user[/user]"
      convert "cast email@example.com" `shouldBe` "cast email@example.com"
