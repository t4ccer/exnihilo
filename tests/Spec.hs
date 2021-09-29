{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Monad.Except
import           Control.Monad.Except.CoHas (runExceptT)
import           Control.Monad.Reader
import           Control.Monad.State
import           System.Directory
import           System.Directory.Tree
import           System.Environment
import           System.FilePath
import           Test.Hspec

import           Exnihilo.App
import           Exnihilo.Error
import qualified Exnihilo.Main              as Main
import           Exnihilo.Parameters
import           Exnihilo.Schema
import           Exnihilo.Template
import           Exnihilo.Variables

main :: IO ()
main = hspec spec

spec = do
  templateSpec
  integrationLocalSpec
  integrationUrlSpec
  integrationGithubSpec

integrationGithubSpec = describe "Integration tests - Github" do
  it "Fetch schema" do
    --
    Main.debug
      " -g t4ccer/exnihilo -s tests/data/simple.yaml --ref a6be66a3551635e60ef81e1c3b698990e1431ee9\
      \ -d tests/data/simple-github.out\
      \ --no-interactive"
    check "simple-github"

integrationUrlSpec = describe "Integration tests - URL" do
  it "Fetch schema" do
    -- a6be66a3551635e60ef81e1c3b698990e1431ee9
    Main.debug
      " -u https://raw.githubusercontent.com/t4ccer/exnihilo/a6be66a3551635e60ef81e1c3b698990e1431ee9/tests/data/simple.yaml\
      \ -d tests/data/simple-url.out\
      \ --no-interactive"
    check "simple-url"

integrationLocalSpec = describe "Integration tests - local path" do
  it "No templates" do
    Main.debug
      " -s tests/data/simple.yaml\
      \ -d tests/data/simple.out\
      \ --no-interactive"
    check "simple"

  it "With templates" do
    Main.debug
      " -s tests/data/simple-templates.yaml\
      \ -d tests/data/simple-templates.out\
      \ --variables tests/data/vars.yaml\
      \ --no-interactive"

    check "simple-templates"

  it "With overrides" do
    Main.debug
      " -s tests/data/simple-overrides.yaml\
      \ -d tests/data/simple-overrides.out\
      \ --variables tests/data/vars.yaml\
      \ -k varString1 -v bar\
      \ --no-interactive"

    check "simple-overrides"

  it "Missing variables" do
    res <-
      Main.debugTry
        " -s whatever\
        \ -d whatever\
        \ --variables tests/data/i-do-not-exist\
        \ --no-interactive"

    res `shouldBe` Left (ErrorFileReadMissing "tests/data/i-do-not-exist")

  it "Invalid variables (list)" do
    res <-
      Main.debugTry
        " -s wahtever\
        \ -d whatever\
        \ --variables tests/data/vars-invalid.yaml\
        \ --no-interactive"

    res `shouldBe`
      Left (ErrorFileParse "tests/data/vars-invalid.yaml" "Aeson exception: Error in $: Variable can be only number, string, or bool. Variable \"varList\" is type of array.")

  it "Missing schema" do
    res <-
      Main.debugTry
        " -s tests/data/i-do-not-exist\
        \ -d whatever\
        \ --no-interactive"

    res `shouldBe` Left (ErrorFileReadMissing "tests/data/i-do-not-exist")

  it "Invalid schema (no files defined)" do
    res <-
      Main.debugTry
        " -s tests/data/invalid.yaml\
        \ -d whatever\
        \ --no-interactive"

    res `shouldBe` Left (ErrorFileParse "tests/data/invalid.yaml" "Aeson exception: Error in $: key \"files\" not found")

  it "With create condition" do
    Main.debug
      " -s tests/data/simple-condition.yaml\
      \ -d tests/data/simple-condition.out\
      \ --variables tests/data/vars.yaml\
      \ --no-interactive"

    check "simple-condition"

  it "With create condition - wrong type" do
    res <- Main.debugTry
      " -s tests/data/simple-condition-wrong-type.yaml\
      \ -d tests/data/whatever.out\
      \ --variables tests/data/vars.yaml\
      \ --no-interactive"

    res `shouldBe` Left (ErrorCreateTypeCheck "")

templateSpec = describe "Template engine" do
  it "Hadnles templates without variables" do
    ("foo bar", mempty) `shouldRender` "foo bar"

  it "Handles misquoted variable" do
    ("Use double braces {foo}", mempty) `shouldRender` "Use double braces {foo}"

  it "Handles templates with one variable" do
    ("Author: {{author}}", fromList [("author", VarString "foo")]) `shouldRender` "Author: foo"

  it "Handles templates with multiple variables" do
    let vars = fromList [("var1", VarString "foo"), ("var2", VarString "bar")]
    ("var1: {{var1}}, var2: {{var2}}, var1again: {{var1}}", vars) `shouldRender` "var1: foo, var2: bar, var1again: foo"

  it "Handles missing variable" do
    ("foo: {{foo}}", mempty) `shouldFailWith` ErrorMissingVariable "foo"

  it "Handles whitespaces around variables" do
    ("foo: {{  foo      }}", fromList [("foo", VarString "bar")]) `shouldRender` "foo: bar"
  where
    shouldRender (inp, vars) exp = testTemplate inp (pure exp) vars
    shouldFailWith (inp, vars) e = testTemplate inp (throwError e) vars
    testTemplate inp exp vars = do
      out <- runExceptT $ evalStateT (parseTemplate inp >>= renderTemplate) vars
      out `shouldBe` exp

check fp = (fp <> ".out") `shouldMatch` (fp <> ".expected")
shouldMatch fp1 fp2 = do
  f1 <- dirTree <$> readDirectory ("tests/data" </> fp1)
  f2 <- dirTree <$> readDirectory ("tests/data" </> fp2)
  case f1 of
    d1@Dir{} -> case f2 of
      d2@Dir{} -> contents d1 `shouldBe` contents d2
      _        -> f1 `shouldBe` f2
    _ -> f1 `shouldBe` f2
  -- res <- contents . dirTree <$> readDirectory ("tests/data" </> fp1)
  -- exp <- contents . dirTree <$> readDirectory ("tests/data" </> fp2)
  -- res `shouldBe` exp

getParams' inp = liftIO $ flip withArgs getParams $ words inp
