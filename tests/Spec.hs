{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Test.Hspec

import           Exnihilo.Error
import           Exnihilo.Template
import           Exnihilo.Variables

main :: IO ()
main = hspec spec

spec = do
  templateSpec

templateSpec = describe "Template engine" do
  it "Hadnles templates without variables" do
    ("foo bar", mempty) `shouldRender` "foo bar"

  it "Handles misquoted variable" do
    ("Use double braces {foo}", mempty) `shouldRender` "Use double braces {foo}"

  it "Handles templates with one variable" do
    ("Author: {{author}}", fromList [("author", "foo")]) `shouldRender` "Author: foo"

  it "Handles templates with multiple variables" do
    let vars = fromList [("var1", "foo"), ("var2", "bar")]
    ("var1: {{var1}}, var2: {{var2}}, var1again: {{var1}}", vars) `shouldRender` "var1: foo, var2: bar, var1again: foo"

  it "Handles missing variable" do
    ("foo: {{foo}}", mempty) `shouldFailWith` ErrorMissingVariable "foo"

  it "Handles whitespaces around variables" do
    ("foo: {{  foo      }}", fromList [("foo", "bar")]) `shouldRender` "foo: bar"

    where
      shouldRender (inp, vars) exp = testTemplate inp (pure exp) vars
      shouldFailWith (inp, vars) e = testTemplate inp (throwError e) vars
      testTemplate inp exp vars = do
        out <- runExceptT $ runReaderT @Variables (parseTemplate inp >>= renderTemplate) vars
        out `shouldBe` exp
