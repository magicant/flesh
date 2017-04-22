{-
Copyright (C) 2017 WATANABE Yuki <magicant@wonderwand.net>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE Trustworthy #-}

module Flesh.Language.Parser.SyntaxSpec (spec) where

import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Lex
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec

spec :: Spec
spec = do
  describe "doubleQuoteUnit" $ do
    context "parses backslashed backslash" $ do
      expectSuccess "\\\\" "" (snd <$> doubleQuoteUnit) (Backslashed '\\')
      expectPosition "\\\\" (fst <$> doubleQuoteUnit) 1

    context "parses literal backslash followed by alphanumeric" $ do
      expectSuccess "\\" "a" (snd <$> doubleQuoteUnit) (Char '\\')

    context "parses literal backslash followed by end-of-input" $ do
      expectSuccessEof "\\" "" (snd <$> doubleQuoteUnit) (Char '\\')

    context "parses single alphanumeric" $ do
      expectSuccess "a" "" (snd <$> doubleQuoteUnit) (Char 'a')
      expectPosition "a" (fst <$> doubleQuoteUnit) 0

    context "skips line continuations" $ do
      expectSuccess "\\\na" "" (snd <$> doubleQuoteUnit) (Char 'a')
      expectPosition "\\\na" (fst <$> doubleQuoteUnit) 2
      expectSuccess "\\\n\\\n\\$" "" (snd <$> doubleQuoteUnit)
        (Backslashed '$')
      expectPosition "\\\n\\\n\\$" (fst <$> doubleQuoteUnit) 5

  describe "doubleQuote" $ do
    context "parses empty quotes" $ do
      expectSuccess "\"\"" "" (snd <$> doubleQuote) (DoubleQuote [])
      expectPosition "\"\"" (fst <$> doubleQuote) 0

    context "parses single character in quotes" $ do
      expectShow "\"a\"" "" (snd <$> doubleQuote) "\"a\""
      expectPosition "\"a\"" (fst <$> doubleQuote) 0

    context "ignores line continuations" $ do
      expectSuccess "\\\n\"\\\n\"" "" (snd <$> doubleQuote) (DoubleQuote [])
      expectPosition "\\\n\"\\\n\"" (fst <$> doubleQuote) 2

    context "fails on unclosed quotes" $ do
      expectFailureEof "\"" doubleQuote Hard UnclosedDoubleQuote 1
      expectFailureEof "\"x" doubleQuote Hard UnclosedDoubleQuote 2
      expectFailureEof "\"\\" doubleQuote Hard UnclosedDoubleQuote 2
      expectFailureEof "\"\\\"" doubleQuote Hard UnclosedDoubleQuote 3

  describe "singleQuote" $ do
    context "parses empty quotes" $ do
      expectSuccess "''" "" (snd <$> singleQuote) (SingleQuote [])
      expectPosition "''" (fst <$> singleQuote) 0

    context "parses characters in quotes" $ do
      expectShow "'a'" "" (snd <$> singleQuote) "'a'"
      expectPosition "'a'" (fst <$> singleQuote) 0
      expectShow "'qwerty'" "" (snd <$> singleQuote) "'qwerty'"
      expectPosition "'qwerty''" (fst <$> singleQuote) 0

    context "ignores line continuations for opening quote" $ do
      expectSuccess "\\\n\\\n''" "" (snd <$> singleQuote) (SingleQuote [])

    context "backslashes are literal inside single quotes" $ do
      expectShow "'\\a\\n\\'" "" (snd <$> singleQuote) "'\\a\\n\\'"

    context "fails on unclosed quotes" $ do
      expectFailureEof "'" singleQuote Hard UnclosedSingleQuote 1
      expectFailureEof "'x" singleQuote Hard UnclosedSingleQuote 2
      expectFailureEof "'\\\\" singleQuote Hard UnclosedSingleQuote 3

  describe "tokenTill" $ do
    context "parses some word units" $ do
      expectShowEof "\\\nabc\\x\"d\"'s'" "" (tokenTill eof) "abc\\x\"d\"'s'"
      expectShow "a\\\nX" "" (tokenTill (lc (char 'X'))) "a"

    context "rejects empty token" $ do
      expectFailureEof "\\\n)" (tokenTill (lc (char ')')))
        Soft UnknownReason 0

  describe "aliasableToken" $ do
    context "returns unmatched token" $ do
      expectShow "foo" ";" aliasableToken "Just foo"

    context "returns quoted token" $ do
      expectShow "f\\oo" ";" aliasableToken "Just f\\oo"
      expectShow "f\"o\"o" "&" aliasableToken "Just f\"o\"o"
      expectShow "f'o'o" ")" aliasableToken "Just f'o'o"

    context "returns non-constant token" $ do
      expectShow "f${1}o" ";" aliasableToken "Just f${1}o"

    context "modifies pending input" $ do
      expectSuccessEof defaultAliasName "" (aliasableToken >> readAll) $
        defaultAliasValue

    it "returns nothing after substitution" $
      let s = defaultAliasName
          s' = spread (dummyPosition s) s
          e = runTester aliasableToken s'
       in fmap fst e `shouldBe` Right Nothing

  describe "simpleCommand" $ do
    context "is some tokens" $ do
      expectShowEof "foo" "" simpleCommand "foo"
      expectShowEof "foo bar" ";" simpleCommand "foo bar"
      expectShowEof "foo  bar\tbaz #X" "\n" simpleCommand "foo bar baz"

    context "rejects empty command" $ do
      expectFailureEof "" simpleCommand Soft UnknownReason 0

-- vim: set et sw=2 sts=2 tw=78:
