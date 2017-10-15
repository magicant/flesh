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

module Flesh.Language.Parser.Syntax_TokenSpec (spec) where

import Data.Foldable (traverse_)
import Data.Text (pack)
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Lex
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "dollarExpansion" $ do
    context "command substitution" $ do
      let reflect s = expectShow s "" (snd <$> dollarExpansion) s

      context "can be empty" $ do
        expectSuccess "$()" "" (snd <$> dollarExpansion) $
          Flesh.Language.Parser.Syntax.CommandSubstitution []
        expectPosition "$()" (fst <$> dollarExpansion) 0
        reflect "$( \t#\\\n)"
        expectPosition "$( \t#\\\n)" (fst <$> dollarExpansion) 0

      context "can contain some commands" $ do
        traverse_ reflect ["$( foo )", "$(foo ||\nbar &)", "$(\nls\n)"]

      context "can contain here documents" $ do
        reflect "$(<<END\n1\n2\n3\nEND\n)"

      context "can contain escapes" $ do
        reflect "$(\\(\\))"

      context "ignores aliases" $ do
        reflect $ "$(" ++ defaultAliasName ++ ")"

      context "requires valid program" $ do
        expectFailure "$(! )" dollarExpansion Hard (MissingCommandAfter "!") 4

      context "must be closed" $ do
        let isExpectedReason (UnclosedCommandSubstitution p) = index p == 1
            isExpectedReason _ = False
        expectFailureEof' "$(X" dollarExpansion Hard isExpectedReason  3

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
      expectFailure "\\\n)" (tokenTill (lc (char ')'))) Soft UnknownReason 0

  describe "reservedOrAliasOrToken" $ do
    let ignorePosition = either (Left . snd) Right
        rat = runAliasT $ ignorePosition <$> reservedOrAliasOrToken
        rat' = runAliasT $ ignorePosition <$> reservedOrAliasOrToken

    context "returns unmatched token" $ do
      expectShow "foo" ";" rat' "Just (Right foo)"

    context "returns quoted token" $ do
      expectShow "f\\oo" ";" rat' "Just (Right f\\oo)"
      expectShow "f\"o\"o" "&" rat' "Just (Right f\"o\"o)"
      expectShow "f'o'o" ")" rat' "Just (Right f'o'o)"

    context "returns non-constant token" $ do
      expectShow "f${1}o" ";" rat' "Just (Right f${1}o)"

    context "returns reserved word" $ do
      expectShow "!" ";" rat' "Just (Left \"!\")"
      expectShowEof reservedWordAliasName "" rat "Just (Left \"while\")"

    it "doesn't perform alias substitution on reserved words" $
      let e = runFullInputTesterAlias rat reservedWordAliasDefinitions s
          s = spread (dummyPosition s') s'
          s' = reservedWordAliasName
       in fmap (show . fst) e `shouldBe` Right "Just (Left \"while\")"

    context "modifies pending input on alias subsitution" $ do
      expectSuccessEof defaultAliasName "" (rat >> readAll) defaultAliasValue

    it "returns nothing after substitution" $
      let e = runFullInputTesterWithDummyPositions rat defaultAliasName
       in fmap fst e `shouldBe` Right Nothing

    it "stops alias substitution on recursion" $
      let e = runFullInputTesterWithDummyPositions
                (reparse reservedOrAliasOrToken >> readAll) defaultAliasName
       in fmap fst e `shouldBe` Right "--color"

    it "stops alias substitution on exact recursion" $
      let e = runFullInputTesterWithDummyPositions
                (reparse reservedOrAliasOrToken >> readAll) recursiveAlias
       in fmap fst e `shouldBe` Right ""

  describe "literal" $ do
    context "returns matching unquoted token" $ do
      expectShowEof "! " "" (literal (pack "!")) "!"
      expectShow    "i\\\nf" "\n" (literal (pack "if")) "if"
      expectShowEof "foo" "" (literal (pack "foo")) "foo"

    context "fails on unmatching unquoted token" $ do
      expectFailureEof "a" (literal (pack "!")) Soft UnknownReason 0
      expectFailureEof "a" (literal (pack "aa")) Soft UnknownReason 0
      expectFailureEof "aa" (literal (pack "a")) Soft UnknownReason 0

    context "fails on quoted token" $ do
      expectFailureEof "\\if" (literal (pack "if")) Soft UnknownReason 0
      expectFailureEof "i\\f" (literal (pack "if")) Soft UnknownReason 0

-- vim: set et sw=2 sts=2 tw=78: