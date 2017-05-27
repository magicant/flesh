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

import Data.List.NonEmpty (NonEmpty(..))
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Lex
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
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
    let at = runAliasT aliasableToken

    context "returns unmatched token" $ do
      expectShow "foo" ";" at "Just foo"

    context "returns quoted token" $ do
      expectShow "f\\oo" ";" at "Just f\\oo"
      expectShow "f\"o\"o" "&" at "Just f\"o\"o"
      expectShow "f'o'o" ")" at "Just f'o'o"

    context "returns non-constant token" $ do
      expectShow "f${1}o" ";" at "Just f${1}o"

    context "modifies pending input" $ do
      expectSuccessEof defaultAliasName "" (at >> readAll) $
        defaultAliasValue

    it "returns nothing after substitution" $
      let e = runTesterWithDummyPositions at defaultAliasName
       in fmap fst e `shouldBe` Right Nothing

    it "stops on recursion" $
      let e = runTesterWithDummyPositions (reparse aliasableToken >> readAll)
                defaultAliasName
       in fmap fst e `shouldBe` Right "--color"

  describe "redirect" $ return () -- FIXME

  describe "hereDocDelimiter" $ do
    context "is a token followed by a newline" $ do
      expectShow "<<X\nX\n" "" completeLine "0<<X"

    -- TODO it "matches an unquoted token" pending

    context "can be indented for <<-" $ do
      expectShow "<<-X\nX\n"       "" completeLine "0<<-X"
      expectShow "<<-X\n\tX\n"     "" completeLine "0<<-X"
      expectShow "<<-X\n\t\t\tX\n" "" completeLine "0<<-X"

  describe "hereDocContent" $ do
    context "ends with delimiter" $ do
      expectShow "<<-X\nX\n" "" completeLine "0<<-X"

      let isExpectedReason (UnclosedHereDocContent (HereDocOp _ 0 True d))
            | show d == "X" = True
          isExpectedReason _ = False
       in expectFailureEof' "<<-X\nfoo\n" completeLine Hard isExpectedReason 5

    -- TODO it "accumulates result" pending

  describe "pendingHereDocContents" $ do
    context "parses 1 pending content" $ do
      expectShow "<<A\nA\n" "" completeLine "0<<A"

    context "parses 2 pending contents" $ do
      expectShow "<<A 1<<B\nA\nB\n" "" completeLine "0<<A 1<<B"

    context "leaves no pending contents" $ return ()
    -- Nothing to test here because 'completeLine' would fail if any contents
    -- are left pending.

  describe "newlineHD" $ do
    context "parses newline" $ do
      expectSuccess "\n" "" (snd <$> fill newlineHD) '\n'
      expectPosition "\n" (fst <$> fill newlineHD) 0

    context "parses pending here doc contents after newline" $ return ()
    -- This property is tested in test cases for other properties.

  describe "simpleCommand" $ do
    let sc = runAliasT $ fill simpleCommand

    context "is some tokens" $ do
      expectShowEof "foo" "" sc "Just foo"
      expectShowEof "foo bar" ";" sc "Just foo bar"
      expectShowEof "foo  bar\tbaz #X" "\n" sc "Just foo bar baz"

    context "rejects empty command" $ do
      expectFailureEof "" sc Soft UnknownReason 0

    it "returns nothing after alias substitution" $
      let e = runTesterWithDummyPositions sc defaultAliasName
       in fmap fst e `shouldBe` Right Nothing

    context "does not alias-substitute second token" $ do
      expectShowEof ("foo " ++ defaultAliasName) "" sc $
        "Just foo " ++ defaultAliasName

  describe "completeLine" $ do
    {- TODO context "can be empty" $ do
      expectShow "\n" "" completeLine "" -}

    context "reparses alias" $ do
      expectShowEof (defaultAliasName ++ "\n") "" completeLine
        defaultAliasValue

    it "fills empty here document content" $
      let f [SimpleCommand [] [] [(_, HereDoc _ c)]] = Just c
          f _ = Nothing
          e = runTesterWithDummyPositions (f <$> completeLine) "<<X\nX\n"
       in fmap fst e `shouldBe` Right (Just (EWord []))

    -- TODO it "fills non-empty here document content" pending

    context "fails with missing here doc contents" $ do
      let isExpectedReason (MissingHereDocContents
            (HereDocOp _ 0 False d :| [])) | show d == "X" = True
          isExpectedReason _ = False
       in expectFailureEof' "<<X" completeLine Hard isExpectedReason 3

    context "may end with EOF with no here doc contents pending" $ do
      expectShowEof "foo bar" "" completeLine "foo bar"

-- vim: set et sw=2 sts=2 tw=78:
