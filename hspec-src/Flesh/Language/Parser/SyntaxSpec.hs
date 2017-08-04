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
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
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
      expectFailure "\\\n)" (tokenTill (lc (char ')'))) Soft UnknownReason 0

  describe "aliasableToken" $ do
    let at = runAliasT aliasableToken
        at' = runAliasT aliasableToken

    context "returns unmatched token" $ do
      expectShow "foo" ";" at' "Just foo"

    context "returns quoted token" $ do
      expectShow "f\\oo" ";" at' "Just f\\oo"
      expectShow "f\"o\"o" "&" at' "Just f\"o\"o"
      expectShow "f'o'o" ")" at' "Just f'o'o"

    context "returns non-constant token" $ do
      expectShow "f${1}o" ";" at' "Just f${1}o"

    context "modifies pending input" $ do
      expectSuccessEof defaultAliasName "" (at >> readAll) defaultAliasValue

    it "returns nothing after substitution" $
      let e = runFullInputTesterWithDummyPositions at defaultAliasName
       in fmap fst e `shouldBe` Right Nothing

    it "stops on recursion" $
      let e = runFullInputTesterWithDummyPositions
                (reparse aliasableToken >> readAll) defaultAliasName
       in fmap fst e `shouldBe` Right "--color"

    it "stops on exact recursion" $
      let e = runFullInputTesterWithDummyPositions
                (reparse aliasableToken >> readAll) recursiveAlias
       in fmap fst e `shouldBe` Right ""

  describe "reserved" $ do
    context "returns matching unquoted token" $ do
      expectShowEof "! " "" (reserved (T.pack "!")) "!"
      expectShowEof "i\\\nf" "\n" (reserved (T.pack "if")) "if"
      expectShowEof "foo" "" (reserved (T.pack "foo")) "foo"

    context "fails on unmatching unquoted token" $ do
      expectFailureEof "a" (reserved (T.pack "!")) Soft UnknownReason 0
      expectFailureEof "a" (reserved (T.pack "aa")) Soft UnknownReason 0
      expectFailureEof "aa" (reserved (T.pack "a")) Soft UnknownReason 0

    context "fails on quoted token" $ do
      expectFailureEof "\\if" (reserved (T.pack "if")) Soft UnknownReason 0
      expectFailureEof "i\\f" (reserved (T.pack "if")) Soft UnknownReason 0

  describe "redirect" $ do
    let yieldDummyContent = HereDocT $
          return () <$ (drainOperators >> yieldContent [])
        rTester = hereDocOp <$> (fill (redirect <* yieldDummyContent))

    context "parses << operator" $ do
      expectPositionEof "12<< END"    (hereDocOpPos <$> rTester) 0
      expectSuccessEof  "12<< END" "" (hereDocFd    <$> rTester) 12
      expectSuccessEof  "12<< END" "" (isTabbed     <$> rTester) False
      expectShowEof     "12<< END" "" (delimiter    <$> rTester) "END"

  describe "hereDocLine" $ do
    context "contains expansions for unquoted delimiter" $ do
      expectShow "<<X\n$foo\\\nX\nX\n" "" completeLine "0<<X"

    context "does not contain expansions for quoted delimiter" $ do
      expectShow "<<\\X\n$foo\\\nX\n"   "" completeLine "0<<\\X"
      expectShow "<<\"X\"\n$foo\\\nX\n" "" completeLine "0<<\"X\""
      expectShow "<<'X'\n$foo\\\nX\n"   "" completeLine "0<<'X'"
      expectShow "<<''\n$foo\\\n\n"     "" completeLine "0<<''"

  describe "hereDocDelimiter" $ do
    context "is a token followed by a newline" $ do
      expectShow "<<X\nX\n" "" completeLine "0<<X"

    context "matches an unquoted token" $ do
      expectShow "<<\\X\nX\n"   "" completeLine "0<<\\X"
      expectShow "<<\"X\"\nX\n" "" completeLine "0<<\"X\""
      expectShow "<<'X'\nX\n"   "" completeLine "0<<'X'"
      expectShow "<<''\n\n"     "" completeLine "0<<''"

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
       in expectFailureEof' "<<-X\nfoo\n" completeLine Hard isExpectedReason 9

    context "accumulates results" $ do
      let t = Token $ (undefined, Unquoted (Char 'E')) :| []
          op = HereDocOp undefined 0 False t
          p = fill $ HereDocT $ fmap return $
            hereDocContent op >> fmap (fmap (snd . unzip)) drainContents
      expectShow           "E\n" "" p "[]"
      expectShow       "EE\nE\n" "" p "[EE\n]"
      expectShow "foo\nbar\nE\n" "" p "[foo\nbar\n]"

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
      let e = runFullInputTesterWithDummyPositions sc defaultAliasName
       in fmap fst e `shouldBe` Right Nothing

    context "does not alias-substitute second token" $ do
      expectShowEof ("foo " ++ defaultAliasName) "" sc $
        "Just foo " ++ defaultAliasName

  describe "pipeSequence" $ do
    let ps = runAliasT $ fill $ NE.toList <$> pipeSequence

    context "can be one simple command" $ do
      expectShowEof "foo bar" "" ps "Just [foo bar]"

    context "can be two simple commands" $ do
      expectShowEof "foo  |  bar" "" ps "Just [foo,bar]"

    context "can be four simple commands" $ do
      expectShowEof "foo|bar|baz|qux" "" ps "Just [foo,bar,baz,qux]"

    context "can have newlines after |" $ do
      expectShowEof "a| \n\\\n \n b" "" ps "Just [a,b]"

    context "cannot have newlines before |" $ do
      expectShowEof "a " "\n|b" ps "Just [a]"

  describe "pipeline" $ do
    let p = runAliasT $ fill pipeline
        p' = runAliasT $ fill pipeline

    context "can start with !" $ do
      expectShowEof "! foo bar " "\n" p "Just ! foo bar"
      expectShowEof "!\t\\\nnew" "" p "Just ! new"

    context "can start without !" $ do
      expectShowEof "foo bar " "\n" p "Just foo bar"
      expectShowEof "\\\nnew" "" p "Just new"

    context "requires command after !" $ do
      expectFailureEof "!"   p  Hard (MissingCommandAfter "!") 1
      expectFailure    "! ;" p' Hard (MissingCommandAfter "!") 2

  describe "conditionalPipeline" $ do
    let cp = runAliasT $ fill conditionalPipeline
        cp' = runAliasT $ fill conditionalPipeline

    context "can start with && followed by pipeline" $ do
      expectShowEof "&&foo" "" cp "Just && foo"
      expectShowEof "&\\\n& ! foo bar |\nbaz" "" cp "Just && ! foo bar | baz"

    context "can start with || followed by pipeline" $ do
      expectShowEof "||foo" "" cp "Just || foo"
      expectShowEof "|\\\n| ! foo bar |\nbaz" "" cp "Just || ! foo bar | baz"

    context "allows linebreak after operator" $ do
      expectShowEof "&& \n\n ! foo" "\n" cp "Just && ! foo"
      expectShowEof "|| \n \n foo" ";" cp "Just || foo"

    context "requires pipeline after operator" $ do
      expectFailureEof "&&"    cp  Hard (MissingCommandAfter "&&") 2
      expectFailure    "||\n;" cp' Hard (MissingCommandAfter "||") 3

    context "must start with operator" $ do
      expectFailure    "foo"   cp' Soft UnknownReason 0
      expectFailure    "! bar" cp' Soft UnknownReason 0
      expectFailureEof ";"     cp  Soft UnknownReason 0

  describe "andOrList" $ do
    let aol = runAliasT $ fill andOrList
        aol' = runAliasT $ fill andOrList

    context "consists of pipelines" $ do
      expectShowEof "foo;" "" aol "Just foo;"
      expectShowEof "foo&&bar||baz;" "" aol "Just foo && bar || baz;"
      expectShowEof "foo \\\n&&! bar ||\nbaz; \t " "" aol
        "Just foo && ! bar || baz;"

    context "can end with &" $ do
      expectShowEof "foo&" "" aol "Just foo&"
      expectShowEof "foo&&bar||baz&" "" aol "Just foo && bar || baz&"
      expectShowEof "foo \\\n&&! bar ||\nbaz& \t " "" aol
        "Just foo && ! bar || baz&"

    context "can end before newline" $ do
      expectShow "foo" "\n" aol' "Just foo;"
      expectShow "foo && bar" "\n" aol' "Just foo && bar;"
      context "cannot have newlines before && or ||" $ do
        expectShowEof "foo" "\n&&bar" aol "Just foo;"
        expectShowEof "foo" "\n||bar" aol "Just foo;"

    context "can end before operators" $ do
      expectShow "foo" ";;" aol' "Just foo;"
      expectShow "foo" "("  aol' "Just foo;"
      expectShow "foo" ")"  aol' "Just foo;"

    context "can end at end of input" $ do
      expectShowEof "foo" "" aol "Just foo;"
      expectShowEof "foo && bar" "" aol "Just foo && bar;"

  describe "completeLine" $ do
    context "can be empty" $ do
      expectShow "\n" "" completeLine ""
      expectShowEof "" "" completeLine ""

    context "can have some and-or lists" $ do
      expectShow "foo\n" "" completeLine "foo"
      expectShow "foo; bar \n" "" completeLine "foo; bar"
      expectShow "foo& bar ;\n" "" completeLine "foo& bar"
      expectShow "foo&bar;baz \n" "" completeLine "foo& bar; baz"
      expectShow "foo&bar;baz & \n" "" completeLine "foo& bar; baz&"

    context "can have preceding whites" $ do
      expectShow " \t\\\n\n" "" completeLine ""
      expectShow " \\\n \t foo\n" "" completeLine "foo"

    context "can end at end-of-file" $ do
      expectShowEof "foo" "" completeLine "foo"
      expectShowEof "foo; bar" "" completeLine "foo; bar"
      expectShowEof "foo; bar&" "" completeLine "foo; bar&"

    context "fails with incomplete line" $ do
      expectFailureEof ";"      completeLine Hard UnknownReason 0
      expectFailureEof "&"      completeLine Hard UnknownReason 0
      expectFailure    "foo;&"  completeLine Hard UnknownReason 4
      expectFailure    "foo("   completeLine Hard UnknownReason 3
      expectFailureEof "foo& ;" completeLine Hard UnknownReason 5
      expectFailureEof "foo;;"  completeLine Hard UnknownReason 3

    context "reparses alias" $ do
      expectShowEof (defaultAliasName ++ "\n") "" completeLine
        defaultAliasValue

    context "fills here document content" $ do
      let f [AndOrList
            (Pipeline (SimpleCommand [] [] [HereDoc _ c] :| _) _) _ _] =
              Just c
          f _ = Nothing
          p = runFullInputTesterWithDummyPositions (f <$> completeLine)

      it "fills empty here document content" $
        fmap fst (p "<<X\nX\n") `shouldBe` Right (Just [])

      it "fills non-empty here document content" $
        fmap (fmap (snd . unzip) . fst) (p "<<X\n\nX\n") `shouldBe`
          Right (Just [Char '\n'])

    context "fails with missing here doc contents" $ do
      let isExpectedReason (MissingHereDocContents
            (HereDocOp _ 0 False d :| [])) | show d == "X" = True
          isExpectedReason _ = False
       in expectFailureEof' "<<X" completeLine Hard isExpectedReason 3

    context "may end with EOF with no here doc contents pending" $ do
      expectShowEof "foo bar" "" completeLine "foo bar"

-- vim: set et sw=2 sts=2 tw=78:
