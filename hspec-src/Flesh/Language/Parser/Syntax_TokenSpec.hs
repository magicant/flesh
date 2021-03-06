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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}

module Flesh.Language.Parser.Syntax_TokenSpec (spec) where

import Data.Foldable (traverse_)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Text (pack, singleton)
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Lex
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Test.Hspec (Spec, context, describe, parallel, it, shouldBe)

spec :: Spec
spec = parallel $ do
  describe "dollarExpansion" $ do
    let reflect s = expectShow s "" (snd <$> dollarExpansion) s

    context "non-braced parameter expansion" $ do
      context "can be a single digit" $ do
        expectSuccess "$1" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '1'
        expectSuccess "$9" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '9'
        expectPosition "$1" (fst <$> dollarExpansion) 0
        expectPosition "$9" (fst <$> dollarExpansion) 0

      context "can be a special parameter" $ do
        expectSuccess "$@" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '@'
        expectSuccess "$*" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '*'
        expectSuccess "$#" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '#'
        expectSuccess "$?" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '?'
        expectSuccess "$-" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '-'
        expectSuccess "$$" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '$'
        expectSuccess "$!" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '!'
        expectSuccess "$0" "" (snd <$> dollarExpansion) $
          Parameter $ singleton '0'
        expectPosition "$@" (fst <$> dollarExpansion) 0
        expectPosition "$*" (fst <$> dollarExpansion) 0
        expectPosition "$#" (fst <$> dollarExpansion) 0
        expectPosition "$?" (fst <$> dollarExpansion) 0
        expectPosition "$-" (fst <$> dollarExpansion) 0
        expectPosition "$$" (fst <$> dollarExpansion) 0
        expectPosition "$!" (fst <$> dollarExpansion) 0
        expectPosition "$0" (fst <$> dollarExpansion) 0

      context "can be a POSIX name" $ do
        expectSuccessEof "$name" "" (snd <$> dollarExpansion) $
          Parameter $ pack "name"
        expectSuccess "$name" ";" (snd <$> dollarExpansion) $
          Parameter $ pack "name"
        expectSuccess "$name" "}" (snd <$> dollarExpansion) $
          Parameter $ pack "name"
        expectSuccessEof "$X" "" (snd <$> dollarExpansion) $
          Parameter $ pack "X"
        expectSuccessEof "$_" "" (snd <$> dollarExpansion) $
          Parameter $ pack "_"
        expectSuccessEof "$foo0_name" "" (snd <$> dollarExpansion) $
          Parameter $ pack "foo0_name"
        expectPositionEof "$name" (fst <$> dollarExpansion) 0

    context "command substitution" $ do
      context "can be empty" $ do
        expectSuccess "$()" "" (snd <$> dollarExpansion) $
          Flesh.Language.Parser.Syntax.CommandSubstitution []
        expectPosition "$()" (fst <$> dollarExpansion) 0
        reflect "$( \t#\\\n)"
        expectPosition "$( \t#\\\n)" (fst <$> dollarExpansion) 0
        reflect "$( \t#\\\n \t)"
        reflect "$( \t\\\n\\\n)"
        reflect "$( \t\n\\\n)"
        reflect "$(\\\n\\\n)"

      context "can contain some commands" $ do
        traverse_ reflect
          ["$( foo )", "$(foo ||\nbar &)", "$(\nls\n )",
           "$(:\\\n)", "$(:\n\\\n)"]

      context "can contain here documents" $ do
        reflect "$(<<END\n1\n2\n3\nEND\n)"

      context "can contain escapes" $ do
        reflect "$(\\(\\))"

      context "ignores aliases" $ do
        reflect $ "$(" ++ defaultAliasName ++ ")"

      context "requires valid program" $ do
        expectFailure "$(! )" dollarExpansion Hard (MissingCommandAfter "!") 4

      context "can have line continuations after $" $ do
        expectShow "$\\\n\\\n()" "" (snd <$> dollarExpansion) "$()"

      context "must be closed" $ do
        expectFailureEof "$(X" dollarExpansion
          Hard UnclosedCommandSubstitution 1

    context "arithmetic expansion" $ do
      let isArithmetic (Arithmetic _) = True
          isArithmetic _ = False
          expectArithmetic s =
            expectSuccess s "" (isArithmetic . snd <$> dollarExpansion)
          reflectArithmetic s = do
            reflect s
            expectArithmetic s True

      context "can be empty" $ do
        reflectArithmetic "$(())"
        expectPosition "$(())" (fst <$> dollarExpansion) 0
        reflectArithmetic "$(( \t#))"
        reflectArithmetic "$(( \t# \t))"
        expectPosition "$(( \t#))" (fst <$> dollarExpansion) 0

      context "can contain expressions" $ do
        reflectArithmetic "$((1))"
        reflectArithmetic "$((1.0))"
        reflectArithmetic "$((1.0 + 2.0 * 3.5))"

      context "can contain command substitutions" $ do
        reflectArithmetic "$(( $(:) ))"

      context "can contain arithmetic expansions" $ do
        reflectArithmetic "$(( $((1 + 2)) ))"

      context "can contain parentheses" $ do
        reflectArithmetic "$(((2 + 3)))"
        reflectArithmetic "$((1 *(2 + 3)/ 4))"
        reflectArithmetic "$((1 * (2 + (3 - 4))))"

      context "can contain line continuations" $ do
        let reflectArithmeticLC s t = do
              expectShow s "" (snd <$> dollarExpansion) t
              expectArithmetic s True
        reflectArithmeticLC "$\\\n(\\\n(\\\n)\\\n)" "$(())"
        reflectArithmeticLC "$((\\\n1\\\n+\\\n2))" "$((1+2))"
        reflectArithmeticLC "$((\\\n$((1))))" "$(($((1))))"

      context "falls back to command substitution" $ do
        expectArithmetic "$((cat);)" False
        expectArithmetic "$((cat); (cat))" False
        expectArithmetic "$(:;(cat))" False

  describe "backquoteExpansion" $ do
    let bq_ = backquoteExpansion undefined
        bqx = backquoteExpansion (const False)
        bq = backquoteExpansion (`elem` "\\\"$`")

    context "parses empty quotes" $ do
      expectSuccess "``" "" (snd <$> bq_) (Backquoted "")
      expectPosition "``" (fst <$> bq_) 0

    context "parses some characters" $ do
      expectSuccess "`a`" "" (snd <$> bq_) (Backquoted "a")
      expectSuccess "`''`" "" (snd <$> bq_) (Backquoted "''")
      expectSuccess "` \t\n`" "" (snd <$> bq_) (Backquoted " \t\n")

    context "ignores line continuations" $ do
      let s = "\\\n\\\n`\\\nA\\\n\\\nBC\\\n`" 
      expectSuccess s "" (snd <$> bq_) (Backquoted "ABC")
      expectPosition s (fst <$> bq_) (fromJust (elemIndex '`' s))

    context "can contain escaped characters" $ do
      expectSuccess "`\\a\\\\\\b\\$\\c\\%\\d\\\n\\e\\`" "\\z`" (snd <$> bqx)
        (Backquoted "\\a\\\\\\b\\$\\c\\%\\d\\e\\")
      expectSuccess "`\\a\\\\\\b\\$\\c\\%\\d\\\n\\e\\`\\z`" "" (snd <$> bq)
        (Backquoted "\\a\\\\b$\\c\\%\\d\\e`\\z")

    context "fails on unclosed quotes" $ do
      let bq_' = backquoteExpansion undefined
          bq' = backquoteExpansion (`elem` "\\\"$`")
      expectFailureEof "`" bq_' Hard UnclosedCommandSubstitution 0
      expectFailureEof "`x" bq_' Hard UnclosedCommandSubstitution 0
      expectFailureEof "`\\" bq_' Hard UnclosedCommandSubstitution 0
      expectFailureEof "`\\`" bq' Hard UnclosedCommandSubstitution 0

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
      expectFailureEof "\"" doubleQuote Hard UnclosedDoubleQuote 0
      expectFailureEof "\"x" doubleQuote Hard UnclosedDoubleQuote 0
      expectFailureEof "\"\\" doubleQuote Hard UnclosedDoubleQuote 0
      expectFailureEof "\"\\\"" doubleQuote Hard UnclosedDoubleQuote 0

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
      expectFailureEof "'" singleQuote Hard UnclosedSingleQuote 0
      expectFailureEof "'x" singleQuote Hard UnclosedSingleQuote 0
      expectFailureEof "'\\\\" singleQuote Hard UnclosedSingleQuote 0

  describe "tokenTill" $ do
    context "parses some word units" $ do
      expectShowEof "\\\nabc\\x\"d\"'s'" "" (tokenTill eof) "abc\\x\"d\"'s'"
      expectShow "a\\\nX" "" (tokenTill (lc (char 'X'))) "a"

    context "rejects empty token" $ do
      expectFailure "\\\n)" (tokenTill (lc (char ')'))) Soft UnknownReason 0

  describe "identifiedToken" $ do
    let ik f a a' = snd <$> identifiedToken f a a'
        ikReserved r = ik r True True
        ikAllReserved = ikReserved (const True)
        ikNoReserved = ikReserved (const False)
        ikNoReserved' = ikReserved (const False)
        ikAlias a = ik (const False) a True
        ikAssignment a = ik (const False) True a

    context "returns current position" $ do
      expectPosition "foo;" (fst <$> identifiedToken (const True) True True) 0

    context "any token can be identified as reserved if accepted" $ do
      expectShow "foo" ";" ikAllReserved "Reserved \"foo\""
      expectShow "if" ";"  ikAllReserved "Reserved \"if\""
      expectShow "w" ";"   (ikReserved (== pack "w")) "Reserved \"w\""

    context "no token can be identified as reserved if rejected" $ do
      expectShow "foo" ";" ikNoReserved "Normal foo"
      expectShow "if" ";"  ikNoReserved "Normal if"
      expectShow "w" ";"   (ikReserved (/= pack "w")) "Normal w"

    context "quoted tokens are not identified as reserved" $ do
      expectShow "f\\oo" ";" ikAllReserved "Normal f\\oo"
      expectShow "f'o'o" ";" ikAllReserved "Normal f'o'o"
      expectShow "f\"o\"o" ";" ikAllReserved "Normal f\"o\"o"

    context "doesn't perform alias substitution on reserved words" $ do
      expectShow defaultAliasName ";" ikAllReserved $
        "Reserved \"" ++ defaultAliasName ++ "\""

    context "modifies pending input on alias substitution" $ do
      return () -- tested in "stops alias substitution on recursion" below

    it "returns nothing after alias substitution" $
      let e = runReparseFullInputTester ikNoReserved' defaultAliasName
       in e `shouldBe` Right Nothing

    it "stops alias substitution on recursion" $
      let e = runFullInputTesterWithDummyPositions
                ((,) <$> ikNoReserved' <*> readAll) defaultAliasName
          f ((l, r), _) = (show l, r)
          ex = "Normal " ++ defaultAliasName
       in fmap f e `shouldBe` Right (ex, "--color")

    it "stops alias substitution on exact recursion" $
      let e = runFullInputTesterWithDummyPositions
                ((,) <$> ikNoReserved' <*> readAll) recursiveAlias
          f ((l, r), _) = (show l, r)
          ex = "Normal " ++ recursiveAlias
       in fmap f e `shouldBe` Right (ex, "")

    context "doesn't perform alias substitution if disabled" $ do
      expectShow recursiveAlias ";" (ikAlias False) $
        "Normal " ++ recursiveAlias ++ ""

    context "performs alias substitution after blank-ending substitution" $ do
      return () -- should be tested elsewhere

    context "identifies assignment" $ do
      expectShow "a=" ";" ikNoReserved "Assignment a="
      expectShow "abc=" ";" ikNoReserved "Assignment abc="
      expectShow "a=xyz" ";" ikNoReserved "Assignment a=xyz"
      --expectShow "foo=b'a'r" ";" ikNoReserved "Assignment foo=b'a'r"
      --expectShow "`a b`=" ";" ikNoReserved "Assignment `a b`="

    context "rejects empty assignment name" $ do
      expectShow "=a" ";" ikNoReserved "Normal =a"

    context "rejects quoted equal as assignment" $ do
      expectShow "a\\=" ";" ikNoReserved "Normal a\\="
      expectShow "a'='" ";" ikNoReserved "Normal a'='"
      expectShow "a\"=\"" ";" ikNoReserved "Normal a\"=\""

    context "doesn't identify assignment if disabled" $ do
      expectShow "a=" ";" (ikAssignment False) "Normal a="

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
