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

module Flesh.Language.Parser.Syntax_CommandSpec (spec) where

import Data.Foldable (toList)
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "subshell" $ do
    let p = dummyPosition "X"
        s = runAliasT (fill (snd <$> subshell))
        s' = runAliasT (fill (snd <$> subshell))

    context "may have one inner command" $ do
      expectShowEof "(foo)" "" s "Just (foo)"
      expectShowEof "(foo;)" "" s "Just (foo)"
      expectShowEof "(foo\n)" "" s "Just (foo)"

    context "may have three inner commands" $ do
      expectShowEof "(foo;bar&baz)" "" s "Just (foo; bar& baz)"
      expectShowEof "(foo&bar;baz&)" "" s "Just (foo& bar; baz&)"
      expectShowEof "(foo\nbar&\nbaz\n \n )" "" s "Just (foo; bar& baz)"

    context "body can have preceding newlines and whiles" $ do
      expectShowEof "(\nfoo)" "" s "Just (foo)"
      expectShowEof "(\n # \n \n \tfoo)" "" s "Just (foo)"

    context "cannot be empty" $ do
      expectFailureEof "("    s Hard (MissingCommandAfter "(") 1
      expectFailureEof "()"   s Hard (MissingCommandAfter "(") 1
      expectFailureEof "(\n)" s Hard (MissingCommandAfter "(") 2

    context "must be closed by parenthesis" $ do
      expectFailureEof "(foo "   s  Hard (UnclosedSubshell p) 5
      expectFailure    "(foo;})" s' Hard (UnclosedSubshell p) 5

  describe "groupingTail" $ do
    let p = dummyPosition "X"
        g = snd <$> fill (groupingTail p)
        g' = snd <$> fill (groupingTail p)

    context "may have one inner command" $ do
      expectShowEof "foo;}" "" g "{ foo; }"
      expectShowEof "bar\n} " "" g "{ bar; }"

    context "may have three inner commands" $ do
      expectShowEof "foo; bar& baz; }" "" g "{ foo; bar& baz; }"
      expectShowEof "foo& bar; baz& }" "" g "{ foo& bar; baz& }"
      expectShowEof "foo\nbar&\nbaz\n \n }" "" g "{ foo; bar& baz; }"

    context "can have preceding newlines and whiles" $ do
      expectShowEof "\nfoo;}" "" g "{ foo; }"
      expectShowEof "\n # \n \n \tfoo;}" "" g "{ foo; }"

    context "cannot be empty" $ do
      expectFailureEof ""  g Soft (MissingCommandAfter "{") 0
      expectFailureEof "}" g Soft (MissingCommandAfter "{") 0

    context "must be closed by brace" $ do
      expectFailureEof "foo " g  Hard (UnclosedGrouping p) 4
      expectFailure    "foo)" g' Hard (UnclosedGrouping p) 3

  describe "doGrouping" $ do
    let p = dummyPosition "X"
        dg = toList <$> reparse (fill (doGrouping UnclosedDoubleQuote))

    context "may have one inner command" $ do
      expectShowEof "do foo;done" "" dg "foo"
      expectShowEof "do\n\tbar\n done" "" dg "bar"

    context "may have three inner commands" $ do
      expectShowEof "do foo; bar& baz; done" "" dg "foo; bar& baz"
      expectShowEof "do foo& bar; baz& done" "" dg "foo& bar; baz&"
      expectShowEof "do\nfoo\nbar&\nbaz\n \n\tdone" "" dg "foo; bar& baz"

    context "cannot be empty" $ do
      expectFailureEof "do     " dg Hard (MissingCommandAfter "do") 7
      expectFailureEof "do esac" dg Hard (MissingCommandAfter "do") 3
      expectFailureEof "do done" dg Hard (MissingCommandAfter "do") 3

    context "must start with do" $ do
      -- Fails with the reason given above
      expectFailureEof "foo" dg Soft UnclosedDoubleQuote 0
      expectFailureEof "fi" dg Soft UnclosedDoubleQuote 0

    context "must be closed by done" $ do
      expectFailureEof "do foo" dg Hard (MissingDoneForDo p) 6

  describe "whileCommandTail" $ do
    let p = dummyPosition "X"
        w = snd <$> reparse (fill (whileCommandTail p))

    context "may have one condition command" $ do
      expectShowEof "foo;do :;done" "" w "while foo; do :; done"
      expectShowEof "bar\ndo :;done" "" w "while bar; do :; done"

    context "may have three condition commands" $ do
      expectShowEof "foo; bar& baz; do :;done" "" w
        "while foo; bar& baz; do :; done"
      expectShowEof "foo& bar; baz& do :;done" "" w
        "while foo& bar; baz& do :; done"
      expectShowEof "foo\nbar&\nbaz\n \n\tdo :; done" "" w
        "while foo; bar& baz; do :; done"

    context "condition cannot be empty" $ do
      expectFailureEof "    " w Soft (MissingCommandAfter "while") 0
      expectFailureEof "do  " w Soft (MissingCommandAfter "while") 0
      expectFailureEof "done" w Soft (MissingCommandAfter "while") 0

    context "must have do...done" $ do
      expectFailureEof "foo      " w Hard (MissingDoForWhile p) 9
      expectFailureEof "foo; esac" w Hard (MissingDoForWhile p) 5
      expectFailureEof "foo; done" w Hard (MissingDoForWhile p) 5

  describe "untilCommandTail" $ do
    let p = dummyPosition "X"
        u = snd <$> reparse (fill (untilCommandTail p))

    context "may have one condition command" $ do
      expectShowEof "foo;do :;done" "" u "until foo; do :; done"
      expectShowEof "bar\ndo :;done" "" u "until bar; do :; done"

    context "must have do...done" $ do
      expectFailureEof "foo" u Hard (MissingDoForUntil p) 3
    -- Other tests are omitted because they are the same with whileCommandTail

  describe "command" $ do
    let sc = runAliasT $ fill command
        sc' = runAliasT $ fill command

    context "as simple command" $ do
      context "is some tokens" $ do
        expectShowEof "foo" "" sc "Just foo"
        expectShowEof "foo bar" ";" sc "Just foo bar"
        expectShow    "foo  bar\tbaz #X" "\n" sc' "Just foo bar baz"

      context "rejects empty command" $ do
        expectFailureEof ""   sc  Soft UnknownReason 0
        expectFailure    "\n" sc' Soft UnknownReason 0

      it "returns nothing after alias substitution" $
        let e = runFullInputTesterWithDummyPositions sc defaultAliasName
         in fmap fst e `shouldBe` Right Nothing

      context "does not alias-substitute second token" $ do
        expectShowEof ("foo " ++ defaultAliasName) "" sc $
          "Just foo " ++ defaultAliasName

    context "as grouping" $ do
      context "starts with a brace" $ do
        expectShowEof "{ foo\n}" "" sc "Just { foo; }"

      context "does not start with a quoted brace" $ do
        expectShowEof "\\{ foo" "\n}" sc "Just \\{ foo"

      context "can have some redirections" $ do
        expectShowEof "{ foo\n}<bar >baz" "" sc "Just { foo; } 0<bar 1>baz"

    context "as subshell" $ do
      context "starts with a parenthesis" $ do
        expectShowEof "(foo)" "" sc "Just (foo)"

      context "does not start with a quoted parenthesis" $ do
        expectShowEof "\\( foo" "\n)" sc "Just \\( foo"

    context "as while command" $ do
      context "starts with a 'while'" $ do
        expectShowEof "while foo; do bar; done" "" sc
          "Just while foo; do bar; done"

      context "does not start with a quoted 'while'" $ do
        expectShowEof "whi\\le foo" "; do :; done" sc "Just whi\\le foo"

    context "as until command" $ do
      context "starts with a 'until'" $ do
        expectShowEof "until foo; do bar; done" "" sc
          "Just until foo; do bar; done"

      context "does not start with a quoted 'until'" $ do
        expectShowEof "unt\\il foo" "; do :; done" sc "Just unt\\il foo"

-- vim: set et sw=2 sts=2 tw=78:
