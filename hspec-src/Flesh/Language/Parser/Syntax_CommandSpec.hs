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

-- vim: set et sw=2 sts=2 tw=78:
