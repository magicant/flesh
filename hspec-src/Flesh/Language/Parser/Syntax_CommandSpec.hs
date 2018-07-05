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
import Data.List.NonEmpty (NonEmpty((:|)))
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec (Spec, context, describe, parallel, it, shouldBe)

spec :: Spec
spec = parallel $ do
  describe "subshell" $ do
    let p = dummyPosition "X"
        s = fill (snd <$> subshell)
        s' = fill (snd <$> subshell)

    context "may have one inner command" $ do
      expectShowEof "(foo)" "" s "(foo)"
      expectShowEof "(foo;)" "" s "(foo)"
      expectShowEof "(foo\n)" "" s "(foo)"

    context "may have three inner commands" $ do
      expectShowEof "(foo;bar&baz)" "" s "(foo; bar& baz)"
      expectShowEof "(foo&bar;baz&)" "" s "(foo& bar; baz&)"
      expectShowEof "(foo\nbar&\nbaz\n \n )" "" s "(foo; bar& baz)"

    context "body can have preceding newlines and whiles" $ do
      expectShowEof "(\nfoo)" "" s "(foo)"
      expectShowEof "(\n # \n \n \tfoo)" "" s "(foo)"

    context "cannot be empty" $ do
      expectFailureEof "("    s Hard (MissingCommandAfter "(") 1
      expectFailureEof "()"   s Hard (MissingCommandAfter "(") 1
      expectFailureEof "(\n)" s Hard (MissingCommandAfter "(") 2

    context "must be closed by parenthesis" $ do
      expectFailureEof "(foo "   s  Hard (UnclosedSubshell p) 5
      expectFailure    "(foo;})" s' Hard (UnclosedSubshell p) 5

  describe "braceGroupTail" $ do
    let p = dummyPosition "X"
        g = snd <$> fill (braceGroupTail p)
        g' = snd <$> fill (braceGroupTail p)

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

  describe "doGroup" $ do
    let p = dummyPosition "X"
        dg = toList <$> fill (doGroup UnclosedDoubleQuote)

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

  describe "forClauseTail" $ do
    let p = dummyPosition "X"
        f = snd <$> fill (forClauseTail p)

    context "simplest form" $ do
      expectShowEof "var do :; done" "" f "for var do :; done"

    context "with separator and newlines" $ do
      expectShowEof "foo \t\ndo :; done" "" f "for foo do :; done"
      expectShowEof "foo ; do :; done" "" f "for foo do :; done"
      expectShowEof "foo ; \n \n do :; done" "" f "for foo do :; done"

    context "with words followed by semicolon" $ do
      expectShowEof "v in;do :; done" "" f "for v in ; do :; done"
      expectShowEof "v in a; do :; done" "" f "for v in a; do :; done"
      expectShowEof "v in a b c;do :; done" "" f "for v in a b c; do :; done"
      expectShowEof "v in do;do :; done" "" f "for v in do; do :; done"

    context "with words followed by newlines" $ do
      expectShowEof "v in\n\n\tdo :; done" "" f "for v in ; do :; done"
      expectShowEof "v in a\n  do :; done" "" f "for v in a; do :; done"

    context "with words followed by semicolon and newlines" $ do
      expectShowEof "v in a; \n\n do :; done" "" f "for v in a; do :; done"

    context "with words preceded by newlines" $ do
      expectShowEof "x\nin;do :; done" "" f "for x in ; do :; done"
      expectShowEof "x \n\n in a;do :; done" "" f "for x in a; do :; done"

    context "must have name" $ do
      expectFailureEof "" f Soft MissingNameAfterFor 0

    context "must have do...done; without semicolon or in" $ do
      expectFailureEof "x" f Hard (MissingDoForFor p) 1
      expectFailureEof "x \n" f Hard (MissingDoForFor p) 3

    context "must have do...done; after semicolon" $ do
      expectFailureEof "x;" f Hard (MissingDoForFor p) 2
      expectFailureEof "x; ?" f Hard (MissingDoForFor p) 3
      expectFailureEof "x;\n ?" f Hard (MissingDoForFor p) 4

    context "must have do...done; after in" $ do
      expectFailureEof "x in" f Hard (MissingDoForFor p) 4
      expectFailureEof "x in a " f Hard (MissingDoForFor p) 7
      expectFailureEof "x in a\n " f Hard (MissingDoForFor p) 8

    context "must have do...done; after in and semicolon" $ do
      expectFailureEof "x in;" f Hard (MissingDoForFor p) 5
      expectFailureEof "x in a; " f Hard (MissingDoForFor p) 8
      expectFailureEof "x in a;\n " f Hard (MissingDoForFor p) 9

    context "cannot have semicolon before in" $ do
      expectFailureEof "x; in;do :;done" f Hard SemicolonBeforeIn 1
      expectFailureEof "x;\nin;do :;done" f Hard SemicolonBeforeIn 1

    context "cannot have semicolon after newline" $ do
      expectFailureEof "x\n;do :;done" f Hard LineBeginningWithSemicolon 2

  describe "ifClauseTail" $ do
    let p = dummyPosition "X"
        ps = iterate next p
        i = fill $ ifClauseTail p
        i1 = fst <$> i
        i2 = snd <$> i

    context "minimum structure" $ do
      expectSuccessEof "a; then b; fi" "" i1 p
      expectShowEof "a; then b; fi" "" i2 "if a; then b; fi"
      expectShowEof "a1; a2& then b1; b2& fi" "" i2
        "if a1; a2& then b1; b2& fi"

    context "with else" $ do
      expectShowEof "a; then b; else z; fi" "" i2 "if a; then b; else z; fi"
      expectShowEof "a1; a2& then b1; b2& else z1; z2& fi" "" i2
        "if a1; a2& then b1; b2& else z1; z2& fi"

    context "with 1 elif" $ do
      expectShowEof "a; then b; elif c; then d; fi" "" i2
        "if a; then b; elif c; then d; fi"
      expectShowEof "a1; a2& then b1; b2& elif c1; c2& then d1; d2& fi" "" i2
        "if a1; a2& then b1; b2& elif c1; c2& then d1; d2& fi"

    context "with 3 elif" $ do
      expectShowEof
        "a; then b; elif c; then d; elif e; then f; elif g; then h& fi" "" i2
        "if a; then b; elif c; then d; elif e; then f; elif g; then h& fi"

    context "with elif and else" $ do
      expectShowEof "a; then b; elif c; then d; else z& fi" "" i2
        "if a; then b; elif c; then d; else z& fi"

    context "missing command after if" $ do
      expectFailureEof "" i2 Soft (MissingCommandAfter "if") 0
      expectFailureEof "then b; fi" i2 Soft (MissingCommandAfter "if") 0

    context "missing then after if" $ do
      expectFailureEof ":" i2 Soft (MissingThenForIf p) 1
      expectFailureEof ":; fi" i2 Soft (MissingThenForIf p) 3

    context "missing command after then" $ do
      expectFailureEof ":; then" i2 Soft (MissingCommandAfter "then") 7
      expectFailureEof ":; then fi" i2 Soft (MissingCommandAfter "then") 8
      expectFailureEof ":; then else :; fi"
        i2 Soft (MissingCommandAfter "then") 8

    context "missing command after elif" $ do
      expectFailureEof ":; then :; elif"
        i2 Hard (MissingCommandAfter "elif") 15
      expectFailureEof ":; then :; elif then :; fi"
        i2 Hard (MissingCommandAfter "elif") 16

    context "missing then after elif" $ do
      expectFailureEof ":; then :; elif :"
        i2 Hard (MissingThenForElif (ps !! 11)) 17
      expectFailureEof ":; then :; elif :; fi"
        i2 Hard (MissingThenForElif (ps !! 11)) 19

    context "missing command after then after elif" $ do
      expectFailureEof ":; then :; elif :; then"
        i2 Hard (MissingCommandAfter "then") 23
      expectFailureEof ":; then :; elif :; then fi"
        i2 Hard (MissingCommandAfter "then") 24

    context "missing command after else" $ do
      expectFailureEof ":; then :; else"
        i2 Hard (MissingCommandAfter "else") 15
      expectFailureEof ":; then :; else fi"
        i2 Hard (MissingCommandAfter "else") 16

    context "missing fi" $ do
      expectFailureEof ":; then :" i2 Soft (MissingFiForIf p) 9
      expectFailureEof ":; then :; }" i2 Soft (MissingFiForIf p) 11

  describe "whileClauseTail" $ do
    let p = dummyPosition "X"
        w = snd <$> fill (whileClauseTail p)

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

  describe "untilClauseTail" $ do
    let p = dummyPosition "X"
        u = snd <$> fill (untilClauseTail p)

    context "may have one condition command" $ do
      expectShowEof "foo;do :;done" "" u "until foo; do :; done"
      expectShowEof "bar\ndo :;done" "" u "until bar; do :; done"

    context "must have do...done" $ do
      expectFailureEof "foo" u Hard (MissingDoForUntil p) 3
    -- Other tests are omitted because they are the same with whileClauseTail

  describe "bourneFunctionDefinitionTail " $ do
    let p = dummyPosition "X"
        t1 = Token $ (p, Unquoted (Char 'f')) :| []
        t2 = Token $ (p, Unquoted (Char '@')) :| []
        f1 = fill $ bourneFunctionDefinitionTail t1
        f2 = fill $ bourneFunctionDefinitionTail t2

    context "simplest form" $ do
      expectShowEof "(){ :;}" "" f1 "f() { :; }"

    context "with whites and linebreak" $ do
      expectShowEof "( \t) \t#foo\n\\\n# \n {\n:; } #" "\n" f1 "f() { :; }"

    context "redirections in body" $ do
      expectShowEof "(){ :;}<a>b" "" f1 "f() { :; } 0<a 1>b"

    context "consumes only one command as the body" $ do
      expectShowEof "(){ :;}" ";:" f1 "f() { :; }"

    context "must start with (" $ do
      expectFailureEof "&" f1 Soft UnknownReason 0
      expectFailureEof ")" f1 Soft UnknownReason 0
      expectFailureEof "&" f2 Soft UnknownReason 0
      expectFailureEof ")" f2 Soft UnknownReason 0

    context "must have ) after (" $ do
      expectFailureEof "(&" f1 Hard MissingRightParenInFunction 1
      expectFailureEof "(X" f1 Hard MissingRightParenInFunction 1
      expectFailureEof "( " f1 Hard MissingRightParenInFunction 2
      expectFailureEof "(&" f2 Hard MissingRightParenInFunction 1
      expectFailureEof "(X" f2 Hard MissingRightParenInFunction 1
      expectFailureEof "( " f2 Hard MissingRightParenInFunction 2

    context "must have a POSIX name" $ do
      expectFailureEof "()" f2 Hard (InvalidFunctionName t2) 0

    context "must have a body" $ do
      expectFailureEof "() #" f1 Hard MissingFunctionBody 4

  describe "command" $ do
    let sc = fill command
        sc' = fill command

    context "as simple command" $ do
      context "cannot be empty" $ do
        expectFailureEof ""   sc  Soft UnknownReason 0
        expectFailure    "\n" sc' Soft UnknownReason 0

      context "can be some tokens" $ do
        expectShowEof "foo" "" sc "foo"
        expectShowEof "foo bar" ";" sc "foo bar"
        expectShow    "foo  bar\tbaz #X" "\n" sc' "foo bar baz"

      context "can be assignments" $ do
        expectShowEof "a=" "" sc "a="
        expectShowEof "a='' b=B" "" sc "a='' b=B"
        expectShowEof "a='' b=~ _=$()" "" sc "a='' b=~ _=$()"

      context "can be some tokens following assignments" $ do
        expectShowEof "a= foo" "" sc "a= foo"
        expectShowEof "a= b=B foo bar" "" sc "a= b=B foo bar"
        expectShowEof "a= b=B foo bar z=" "" sc "a= b=B foo bar z="

      context "can be redirections" $ do
        expectShowEof ">a" "" sc "1>a"
        expectShowEof "<a >b" "" sc "0<a 1>b"
        expectShowEof "<a 2>b >c" "" sc "0<a 2>b 1>c"

      context "can have redirections anywhere" $ do
        expectShowEof ">a foo" "" sc "foo 1>a"
        expectShowEof "foo <a" "" sc "foo 0<a"
        expectShowEof ">a f=o" "" sc "f=o 1>a"
        expectShowEof "f=o <a" "" sc "f=o 0<a"
        expectShowEof "foo <a bar" "" sc "foo bar 0<a"
        expectShowEof "foo bar <a" "" sc "foo bar 0<a"
        expectShowEof "f=o <a bar" "" sc "f=o bar 0<a"
        expectShowEof "f=o bar <a" "" sc "f=o bar 0<a"
        expectShowEof "f=o <a b=r" "" sc "f=o b=r 0<a"
        expectShowEof "f=o b=r <a" "" sc "f=o b=r 0<a"

      it "returns nothing after alias substitution" $
        let e = runReparseFullInputTester sc defaultAliasName
         in e `shouldBe` Right Nothing

      context "does not alias-substitute second token" $ do
        let t = "foo " ++ defaultAliasName
        expectShowEof t "" sc t
          

    context "as grouping" $ do
      context "starts with a brace" $ do
        expectShowEof "{ foo\n}" "" sc "{ foo; }"

      context "does not start with a quoted brace" $ do
        expectShowEof "\\{ foo" "\n}" sc "\\{ foo"

      context "can have some redirections" $ do
        expectShowEof "{ foo\n}<bar >baz" "" sc "{ foo; } 0<bar 1>baz"

    context "as subshell" $ do
      context "starts with a parenthesis" $ do
        expectShowEof "(foo)" "" sc "(foo)"

      context "does not start with a quoted parenthesis" $ do
        expectShowEof "\\( foo" "\n)" sc "\\( foo"

    context "as while command" $ do
      context "starts with a 'while'" $ do
        expectShowEof "while foo; do bar; done" "" sc
          "while foo; do bar; done"

      context "does not start with a quoted 'while'" $ do
        expectShowEof "whi\\le foo" "; do :; done" sc "whi\\le foo"

    context "as until command" $ do
      context "starts with a 'until'" $ do
        expectShowEof "until foo; do bar; done" "" sc
          "until foo; do bar; done"

      context "does not start with a quoted 'until'" $ do
        expectShowEof "unt\\il foo" "; do :; done" sc "unt\\il foo"

    context "as Bourne-style function definition" $ do
      context "starts with a name followed by a '('" $ do
        expectShowEof "foo()(:)" "" sc "foo() (:)"

      context "does not start with a reserved word" $ do
        expectFailureEof "in()(:)" sc Soft UnknownReason 0

      context "does not start with an assignment" $ do
        expectShowEof "a=1" "()(:)" sc "a=1"

-- vim: set et sw=2 sts=2 tw=78:
