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

module Flesh.Language.Parser.Syntax_ListSpec (spec) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "pipeSequence" $ do
    let ps = runAliasT $ fill $ toList <$> pipeSequence
        ps' = runAliasT $ fill $ toList <$> pipeSequence

    context "can be one simple command" $ do
      expectShowEof "foo bar" "" ps "Just [foo bar]"

    context "can be two simple commands" $ do
      expectShowEof "foo  |  bar" "" ps "Just [foo,bar]"

    context "can be four simple commands" $ do
      expectShowEof "foo|bar|baz|qux" "" ps "Just [foo,bar,baz,qux]"

    context "can have newlines after |" $ do
      expectShowEof "a| \n\\\n \n b" "" ps "Just [a,b]"

    context "cannot have newlines before |" $ do
      expectShow "a " "\n|b" ps' "Just [a]"

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
      expectFailure    "! )" p' Hard (MissingCommandAfter "!") 2

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
      expectShow    "&& \n\n ! foo" "\n" cp' "Just && ! foo"
      expectShowEof "|| \n \n foo" ";" cp "Just || foo"

    context "requires pipeline after operator" $ do
      expectFailureEof "&&"    cp  Hard (MissingCommandAfter "&&") 2
      expectFailure    "||\n)" cp' Hard (MissingCommandAfter "||") 3

    context "must start with operator" $ do
      expectFailure    "foo"   cp' Soft UnknownReason 0
      expectFailure    "! bar" cp' Soft UnknownReason 0
      expectFailureEof ";"     cp  Soft UnknownReason 0

  describe "andOrList" $ do
    let aol = fmap (fmap ($ False)) $ runAliasT $ fill andOrList
        aol' = fmap (fmap ($ False)) $ runAliasT $ fill andOrList
        aol'' = fmap (fmap ($ True)) $ runAliasT $ fill andOrList

    context "consists of pipelines" $ do
      expectShowEof "foo" ";" aol "Just foo;"
      expectShowEof "foo&&bar||baz" ";" aol "Just foo && bar || baz;"
      expectShowEof "foo \\\n&&! bar ||\nbaz" "&" aol
        "Just foo && ! bar || baz;"

    context "takes asynchronicity parameter" $ do
      expectShowEof "foo" ";" aol'' "Just foo&"

    context "can end before operators" $ do
      expectShow "foo" ";;" aol' "Just foo;"
      expectShow "foo" "("  aol' "Just foo;"
      expectShow "foo" ")"  aol' "Just foo;"

    context "can end at end of input" $ do
      expectShowEof "foo" "" aol "Just foo;"
      expectShowEof "foo && bar" "" aol "Just foo && bar;"

  describe "compoundList" $ do
    let cl = runAliasT $ fill $ toList <$> compoundList
        cl' = runAliasT $ fill $ toList <$> compoundList

    context "is not empty" $ do
      expectShow "foo" ";;" cl' "Just foo"
      expectFailure    ";;" cl' Soft UnknownReason 0
      expectFailureEof ""   cl  Soft UnknownReason 0
      expectFailureEof "  " cl  Soft UnknownReason 0

    context "is some and-or lists" $ do
      expectShowEof "foo; bar"       "" cl "Just foo; bar"
      expectShowEof "foo; bar& baz;" "" cl "Just foo; bar& baz"

    context "spans multiple lines" $ do
      expectShowEof "foo\nbar"                 "" cl "Just foo; bar"
      expectShowEof "foo&\nbar"                "" cl "Just foo& bar"
      expectShowEof "foo #X\n #comment\n\tbar" "" cl "Just foo; bar"
      expectShowEof "a;b\nc&d"                 "" cl "Just a; b; c& d"

    context "can have preceding newlines and whites" $ do
      expectShowEof "\nfoo"              "" cl "Just foo"
      expectShowEof "\n \tfoo"           "" cl "Just foo"
      expectShowEof "\n \t#comment\nfoo" "" cl "Just foo"
      expectShowEof "\n\n\nfoo"          "" cl "Just foo"

    context "can have trailing newlines and whites" $ do
      expectShowEof "foo\n"              ""   cl  "Just foo"
      expectShow    "foo\n"              ";;" cl' "Just foo"
      expectShowEof "foo\n\t "           ""   cl  "Just foo"
      expectShowEof "foo\n\t #comment\n" ""   cl  "Just foo"
      expectShow    "foo\n\n\n"          ";;" cl' "Just foo"

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
      expectFailure    "foo;& " completeLine Hard UnknownReason 4
      expectFailure    "foo("   completeLine Hard UnknownReason 3
      expectFailureEof "foo& ;" completeLine Hard UnknownReason 5
      expectFailureEof "foo;;"  completeLine Hard UnknownReason 3

    context "reparses alias" $ do
      expectShow (defaultAliasName ++ "\n") "" completeLine defaultAliasValue

    context "fills here document content" $ do
      let f [AndOrList
            (Pipeline (SimpleCommand [] [] [HereDoc _ c] :| _) _) _ _] =
              Just c
          f _ = Nothing
          p = runOverrunTesterWithDummyPositions (f <$> completeLine)

      it "fills empty here document content" $
        fmap (fmap fst) (p "<<X\nX\n") `shouldBe` Just (Right (Just []))

      it "fills non-empty here document content" $
        fmap (fmap (fmap (snd . unzip) . fst)) (p "<<X\n\nX\n") `shouldBe`
          Just (Right (Just [Char '\n']))

    context "fails with missing here doc contents" $ do
      let isExpectedReason (MissingHereDocContents
            (HereDocOp _ 0 False d :| [])) | show d == "X" = True
          isExpectedReason _ = False
       in expectFailureEof' "<<X" completeLine Hard isExpectedReason 3

    context "may end with EOF with no here doc contents pending" $ do
      expectShowEof "foo bar" "" completeLine "foo bar"

-- vim: set et sw=2 sts=2 tw=78:
