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

module Flesh.Language.Parser.Syntax_RedirSpec (spec) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Syntax
import Flesh.Language.Parser.TestUtil
import Test.Hspec (Spec, context, describe)

spec :: Spec
spec = do
  describe "redirect" $ do
    context "parses < operator" $ do
      expectPositionEof "29< foo"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "29< foo" "" (fileOpFd     <$> fill redirect) 29
      expectSuccessEof  "29< foo" "" (fileOp       <$> fill redirect) In
      expectShowEof     "29< foo" "" (fileOpTarget <$> fill redirect) "foo"

    context "parses <> operator" $ do
      expectPositionEof "9<> foo"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "9<> foo" "" (fileOpFd     <$> fill redirect) 9
      expectSuccessEof  "9<> foo" "" (fileOp       <$> fill redirect) InOut
      expectShowEof     "9<> foo" "" (fileOpTarget <$> fill redirect) "foo"

    context "parses > operator" $ do
      expectPositionEof "2> foo"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "2> foo" "" (fileOpFd     <$> fill redirect) 2
      expectSuccessEof  "2> foo" "" (fileOp       <$> fill redirect) Out
      expectShowEof     "2> foo" "" (fileOpTarget <$> fill redirect) "foo"

    context "parses >> operator" $ do
      expectPositionEof "29>>foo"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "29>>foo" "" (fileOpFd     <$> fill redirect) 29
      expectSuccessEof  "29>>foo" "" (fileOp       <$> fill redirect) Append
      expectShowEof     "29>>foo" "" (fileOpTarget <$> fill redirect) "foo"

    context "parses >| operator" $ do
      expectPositionEof "29>| bar"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "29>| bar" "" (fileOpFd     <$> fill redirect) 29
      expectSuccessEof  "29>| bar" "" (fileOp       <$> fill redirect) Clobber
      expectShowEof     "29>| bar" "" (fileOpTarget <$> fill redirect) "bar"

    context "parses <& operator" $ do
      expectPositionEof "29<& foo"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "29<& foo" "" (fileOpFd     <$> fill redirect) 29
      expectSuccessEof  "29<& foo" "" (fileOp       <$> fill redirect) DupIn
      expectShowEof     "29<& foo" "" (fileOpTarget <$> fill redirect) "foo"

    context "parses >& operator" $ do
      expectPositionEof "29>& foo"    (fileOpPos    <$> fill redirect) 0
      expectSuccessEof  "29>& foo" "" (fileOpFd     <$> fill redirect) 29
      expectSuccessEof  "29>& foo" "" (fileOp       <$> fill redirect) DupOut
      expectShowEof     "29>& foo" "" (fileOpTarget <$> fill redirect) "foo"

    let yieldDummyContent = HereDocT $
          return () <$ (drainOperators >> yieldContent [])
        rTester = hereDocOp <$> fill (redirect <* yieldDummyContent)

    context "parses << operator" $ do
      expectPositionEof "12<< END"    (hereDocOpPos <$> rTester) 0
      expectSuccessEof  "12<< END" "" (hereDocFd    <$> rTester) 12
      expectSuccessEof  "12<< END" "" (isTabbed     <$> rTester) False
      expectShowEof     "12<< END" "" (delimiter    <$> rTester) "END"

    context "rejects quoted FD" $ do
      expectFailure "1\\2" (fill redirect) Soft UnknownReason 0

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
            hereDocContent op >> fmap (fmap (map snd)) drainContents
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

-- vim: set et sw=2 sts=2 tw=78:
