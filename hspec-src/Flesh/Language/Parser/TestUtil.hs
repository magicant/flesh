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

module Flesh.Language.Parser.TestUtil where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Flesh.Language.Parser.Error
--import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

type Tester = ParserT
  (StateT PositionedString (ExceptT (Severity, Error) Identity))

runTester :: Tester a -> PositionedString
          -> Either (Severity, Error) (a, PositionedString)
runTester parser = runIdentity . runExceptT . runStateT (runParserT parser)

-- | @expectSuccessEof consumed lookahead parser result@ runs the given
-- @parser@ for the source code @consumed ++ lookahead@ and tests if the
-- expected @result@ is returned and if the expected @consumed@ part of the
-- code is actually consumed.
expectSuccessEof :: (Eq a, Show a) =>
  String -> String -> Tester a -> a -> SpecWith ()
expectSuccessEof consumed lookahead parser result =
  let s = consumed ++ lookahead
      s' = spread (dummyPosition s) s
      e = runTester parser s'
   in context s $ do
     it "returns expected result successfully" $
       fmap fst e `shouldBe` Right result

     it "consumes expected part of source code" $
       fmap snd e `shouldBe` Right (dropP (length consumed) s')

-- | Like 'expectSuccessEof', but tries many arbitrary remainders.
expectSuccess :: (Eq a, Show a) =>
  String -> String -> Tester a -> a -> SpecWith ()
expectSuccess consumed lookahead parser result =
  context (consumed ++ lookahead ++ "...") $
    prop "returns expected result and state" $ \remainder ->
      let s = consumed ++ lookahead ++ remainder
          s' = spread (dummyPosition s) s
          e = runTester parser s'
       in e === Right (result, dropP (length consumed) s')

-- | @expectPositionEof input parser expectedPositionIndex@ runs the given
-- @parser@ for the given @input@ and tests if the result is a position at the
-- given index withing the input.
expectPositionEof :: String -> Tester Position -> Int -> SpecWith ()
expectPositionEof input parser expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $ do
     it "returns expected position" $
       fmap fst e `shouldBe` Right expectedPosition

-- | Like 'expectPositionEof', but tries many arbitrary remainders.
expectPosition :: String -> Tester Position -> Int -> SpecWith ()
expectPosition input parser expectedPositionIndex =
  context (input ++ "...") $
    prop "returns expected position" $ \remainder ->
      let s = input ++ remainder
          s' = spread (dummyPosition s) s
          e = runTester parser s'
          expectedPosition = headPosition (dropP expectedPositionIndex s')
       in fmap fst e === Right expectedPosition

-- | Like 'expectSuccessEof', but compares string representation of the result
-- with the given expected string.
expectShowEof :: (Eq a, Show a) =>
  String -> String -> Tester a -> String -> SpecWith ()
expectShowEof consumed lookahead parser =
  expectSuccessEof consumed lookahead (show <$> parser)

-- | Like 'expectShowEof', but tries many arbitrary remainders.
expectShow :: (Eq a, Show a) =>
  String -> String -> Tester a -> String -> SpecWith ()
expectShow consumed lookahead parser =
  expectSuccess consumed lookahead (show <$> parser)

-- | @expectFailureEof input parser severity reason position@ runs the given
-- @parser@ for the given @input@ and tests if it fails for the expected error
-- of the given @severity@, @reason@, and @position@.
--
-- Type parameter @a@ needs to be 'Show' and 'Eq'. Map to @()@ if you want to
-- apply to a non-Show or non-Eq @a@.
expectFailureEof :: (Eq a, Show a) =>
  String -> Tester a -> Severity -> Reason -> Int -> SpecWith ()
expectFailureEof input parser s r expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $ do
     it "fails" $
       e `shouldBe` Left (s, Error r expectedPosition)

-- vim: set et sw=2 sts=2 tw=78:
