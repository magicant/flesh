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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Flesh.Language.Parser.TestUtil where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Flesh.Language.Alias as Alias
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype Overrun a = Overrun {runOverrun :: ExceptT Failure Maybe a}

instance Functor Overrun where
  fmap f = Overrun . fmap f . runOverrun

instance Applicative Overrun where
  pure = Overrun . pure
  Overrun a <*> Overrun b = Overrun (a <*> b)

instance Monad Overrun where
  Overrun a >>= f = Overrun (a >>= runOverrun . f)
  Overrun a >> Overrun b = Overrun (a >> b)

instance MonadError Failure Overrun where
  throwError = Overrun . throwError
  catchError (Overrun m) f = Overrun (catchError m (runOverrun . f))

instance MonadInput (StateT [Positioned Char] Overrun) where
  popChar = do
    cs <- get
    case cs of
      [] -> lift $ Overrun $ lift $ Nothing
      (c:cs') -> do
        put cs'
        return (Right c)

  lookahead m = do
    savedstate <- get
    result <- m
    put savedstate
    return result

  peekChar = do
    cs <- get
    case cs of
      [] -> lift $ Overrun $ lift $ Nothing
      (c:_) -> return (Right c)

  pushChars cs = modify' (cs ++)

type Tester = ParserT (ReaderT Alias.DefinitionSet
  (StateT PositionedString (ExceptT Failure Identity)))

defaultAliasName :: String
defaultAliasName = "ls"

defaultAliasValue :: String
defaultAliasValue = defaultAliasName ++ " --color"

recursiveAlias :: String
recursiveAlias = "rec"

defaultAliasDefinitions :: Alias.DefinitionSet
defaultAliasDefinitions =
  M.insert n (Alias.definition n v p) $
    M.singleton r (Alias.definition r r pr)
      where n = T.pack defaultAliasName
            v = T.pack defaultAliasValue
            p = dummyPosition "alias ls='ls --color'"
            r = T.pack recursiveAlias
            pr = dummyPosition "alias rec=rec"

runTesterAlias :: Tester a -> Alias.DefinitionSet -> PositionedString
               -> Either Failure (a, PositionedString)
runTesterAlias parser defs ps =
  runIdentity $ runExceptT $ runStateT p1 ps
    where p1 = runReaderT p2 defs
          p2 = runParserT parser

runTester :: Tester a -> PositionedString
          -> Either Failure (a, PositionedString)
runTester parser = runTesterAlias parser defaultAliasDefinitions

runTesterWithDummyPositions :: Tester a -> String
                            -> Either Failure (a, PositionedString)
runTesterWithDummyPositions parser s = runTester parser s'
  where s' = spread (dummyPosition s) s

readAll :: MonadParser m => m String
readAll = fmap (fmap snd) (many anyChar)

-- | @expectSuccessEof consumed unconsumed parser result@ runs the given
-- @parser@ for the source code @consumed ++ unconsumed@ and tests if the
-- expected @result@ is returned and if the expected @consumed@ part of the
-- code is actually consumed.
expectSuccessEof :: (Eq a, Show a) =>
  String -> String -> Tester a -> a -> SpecWith ()
expectSuccessEof consumed unconsumed parser result =
  let s = consumed ++ unconsumed
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
expectSuccess consumed unconsumed parser result =
  context (consumed ++ unconsumed ++ "...") $
    prop "returns expected result and state" $ \remainder ->
      let s = consumed ++ unconsumed ++ remainder
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
expectShowEof :: Show a =>
  String -> String -> Tester a -> String -> SpecWith ()
expectShowEof consumed unconsumed parser =
  expectSuccessEof consumed unconsumed (show <$> parser)

-- | Like 'expectShowEof', but tries many arbitrary remainders.
expectShow :: Show a => String -> String -> Tester a -> String -> SpecWith ()
expectShow consumed unconsumed parser =
  expectSuccess consumed unconsumed (show <$> parser)

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

-- | @expectFailureEof'@ is like 'expectFailureEof', but tests the reason by
-- predicate rather than direct comparison. This is useful when the reason
-- cannot be easily constructed.
expectFailureEof' :: Show a =>
  String -> Tester a -> Severity -> (Reason -> Bool) -> Int -> SpecWith ()
expectFailureEof' input parser s r expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $
       case e of
         Right e' ->
           it "fails" $ expectationFailure $ show e'
         Left (as, Error ar apos) -> do
           it "fails with expected severity" $ as `shouldBe` s
           it "fails with expected reason" $ ar `shouldSatisfy` r
           it "fails at expected position" $ apos `shouldBe` expectedPosition

-- vim: set et sw=2 sts=2 tw=78:
