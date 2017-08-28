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

newtype Overrun a = Overrun {
  runOverrun :: StateT PositionedString (ExceptT Failure Maybe) a}

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

instance MonadInput Overrun where
  popChar = Overrun $ do
    cs <- get
    case cs of
      Nil _ -> lift $ lift Nothing
      c :~ cs' -> do
        put cs'
        return $ Right c

  lookahead m = Overrun $ do
    savedstate <- get
    result <- runOverrun m
    put savedstate
    return result

  peekChar = Overrun $ do
    cs <- get
    case cs of
      Nil _ -> lift $ lift Nothing
      c :~ _ -> return $ Right c

  currentPosition = Overrun $ headPosition <$> get

  pushChars [] = return ()
  pushChars (c:cs) = do
    pushChars cs
    Overrun $ modify' (c :~)

type TesterT m = ParserT (ReaderT Alias.DefinitionSet m)
type OverrunTester = TesterT Overrun
type FullInputTester =
  TesterT (StateT PositionedString (ExceptT Failure Identity))

defaultAliasName :: String
defaultAliasName = "ls"

defaultAliasValue :: String
defaultAliasValue = defaultAliasName ++ " --color"

recursiveAlias :: String
recursiveAlias = "rec"

reservedWordAliasName :: String
reservedWordAliasName = "while"

reservedWordAliasValue :: String
reservedWordAliasValue = ";;"

defaultAliasDefinitions :: Alias.DefinitionSet
defaultAliasDefinitions =
  M.insert n (Alias.definition n v p) $
    M.singleton r (Alias.definition r r pr)
      where n = T.pack defaultAliasName
            v = T.pack defaultAliasValue
            p = dummyPosition "alias ls='ls --color'"
            r = T.pack recursiveAlias
            pr = dummyPosition "alias rec=rec"

reservedWordAliasDefinitions :: Alias.DefinitionSet
reservedWordAliasDefinitions = M.singleton n (Alias.definition n v p)
  where n = T.pack reservedWordAliasName
        v = T.pack reservedWordAliasValue
        p = dummyPosition "alias while=';;'"

runTesterAliasT :: TesterT m a -> Alias.DefinitionSet -> m a
runTesterAliasT parser defs = runReaderT (runParserT parser) defs

runFullInputTesterAlias :: FullInputTester a
                        -> Alias.DefinitionSet -> PositionedString
                        -> Either Failure (a, PositionedString)
runFullInputTesterAlias parser defs ps =
  runIdentity $ runExceptT $ runStateT (runTesterAliasT parser defs) ps

runOverrunTesterAlias :: OverrunTester a
                      -> Alias.DefinitionSet -> PositionedString
                      -> Maybe (Either Failure (a, PositionedString))
runOverrunTesterAlias parser defs ps =
  runExceptT $ runStateT m ps
    where m = runOverrun $ runTesterAliasT parser defs

runFullInputTester :: FullInputTester a -> PositionedString
                   -> Either Failure (a, PositionedString)
runFullInputTester parser =
  runFullInputTesterAlias parser defaultAliasDefinitions

runOverrunTester :: OverrunTester a -> PositionedString
                 -> Maybe (Either Failure (a, PositionedString))
runOverrunTester parser = runOverrunTesterAlias parser defaultAliasDefinitions

runFullInputTesterWithDummyPositions :: FullInputTester a -> String
                            -> Either Failure (a, PositionedString)
runFullInputTesterWithDummyPositions parser s = runFullInputTester parser s'
  where s' = spread (dummyPosition s) s

runOverrunTesterWithDummyPositions :: OverrunTester a -> String ->
  Maybe (Either Failure (a, PositionedString))
runOverrunTesterWithDummyPositions parser s = runOverrunTester parser s'
  where s' = spread (dummyPosition s) s

readAll :: MonadParser m => m String
readAll = fmap (fmap snd) (many anyChar)

-- | @expectSuccessEof consumed unconsumed parser result@ runs the given
-- @parser@ for the source code @consumed ++ unconsumed@ and tests if the
-- expected @result@ is returned and if the expected @consumed@ part of the
-- code is actually consumed. If the parser tries to read beyond the
-- @unconsumed@ part, it receives end-of-file.
expectSuccessEof :: (Eq a, Show a) =>
  String -> String -> FullInputTester a -> a -> SpecWith ()
expectSuccessEof consumed unconsumed parser result =
  let s = consumed ++ unconsumed
      s' = spread (dummyPosition s) s
      e = runFullInputTester parser s'
   in context s $ do
     it "returns expected result successfully" $
       fmap fst e `shouldBe` Right result

     it "consumes expected part of source code" $
       fmap snd e `shouldBe` Right (dropP (length consumed) s')

-- | @expectSuccess consumed unconsumed parser result@ runs the given @parser@
-- for the source code @consumed ++ unconsumed@ and tests if the expected
-- @result@ is returned and if the expected @consumed@ part of the code is
-- actually consumed. The test fails if the parser tries to look ahead beyond
-- the @unconsumed@ part.
expectSuccess :: (Eq a, Show a) =>
  String -> String -> OverrunTester a -> a -> SpecWith ()
expectSuccess consumed unconsumed parser result =
  let s = consumed ++ unconsumed
      s' = spread (dummyPosition s) s
      e = runOverrunTester parser s'
   in context s $ do
     it "returns expected result successfully" $
       fmap (fmap fst) e `shouldBe` Just (Right result)

     it "consumes expected part of source code" $
       fmap (fmap snd) e `shouldBe` Just (Right (dropP (length consumed) s'))

-- | @expectPositionEof input parser expectedPositionIndex@ runs the given
-- @parser@ for the given @input@ and tests if the result is a position at the
-- given index withing the input. If the parser tries to read beyond the
-- @input@, it receives end-of-file.
expectPositionEof :: String -> FullInputTester Position -> Int -> SpecWith ()
expectPositionEof input parser expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runFullInputTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $ do
     it "returns expected position" $
       fmap fst e `shouldBe` Right expectedPosition

-- | @expectPosition input parser expectedPositionIndex@ runs the given
-- @parser@ for the given @input@ and tests if the result is a position at the
-- given index withing the input. The test fails if the parser tries to look
-- ahead beyond the @input@.
expectPosition :: String -> OverrunTester Position -> Int -> SpecWith ()
expectPosition input parser expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runOverrunTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $ do
     it "returns expected position" $
       fmap (fmap fst) e `shouldBe` Just (Right expectedPosition)

-- | Like 'expectSuccessEof', but compares string representation of the result
-- with the given expected string.
expectShowEof :: Show a =>
  String -> String -> FullInputTester a -> String -> SpecWith ()
expectShowEof consumed unconsumed parser =
  expectSuccessEof consumed unconsumed (show <$> parser)

-- | Like 'expectShowEof', but tries many arbitrary remainders.
expectShow :: Show a
           => String -> String -> OverrunTester a -> String -> SpecWith ()
expectShow consumed unconsumed parser =
  expectSuccess consumed unconsumed (show <$> parser)

-- | @expectFailureEof input parser severity reason position@ runs the given
-- @parser@ for the given @input@ and tests if it fails for the expected error
-- of the given @severity@, @reason@, and @position@. If the parser tries to
-- read beyond the @unconsumed@ part, it receives end-of-file.
--
-- Type parameter @a@ needs to be 'Show' and 'Eq'. Map to @()@ if you want to
-- apply to a non-Show or non-Eq @a@.
expectFailureEof :: (Eq a, Show a) =>
  String -> FullInputTester a -> Severity -> Reason -> Int -> SpecWith ()
expectFailureEof input parser s r expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runFullInputTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $ do
     it "fails" $
       e `shouldBe` Left (s, Error r expectedPosition)

-- | Like 'expectFailureEof', but the test fails if the parser tries to look
-- ahead beyond the @input@.
expectFailure :: (Eq a, Show a) =>
  String -> OverrunTester a -> Severity -> Reason -> Int -> SpecWith ()
expectFailure input parser s r expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runOverrunTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $ do
     it "fails" $
       e `shouldBe` Just (Left (s, Error r expectedPosition))

-- | @expectFailureEof'@ is like 'expectFailureEof', but tests the reason by
-- predicate rather than direct comparison. This is useful when the reason
-- cannot be easily constructed.
expectFailureEof' :: Show a =>
  String -> FullInputTester a -> Severity -> (Reason -> Bool) -> Int ->
    SpecWith ()
expectFailureEof' input parser s r expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runFullInputTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $
       case e of
         Right e' ->
           it "fails" $ expectationFailure $ show e'
         Left (as, Error ar apos) -> do
           it "fails with expected severity" $ as `shouldBe` s
           it "fails with expected reason" $ ar `shouldSatisfy` r
           it "fails at expected position" $ apos `shouldBe` expectedPosition

-- | @expectFailure'@ is like 'expectFailure', but tests the reason by
-- predicate rather than direct comparison. This is useful when the reason
-- cannot be easily constructed.
expectFailure' :: Show a =>
  String -> OverrunTester a -> Severity -> (Reason -> Bool) -> Int ->
    SpecWith ()
expectFailure' input parser s r expectedPositionIndex =
  let s' = spread (dummyPosition input) input
      e = runOverrunTester parser s'
      expectedPosition = headPosition (dropP expectedPositionIndex s')
   in context input $
       case e of
         Nothing ->
           it "fails" $ expectationFailure "input overrun"
         Just (Right e') ->
           it "fails" $ expectationFailure $ show e'
         Just (Left (as, Error ar apos)) -> do
           it "fails with expected severity" $ as `shouldBe` s
           it "fails with expected reason" $ ar `shouldSatisfy` r
           it "fails at expected position" $ apos `shouldBe` expectedPosition

-- vim: set et sw=2 sts=2 tw=78:
