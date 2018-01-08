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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flesh.Language.Parser.ErrorSpec (spec) where

import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Flesh.Language.Parser.Class
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary, Gen, arbitrary, elements, oneof, (===), (==>))

instance Arbitrary Reason where
  arbitrary = oneof [
    return UnknownReason,
    return UnclosedDoubleQuote,
    return UnclosedSingleQuote,
    return MissingExpansionAfterDollar,
    return MissingRedirectionTarget,
    -- return UnclosedHereDocContent ...
    -- return MissingHereDocContents ...
    MissingCommandAfter <$> arbitrary,
    UnclosedSubshell . dummyPosition <$> arbitrary,
    UnclosedGrouping . dummyPosition <$> arbitrary,
    MissingDoForWhile . dummyPosition <$> arbitrary,
    MissingDoForUntil . dummyPosition <$> arbitrary,
    MissingDoneForDo . dummyPosition <$> arbitrary]

instance Arbitrary Error where
  arbitrary = do
    r <- arbitrary
    s <- arbitrary
    return $ Error r $ dummyPosition s

instance Arbitrary Severity where
  arbitrary = elements [Hard, Soft]

instance (MonadError Failure m, Arbitrary a) => Arbitrary (ParserT m a) where
  arbitrary = oneof [success_, failure_]
    where success_ = return <$> arbitrary
          failure_ = do
            s <- arbitrary :: Gen Severity
            e <- arbitrary :: Gen Error
            return $ throwError (s, e)

type AE = ParserT (RecordT (ExceptT Failure Identity))

runAE :: AE a -> Either Failure a
runAE = runIdentity . runExceptT . evalRecordT . runParserT

isUnknownReason :: AE a -> Bool
isUnknownReason a =
  case runAE a of
    Left (_, Error UnknownReason _) -> True
    _                               -> False

isHardError :: AE a -> Bool
isHardError a =
  case runAE a of
    Left (Hard, _) -> True
    _              -> False

isSoftError :: AE a -> Bool
isSoftError a =
  case runAE a of
    Left (Soft, _) -> True
    _              -> False

spec :: Spec
spec = do
  describe "setReason" $ do
    prop "replaces UnknownReason" $ \s e ->
      let Error r p = e
          a         = throwError (s, e) :: AE Int
          a'        = throwError (s, Error UnknownReason p)
       in setReason r a' === a

    prop "retains known reason" $ \r a ->
      not (isUnknownReason (a :: AE Int)) ==>
        setReason r a === a

  describe "try" $ do
    prop "converts hard errors to soft" $ \e ->
      let h = throwError (Hard, e) :: AE Int
          s = throwError (Soft, e) :: AE Int
       in try h === s

    prop "retains successes and soft errors intact" $ \a ->
      let _ = a :: AE Int
       in not (isHardError a) ==> try a === a

  describe "require" $ do
    prop "converts soft errors to hard" $ \e ->
      let h = throwError (Hard, e) :: AE Int
          s = throwError (Soft, e) :: AE Int
       in require s === h

    prop "retains successes and hard errors intact" $ \a ->
      let _ = a :: AE Int
       in not (isSoftError a) ==> require a === a

-- vim: set et sw=2 sts=2 tw=78:
