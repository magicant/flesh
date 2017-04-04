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

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Flesh.Language.Parser.Error
import Flesh.Source.Position
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary Reason where
  arbitrary = elements [UnknownReason, SomeReason]

instance Arbitrary Error where
  arbitrary = do
    r <- arbitrary
    s <- arbitrary
    return $ Error r $ dummyPosition s

instance Arbitrary Severity where
  arbitrary = elements [Hard, Soft]

instance (MonadError (Severity, Error) m, Arbitrary a)
    => Arbitrary (AttemptT m a) where
  arbitrary = oneof [success_, failure_]
    where success_ = return <$> arbitrary
          failure_ = do
            s <- arbitrary :: Gen Severity
            e <- arbitrary :: Gen Error
            return $ throwError (s, e)

instance Arbitrary a => Arbitrary (PositionedList a) where
  arbitrary = do
    s <- arbitrary
    xs <- arbitrary
    return $ spread (dummyPosition s) xs

type AE = AttemptT (ExceptT (Severity, Error) Identity)
type AES = AttemptT (ExceptT (Severity, Error) (State PositionedString))

isUnknownReason :: AE a -> Bool
isUnknownReason a =
  case runIdentity $ runExceptT $ runAttemptT a of
    Left (_, Error UnknownReason _) -> True
    _                               -> False

isHardError :: AE a -> Bool
isHardError a =
  case runIdentity $ runExceptT $ runAttemptT a of
    Left (Hard, _) -> True
    _              -> False

run :: AES a
    -> PositionedString -> (Either (Severity, Error) a, PositionedString)
run = runState . runExceptT . runAttemptT

aesFromAE :: AE a -> AES a
aesFromAE = AttemptT . mapExceptT (return . runIdentity) . runAttemptT

spec :: Spec
spec = do
  describe "notFollowedBy" $ do
    prop "succeeds if argument fails" $ \e i ->
      let f = failureOfError e
       in run (notFollowedBy f) i === run (return ()) i

    prop "fails if argument succeeds" $ \v i ->
      let _ = v :: Int
       in run (notFollowedBy (return v)) i === run failure i

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

  describe "Alternative (AttemptT m) (<|>)" $ do
    prop "returns hard errors intact" $ \e a i ->
      let _ = a :: AE Int
          a' = aesFromAE a
          f  = failureOfError e
       in run (f <|> a') i === run f i

    prop "returns success intact" $ \v a i ->
      let _ = a :: AE Int
          a' = aesFromAE a
          s  = return v
       in run (s <|> a') i === run s i

    prop "recovers soft errors" $ \e a i ->
      let _ = a :: AE Int
          a' = aesFromAE a
          f  = try $ failureOfError e
       in run (f <|> a') i === run a' i

-- vim: set et sw=2 sts=2 tw=78:
