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

{-# OPTIONS_GHC -Wno-orphans #-}

module Flesh.Language.Parser.ErrorSpec (spec) where

import Control.Applicative
import Control.Monad.Identity
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

instance (Monad m, Arbitrary a) => Arbitrary (AttemptT m a) where
  arbitrary = oneof [success_, failure_]
    where success_ = return <$> arbitrary
          failure_ = do
            s <- arbitrary
            e <- arbitrary
            return $ throwError (s, e)

isUnknownReason :: AttemptT Identity a -> Bool
isUnknownReason a =
  case runIdentity $ runAttemptT a of
    Left (_, Error UnknownReason _) -> True
    _                               -> False

spec :: Spec
spec = do
  describe "Alternative (AttemptT m) (<|>)" $ do
    prop "returns hard errors intact" $ \e a ->
      let f = failure e
       in (f <|> (a :: AttemptT Identity Int)) === f

    prop "returns success intact" $ \i a ->
      let s = return i
       in (s <|> (a :: AttemptT Identity Int)) === s

    prop "recovers soft errors" $ \e a ->
      let f = try $ failure e
       in (f <|> (a :: AttemptT Identity Int)) === a

  describe "MonadAttempt (AttemptT m) setReason" $ do
    prop "replaces UnknownReason" $ \s e ->
      let Error _ p = e
       in setReason e (throwError (s, Error UnknownReason p)) ===
         (throwError (s, e) :: AttemptT Identity Int)

    prop "retains known reason" $ \e a ->
      not (isUnknownReason a) ==>
        setReason e a === (a :: AttemptT Identity Int)

-- vim: set et sw=2 sts=2 tw=78:
