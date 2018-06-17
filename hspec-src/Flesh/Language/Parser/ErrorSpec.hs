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

module Flesh.Language.Parser.ErrorSpec (spec) where

import Flesh.Language.Parser.Error
import Flesh.Language.Parser.ErrorTestUtil ()
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), (==>))

isUnknownReason :: Either Failure a -> Bool
isUnknownReason (Left (_, Error UnknownReason _)) = True
isUnknownReason _                                 = False

isHardError :: Either Failure a -> Bool
isHardError (Left (Hard, _)) = True
isHardError _                = False

isSoftError :: Either Failure a -> Bool
isSoftError (Left (Soft, _)) = True
isSoftError _                = False

spec :: Spec
spec = parallel $ do
  describe "setReason" $ do
    prop "replaces UnknownReason" $ \s e ->
      let Error r p = e
          a         = throwError (s, e) :: Either Failure Int
          a'        = throwError (s, Error UnknownReason p)
       in setReason r a' === a

    prop "retains known reason" $ \r a ->
      not (isUnknownReason (a :: Either Failure Int)) ==>
        setReason r a === a

  describe "try" $ do
    prop "converts hard errors to soft" $ \e ->
      let h = throwError (Hard, e) :: Either Failure Int
          s = throwError (Soft, e) :: Either Failure Int
       in try h === s

    prop "retains successes and soft errors intact" $ \a ->
      let _ = a :: Either Failure Int
       in not (isHardError a) ==> try a === a

  describe "require" $ do
    prop "converts soft errors to hard" $ \e ->
      let h = throwError (Hard, e) :: Either Failure Int
          s = throwError (Soft, e) :: Either Failure Int
       in require s === h

    prop "retains successes and hard errors intact" $ \a ->
      let _ = a :: Either Failure Int
       in not (isSoftError a) ==> require a === a

-- vim: set et sw=2 sts=2 tw=78:
