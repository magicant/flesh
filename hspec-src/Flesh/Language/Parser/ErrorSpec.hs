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

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Flesh.Language.Parser.Buffer
import Flesh.Language.Parser.Class
import Flesh.Language.Parser.ClassTestUtil ()
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.ErrorTestUtil ()
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), (==>))

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
spec = parallel $ do
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
