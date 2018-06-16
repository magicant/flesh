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

module Flesh.Language.Parser.ClassTestUtil () where

import Flesh.Language.Parser.Class
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.ErrorTestUtil ()
import Test.QuickCheck (Arbitrary, arbitrary, oneof)

-- TODO Take ReparseT etc. into account
instance (Monad m, Arbitrary a) => Arbitrary (ParserT c m a) where
  arbitrary = oneof [success_, failure_]
    where success_ = return <$> arbitrary
          failure_ = do
            s <- arbitrary
            e <- arbitrary
            return $ ParserT $ throwError (s, e)

-- This is silly, just for making QuickCheck happy.
instance Show (ParserT c m a) where
  showsPrec _ _ = id

-- vim: set et sw=2 sts=2 tw=78:
