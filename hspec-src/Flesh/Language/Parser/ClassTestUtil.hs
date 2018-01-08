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
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof)

instance (MonadError Failure m, Arbitrary a) => Arbitrary (ParserT m a) where
  arbitrary = oneof [success_, failure_]
    where success_ = return <$> arbitrary
          failure_ = do
            s <- arbitrary :: Gen Severity
            e <- arbitrary :: Gen Error
            return $ throwError (s, e)

-- vim: set et sw=2 sts=2 tw=78:
