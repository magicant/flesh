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

{-# LANGUAGE Safe #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : portable

This module defines types and functions for handling aliases in the syntax
parser.
-}
module Flesh.Language.Parser.Alias (
  module Flesh.Language.Alias,
  -- * Context
  ContextT,
  -- * Alias substitution results
  AliasT, reparse) where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Flesh.Language.Alias

-- | Monad transformer that makes parse results depend on alias definitions.
type ContextT = ReaderT DefinitionSet

-- | Monad transformer that makes parse results optional.
--
-- When an alias substitution occurred, the result will be 'Nothing' to force
-- the parse process to restart from higher syntax level.
type AliasT = MaybeT

-- | Modifies a parser so that it retries parsing while parsing is interrupted
-- by alias substitution.
reparse :: Monad m => AliasT m a -> m a
reparse a = do
  m <- runMaybeT a
  case m of
    Nothing -> reparse a
    Just v -> return v

-- vim: set et sw=2 sts=2 tw=78:
