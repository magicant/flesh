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
{-# LANGUAGE Safe #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (flexible contexts)

This module defines types and functions for handling aliases in the syntax
parser.
-}
module Flesh.Language.Parser.Alias (
  module Flesh.Language.Alias,
  -- * Context
  ContextT,
  -- * Alias substitution
  substituteAlias) where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Flesh.Language.Alias
import Flesh.Language.Parser.Input
import Flesh.Source.Position

-- | Monad transformer that makes parse results depend on alias definitions.
type ContextT = ReaderT DefinitionSet

-- | Returns 'True' iff the given position is applicable for alias
-- substitution of the given name. The name is not applicable if the current
-- position is already a result of alias substitution of the name.
applicable :: T.Text -> Position -> Bool
applicable t (Position (Fragment _ (Alias pos def) _) _)
  | name def == t = False
  | otherwise     = applicable t pos
applicable _ _ = True

-- | Performs alias substitution if the text is an alias defined in the
-- context.
--
-- This function substitutes a single alias only. It does not substitute
-- recursively nor substitute the next token (for an alias value ending with a
-- blank).
--
-- Returns @'return' ()@ if substitution was performed; returns 'Nothing'
-- otherwise.
substituteAlias :: (MonadReader DefinitionSet m, MonadInput m)
                => T.Text -> MaybeT m ()
substituteAlias t = do
  defs <- ask
  def <- MaybeT $ return $ M.lookup t defs
  pos' <- currentPosition
  guard $ applicable t pos'
  let a = Alias pos' def
      v = T.unpack $ value def
      frag = Fragment v a 0
      pos = Position frag 0
      cs = unposition $ spread pos $ v
  pushChars cs

-- vim: set et sw=2 sts=2 tw=78:
