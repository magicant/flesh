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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

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
  -- * AliasT
  AliasT(..), mapAliasT, toMaybeT, fromMaybeT,
  -- * Helper functions
  isAfterBlankEndingSubstitution, substituteAlias, reparse) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus, guard)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, reader)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Map.Strict (lookup)
import Data.Text (Text, unpack)
import Flesh.Data.Char
import Flesh.Language.Alias
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Prelude hiding (lookup)

-- | Monad transformer that makes parse results depend on alias definitions.
type ContextT = ReaderT DefinitionSet

-- | Monad transformer that represents results of parse that may be
-- interrupted by alias substitution.
--
-- If alias substitution occurs on the first token in the parser, the result
-- will be 'Nothing' and the parser must be applied again.
--
-- The '<*>' and '>>=' operators for 'AliasT' behave differently from those of
-- 'MaybeT'. They try to re-parse the right hand side if it returned 'Nothing'
-- and the left hand side consumed any input characters. In other words, the
-- right hand side is implicitly re-parsed if the left hand side was
-- successfully parsed as a non-empty non-terminal.
newtype AliasT m a = AliasT {runAliasT :: m (Maybe a)}

-- | Modifies the content of AliasT.
mapAliasT :: (m (Maybe a) -> n (Maybe b)) -> AliasT m a -> AliasT n b
mapAliasT f = AliasT . f . runAliasT

inverse :: Functor m => m (Maybe a) -> m (Maybe ())
inverse = fmap inverse'
  where inverse' Nothing = Just ()
        inverse' (Just _) = Nothing

-- | Converts an 'AliasT' monad to a 'MaybeT' monad.
--
-- Although 'AliasT' and 'MaybeT' share the same value type @m (Maybe a)@,
-- they are semantically inverse: The Nothing value of 'AliasT' means the
-- parser has been aborted due to alias substitution, while that of 'MaybeT'
-- means alias substitution is not applicable in the current parsing state.
-- Hence, 'toMaybeT' inverts Nothing and Just values.
toMaybeT :: Functor m => AliasT m a -> MaybeT m ()
toMaybeT = MaybeT . inverse . runAliasT

-- | The inverse of 'toMaybeT'.
fromMaybeT :: Functor m => MaybeT m a -> AliasT m ()
fromMaybeT = AliasT . inverse . runMaybeT

instance MonadTrans AliasT where
  lift = AliasT . fmap Just

instance Functor m => Functor (AliasT m) where
  fmap f = mapAliasT $ fmap $ fmap f
  (<$) x = mapAliasT (Just x <$)

instance MonadParser m => Applicative (AliasT m) where
  pure = AliasT . pure . Just
  af <*> ax = af >>= (<$> ax)

instance MonadParser m => Alternative (AliasT m) where
  empty = lift empty
  a <|> b = AliasT $ runAliasT a <|> runAliasT b

instance MonadParser m => Monad (AliasT m) where
  return = pure
  ax >>= af = AliasT $ do
    p1 <- currentPosition
    mx <- runAliasT ax
    case mx of
      Nothing -> return Nothing
      Just x -> do
        p2 <- currentPosition
        let parseRhs = do
              mb <- runAliasT $ af x
              case mb of
                Nothing -> if p1 == p2 then pure Nothing else parseRhs
                Just b -> pure $ Just b
         in parseRhs

instance MonadParser m => MonadPlus (AliasT m)

instance MonadParser m => MonadInput (AliasT m) where
  popChar = lift popChar
  lookahead = mapAliasT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

instance MonadParser m => MonadInputRecord (AliasT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance (MonadParser m, MonadError e m) => MonadError e (AliasT m) where
  throwError = lift . throwError
  catchError m f = AliasT $ catchError (runAliasT m) (runAliasT . f)

instance (MonadParser m, MonadReader r m) => MonadReader r (AliasT m) where
  ask = lift ask
  local f = mapAliasT $ local f
  reader f = lift $ reader f

instance MonadParser m => MonadParser (AliasT m)

-- | Tests if the current position is after an alias substitution whose value
-- ends with a blank.
isAfterBlankEndingSubstitution :: MonadInputRecord m => m Bool
isAfterBlankEndingSubstitution = do
  rcc <- reverseConsumedChars
  cp <- currentPosition
  return $ test rcc cp
    where test ((p, c) : cs) p' | isBlank c =
            isAlias p && differentSituations p p' || test cs p
          test _ _ = False
          isAlias p = isAlias' (s p)
          isAlias' (Alias _ _) = True
          isAlias' _ = False
          differentSituations p1 p2 = s p1 /= s p2
          s = situation . fragment

-- | Returns 'True' iff the given position is applicable for alias
-- substitution of the given name. The name is not applicable if the current
-- position is already a result of alias substitution of the name.
applicable :: Text -> Position -> Bool
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
                => Position -> Text -> MaybeT m ()
substituteAlias pos' t = do
  defs <- ask
  def <- MaybeT $ return $ lookup t defs
  guard $ applicable t pos'
  let a = Alias pos' def
      v = unpack $ value def
      frag = Fragment v a 0
      pos = Position frag 0
      cs = unposition $ spread pos $ v
  pushChars cs

-- | Modifies a parser so that it retries parsing while it is failing due to
-- alias substitution.
reparse :: Monad m => AliasT m a -> m a
reparse a = reparse_a
  where reparse_a = do
          m <- runAliasT a
          case m of
            Nothing -> reparse_a
            Just v -> return v

-- vim: set et sw=2 sts=2 tw=78:
