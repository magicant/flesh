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
Portability : non-portable (GHC language extensions)

This module defines types that describe parse errors for the shell language
and a monad transformer that injects parse error handling into another monad.
-}
module Flesh.Language.Parser.Class (
  -- * The 'MonadParser' class
  MonadParser, failure, failureOfReason, satisfying, satisfyingP,
  notFollowedBy, some',
  -- * The 'ParserT' monad transformer
  ParserT(..), runParserT, mapParserT) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad (MonadPlus, join)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, reader)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Foldable (fold, foldl, foldl', foldr, foldr', toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Buffer
import Flesh.Language.Parser.Error
import Flesh.Source.Position

-- | Collection of properties required for basic parser implementation.
--
-- @MonadParser@ is a subclass of the input and error handling monads,
-- supporting the basic behavior of the parser. It is also an instance of
-- 'Alternative' and 'MonadPlus', where
--
--  * 'empty' and 'mzero' are equal to 'failure'; and
--  * '<|>' and 'mplus' behave like 'catchError' but they only catch 'Soft'
--    failures.
class (
  MonadBuffer m,
  MonadReparse m,
  MonadRecord m,
  MonadError Failure m,
  MonadPlus m)
    => MonadParser m

-- | Failure of unknown reason at the current position.
failure :: MonadParser m => m a
failure = currentPosition >>= failureOfPosition

-- | Failure of the given reason at the current position.
failureOfReason :: MonadParser m => Reason -> m a
failureOfReason r = do
  p <- currentPosition
  failureOfError (Error r p)

-- | @satisfying m p@ behaves like @m@ but fails if the result of @m@ does not
-- satisfy predicate @p@. This is analogous to @'flip' 'mfilter'@.
satisfying :: MonadParser m => m a -> (a -> Bool) -> m a
satisfying m p = do
  pos <- currentPosition
  r <- m
  if p r then return r else failureOfPosition pos

-- | 'satisfyingP' is like 'satisfying' but applies the predicate to the
-- second item of the pair.
satisfyingP :: MonadParser m
            => m (Positioned a) -> (a -> Bool) -> m (Positioned a)
-- satisfyingP m p = satisfying m (p . snd)
-- This would return a better error position:
satisfyingP m p = do
  posr@(pos, r) <- m
  if p r then return posr else failureOfPosition pos

-- | @notFollowedBy m@ succeeds if @m@ fails. If @m@ succeeds, it is
-- equivalent to 'failure'.
notFollowedBy :: MonadParser m => m a -> m ()
notFollowedBy m = do
  pos <- currentPosition
  let m' = m >> return (failureOfPosition pos)
  join $ catchError m' (const $ return $ return ())

-- | @some' a@ is like @some a@, but returns a NonEmpty list.
some' :: Alternative m => m a -> m (NonEmpty a)
some' a = (:|) <$> a <*> many a

instance MonadParser m => MonadParser (ReaderT r m)

instance MonadParser m => MonadParser (ReparseT m)

-- | Monad wrapper that instantiates 'MonadParser' from 'MonadInput' and
-- 'MonadError'.
--
-- As an instance of 'Functor', 'Foldable', 'Applicative' and 'Monad',
-- @ParserT m@ behaves the same as the original monad @m@. The 'Alternative'
-- instance for @ParserT@ is constructed from the 'MonadError' instance to
-- obey the 'MonadParser' laws.
newtype ParserT m a = ParserT (m a)
  deriving (Eq, Show)

-- | Returns the value of 'ParserT'.
runParserT :: ParserT m a -> m a
runParserT (ParserT m) = m

-- | Directly modifies the value of 'ParserT'.
mapParserT :: (m a -> n b) -> ParserT m a -> ParserT n b
mapParserT f = ParserT . f . runParserT

instance Functor m => Functor (ParserT m) where
  fmap f = ParserT . fmap f . runParserT
  a <$ ParserT b = ParserT (a <$ b)

instance Foldable m => Foldable (ParserT m) where
  fold = fold . runParserT
  foldMap f = foldMap f . runParserT
  foldr f z = foldr f z . runParserT
  foldr' f z = foldr' f z . runParserT
  foldl f z = foldl f z . runParserT
  foldl' f z = foldl' f z . runParserT
  foldr1 f = foldr1 f . runParserT
  foldl1 f = foldl1 f . runParserT
  toList = toList . runParserT
  null = null . runParserT
  length = length . runParserT
  elem e = elem e . runParserT
  maximum = maximum . runParserT
  minimum = minimum . runParserT
  sum = sum . runParserT
  product = product . runParserT

instance Applicative m => Applicative (ParserT m) where
  pure = ParserT . pure
  ParserT a <*> ParserT b = ParserT (a <*> b)
  ParserT a  *> ParserT b = ParserT (a  *> b)
  ParserT a <*  ParserT b = ParserT (a <*  b)

instance Monad m => Monad (ParserT m) where
  return = ParserT . return
  ParserT a >>= f = ParserT (a >>= runParserT . f)
  ParserT a >> ParserT b = ParserT (a >> b)

instance MonadTrans ParserT where
  lift = ParserT

instance MonadBuffer m => MonadBuffer (ParserT m) where
  popChar = lift popChar
  lookahead = mapParserT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition

instance MonadReparse m => MonadReparse (ParserT m) where
  maybeReparse = mapParserT maybeReparse
  maybeReparse' = mapParserT maybeReparse'

instance MonadRecord m => MonadRecord (ParserT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadError e m => MonadError e (ParserT m) where
  throwError = ParserT . throwError
  catchError (ParserT m) f = ParserT (catchError m (runParserT . f))

instance (MonadReparse m, MonadRecord m, MonadError Failure m)
    => Alternative (ParserT m) where
  empty = failure
  a <|> b =
    a `catchError` handle
      where handle (Soft, _) = b
            handle e = throwError e

instance (MonadReparse m, MonadRecord m, MonadError Failure m)
  => MonadPlus (ParserT m)

instance (MonadReparse m, MonadRecord m, MonadError Failure m)
  => MonadParser (ParserT m)

instance MonadReader r m => MonadReader r (ParserT m) where
  ask = lift ask
  local f = mapParserT $ local f
  reader = lift . reader

-- vim: set et sw=2 sts=2 tw=78:
