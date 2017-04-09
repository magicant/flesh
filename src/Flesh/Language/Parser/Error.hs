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
module Flesh.Language.Parser.Error (
  -- * Basic types
  Reason(..), Error(..), Severity(..),
  -- * Utilities for 'MonadError'
  MonadError(..), failureOfError, failureOfPosition, failure, satisfying,
  notFollowedBy, manyTill, recover, setReason, try,
  -- * The 'AttemptT' monad transformer
  AttemptT(..), runAttemptT, mapAttemptT) where

import Control.Applicative
import Control.Monad.Except
import Data.Foldable
import Flesh.Language.Parser.Input
import qualified Flesh.Source.Position as P

-- | Reason of a parse error.
data Reason =
  UnknownReason -- TODO TBD
  | SomeReason -- ^ only for testing
  | UnclosedDoubleQuote
  deriving (Eq, Show)

-- | Parse error description.
data Error = Error {reason :: !Reason, position :: !P.Position}
  deriving (Eq, Show)

-- | Error severity.
data Severity =
  -- | Severity of errors that are not intended to be recovered by another
  -- possible parse.
  Hard
  -- | Severity of errors that may be recovered by another parse.
  | Soft
  deriving (Eq, Show)

-- | Returns a failed attempt with the given (hard) error.
failureOfError :: MonadError (Severity, Error) m => Error -> m a
failureOfError e = throwError (Hard, e)

-- | Failure of unknown reason.
failureOfPosition :: MonadError (Severity, Error) m => P.Position -> m a
failureOfPosition p = failureOfError (Error UnknownReason p)

-- | Failure of unknown reason at the current position.
failure :: (MonadInput m, MonadError (Severity, Error) m) => m a
failure = currentPosition >>= failureOfPosition

-- | @satisfying m p@ behaves like @m@ but fails if the result of @m@ does not
-- satisfy predicate @p@. This is analogous to @'flip' 'mfilter'@.
satisfying :: (MonadInput m, MonadError (Severity, Error) m)
           => m a -> (a -> Bool) -> m a
satisfying m p = do
  pos <- currentPosition
  r <- m
  if p r then return r else failureOfPosition pos

-- | @notFollowedBy m@ succeeds if @m@ fails. If @m@ succeeds, it is
-- equivalent to 'failure'.
notFollowedBy :: (MonadInput m, MonadError (Severity, Error) m) => m a -> m ()
notFollowedBy m = do
  pos <- currentPosition
  let m' = m >> return (failureOfPosition pos)
  join $ catchError m' (const $ return $ return ())

-- | @a `manyTill` end@ parses any number of @a@ until @end@ occurs.
manyTill :: MonadError (Severity, Error) m => m a -> m end -> m [a]
a `manyTill` end = m
  where m = catchError ([] <$ end) $ \e1 ->
              let a' = catchError a $ \e2 ->
                        throwError $ case (e1, e2) of
                                      ((Soft, _), (Hard, _)) -> e2
                                      _                      -> e1
               in (:) <$> a' <*> m
-- Using 'catchError' to recover from not only soft but also hard errors.
-- If @a@ fails, re-throw the original error from @end@ for a better error
-- message (unless the error severity of @a@ wins).

-- | Recovers from an error. This is a simple wrapper around 'catchError' that
-- ignores the error's 'Severity'.
recover :: MonadError (Severity, Error) m => m a -> (Error -> m a) -> m a
recover a f = catchError a (f . snd)

-- | @setReason r a@ modifies the result of attempt @a@ by replacing an error
-- of 'UnknownReason' with the given reason @r@. For other reasons or
-- successful results, 'setReason' does not do anything.
setReason :: MonadError (Severity, Error) m => Reason -> m a -> m a
setReason r m = catchError m (throwError . fmap handle)
  where handle (Error UnknownReason p) = Error r p
        handle x                       = x

-- | 'try' rewrites the result of an attempt by converting a hard error to a
-- soft error with the same reason. The result is not modified if
-- successful.
try :: MonadError (Severity, Error) m => m a -> m a
try m = catchError m (throwError . handle)
  where handle (_, e) = (Soft, e)

-- | Modifies the behavior of a monad in the 'Alternative' (and 'MonadPlus')
-- operations so that 'Hard' errors are not recovered by the '<|>' operation.
--
-- As an instance of 'Functor', 'Foldable', 'Applicative' and 'Monad',
-- @AttemptT m@ behaves the same as the original monad @m@. The difference
-- between them lies in the implementation of 'Alternative'. @'Alternative'
-- ('ExceptT' e m)@ requires the error type @e@ to be a 'Monoid', but
-- 'AttemptT' does not. An error value of 'AttemptT' always denotes a single
-- error. For 'AttemptT', 'empty' is defined as 'failure', whose reason should
-- be set by 'setReason'. Errors in 'AttemptT' are categorized into two levels
-- of severity: 'Hard' and 'Soft'. The 'failure' function returns 'Hard'
-- errors, but they can be converted to 'Soft' errors by 'try'. Only 'Soft'
-- errors are recovered by the '<|>' operator, which helps returning
-- user-friendly error messages.
newtype AttemptT m a = AttemptT (m a)
  deriving (Eq, Show)

-- | Returns the value of 'AttemptT'.
runAttemptT :: AttemptT m a -> m a
runAttemptT (AttemptT m) = m

-- | Directly modifies the value of 'AttemptT'.
mapAttemptT :: (m a -> n b) -> AttemptT m a -> AttemptT n b
mapAttemptT f = AttemptT . f . runAttemptT

instance Functor m => Functor (AttemptT m) where
  fmap f = AttemptT . fmap f . runAttemptT
  a <$ AttemptT b = AttemptT (a <$ b)

instance Foldable m => Foldable (AttemptT m) where
  fold = fold . runAttemptT
  foldMap f = foldMap f . runAttemptT
  foldr f z = foldr f z . runAttemptT
  foldr' f z = foldr' f z . runAttemptT
  foldl f z = foldl f z . runAttemptT
  foldl' f z = foldl' f z . runAttemptT
  foldr1 f = foldr1 f . runAttemptT
  foldl1 f = foldl1 f . runAttemptT
  toList = toList . runAttemptT
  null = null . runAttemptT
  length = length . runAttemptT
  elem e = elem e . runAttemptT
  maximum = maximum . runAttemptT
  minimum = minimum . runAttemptT
  sum = sum . runAttemptT
  product = product . runAttemptT

instance Applicative m => Applicative (AttemptT m) where
  pure = AttemptT . pure
  AttemptT a <*> AttemptT b = AttemptT (a <*> b)
  AttemptT a  *> AttemptT b = AttemptT (a  *> b)
  AttemptT a <*  AttemptT b = AttemptT (a <*  b)

instance Monad m => Monad (AttemptT m) where
  return = AttemptT . return
  AttemptT a >>= f = AttemptT (a >>= runAttemptT . f)
  AttemptT a >> AttemptT b = AttemptT (a >> b)

instance MonadTrans AttemptT where
  lift = AttemptT

instance MonadInput m => MonadInput (AttemptT m) where
  popChar = lift popChar
  followedBy = mapAttemptT followedBy
  peekChar = lift peekChar
  pushChars = lift <$> pushChars

instance MonadError e m => MonadError e (AttemptT m) where
  throwError = AttemptT . throwError
  catchError (AttemptT m) f = AttemptT (catchError m (runAttemptT . f))

instance (MonadInput m, MonadError (Severity, Error) m)
    => Alternative (AttemptT m) where
  empty = failure
  a <|> b =
    a `catchError` handle
      where handle (Soft, _) = b
            handle e = throwError e

instance (MonadInput m, MonadError (Severity, Error) m)
  => MonadPlus (AttemptT m)

-- vim: set et sw=2 sts=2 tw=78:
