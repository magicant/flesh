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

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (GHC language extensions)

This module defines types that describe parse errors for the shell language
and a monad transformer that injects parse error handling into another monad.
-}
module Flesh.Language.Parser.Error (
  MonadError(..),
  -- * Basic types
  Reason(..), Error(..), Severity(..),
  -- * The AttemptT monad transformer
  AttemptT(..), attempt, runAttemptT, mapAttemptT,
  -- * Class of attempt monads
  MonadAttempt(..), failure, failure', recover) where

import Control.Applicative
import Control.Monad.Except
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Data.Foldable
import Flesh.Language.Parser.Input
import qualified Flesh.Source.Position as P

-- | Reason of a parse error.
data Reason =
  UnknownReason -- TODO TBD
  | SomeReason -- ^ only for testing
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

-- | Result of an attempt to parse something. Type parameter @a@ is the result
-- type of a successful parse. As a monad transformer, @AttemptT@ injects the
-- result into monad @m@.
--
-- As an instance of 'Functor', 'Foldable', 'Applicative' and 'Monad',
-- 'AttemptT' behaves the same as 'ExceptT'. The difference between them lies
-- in the implementation of 'Alternative'. 'ExceptT' requires the error type
-- to be a 'Monoid', but 'AttemptT' does not. An error value of 'AttemptT'
-- always denotes a single error. For 'AttemptT', 'empty' is defined as an
-- unmeaningful default error that should be replaced by 'setReason'. Errors
-- in 'AttemptT' are categorized into two levels of severity: 'Hard' and
-- 'Soft'. The 'failure' function returns 'Hard' errors, but they can be
-- converted to 'Soft' errors by 'try'. Only 'Soft' errors are recovered by
-- the '<|>' operator, which helps returning user-friendly error messages.
newtype AttemptT m a = AttemptT (ExceptT (Severity, Error) m a)
  deriving (Eq, Show)

-- | Converts 'AttemptT' to 'ExceptT'.
exceptFromAttemptT :: AttemptT m a -> ExceptT (Severity, Error) m a
exceptFromAttemptT (AttemptT e) = e

-- | Constructs an 'AttemptT' value.
attempt :: m (Either (Severity, Error) a) -> AttemptT m a
attempt = AttemptT . ExceptT

-- | Returns the value of 'AttemptT'.
runAttemptT :: AttemptT m a -> m (Either (Severity, Error) a)
runAttemptT = runExceptT . exceptFromAttemptT

-- | Directly modifies the value of 'AttemptT'.
mapAttemptT ::
  (m (Either (Severity, Error) a) -> n (Either (Severity, Error) b))
  -> AttemptT m a -> AttemptT n b
mapAttemptT f = attempt . f . runAttemptT

instance Monad m => MonadError (Severity, Error) (AttemptT m) where
  throwError = AttemptT . throwError
  catchError (AttemptT a) f =
    AttemptT $ catchError a (exceptFromAttemptT . f)

instance Functor m => Functor (AttemptT m) where
  fmap f = AttemptT . fmap f . exceptFromAttemptT
  a <$ AttemptT b = AttemptT (a <$ b)

instance Foldable m => Foldable (AttemptT m) where
  fold = fold . exceptFromAttemptT
  foldMap f = foldMap f . exceptFromAttemptT
  foldr f z = foldr f z . exceptFromAttemptT
  foldr' f z = foldr' f z . exceptFromAttemptT
  foldl f z = foldl f z . exceptFromAttemptT
  foldl' f z = foldl' f z . exceptFromAttemptT
  foldr1 f = foldr1 f . exceptFromAttemptT
  foldl1 f = foldl1 f . exceptFromAttemptT
  toList = toList . exceptFromAttemptT
  null = null . exceptFromAttemptT
  length = length . exceptFromAttemptT
  elem e = elem e . exceptFromAttemptT
  maximum = maximum . exceptFromAttemptT
  minimum = minimum . exceptFromAttemptT
  sum = sum . exceptFromAttemptT
  product = product . exceptFromAttemptT

instance Monad m => Applicative (AttemptT m) where
  pure = AttemptT . pure
  AttemptT a <*> AttemptT b = AttemptT (a <*> b)
  AttemptT a  *> AttemptT b = AttemptT (a  *> b)
  AttemptT a <*  AttemptT b = AttemptT (a <*  b)

instance Monad m => Monad (AttemptT m) where
  return = AttemptT . return
  AttemptT a >>= f = AttemptT (a >>= exceptFromAttemptT . f)
  AttemptT a >> AttemptT b = AttemptT (a >> b)

instance MonadTrans AttemptT where
  lift = AttemptT . ExceptT . liftM Right

instance MonadInput m => MonadInput (AttemptT m) where
  popChar = lift popChar
  followedBy = mapAttemptT followedBy
  peekChar = lift peekChar
  pushChars = lift <$> pushChars

instance MonadInput m => Alternative (AttemptT m) where
  empty = do
    p <- currentPosition
    throwError (Hard, Error UnknownReason p)
  a <|> b = do
    a `catchError` handle
      where handle (Soft, _) = b
            handle e = throwError e

instance MonadInput m => MonadPlus (AttemptT m)

-- | Extension of 'MonadError' with operations to modify attempt results.
class MonadError (Severity, Error) m => MonadAttempt m where
  -- | @setReason e a@ modifies the result of attempt @a@ by replacing an
  -- error of 'UnknownReason' with the given error @e@. For other reasons or
  -- successful results, 'setReason' does not do anything.
  setReason :: Error -> m a -> m a
  -- | 'try' rewrites the result of an attempt by converting a hard error to a
  -- soft error with the same reason. The result is not modified if
  -- successful.
  try :: m a -> m a

instance Monad m => MonadAttempt (AttemptT m) where
  setReason e = mapAttemptT (fmap f)
    where f (Left (s, (Error UnknownReason _))) = Left (s, e)
          f r                                   = r
  try = mapAttemptT (fmap f)
    where f (Left (Hard, e)) = Left (Soft, e)
          f r                = r

instance (Monoid w, MonadAttempt m) => MonadAttempt (WL.WriterT w m) where
  setReason e = WL.mapWriterT (setReason e)
  try = WL.mapWriterT try

instance (Monoid w, MonadAttempt m) => MonadAttempt (WS.WriterT w m) where
  setReason e = WS.mapWriterT (setReason e)
  try = WS.mapWriterT try

-- | Returns a failed attempt with the given (hard) error.
failure :: MonadError (Severity, Error) m => Error -> m a
failure e = throwError (Hard, e)

-- | Failure of unknown reason.
failure' :: MonadError (Severity, Error) m => P.Position -> m a
failure' p = failure (Error {reason = UnknownReason, position = p})

-- | Recovers from an error. This is a simple wrapper around 'catchError' that
-- ignores the error's 'Severity'.
recover :: MonadError (Severity, Error) m => m a -> (Error -> m a) -> m a
recover a f = catchError a (f . snd)

-- vim: set et sw=2 sts=2 tw=78:
