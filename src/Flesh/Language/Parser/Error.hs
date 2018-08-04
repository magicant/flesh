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
  Reason(..), Error(..), Severity(..), Failure,
  -- * Utilities for 'MonadError'
  MonadError(..), failureOfError, failureOfPosition, manyTill, someTill,
  manyTo, recover, setReason, try, require) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Data.List.NonEmpty (NonEmpty((:|)))
import Flesh.Language.Syntax
import Flesh.Source.Position

-- | Reason of a parse error.
data Reason =
  UnknownReason -- ^ Default reason that should be replaced by 'setReason'.
  | LineBeginningWithSemicolon
  | UnclosedDoubleQuote
  | UnclosedSingleQuote
  | UnclosedCommandSubstitution
  | MissingExpansionAfterDollar
  | MissingParameterName
  | UnclosedExpansion
  | MissingRedirectionTarget
  | UnclosedHereDocContent HereDocOp
  | MissingHereDocContents (NonEmpty HereDocOp)
  | MissingCommandAfter String
  | UnclosedSubshell Position -- ^ with position of the open parenthesis
  | UnclosedGrouping Position -- ^ with position of the open brace
  | MissingThenForIf Position -- ^ with position of the "if" keyword
  | MissingThenForElif Position -- ^ with position of the "elif" keyword
  | MissingFiForIf Position -- ^ with position of the "if" keyword
  | MissingNameAfterFor
  | SemicolonBeforeIn
  | MissingDoForFor Position -- ^ with position of the "for" keyword
  | MissingDoForWhile Position -- ^ with position of the "while" keyword
  | MissingDoForUntil Position -- ^ with position of the "until" keyword
  | MissingDoneForDo Position -- ^ with position of the "do" keyword
  | MissingWordAfterCase
  | MissingInForCase Position -- ^ with position of the "case" keyword
  | MissingPatternAfter String
  | MissingRightParenInCase
  | EsacAsCasePattern
  | MissingEsacForCase Position -- ^ with position of the "case" keyword
  | MissingRightParenInFunction
  | InvalidFunctionName Token
  | MissingFunctionBody
  deriving (Eq, Show)

-- | Parse error description.
data Error = Error {reason :: !Reason, position :: !Position}
  deriving (Eq, Show)

-- | Error severity.
data Severity =
  -- | Severity of errors that are not intended to be recovered by another
  -- possible parse.
  Hard
  -- | Severity of errors that may be recovered by another parse.
  | Soft
  deriving (Eq, Show)

-- | Result of a failed parse.
type Failure = (Severity, Error)

-- | Returns a failed attempt with the given (soft) error.
failureOfError :: MonadError Failure m => Error -> m a
failureOfError e = throwError (Soft, e)

-- | Failure of unknown reason.
failureOfPosition :: MonadError Failure m => Position -> m a
failureOfPosition p = failureOfError (Error UnknownReason p)

-- | Helper function for 'manyTill'. Re-throws the better error.
errorSelecting :: MonadError Failure m
               => (a -> b -> c) -> m a -> m c -> m b -> m c
errorSelecting f a b x = catchError b cont
  where cont e@(Hard, _) = throwError e
        cont e@(Soft, _) = f <$> a' <*> x
          where a' = catchError a reerror
                reerror e'@(Hard, _) = throwError e'
                reerror _            = throwError e

-- | @a `manyTill` end@ parses any number of @a@ until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'lookahead' end@ to keep @end@
-- unconsumed.
manyTill :: MonadError Failure m => m a -> m end -> m [a]
a `manyTill` end = m
  where m = errorSelecting (:) a ([] <$ end) m

-- | @a `someTill` end@ parses one or more @a@ until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'lookahead' end@ to keep @end@
-- unconsumed.
--
-- Also note that @end@ is not tested before @a@ succeeds first. Use
-- @'notFollowedBy' end@ to test @end@ first.
someTill :: MonadError Failure m => m a -> m end -> m (NonEmpty a)
a `someTill` end = (:|) <$> a <*> (a `manyTill` end)

-- | @a manyTo end@ is the same as @a 'manyTill' end@ but the result of
-- @end@ is included in the final result as the last element of the list.
manyTo :: MonadError Failure m => m a -> m a -> m (NonEmpty a)
a `manyTo` end = errorSelecting (:|) a ((:| []) <$> end) m
  where m = errorSelecting (:) a ((:[]) <$> end) m

-- | Recovers from an error. This is a simple wrapper around 'catchError' that
-- ignores the error's 'Severity'.
recover :: MonadError Failure m => m a -> (Error -> m a) -> m a
recover a f = catchError a (f . snd)

-- | @setReason r a@ modifies the result of attempt @a@ by replacing an error
-- of 'UnknownReason' with the given reason @r@. For other reasons or
-- successful results, 'setReason' does not do anything.
setReason :: MonadError Failure m => Reason -> m a -> m a
setReason r m = catchError m (throwError . fmap handle)
  where handle (Error UnknownReason p) = Error r p
        handle x                       = x

-- | 'try' rewrites the result of an attempt by converting a hard error to a
-- soft error with the same reason. The result is not modified if
-- successful.
try :: MonadError Failure m => m a -> m a
try m = catchError m (throwError . handle)
  where handle (_, e) = (Soft, e)

-- | 'require' rewrites the result of an attempt by converting a soft error to
-- a hard error with the same reason. The result is not modified if
-- successful.
require :: MonadError Failure m => m a -> m a
require m = catchError m (throwError . handle)
  where handle (_, e) = (Hard, e)

-- vim: set et sw=2 sts=2 tw=78:
