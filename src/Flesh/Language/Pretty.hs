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
Portability : portable

This module implements pretty printing feature.
-}
module Flesh.Language.Pretty (main) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Map.Lazy
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Language.Parser.Syntax
import Flesh.Language.Syntax.Print
import Flesh.Source.Position
import System.Exit
import System.IO.Error

data InputRecord = InputRecord {
  readChars :: [Positioned Char],
  eofError :: Maybe IOError}

type StandardInputT = StateT InputRecord

runStandardInputT :: Functor m => StandardInputT m a -> m a
runStandardInputT = fmap fst . flip runStateT r
  where r = InputRecord {readChars = [], eofError = Nothing}

stdinFragment :: String -> Int -> Fragment
stdinFragment = flip Fragment StandardInput

nextPosition :: String -> [Positioned Char] -> Position
nextPosition s [] = Position {fragment = stdinFragment s 0, index = 0}
nextPosition s [pc] = Position {fragment = stdinFragment s n, index = 0}
  where n = lineNo (fragment (fst pc)) + 1
nextPosition s (_:pcs) = nextPosition s pcs

readNextLine :: (MonadState InputRecord m, MonadIO m) => m ()
readNextLine = do
  errorOrLine <- liftIO (tryIOError getLine)
  InputRecord {readChars = rcs, eofError = ee} <- get
  case errorOrLine of
    Left e -> put InputRecord {readChars = rcs, eofError = Just e}
    Right l -> put InputRecord {readChars = rcs ++ lcs, eofError = ee}
      where lcs = unposition (spread p l')
            p = nextPosition l' rcs
            l' = l ++ "\n"

inputChar :: (MonadState InputRecord m, MonadIO m)
     => Int -> m (Either Position (Positioned Char))
inputChar i = ci
  where ci = do
          InputRecord {readChars = rcs, eofError = ee} <- get
          case drop i rcs of
            (pc:_) -> return (Right pc)
            [] ->
              case ee of
                Just _ -> return (Left (nextPosition "" rcs))
                Nothing -> readNextLine >> ci

inputCharPosition :: (MonadState InputRecord m) => Int -> m Position
inputCharPosition i = do
  InputRecord {readChars = rcs, eofError = _} <- get
  return $ case drop i rcs of
    [] -> nextPosition "" rcs
    ((p, _):_) -> p

data InputPosition = InputPosition {
  underlyingIndex :: Int,
  pushedChars :: [Positioned Char]}

newtype CursorT m a = CursorT (StateT InputPosition m a)

runCursorT :: CursorT m a -> StateT InputPosition m a
runCursorT (CursorT m) = m

runCursorT' :: Functor m => CursorT m a -> m a
runCursorT' = fmap fst . flip runStateT p . runCursorT
  where p = InputPosition {underlyingIndex = 0, pushedChars = []}

instance Functor m => Functor (CursorT m) where
  fmap f = CursorT . fmap f . runCursorT

instance Monad m => Applicative (CursorT m) where
  pure = CursorT . pure
  CursorT a <*> CursorT b = CursorT (a <*> b)

instance Monad m => Monad (CursorT m) where
  CursorT a >>= f = CursorT (a >>= runCursorT . f)

instance MonadError e m => MonadError e (CursorT m) where
  throwError = CursorT . throwError
  catchError (CursorT a) f =
    CursorT (catchError a (runCursorT . f))

instance (MonadState InputRecord m, MonadIO m) => MonadInput (CursorT m) where
  popChar = CursorT $ do
    InputPosition {underlyingIndex = i, pushedChars = pcs} <- get
    case pcs of
      [] -> do
        put InputPosition {underlyingIndex = i + 1, pushedChars = []}
        lift (inputChar i)
      (c:cs) -> do
        put InputPosition {underlyingIndex = i, pushedChars = cs}
        return $ Right c

  peekChar = CursorT $ do
    InputPosition {underlyingIndex = i, pushedChars = pcs} <- get
    case pcs of
      [] -> lift (inputChar i)
      (c:_) -> return $ Right c

  lookahead (CursorT m) = CursorT $ do
    oldPos <- get
    v <- m
    put oldPos
    return v

  currentPosition = CursorT $ do
    InputPosition {underlyingIndex = i, pushedChars = pcs} <- get
    case pcs of
      [] -> lift (inputCharPosition i)
      ((p, _):_) -> return p

  pushChars cs = CursorT $ do
    InputPosition {underlyingIndex = i, pushedChars = pcs} <- get
    put InputPosition {underlyingIndex = i, pushedChars = cs ++ pcs}

readCompleteLine :: IO (Either Failure [AndOrList])
readCompleteLine = runStandardInputT $ runExceptT $ runCursorT' $
  flip runReaderT empty $ runParserT $ notFollowedBy eof *> completeLine

writeCompleteLine :: Either Failure [AndOrList] -> IO ()
writeCompleteLine (Left _e) = exitFailure -- TODO write error
writeCompleteLine (Right aols) = putStr (runPrint (printList aols) "")

-- | Repeatedly reads commands from the standard input and pretty-prints them
-- to the standard output.
main :: IO ()
main = readCompleteLine >>= writeCompleteLine >> main

-- vim: set et sw=2 sts=2 tw=78:
