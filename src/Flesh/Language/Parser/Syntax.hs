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

Collection of parser monads that take some input and return abstract syntax
tree, error, and warnings.
-}
module Flesh.Language.Parser.Syntax (
  module Flesh.Language.Syntax,
  -- * Tokens
  backslashed, doubleQuoteUnit, doubleQuote, singleQuote, wordUnit, tokenTill,
  normalToken, aliasableToken,
  -- * Syntax
  simpleCommand, list) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Flesh.Language.Alias as Alias
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Lex
import Flesh.Language.Syntax
import Flesh.Source.Position

-- | Parses a backslash-escaped character that is parsed by the given parser.
backslashed :: MonadParser m
            => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
backslashed m = char '\\' *> fmap (fmap Backslashed) m

-- | Parses a double-quote unit, possibly preceded by line continuations.
--
-- The argument parser is used to parse a backslashed character.
doubleQuoteUnit' :: MonadParser m
                 => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
doubleQuoteUnit' c = lc $ -- TODO parse expansions
  backslashed c <|> fmap (fmap Char) anyChar

-- | Parses a double-quote unit, possibly preceded by line continuations.
doubleQuoteUnit :: MonadParser m => m (Positioned DoubleQuoteUnit)
doubleQuoteUnit = doubleQuoteUnit' (oneOfChars "\\\"$`")

-- | Parses a pair of double quotes containing any number of double-quote
-- units.
doubleQuote :: MonadParser m => m (Positioned WordUnit)
doubleQuote = do
  let dq = lc (char '"')
  (p, _) <- dq
  let f units = (p, DoubleQuote units)
      closeQuote = setReason UnclosedDoubleQuote dq
  require $ f <$> doubleQuoteUnit `manyTill` closeQuote

-- | Parses a pair of single quotes containing any number of characters.
singleQuote :: MonadParser m => m (Positioned WordUnit)
singleQuote = do
  let sq = char '\''
  (p, _) <- lc sq
  let f chars = (p, SingleQuote chars)
      closeQuote = setReason UnclosedSingleQuote (char '\'')
  require $ f <$> anyChar `manyTill` closeQuote

-- | Parses a word unit.
wordUnit :: MonadParser m => m (Positioned WordUnit)
wordUnit = lc $
  doubleQuote <|> singleQuote <|>
    fmap (fmap Unquoted) (doubleQuoteUnit' anyChar)

-- | @tokenTill end@ parses a token, or non-empty word, until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'lookahead' end@ to keep @end@
-- unconsumed.
tokenTill :: MonadParser m => m a -> m Token
tokenTill a = notFollowedBy a >> (require $ Token <$> wordUnit `someTill` a)

-- | Parses a normal non-empty token, delimited by 'endOfToken'. Skips
-- whitespaces after the token.
normalToken :: MonadParser m => m Token
normalToken = tokenTill endOfToken <* whites

-- | Like 'normalToken', but tries to perform alias substitution on the
-- result. If alias substitution was successful, this parser returns
-- 'Nothing'.
aliasableToken :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => m (Maybe Token)
aliasableToken = do
  t <- normalToken
  let inv Nothing = Just t
      inv (Just ()) = Nothing
      tt = MaybeT $ return $ tokenText t
   in fmap inv $ runMaybeT $ tt >>= substituteAlias
   -- TODO substitute the next token if the current substitute ends with a
   -- blank.

-- | Parses a simple command. Skips whitespaces after the command.
--
-- Returns 'Nothing' if the command was not parsed because of alias
-- substitution.
simpleCommand :: (MonadParser m, MonadReader Alias.DefinitionSet m)
              => m (Maybe Command)
simpleCommand = runMaybeT $ f <$> h <*> t
  where f h' t' = SimpleCommand (h':t') [] []
        h = MaybeT aliasableToken
        t = lift (many normalToken)
-- TODO global aliases
-- TODO assignments
-- TODO Redirections

-- | FIXME
list :: (MonadParser m, MonadReader Alias.DefinitionSet m) => m Command
list = reparse simpleCommand

-- vim: set et sw=2 sts=2 tw=78:
