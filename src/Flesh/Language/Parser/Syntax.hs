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
  HereDocAliasT,
  -- * Tokens
  backslashed, dollarExpansion, doubleQuoteUnit, doubleQuote, singleQuote,
  wordUnit, tokenTill, neutralToken, identifiedToken, literal,
  -- * Redirections and here-documents
  redirect, hereDocContent, newlineHD, whitesHD, linebreak,
  -- * Syntax
  -- ** Commands
  subshell, groupingTail, doGrouping, whileCommandTail, untilCommandTail,
  command,
  -- ** Lists
  pipeSequence, pipeline, conditionalPipeline, andOrList, compoundList,
  completeLine, program) where

import Control.Applicative (liftA3, many, optional, some, (<|>))
import Control.Monad (join, void, when)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (sequenceA_, toList)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (empty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Flesh.Language.Alias as Alias
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Capture
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Input
import Flesh.Language.Parser.Lex
import Flesh.Language.Syntax
import Flesh.Source.Position
import Numeric.Natural (Natural)
import Prelude hiding (words)

-- | Combination of 'HereDocT' and 'AliasT'.
type HereDocAliasT m a = HereDocT (AliasT m) a

joinAliasHereDocAliasT :: MonadParser m
                       => AliasT m (HereDocAliasT m a) -> HereDocAliasT m a
joinAliasHereDocAliasT = HereDocT . join . lift . fmap runHereDocT

-- | Parses a backslash-escaped character that satisfies the given predicate.
backslashed :: MonadParser m
            => (Char -> Bool) -> m (Positioned DoubleQuoteUnit)
backslashed p = char '\\' *> fmap (fmap Backslashed) (satisfy p)

-- | Parses an expansion that occurs after a dollar.
dollarExpansionTail :: MonadParser m => m DoubleQuoteUnit
dollarExpansionTail = do
  ~(p, c) <- setReason MissingExpansionAfterDollar anyChar
  case c of
    -- TODO arithmetic expansion
    -- TODO braced parameter expansion
    -- TODO unbraced parameter expansion
    '(' -> require $ f <$> execCaptureT cmdsubstBody <* closeParan
      where f = Flesh.Language.Syntax.CommandSubstitution . snd . unzip
            cmdsubstBody = runReaderT program empty
            closeParan = setReason (UnclosedCommandSubstitution p) (char ')')
    _ -> failureOfError (Error MissingExpansionAfterDollar p)

-- | Parses an expansion that starts with a dollar.
dollarExpansion :: MonadParser m => m (Positioned DoubleQuoteUnit)
dollarExpansion = do
  ~(p, _) <- char '$'
  dqu <- dollarExpansionTail
  return (p, dqu)

-- | Parses a double-quote unit, possibly preceded by line continuations.
--
-- The argument predicate defines if a character can be backslash-escaped.
doubleQuoteUnit' :: MonadParser m
                 => (Char -> Bool) -> m (Positioned DoubleQuoteUnit)
doubleQuoteUnit' p = lc $ -- TODO backquote
  backslashed p <|> dollarExpansion <|> fmap (fmap Char) anyChar

-- | Parses a double-quote unit, possibly preceded by line continuations.
doubleQuoteUnit :: MonadParser m => m (Positioned DoubleQuoteUnit)
doubleQuoteUnit = doubleQuoteUnit' (`elem` "\\\"$`")

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
    fmap (fmap Unquoted) (doubleQuoteUnit' (const True))

-- | @tokenTill end@ parses a token, or non-empty word, until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'lookahead' end@ to keep @end@
-- unconsumed.
tokenTill :: MonadParser m => m a -> m Token
tokenTill a = notFollowedBy a >> (require $ Token <$> wordUnit `someTill` a)

-- | Parses a normal non-empty token, delimited by 'endOfToken', without
-- determining its token identifier. Skips whitespaces after the token.
neutralToken :: MonadParser m => m Token
neutralToken = tokenTill endOfToken <* whites

-- | Parses a normal non-empty token followed by optional whitespaces. The
-- token is identified by 'identify'.
identifiedToken :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                => (Text -> Bool)
                -- ^ Function that tests if a token is reserved.
                -> Bool
                -- ^ Whether the token should be checked for an alias. If the
                -- current position 'isAfterBlankEndingSubstitution', this
                -- argument is ignored.
                -> AliasT m (Positioned IdentifiedToken)
identifiedToken isReserved' isAliasable = do
  iabes <- isAfterBlankEndingSubstitution
  pos <- currentPosition
  it <- AliasT $ do
    t <- neutralToken
    runAliasT $ identify isReserved' (isAliasable || iabes) pos t
  return (pos, it)

-- | Parses an unquoted token that matches the given text.
literal :: MonadParser m => Text -> m Token
literal w = neutralToken `satisfying` (\t -> tokenText t == Just w)

-- | Parses a redirection operator (@io_redirect@) and returns the raw result.
-- Skips trailing whitespaces.
redirectBody :: MonadParser m
             => m (Maybe Natural, Positioned String, Token)
redirectBody = liftA3 (,,) (optional ioNumber) redirectOperatorToken
  (require (setReason MissingRedirectionTarget neutralToken))

yieldHereDoc :: Monad m => HereDocOp -> AccumT m (Filler Redirection)
yieldHereDoc op = do
  yieldOperator op
  return $ do
    c <- popContent
    return $ HereDoc op c

-- | Parses a redirection operator (@io_redirect@). Skips trailing
-- whitespaces.
redirect :: MonadParser m => HereDocT m Redirection
redirect = HereDocT $ do
  pos <- currentPosition
  (maybeFd, (_opPos, op), t) <- lift redirectBody
  -- TODO define 0 and 1 as constants elsewhere
  let defaultFd = if "<" `isPrefixOf` op then 0 else 1
      fd' = fromMaybe defaultFd maybeFd
      fileRedir op' = return $ return $ FileRedirection pos fd' op' t
  case op of
    "<"  -> fileRedir In
    "<>" -> fileRedir InOut
    "<&" -> fileRedir DupIn
    ">"  -> fileRedir Out
    ">>" -> fileRedir Append
    ">|" -> fileRedir Clobber
    ">&" -> fileRedir DupOut
    "<<" -> yieldHereDoc $ HereDocOp pos fd' False t
    "<<-" -> yieldHereDoc $ HereDocOp pos fd' True t
    _ -> error $ "unexpected redirection operator " ++ op

hereDocTab :: MonadParser m => Bool -> m ()
hereDocTab tabbed = when tabbed $ void $ many $ char '\t'

hereDocLine :: MonadParser m => Bool -> Bool -> m [Positioned DoubleQuoteUnit]
hereDocLine tabbed isLiteral = do
  hereDocTab tabbed
  fmap NE.toList $ if isLiteral
     then fmap (fmap Char) anyChar `manyTo` nl
     else doubleQuoteUnit' (`elem` "\\$`") `manyTo` lc nl
       where nl = fmap (fmap Char) (char '\n')

hereDocDelimiter :: MonadParser m => Bool -> [DoubleQuoteUnit] -> m ()
hereDocDelimiter tabbed delim = do
  hereDocTab tabbed
  _ <- string (show delim)
  _ <- char '\n'
  return ()

-- | Parses the content of a here-document, including the delimiter.
hereDocContent :: (MonadParser m, MonadAccum m) => HereDocOp -> m ()
hereDocContent op = do
  ls <- line `manyTill` del
  yieldContent $ concat ls
    where line = hereDocLine tabbed quoted
          del = setReason (UnclosedHereDocContent op) $
            hereDocDelimiter tabbed delim
          tabbed = isTabbed op
          ~(quoted, delim) = unquoteToken (delimiter op)

pendingHereDocContents :: (MonadParser m, MonadAccum m) => m ()
pendingHereDocContents = do
  os <- drainOperators
  sequenceA_ $ hereDocContent <$> os

-- | Parses a newline character. Pending here document contents, if any, are
-- also parsed.
newlineHD :: MonadParser m => HereDocT m (Positioned Char)
newlineHD = lift (lc (char '\n')) <*
  HereDocT (return () <$ require pendingHereDocContents)

-- | 'whites' wrapped in 'HereDocT'.
whitesHD :: MonadParser m => HereDocT m (Maybe [Positioned Char])
whitesHD = lift whites

-- | Parses one or more 'newlineHD's optionally followed by 'whites'.
newlineList :: MonadParser m => HereDocT m ()
newlineList = void (some (newlineHD *> whitesHD))

-- | Parses any number of 'newlineHD' optionally followed by 'whites'.
linebreak :: MonadParser m => HereDocT m ()
linebreak = void (many (newlineHD *> whitesHD))

-- | Parses a sequence of (non-assignment) words and redirections.
--
-- Returns a triple of parsed tokens, empty list, and redirections.
simpleCommandArguments :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                       => HereDocAliasT m ([Token], [a], [Redirection])
simpleCommandArguments = arg <*> simpleCommandArguments <|> pure ([], [], [])
  where arg = consRedir <$> redirect' <|> consToken <$> neutralToken'
        consRedir r (ts, as, rs) = (ts, as, r:rs)
        consToken t (ts, as, rs) = (t:ts, as, rs)
        redirect' = mapHereDocT lift redirect
        neutralToken' = lift neutralToken
-- TODO global aliases

-- | Parses a simple command but the first token.
--
-- The first token of the simple command is not parsed by this parser. It must
-- have been parsed by another parser and must be passed as the argument.
simpleCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                  => Token -> HereDocAliasT m Command
simpleCommandTail t1 = toCommand . consToken t1 <$> simpleCommandArguments
  where toCommand (ts, as, rs) = SimpleCommand ts as rs
        consToken t (ts, as, rs) = (t:ts, as, rs)
-- TODO assignments

-- | Parses a subshell command.
subshell :: (MonadParser m, MonadReader Alias.DefinitionSet m)
         => HereDocAliasT m (Positioned CompoundCommand)
subshell = HereDocT $ do
  (pos, _) <- operatorToken "("
  require $ do
    lsFiller <- setReason (MissingCommandAfter "(") $ runHereDocT compoundList
    _ <- setReason (UnclosedSubshell pos) $ operatorToken ")"
    return $ do
      ls <- lsFiller
      return (pos, Subshell ls)

-- | Parses a grouping except the first open brace, which must have just been
-- parsed.
groupingTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => Position -- ^ Position of the open brace.
             -> HereDocT m (Positioned CompoundCommand)
groupingTail p = f <$> body <* closeBrace
  where f ls = (p, Grouping ls)
        body = setReasonHD (MissingCommandAfter openBraceString) $
          mapHereDocT reparse compoundList
        openBraceString = unpack reservedOpenBrace
        closeBrace = lift $ require $ setReason (UnclosedGrouping p) $
          literal reservedCloseBrace

-- | Parses a 'compoundList' surrounded with the "do" and "done" keywords.
doGrouping :: (MonadParser m, MonadReader Alias.DefinitionSet m)
           => Reason -- ^ Error reason in case "do" is missing
           -> HereDocAliasT m (NonEmpty AndOrList)
doGrouping r = HereDocT $ do
  p <- currentPosition
  _ <- setReason r $ literal reservedDo
  require $ do
    body <- setReason (MissingCommandAfter "do") $ runHereDocT compoundList
    _ <- setReason (MissingDoneForDo p) $ literal reservedDone
    return body

whileUntilCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
  => String -- ^ "while" or "until"
  -> (NonEmpty AndOrList -> NonEmpty AndOrList -> CompoundCommand)
  -> (Position -> Reason) -- ^ Error reason in case "do" is missing
  -> Position -- ^ Position of "while" or "until"
  -> HereDocAliasT m (Positioned CompoundCommand)
whileUntilCommandTail s r e p = f <$> cond <*> requireHD (doGrouping (e p))
  where f c b = (p, r c b)
        cond = setReasonHD (MissingCommandAfter s) compoundList

-- | Parses a "while" command except the first "while" keyword, which must
-- have just been parsed.
whileCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                 => Position -- ^ Position of the "while" keyword.
                 -> HereDocAliasT m (Positioned CompoundCommand)
whileCommandTail = whileUntilCommandTail "while" While MissingDoForWhile

-- | Parses a "until" command except the first "until" keyword, which must
-- have just been parsed.
untilCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                 => Position -- ^ Position of the "until" keyword.
                 -> HereDocAliasT m (Positioned CompoundCommand)
untilCommandTail = whileUntilCommandTail "until" Until MissingDoForUntil

-- | Parses a compound command except the first token that determines the type
-- of the compound command.
--
-- The first token is not parsed by this parser. It must have been parsed by
-- another parser and must be passed as the argument. This parser fails if the
-- first token does not start a compound command.
compoundCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                    => Positioned Text
                    -> HereDocAliasT m (Positioned CompoundCommand)
compoundCommandTail (p, t)
  | t == reservedOpenBrace = mapHereDocT lift $ requireHD $ groupingTail p
  | t == reservedWhile = requireHD $ whileCommandTail p
  | t == reservedUntil = requireHD $ untilCommandTail p
  | otherwise = lift $ lift $ failureOfPosition p
  -- TODO if, for, case

-- | Parses a command.
command :: (MonadParser m, MonadReader Alias.DefinitionSet m)
        => HereDocAliasT m Command
command =
  -- First, check if this is a subshell.
  CompoundCommand <$> subshell <*> many redirect' <|>
  -- Next, if the command begins with a redirection, it is a simple command.
  toCommand <$> (consRedir <$> redirect' <*> simpleCommand') <|>
  -- Otherwise, the first token can be a reserved word or alias.
  joinAliasHereDocAliasT
    (compoundOrSimple <$> identifiedToken isReserved True)
    where toCommand (ts, as, rs) = SimpleCommand ts as rs
          consRedir r (ts, as, rs) = (ts, as, r:rs)
          redirect' = mapHereDocT lift redirect
          simpleCommand' = simpleCommandArguments -- TODO parse assignments
          compoundOrSimple (p, Reserved t) = compoundCommandTail' (p, t)
          compoundOrSimple (_, Normal t) = simpleCommandTail t
          compoundCommandTail' t = CompoundCommand <$>
            compoundCommandTail t <*> many redirect
            -- :: Positioned Text -> HereDocAliasT m Command
-- TODO parse function definitions

-- | Parses a @pipe_sequence@, a sequence of one or more commands.
pipeSequence :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => HereDocAliasT m (NonEmpty Command)
pipeSequence = (:|) <$> command <*> many trailer
  where trailer = lift (operatorToken "|") *> linebreak *> requireHD command

-- | Parses a @pipeline@, that is, a 'pipeSequence' optionally preceded by the
-- @!@ reserved word.
pipeline :: (MonadParser m, MonadReader Alias.DefinitionSet m)
         => HereDocAliasT m Pipeline
pipeline =
  lift (literal reservedBang) *> req (make True <$> pipeSequence) <|>
  make False <$> pipeSequence
    where req = setReasonHD (MissingCommandAfter "!") . requireHD
          make = flip Pipeline

-- | Parses an and-or condition token (@&&@ or @||@).
andOrCondition :: MonadParser m => m AndOrCondition
andOrCondition = op <* whites
  where op = do
          (p, o) <- anyOperator
          case o of
            "&&" -> return AndThen
            "||" -> return OrElse
            _ -> failureOfPosition p

-- | Parses a conditional pipeline.
conditionalPipeline :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                    => HereDocAliasT m ConditionalPipeline
conditionalPipeline =
{-
  make <$> lift andOrCondition <* linebreak <*> req pipeline
    where make c p = ConditionalPipeline (c, p)
          req = setReasonHD (MissingCommandAfter undefined) . requireHD
-}
  HereDocT $ do
    c <- andOrCondition
    let req = setReasonHD (MissingCommandAfter (show c)) . requireHD
        make p = ConditionalPipeline (c, p)
    runHereDocT $ linebreak *> (make <$> req pipeline)

-- | Parses a separator operator (@;@ or @&@). Returns True and False if the
-- separator is @&@ and @;@, respectively.
separatorOp :: MonadParser m => m Bool
separatorOp = op <* whites
  where op = do
          (p, o) <- anyOperator
          case o of
            ";" -> return False
            "&" -> return True
            _ -> failureOfPosition p

-- | Parses a separator operator (@;@ or @&@) and/or newlines. Returns True
-- and False if the separator is @&@ and @;@, respectively.
separator :: MonadParser m => HereDocT m Bool
separator = lift separatorOp <* linebreak <|> False <$ newlineList

-- | Parses an and-or list (@and_or@), not including a trailing separator.
andOrList :: (MonadParser m, MonadReader Alias.DefinitionSet m)
          => HereDocAliasT m (Bool -> AndOrList)
andOrList = AndOrList <$> pipeline <*> many conditionalPipeline
  --where sep = lift $ separatorOp <|> return False

-- | Given a separator parser, returns a pair of manyAndOrLists and
-- someAndOrLists.
manySomeAndOrLists :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                   => HereDocAliasT m Bool
                   -> (HereDocAliasT m [AndOrList],
                       HereDocAliasT m (NonEmpty AndOrList))
manySomeAndOrLists sep = (manyAols, someAols)
  where manyAols = toList <$> someAols <|> pure []
        someAols = cons <$> andOrList <*> maybeSepAndMany
        cons aol (a, aols) = aol a :| aols
        maybeSepAndMany = (,) <$> sep <*> manyAols <|> pure (False, [])

-- | Given a separator parser, parses a possibly empty sequence of and-or
-- lists. The lists must be each separated by the separator. The separator is
-- optional after the last list.
manyAndOrLists :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => HereDocAliasT m Bool -> HereDocAliasT m [AndOrList]
manyAndOrLists = fst . manySomeAndOrLists

-- | Given a separator parser, parses a non-empty sequence of and-or lists.
-- The lists must be each separated by the separator. The separator is
-- optional after the last list.
someAndOrLists :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => HereDocAliasT m Bool -> HereDocAliasT m (NonEmpty AndOrList)
someAndOrLists = snd . manySomeAndOrLists

-- | Parses a sequence of one or more and-or lists surrounded by optional
-- linebreaks.
compoundList :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => HereDocAliasT m (NonEmpty AndOrList)
compoundList = linebreak *> someAndOrLists separator

completeLineBody :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                 => HereDocAliasT m [AndOrList]
completeLineBody =
  manyAndOrLists (lift separatorOp) <*
    requireHD (void newlineHD <|> lift (void eof))

-- | Parses a line.
--
-- 1. A line starts with 'optional' 'whites'.
-- 1. A line may contain any number of and-or lists. The lists must be
--    delimited by @;@ or @&@ except the last @;@ may be omitted.
-- 1. A line must be delimited by a 'newlineHD' or 'eof'.
completeLine :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => m [AndOrList]
completeLine = do
  _ <- whites
  reparse $ fill completeLineBody

-- | Parses an entire program.
program :: (MonadParser m, MonadReader Alias.DefinitionSet m) => m [AndOrList]
program = reparse $ fill $ whitesHD *> linebreak *> manyAndOrLists separator
-- TODO should not return UnknownReason

-- vim: set et sw=2 sts=2 tw=78:
