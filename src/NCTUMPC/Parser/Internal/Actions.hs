-- |
-- Module      : NCTUMPC.Parser.Internal.Actions
-- Description : Scanner and parser actions
-- Copyright   : (c) Po-Yi Tsai, 2021
-- License     : Unlicense
-- Maintainer  : abt8601@protonmail.ch
-- Stability   : unstable
-- Portability : portable
--
-- Scanner and parser actions.
module NCTUMPC.Parser.Internal.Actions where

import Control.Monad (when)
import Control.Monad.State (get, gets, modify, put)
import Control.Monad.Writer (tell)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import NCTUMPC.Loc (Loc (..), LocSpan (..))
import NCTUMPC.Parser.Types
  ( AlexInput (..),
    P,
    PLog (..),
    POpts (..),
    PState (..),
    alexGetByte,
  )

-- * State Action

-- | @updateToken input len@ sets the token text to the first @len@ bytes of the
-- input, increments the last column number by @len@, and sets the input to
-- @input@.
--
-- Used right after a token of length @len@ is scanned by the scanner.
updateToken :: AlexInput -> Int -> P ()
updateToken input len = modify $
  \s@PState {psInput = AlexInput {aiBytes = bs}, psLastColNo = c} ->
    s
      { psInput = input,
        psTokenText = BL.toStrict $ BL.take (fromIntegral len) bs,
        psLastColNo = c + len
      }

-- | Set start code.
begin :: Int -> P ()
begin code = modify $ \s -> s {psStartCode = code}

-- | Copy end column number to start column number.
xferColNo :: P ()
xferColNo = modify $ \s@PState {psLastColNo = c} -> s {psFirstColNo = c}

-- | Increment line number and reset column number.
nextLine :: P ()
nextLine = modify $
  \s@PState {psLineNo = l} -> s {psLineNo = l + 1, psLastColNo = 1}

-- | Set 'pOptList'.
setOptList :: Bool -> P ()
setOptList opt = modify $
  \s@PState {psOpts = opts} -> s {psOpts = opts {pOptList = opt}}

-- | Set 'pOptToken'.
setOptToken :: Bool -> P ()
setOptToken opt = modify $
  \s@PState {psOpts = opts} -> s {psOpts = opts {pOptToken = opt}}

-- | Get and parse token text.
readTokenText :: (Read a) => P a
readTokenText = gets (read . T.unpack . decodeUtf8 . psTokenText)

-- | Get token location.
getTokenLoc :: P LocSpan
getTokenLoc = do
  PState {psLineNo = l, psFirstColNo = sc, psLastColNo = ec} <- get
  return
    LocSpan
      { locFirst = Loc {locLn = l, locCol = sc},
        locLast = Loc {locLn = l, locCol = ec}
      }

-- * Input Action

-- | Advance the given AlexInput by a byte.
--
-- Crashes when the end of input is encountered.
advanceInput :: AlexInput -> AlexInput
advanceInput =
  snd . expect "advanceInput: end of input encountered" . alexGetByte
  where
    expect _ (Just x) = x
    expect msg Nothing = error msg

-- * Log Action

-- | Add the current token text to the line buffer.
list :: P ()
list = modify $ \s@PState {psTokenText = txt, psLineBuffer = buf} ->
  s {psLineBuffer = buf <> byteString txt}

-- | Flush the line buffer and produce a source code listing log message.
listFlush :: P ()
listFlush = do
  s@PState {psOpts = POpts {pOptList = list}, psLineBuffer = buf} <- get
  when list $ tell [List buf]
  put s {psLineBuffer = mempty}

-- | Log token.
logToken ::
  -- | Token type.
  String ->
  P ()
logToken tokType = do
  PState
    { psOpts = POpts {pOptToken = token},
      psTokenText = txt,
      psLineNo = l,
      psFirstColNo = c
    } <-
    get
  when token $
    tell
      [ LogToken
          { plLoc = Loc {locLn = l, locCol = c},
            plTokenType = tokType,
            plTokenText = txt
          }
      ]

-- | Log line comment.
logLineComment :: P ()
logLineComment = do
  PState {psOpts = POpts {pOptToken = token}, psLineNo = l, psFirstColNo = c} <-
    get
  when token $ tell [LogLineComment {plLoc = Loc {locLn = l, locCol = c}}]

-- | Log start of block comment.
logBlockCommentStart :: P ()
logBlockCommentStart = do
  PState {psOpts = POpts {pOptToken = token}, psLineNo = l, psFirstColNo = c} <-
    get
  when token $ tell [LogBlockCommentStart {plLoc = Loc {locLn = l, locCol = c}}]

-- | Log end of block comment.
logBlockCommentEnd :: P ()
logBlockCommentEnd = do
  PState {psOpts = POpts {pOptToken = token}, psLineNo = l, psFirstColNo = c} <-
    get
  when token $ tell [LogBlockCommentEnd {plLoc = Loc {locLn = l, locCol = c}}]

-- | Log lexical analyser error.
logError :: P ()
logError = do
  PState {psTokenText = txt, psLineNo = l, psFirstColNo = c} <- get
  tell [LogLexError {plLoc = Loc {locLn = l, locCol = c}, plTokenText = txt}]
