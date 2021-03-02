module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (runRWS)
import qualified Data.ByteString as B
import Data.ByteString.Builder (hPutBuilder)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import NCTUMPC.Loc (Loc (..))
import NCTUMPC.Parser.Scanner (scan)
import NCTUMPC.Parser.Types
  ( PLog (..),
    POpts (..),
    Token (..),
    mkPState,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (IOMode (..), hPutChar, hPutStrLn, stderr, stdout)
import Text.Printf (hPrintf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      bs <- BL.readFile filepath `catch` openFileXcptHandler filepath
      runScanner bs
    _ -> printUsage

-- | Run the scanner on the given input, printing the log messages to the
-- standard output/error.
runScanner :: BL.ByteString -> IO ()
runScanner = traverse_ f . scanAll
  where
    f List {plSrcLn = ln} = hPutBuilder stdout ln
    f
      LogToken
        { plLoc = Loc {locLn = l, locCol = c},
          plTokenType = tokType,
          plTokenText = txt
        } = do
        hPrintf stderr "token(type:%-10s) on line %4d, %3d : " tokType l c
        B.hPutStr stderr txt
        hPutChar stderr '\n'
    f LogLineComment {plLoc = Loc {locLn = l, locCol = c}} =
      hPrintf stderr "[INFO ] line %4d:%3d comment string\n" l c
    f LogBlockCommentStart {plLoc = Loc {locLn = l, locCol = c}} =
      hPrintf stderr "[INFO ] line %4d:%3d comment string start\n" l c
    f LogBlockCommentEnd {plLoc = Loc {locLn = l, locCol = c}} =
      hPrintf stderr "[INFO ] line %4d:%3d comment string end\n" l c
    f LogLexError {plLoc = Loc {locLn = l, locCol = c}, plTokenText = txt} = do
      hPrintf stderr "[ERROR] line %4d:%3d lexical analyzer error " l c
      B.hPutStr stderr txt
      hPutChar stderr '\n'

-- | Scan all tokens of the given input and return the log messages produced by
-- the scanner.
scanAll :: BL.ByteString -> [PLog]
scanAll input =
  let (_, _, logs) = runRWS (runExceptT go) () (mkPState initOpts input) in logs
  where
    go = do
      token <- scan
      case token of
        (_, TokenEOF) -> return ()
        _ -> go

    initOpts = POpts {pOptList = True, pOptToken = True}

-- | Exception handler for opening the input file.
openFileXcptHandler :: FilePath -> IOException -> IO a
openFileXcptHandler filepath _ = do
  hPutStrLn stderr $ "Error: file \"" ++ filepath ++ "\" open failed."
  exitWith (ExitFailure 1)

-- | Print the program usage to standard error.
printUsage :: IO ()
printUsage = hPutStrLn stderr "Usage: ./scanner [filename]"
