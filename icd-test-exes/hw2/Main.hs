module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS (runRWS)
import qualified Data.ByteString as B
import Data.ByteString.Builder (hPutBuilder)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import NCTUMPC.Loc (Loc (..))
import NCTUMPC.Parser (parse)
import NCTUMPC.Parser.Types
  ( PLog (..),
    POpts (..),
    Token (..),
    mkPState,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (IOMode (..), hPutChar, hPutStr, hPutStrLn, stderr, stdout)
import Text.Printf (hPrintf)

main :: IO ()
main = do
  args <- getArgs
  bs <- case args of
    [] -> BL.getContents
    [filepath] -> BL.readFile filepath `catch` openFileXcptHandler filepath
    _ -> do
      printUsage
      exitSuccess
  runParser bs

-- | Run the parser on the given input, printing the log messages to the
-- standard output/error.
runParser :: BL.ByteString -> IO ()
runParser input = traverse_ f logs
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
    f
      LogParseError
        { plLoc = Loc {locLn = l, locCol = c},
          plLineBuffer = buf,
          plTokenText = txt
        } = do
        hPrintf stderr "[ERROR] line %4d:%3d " l c
        hPutBuilder stderr buf
        hPutStr stderr ", Unmatched token: "
        B.hPutStr stderr txt
        hPutChar stderr '\n'

    (_, _, logs) = runRWS (runExceptT parse) () (mkPState initOpts input)

    initOpts = POpts {pOptList = True, pOptToken = False}

-- | Exception handler for opening the input file.
openFileXcptHandler :: FilePath -> IOException -> IO a
openFileXcptHandler filepath _ = do
  hPutStrLn stderr $ "Error: file \"" ++ filepath ++ "\" open failed."
  exitWith (ExitFailure 1)

-- | Print the program usage to standard error.
printUsage :: IO ()
printUsage = hPutStrLn stderr "Usage: ./scanner [filename]"
