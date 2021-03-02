{
-- |
-- Module      : NCTUMPC.Parser.Scanner
-- Description : MiniPascal scanner
-- Copyright   : (c) Po-Yi Tsai, 2021
-- License     : Unlicense
-- Maintainer  : abt8601@protonmail.ch
-- Stability   : experimental
-- Portability : portable
--
-- MiniPascal scanner.
module NCTUMPC.Parser.Scanner
  ( scan,
    scanCont,
  )
where

import Control.Monad.State (get, gets)
import NCTUMPC.Loc
import NCTUMPC.Parser.Internal.Actions
import NCTUMPC.Parser.Types
}

$a = [aA]
$b = [bB]
$c = [cC]
$d = [dD]
$e = [eE]
$f = [fF]
$g = [gG]
$h = [hH]
$i = [iI]
$j = [jJ]
$k = [kK]
$l = [lL]
$m = [mM]
$n = [nN]
$o = [oO]
$p = [pP]
$q = [qQ]
$r = [rR]
$s = [sS]
$t = [tT]
$u = [uU]
$v = [vV]
$w = [wW]
$x = [xX]
$y = [yY]
$z = [zZ]

@ident = [a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9]|[a-zA-Z]

@integer = [0-9]+
@real = [0-9]+\.[0-9]+
@scientific = [0-9]+(\.[0-9]+)?[eE][\+\-]?[0-9]+

@stringlit = \"([^\"\\]|\\.)*\"

@pragma_list_on = \ *\#\ *pragma\ +list\ +on\ *
@pragma_list_off = \ *\#\ *pragma\ +list\ +off\ *
@pragma_token_on = \ *\#\ *pragma\ +token\ +on\ *
@pragma_token_off = \ *\#\ *pragma\ +token\ +off\ *

minipascal :-

  <0> $p$r$o$g$r$a$m       { logToken "KEYWORD" >> return (Just TokenProgram) }
  <0> $v$a$r               { logToken "KEYWORD" >> return (Just TokenVar) }
  <0> $a$r$r$a$y           { logToken "KEYWORD" >> return (Just TokenArray) }
  <0> $o$f                 { logToken "KEYWORD" >> return (Just TokenOf) }
  <0> $i$n$t$e$g$e$r       { logToken "KEYWORD" >> return (Just TokenInteger) }
  <0> $r$e$a$l             { logToken "KEYWORD" >> return (Just TokenReal) }
  <0> $s$t$r$i$n$g         { logToken "KEYWORD" >> return (Just TokenString) }
  <0> $f$u$n$c$t$i$o$n     { logToken "KEYWORD" >> return (Just TokenFunction) }
  <0> $p$r$o$c$e$d$u$r$e   { logToken "KEYWORD" >> return (Just TokenProcedure) }
  <0> $b$e$g$i$n           { logToken "KEYWORD" >> return (Just TokenBegin) }
  <0> $e$n$d               { logToken "KEYWORD" >> return (Just TokenEnd) }
  <0> $i$f                 { logToken "KEYWORD" >> return (Just TokenIf) }
  <0> $t$h$e$n             { logToken "KEYWORD" >> return (Just TokenThen) }
  <0> $e$l$s$e             { logToken "KEYWORD" >> return (Just TokenElse) }
  <0> $w$h$i$l$e           { logToken "KEYWORD" >> return (Just TokenWhile) }
  <0> $d$o                 { logToken "KEYWORD" >> return (Just TokenDo) }
  <0> $n$o$t               { logToken "KEYWORD" >> return (Just TokenNot) }
  <0> $a$n$d               { logToken "KEYWORD" >> return (Just TokenAnd) }
  <0> $o$r                 { logToken "KEYWORD" >> return (Just TokenOr) }

  <0> "("                  { logToken "KEYWORD" >> return (Just TokenLParen) }
  <0> ")"                  { logToken "KEYWORD" >> return (Just TokenRParen) }
  <0> ";"                  { logToken "KEYWORD" >> return (Just TokenSemicolon) }
  <0> "."                  { logToken "KEYWORD" >> return (Just TokenDot) }
  <0> ","                  { logToken "KEYWORD" >> return (Just TokenComma) }
  <0> ":"                  { logToken "KEYWORD" >> return (Just TokenColon) }
  <0> "["                  { logToken "KEYWORD" >> return (Just TokenLBrace) }
  <0> "]"                  { logToken "KEYWORD" >> return (Just TokenRBrace) }
  <0> ".."                 { logToken "KEYWORD" >> return (Just TokenDotDot) }
  <0> ":="                 { logToken "KEYWORD" >> return (Just TokenAssignment) }
  <0> "+"                  { logToken "KEYWORD" >> return (Just TokenAddOp) }
  <0> "-"                  { logToken "KEYWORD" >> return (Just TokenSubOp) }
  <0> "*"                  { logToken "KEYWORD" >> return (Just TokenMulOp) }
  <0> "/"                  { logToken "KEYWORD" >> return (Just TokenDivOp) }
  <0> ">"                  { logToken "KEYWORD" >> return (Just TokenGtOp) }
  <0> "<"                  { logToken "KEYWORD" >> return (Just TokenLtOp) }
  <0> "="                  { logToken "KEYWORD" >> return (Just TokenEqOp) }
  <0> ">="                 { logToken "KEYWORD" >> return (Just TokenGetOp) }
  <0> "<="                 { logToken "KEYWORD" >> return (Just TokenLetOp) }
  <0> "!="                 { logToken "KEYWORD" >> return (Just TokenNeqOp) }

  <0> @ident               { logToken "IDENTIFIER" >> Just . TokenIdentifier <$> gets psTokenText }

  <0> @integer             { logToken "NUMBER" >> Just . TokenIntegerNum <$> readTokenText }
  <0> @real                { logToken "NUMBER" >> Just . TokenRealNumber <$> readTokenText }
  <0> @scientific          { logToken "NUMBER" >> Just . TokenScientific <$> readTokenText }

  <0> "//".*               { logLineComment >> return Nothing }

  <0> "/*"                 { logBlockCommentStart >> begin blockComment >> return Nothing }
  <blockComment> "*/"      { logBlockCommentEnd >> begin 0 >> return Nothing }
  <blockComment> .         ;

  <0> @stringlit           { logToken "STRING" >> Just . TokenLiteralStr <$> gets psTokenText }

  <0> ^@pragma_list_on$    { setOptList True >> return Nothing }
  <0> ^@pragma_list_off$   { setOptList False >> return Nothing }
  <0> ^@pragma_token_on$   { setOptToken True >> return Nothing }
  <0> ^@pragma_token_off$  { setOptToken False >> return Nothing }

  <0> [\ \t\f\r]           ;

  \n                       { listFlush >> nextLine >> return Nothing }

{
-- | Scan a token.
scan :: P (LocSpan, Token)
scan = do
  xferColNo
  PState {psInput = i, psStartCode = s} <- get
  case alexScan i s of
    AlexEOF -> do
      loc <- getTokenLoc
      return (loc, TokenEOF)
    AlexError _ -> do
      updateToken (advanceInput i) 1
      list
      logError
      scan
    AlexSkip i' l -> do
      updateToken i' l
      list
      scan
    AlexToken i' l action -> do
      updateToken i' l
      list
      result <- action
      case result of
        Nothing -> scan
        Just tok -> do
          loc <- getTokenLoc
          return (loc, tok)

-- | Scan a token and run the given continuation.
scanCont :: ((LocSpan, Token) -> P a) -> P a
scanCont cont = scan >>= cont
}
