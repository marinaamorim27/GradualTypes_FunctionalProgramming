
{
module Lexer where
}

%wrapper "basic"


$digit = [0-9]
$alpha = [_a-zA-Z]
$white = [\ \t\n\r]

tokens :-

$white+                      ;
if                           {\_ -> TokenIF}
then                         {\_ -> TokenTHEN}
else                         {\_ -> TokenELSE}
int                          {\_ -> TokenInt}
bool                         {\_ -> TokenBool}
true                         {\_ -> TokenTrue}
false                        {\_ -> TokenFalse}
"("                          {\_ -> TokenLParen}
")"                          {\_ -> TokenRParen}
","                          {\_ -> TokenComma}
"?"                          {\_ -> TokenDyn}
":"                          {\_ -> TokenColon}
"-"                          {\_ -> TokenHyphen}
"+"                          {\_ -> TokenPlus}
Dyn                          {\_ -> TokenDYN}
$alpha($alpha|$digit)*       {\s -> TokenString s}
$digit+                      {\s -> TokenNum(read s)}


{
data Token = TokenIF
           | TokenTHEN
           | TokenELSE
           | TokenInt
           | TokenBool
           | TokenTrue 
           | TokenFalse
           | TokenRParen
           | TokenLParen
           | TokenComma
           | TokenDyn 
           | TokenColon
           | TokenHyphen
           | TokenPlus
           | TokenDYN
           | TokenString String
           | TokenNum Integer
          deriving(Eq,Show)
}