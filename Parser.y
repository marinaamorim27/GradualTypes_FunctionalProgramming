{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

if                           {TokenIF}
then                         {TokenTHEN}
else                         {TokenELSE}
int                          {TokenInt}
bool                         {TokenBool}
true                         {TokenTrue}
false                        {TokenFalse}
'('                          {TokenLParen}
')'                          {TokenRParen}
','                          {TokenComma}
'?'                          {TokenDyn}
':'                          {TokenColon}
'-'                          {TokenHyphen}
'+'                          {TokenPlus}
Dyn                          {TokenDYN}
string                       {TokenString $$}
num                          {TokenNum $$}


%left '(' ')'

%%

Derivation
         : Context '-' Term ':' Ty                  {($1,$3,$5)} 

Context
      : '('string ',' Ty ')' ',' Context            {($2,$4):$7}
      | '(' string ',' Ty ')'                       {[($2,$4)]}


Type 
 : bool                          {TypeBool}
 | int                           {TypeInt}
 | Ty Ty                         {TypeArr $1 $2}
 | Dyn                           {TyDyn $1} 

Term 
    : true                            {TmTrue}
    | false                           {TmFalse}
    | num                             {TmInt $1}
    | '?'                             {TmDyn}                    
    | if Term then Term else Term     {TmIf $2 $4 $6}                          
    | string                          {TmVar $1}
    | string Ty Term                  {TmAbs $1 $2 $3}
    | Term Term                       {TmApp $1 $2}
    | num '+' num                     {TmAdd $1 $3}

{        

type Context = [(String,Ty)]

data Type = TypeBool
          | TypeInt
          | TypeArr Type Type
          | TypeDyn 

data Term = TmTrue                          
          | TmFalse
          | TmInt Integer
          | TmDyn                         
          | TmIf Term Term Term       
          | TmVar String                 
          | TmAbs String Ty Term        
          | TmApp Term Term
          | TmSucc Term 
          | TmAdd Integer Integer             
          deriving (Eq,Show)

parseError :: [Token] -> a
parseError toks = error("parse error at " ++ show toks)
}
