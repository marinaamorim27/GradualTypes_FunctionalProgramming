module STLC where

import Parser
import Lexer 


addbinding :: Context -> String -> Ty -> Context
addbinding ctx x ty = (x,ty):ctx

typeof :: Context -> Term -> Ty
typeof ctx TmTrue = TyBool
typeof ctx TmFalse = TyBool
typeof ctx (TmInt n) = TyInt
typeof ctx (TmSucc t1) = if (typeof ctx t1 == TyInt) then TyInt else error "type error"
typeof ctx (TmIf t1 t2 t3) =
     case (typeof ctx t1) of
          TyBool -> let tyT2 = typeof ctx t2 
                        tyT3 = typeof ctx t3  
                        in if tyT2 == tyT3 then tyT2 else error "arms of conditional have different types"
          _      -> error "guard of conditional not a boolean"

typeof ctx (TmAbs x tyT1 t2) = 
    let ctx1 = addbinding ctx x tyT1  
        tyT2 = typeof ctx1 t2 in
            TyArr tyT1 tyT2

typeof ctx (TmVar x) = search x ctx 

typeof ctx (TmApp t1 t2) = let tyT1 = typeof ctx t1 
                               tyT2 = typeof ctx t2 in
                               case tyT1 of (TyArr tyT11 tyT12) -> if tyT2 == tyT11 then tyT12 else error "parameter type mismatch"
                                            _                   -> error "arrow type expected"
isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal (TmAbs _ _ t) = isVal t
isVal (TmApp t1 t2) = (isVal t1) && (isVal t2)
isVal (TmVar x) = True
isVal _ = False

search :: String -> Context -> Ty
search v [] = error "variable not found"
search v ((x,y):xs) = if x == v then y else (search v xs)


substitution :: Term -> String -> Term -> Term
substitution (TmVar s) x y = if (x==s) then y else (TmVar s)
substitution (TmAbs y ty t1) x s = if (x == y) then (TmAbs y ty t1) else (TmAbs y ty (substitution t1 x s))
substution (TmApp t1 t2) x s = TmApp (substitution t1 x s) (substitution t2 x s) 


evaluation :: Term -> Term
evaluation (TmApp (TmAbs x ty t12) v2) = substitution t12 x v2                     -- E-APPABS
evaluation (TmApp t1 t2) = if (isVal t1) then (TmApp t11 t2) else (TmApp t1 t21)   -- E-APP1 E-APP2
                        where t11 = evaluation t1
                              t21 = evaluation t2

 



