
module GTLC where

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

fun :: Ty -> Ty
fun TyDyn = TyArr TyDyn TyDyn
fun (TyArr t11 t12) = TyArr t11 t12

expTyp :: Ty -> Ty -> Ty 
expTyp t1 TyDyn = t1 
expTyp TyDyn t1 = t1 
expTyp TyInt TyInt = TyInt
expTyp TyBool TyBool = TyBool
expTyp (TyArr t11 t12) (TyArr t21 t22) = TyArr (expTyp t11 t21) (expTyp t12 t22)
expTyp _ _ = error "error"

consis :: Ty -> Ty -> Bool
consis t1 t2 = if (expTyp t1 t2 /= error "error") then True else False
