module Main (main) where

import Test.LeanCheck

data Program = Program String [Input] [Statement] deriving Show
data Input = Input String Type deriving Show
data Type = U8 deriving Show
data Statement = Assign String Expression deriving Show
data Expression = Binary Value Operator Expression | Val Value deriving Show
data Value = Integer' Int | Identifier String | Expr Expression deriving Show
data Operator = Add | Subtract | Multiply | Divide deriving Show

instance Listable Program where
  tiers = cons3 Program

instance Listable Input where
  tiers = cons2 Input

instance Listable Type where
  tiers = cons0 U8

instance Listable Statement where
  tiers = cons2 Assign

instance Listable Expression where
  tiers =  cons3 Binary
        \/ cons1 Val

instance Listable Value where
  tiers =  cons1 Integer'
        \/ cons1 Identifier
        \/ cons1 Expr

instance Listable Operator where
  tiers =  cons0 Add
        \/ cons0 Subtract
        \/ cons0 Multiply
        \/ cons0 Divide

eval :: Operator -> Int -> Int -> Int
eval Add m n = m + n
eval Subtract m n = m - n
eval Multiply m n = m * n
eval Divide m n = m `div` n

foldVal :: Value -> Value
foldVal (Expr e) =
  case foldExpr e of
    Val v -> v
    e' -> Expr e'
foldVal v = v

foldExpr :: Expression -> Expression
foldExpr (Val v) = Val (foldVal v)
foldExpr (Binary v op expr) =
  case (v', expr') of
    (Integer' m, Val (Integer' n)) -> Val (Integer' (eval op m n))
    _ -> Binary v' op expr'
  where
    v' = foldVal v
    expr' = foldExpr expr

foldStmt :: Statement -> Statement
foldStmt (Assign var expr) = Assign var (foldExpr expr)

fold :: Program -> Program
fold (Program name inputs stmts) = Program name inputs (map foldStmt stmts)

isFoldedVal :: Value -> Bool
isFoldedVal (Expr e) = isFoldedExpr e
isFoldedVal _ = True

isFoldedExpr :: Expression -> Bool
isFoldedExpr (Val v) = isFoldedVal v
isFoldedExpr (Binary (Integer' _) _ (Val (Integer' _))) = False
isFoldedExpr (Binary v _ e) = isFoldedVal v && isFoldedExpr e

isFoldedStmt :: Statement -> Bool
isFoldedStmt (Assign _ expr) = isFoldedExpr expr

isFoldedProgram :: Program -> Bool
isFoldedProgram (Program _ _ stmts) = all isFoldedStmt stmts

main :: IO ()
main = do
  print $ foldExpr $ Binary (Integer' 5) Add (Val (Integer' 3))
  print $ foldExpr $ Binary (Integer' 5) Add (Binary (Integer' 13) Multiply (Val (Integer' 14)))
  checkFor 10000 $ \p -> isFoldedProgram $ fold p
