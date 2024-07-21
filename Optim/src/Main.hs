module Main (main) where

import Data.Map (Map)
import Data.List (foldl')
import Test.LeanCheck

import qualified Data.Map as M

-- =============================================================================
--  Types
-- =============================================================================

data Program = Program String [Input] [Stmt] deriving Show
data Input = Input String Type deriving Show
data Type = U8 deriving Show
data Stmt = Assign String Expr deriving Show
data Expr = Binary Value Op Expr | Val Value deriving Show
data Value = Int' Int | Identifier String | Expr' Expr deriving Show
data Op = Add | Sub | Mul | Div deriving Show

-- =============================================================================
--  Constant-folding optimization
-- =============================================================================

eval :: Op -> Int -> Int -> Int
eval Add m n = m + n
eval Sub m n = m - n
eval Mul m n = m * n
eval Div m n = m `div` n

foldVal :: Value -> Value
foldVal (Expr' e) =
  case foldExpr e of
    Val v -> v
    e' -> Expr' e'
foldVal v = v

foldExpr :: Expr -> Expr
foldExpr (Val v) = Val (foldVal v)
foldExpr (Binary v op expr) = go (foldVal v) (foldExpr expr)
  where
    go (Int' m) (Val (Int' n)) = Val (Int' (eval op m n))
    go v' expr' = Binary v' op expr'

foldStmt :: Stmt -> Stmt
foldStmt (Assign var expr) = Assign var (foldExpr expr)

fold :: Program -> Program
fold (Program name inputs stmts) = Program name inputs (map foldStmt stmts)

propagateVal :: Map String Int -> Value -> Value
propagateVal consts  = undefined

propagateExpr :: Map String Int -> Expr -> Expr
propagateExpr consts (Binary v op e) = _ $ Binary (propagateVal consts v) op (propagateExpr consts e)
propagateExpr consts (Val (Identifier s)) =
  case M.lookup s consts of
    Nothing -> Val (Identifier s)
    Just n  -> Val (Int' n)
propagateExpr _ (Val v) = Val v

propagate1 :: (Map String Int, [Stmt]) -> Stmt -> (Map String Int, [Stmt])
propagate1 (consts, result) (Assign var e) = (undefined, Assign var (propagateExpr consts e) : result)

propagate :: Program -> Program
propagate (Program name inputs stmts) = Program name inputs result
  where
    (consts, result) = foldl' propagate1 (M.empty, []) stmts

optimize :: Program -> Program
optimize = propagate . fold

-- =============================================================================
--  Enumerative testing
-- =============================================================================

instance Listable Program where
  tiers = cons3 Program

instance Listable Input where
  tiers = cons2 Input

instance Listable Type where
  tiers = cons0 U8

instance Listable Stmt where
  tiers = cons2 Assign

instance Listable Expr where
  tiers =  cons3 Binary
        \/ cons1 Val

instance Listable Value where
  tiers =  cons1 Int'
        \/ cons1 Identifier
        \/ cons1 Expr'

instance Listable Op where
  tiers =  cons0 Add
        \/ cons0 Sub
        \/ cons0 Mul
        \/ cons0 Div

isFoldedVal :: Value -> Bool
isFoldedVal (Expr' e) = isFoldedExpr e
isFoldedVal _ = True

isFoldedExpr :: Expr -> Bool
isFoldedExpr (Val v) = isFoldedVal v
isFoldedExpr (Binary (Int' _) _ (Val (Int' _))) = False
isFoldedExpr (Binary v _ e) = isFoldedVal v && isFoldedExpr e

isFoldedStmt :: Stmt -> Bool
isFoldedStmt (Assign _ expr) = isFoldedExpr expr

isFoldedProgram :: Program -> Bool
isFoldedProgram (Program _ _ stmts) = all isFoldedStmt stmts

main :: IO ()
main = do
  print $ foldExpr $ Binary (Int' 5) Add (Val (Int' 3))
  print $ foldExpr $ Binary (Int' 5) Add (Binary (Int' 13) Mul (Val (Int' 14)))
  checkFor 10000 $ \p -> isFoldedProgram $ fold p
