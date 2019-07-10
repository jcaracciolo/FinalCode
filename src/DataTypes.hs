module DataTypes(
BBinaryOp (..),
BCompareOp (..),
BExpr(..),

ABinaryOp(..),
AExpr(..),

Stmt(..),
GenericExpr(..),
AssignableE(..),
FDExpr(..),
FCExpr(..),

VariableType(..),
ProgramState,
ScopeVariables,
MState) where

import Control.Monad.State

-- Binary Operations
data BBinaryOp = And | Or deriving (Show)
data BCompareOp = Greater | GreaterE | Equal | LessE | Less deriving (Show)

data BExpr = BConst Bool
          | Not BExpr
          | BBinary BBinaryOp BExpr BExpr
          | BCompare BCompareOp AExpr AExpr
          | VarB String
          | BFCall FCExpr
          deriving (Show)


data ABinaryOp = Add | Subtract | Multiply | Divide deriving (Show)
data AExpr = Neg AExpr
           | IntConst Integer
           | ABinary ABinaryOp AExpr AExpr
           | VarA String
           | AFCExpr FCExpr
           | AFCall FCExpr
             deriving (Show)

data FDExpr         = FDExpr [String] Stmt deriving (Show)
data FCExpr         = FCExpr String [GenericExpr]  deriving (Show)

data GenericExpr    = AlgebraicE AExpr | BooleanE BExpr | IdentifierE String | FunctionCallE FCExpr  deriving (Show)
data AssignableE    = ValueE GenericExpr | FDeclare FDExpr deriving (Show)

data Stmt =    Seq [Stmt]
               | AssignLet String AssignableE
               | AssignVar String AssignableE
               | ChangeVal String AssignableE
               | Return AssignableE
               | FCall FCExpr
               | If BExpr Stmt Stmt
               | While BExpr Stmt
               | Print String
               | Skip
                deriving (Show)

data VariableType = IntT Integer | StrT String | BoolT Bool | FunctionT FDExpr | Undefined deriving (Show)


type ProgramState = [ScopeVariables]
type ProgramExecution a = (ProgramState, IO ())
type ScopeVariables = [(String, VariableType)]

type MState s = StateT s IO
