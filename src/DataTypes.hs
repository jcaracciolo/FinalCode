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
MState,
ObjCall(..),
ObjDec(..)
) where

import Control.Monad.State


--- Type Declarations
type MState s = StateT s IO

type ProgramState       = [ScopeVariables]
type ProgramExecution a = (ProgramState, IO ())
type ScopeVariables     = [(String, VariableType)]
data VariableType       = IntT Integer | StrT String | BoolT Bool | FunctionT FDExpr | ObjectT [(String, VariableType)] | Undefined deriving (Show)


data ObjCall = ObjCall ObjCall String | ObjFCall ObjCall FCExpr | ObjFBase FCExpr | ObjIBase String deriving(Show)
data ObjDec  = ObjDec [(String, AssignableE)] deriving(Show)

---- Statements
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
               | OCall ObjCall
                deriving (Show)

data AssignableE    = ValueE GenericExpr | FDeclare FDExpr | ODec ObjDec deriving (Show)
data GenericExpr    = AlgebraicE AExpr | BooleanE BExpr | IdentifierE String | FunctionCallE FCExpr | ObjCallE ObjCall deriving (Show)

-- Binary Operations
data BBinaryOp  = And | Or deriving (Show)
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

-- Function Expressions (Declaration and Call)
data FDExpr         = FDExpr [String] Stmt deriving (Show)
data FCExpr         = FCExpr String [GenericExpr]  deriving (Show)


