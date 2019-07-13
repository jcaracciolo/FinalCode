module DataTypes(
BBinaryOp (..),
BCompareOp (..),
BExpr(..),

ABinaryOp(..),
AExpr(..),

Stmt(..),
GenericExpr(..),
AssignableE(..),
ValueHolder(..),
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
type ScopeVariables     = [(String, VariableType)]

data VariableType       = NumericT Double | StrT String | BoolT Bool | FunctionT FDExpr | ObjectT [(String, VariableType)] | Undefined deriving (Eq, Show)


data ObjCall = ObjCall ObjCall String | ObjFCall ObjCall FCExpr | ObjFBase FCExpr | ObjIBase String deriving(Eq, Show)
data ObjDec  = ObjDec [(String, AssignableE)] deriving(Eq, Show)

---- Statements
data Stmt =    Seq [Stmt]
               | AssignLet String AssignableE
               | AssignVar String AssignableE
               | ChangeVal ValueHolder AssignableE
               | Return AssignableE
               | FCall FCExpr
               | OCall ObjCall
               | If BExpr Stmt Stmt
               | While BExpr Stmt
               | Print AssignableE
               | Skip
                deriving (Eq, Show)

data ValueHolder    = IdentVH String | ObjectVH ObjCall deriving(Eq, Show)

data AssignableE    = ValueE GenericExpr | FDeclare FDExpr | ODec ObjDec deriving (Eq, Show)
data GenericExpr    = AlgebraicE AExpr
                      | BooleanE BExpr
                      | StringE String
                      | IdentifierE String
                      | FunctionCallE FCExpr
                      | ObjCallE ObjCall deriving (Eq, Show)

-- Binary Operations
data BBinaryOp  = And | Or deriving (Eq, Show)
data BCompareOp = Greater | GreaterE | Equal | LessE | Less deriving (Eq, Show)

data BExpr = BConst Bool
          | Not BExpr
          | BBinary BBinaryOp BExpr BExpr
          | BCompare BCompareOp AExpr AExpr
          | VarB String
          | BFCall FCExpr
          | BOCall ObjCall
          deriving (Eq, Show)


data ABinaryOp = Add | Subtract | Multiply | Divide deriving (Eq, Show)
data AExpr = Neg AExpr
           | NumericConst Double
           | ABinary ABinaryOp AExpr AExpr
           | VarA String
           | AFCall FCExpr
           | AOCall ObjCall
             deriving (Eq, Show)

-- Function Expressions (Declaration and Call)
data FDExpr         = FDExpr [String] Stmt deriving (Eq, Show)
data FCExpr         = FCExpr String [GenericExpr]  deriving (Eq, Show)

