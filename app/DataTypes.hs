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
Program,
ProgramState,
ScopeVariables) where


-- Binary Operations
data BBinaryOp = And | Or deriving (Show)
data BCompareOp = Greater | GreaterE | Equal | LessE | Less deriving (Show)

data BExpr = BConst Bool
          | Not BExpr
          | BBinary BBinaryOp BExpr BExpr
          | BCompare BCompareOp AExpr AExpr
          | VarB String
          deriving (Show)


data ABinaryOp = Add | Subtract | Multiply | Divide deriving (Show)
data AExpr = Neg AExpr
           | IntConst Integer
           | ABinary ABinaryOp AExpr AExpr
           | VarA String
             deriving (Show)

data FDExpr         = FDExpr [String] Stmt deriving (Show)
data FCExpr         = FCExpr String [GenericExpr]  deriving (Show)

data GenericExpr    = AlgebraicE AExpr | BooleanE BExpr | IdentifierE String | FunctionCallE FCExpr  deriving (Show)
data AssignableE    = ValueE GenericExpr | FDeclare FDExpr deriving (Show)

data Stmt =    Seq [Stmt]
               | AssignLet String AssignableE
               | AssignVar String AssignableE
               | ChangeVal String AssignableE
               | FCall FCExpr
               | If BExpr Stmt Stmt
               | While BExpr Stmt
               | Print String
               | Skip
                deriving (Show)

data VariableType = IntT Integer | StrT String | BoolT Bool | FunctionT FDExpr | Undefined deriving (Show)


type Program = (Stmt, ProgramState)
type ProgramState = [ScopeVariables]
type ProgramExecution a = (ProgramState, IO ())
type ScopeVariables = [(String, VariableType)]

-- newtype IOState s a = IOState { runState :: s -> (a, s) }

