module DataTypes(
BBinaryOp (And , Or),
BCompareOp (Greater, GreaterE, Equal, LessE, Less),
BExpr(BConst, Not, BBinary, BCompare, VarB),
ABinaryOp(Add , Subtract , Multiply , Divide),
AExpr(Neg, IntConst, ABinary, VarA),
Stmt(Seq,AssignLetA, AssignVarA, ChangeValA, AssignLetB, AssignVarB, ChangeValB, If, While, Print, Skip),
VariableType(IntT, StrT, BoolT),
Program,
Scope,
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

data Stmt =    Seq [Stmt]
               | AssignLetA String AExpr
               | AssignVarA String AExpr
               | ChangeValA String AExpr
               | AssignLetB String BExpr
               | AssignVarB String BExpr
               | ChangeValB String BExpr
               | If BExpr Stmt Stmt
               | While BExpr Stmt
               | Print String
               | Skip
                deriving (Show)

data VariableType = IntT Integer | StrT String | BoolT Bool deriving (Show)


type Program = [Scope]
type Scope = (Stmt, [ScopeVariables])
type ScopeVariables = [(String, VariableType)]
