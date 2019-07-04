module DataTypes(
BBinaryOp (And , Or),
BCompareOp (Greater, GreaterE, Equal, LessE, Less),
BExpr(BConst, Not, BBinary, BCompare, VarB),

ABinaryOp(Add , Subtract , Multiply , Divide),
AExpr(Neg, IntConst, ABinary, VarA),

Stmt(Seq, AssignLet, AssignVar, ChangeVal, FCall, If, While, Print, Skip),
GenericExpr(AlgebraicE, BooleanE, IdentifierE, FunctionCallE),
AssignableE(ValueE, FDeclare),
FDExpr(FDExpr),
FCExpr(FCExpr),

VariableType(IntT, StrT, BoolT, FunctionT, Undefined),
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


type Program = [Scope]
type Scope = (Stmt, [ScopeVariables])
type ScopeVariables = [(String, VariableType)]
