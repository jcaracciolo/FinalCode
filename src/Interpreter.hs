module Interpreter(
mainInterpreter
)where

import System.IO
import Compiler
import DataTypes
import Control.Monad.State.Lazy
import LanguageDef
import TokenParser
import Text.Parsec.Error
import Text.ParserCombinators.Parsec
import Data.List
import qualified Control.Exception as Ex
import PrettyPrinter



data InterpreterResult = IError ParseError | IException Ex.SomeException | IExpectedMore String | ISuccess VariableType

evalInterpreter::Stmt -> MState ProgramState VariableType
evalInterpreter (Seq ((OCall objCall):[]))                          = evalObjCall objCall
evalInterpreter (Seq ((FCall fcexpr):[]))                           = evalFCall fcexpr
evalInterpreter (a)                                      = eval a >> (return Undefined)


tryInterpret::String -> MState ProgramState InterpreterResult
tryInterpret append =
                        do
                        line <- (liftIO getLine)
                        case parse (whiteSpace >> program) "" (append ++ line) of
                            Left e  -> do
                                       if (isInfixOf ("unexpected end of input") (show e))
                                       then return $ IExpectedMore (append ++ line)
                                       else return $ IError e
                            Right r -> do
                                       result <- evalInterpreter r
                                       return (ISuccess result)

returnExc::Ex.SomeException -> MState ProgramState InterpreterResult
returnExc e = return $ IException e

mainInterpreter = do
           putStrLn "Welcome to Final Code Interpreter"
           putStr ">> "
           hFlush stdout
           runStateT (loopInterpreter "") [[]]
           print "Bye"

loopInterpreter:: String -> MState ProgramState InterpreterResult
loopInterpreter s     = do
                        result <- tryInterpret s
                        case result of
                             IError e        -> liftIO (print e) >> liftIO (putStr ">>") >> liftIO (hFlush stdout) >> loopInterpreter ""
                             IExpectedMore s -> loopInterpreter s
                             ISuccess Undefined -> liftIO (putStr ">> ") >> liftIO (hFlush stdout) >> loopInterpreter ""
                             ISuccess a         -> liftIO (putStrLn (toStringOut a)) >> liftIO (putStr ">> ") >> liftIO (hFlush stdout) >> loopInterpreter ""
