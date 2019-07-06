module Interpreter(
mainInterpreter
)where

import Compiler
import DataTypes
import Control.Monad.State.Lazy
import LanguageDef
import TokenParser
import Text.Parsec.Error
import Text.ParserCombinators.Parsec
import Data.List



data InterpreterResult = IError ParseError | IExpectedMore String | ISuccess

tryInterpret::String -> MState ProgramState InterpreterResult
tryInterpret append = do
                      line <- (liftIO getLine)
                      case parse (whiteSpace >> program) "" (append ++ line) of
                          Left e  -> do
                                     if (isInfixOf ("unexpected end of input") (show e))
                                     then return $ IExpectedMore (append ++ line)
                                     else return $ IError e
                          Right r -> do
                                     eval r
                                     return ISuccess
                                     -- TODO there is an issue with var return = 5



mainInterpreter = do
           putStrLn "Welcome to Final Code Interpreter"
           runStateT (loopInterpreter "") [[]]
           print "Bye"

loopInterpreter:: String -> MState ProgramState InterpreterResult
loopInterpreter s     = do
                        liftIO (putStr ">>")
                        result <- tryInterpret s
                        case result of
                             IError e        -> liftIO (print e) >> loopInterpreter ""
                             IExpectedMore s -> loopInterpreter s
                             ISuccess        -> loopInterpreter ""
