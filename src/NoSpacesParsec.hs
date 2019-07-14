module NoSpacesParsec(
noSpacesIdentifier,
noSpacesReserved,
noSpacesReservedOp,
whiteSpace1,
noSpacesParens,
noSpacesBrackets,
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import LanguageDef
import Data.List
import Data.Char

noSpacesReserved::String -> Parser String
noSpacesReserved name = string name

ident
    = do{ c <- identStart languageDef
        ; cs <- many (identLetter languageDef)
        ; return (c:cs)
        }
    <?> "identifier"

noSpacesIdentifier  = try $
                      do{ name <- ident
                        ; if (isReservedName name)
                           then unexpected ("reserved word " ++ show name)
                           else return name
                        }

isReservedName name
    = isReserved theReservedNames caseName
    where
      caseName      | caseSensitive languageDef  = name
                    | otherwise               = map toLower name

isReserved names name
    = scan names
    where
      scan []       = False
      scan (r:rs)   = case (compare r name) of
                        LT  -> scan rs
                        EQ  -> True
                        GT  -> False

theReservedNames
    | caseSensitive languageDef  = sort reserved
    | otherwise                  = sort . map (map toLower) $ reserved
        where
          reserved = reservedNames languageDef


noSpacesReservedOp name =
    try $
    do{ _ <- string name
      ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
      }

operator =
    try $
    do{ name <- oper
      ; if (isReservedOp name)
         then unexpected ("reserved operator " ++ show name)
         else return name
      }

oper =
    do{ c <- (opStart languageDef)
      ; cs <- many (opLetter languageDef)
      ; return (c:cs)
      }
    <?> "operator"

isReservedOp name =
    isReserved (sort (reservedOpNames languageDef)) name


whiteSpace1
    | noLine && noMulti  = skipMany1 (simpleSpace <?> "")
    | noLine             = skipMany1 (simpleSpace <|> multiLineComment <?> "")
    | noMulti            = skipMany1 (simpleSpace <|> oneLineComment <?> "")
    | otherwise          = skipMany1 (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
    where
      noLine  = null (commentLine languageDef)
      noMulti = null (commentStart languageDef)


simpleSpace =
    skipMany1 (satisfy isSpace)

oneLineComment =
    do{ _ <- try (string (commentLine languageDef))
      ; skipMany (satisfy (/= '\n'))
      ; return ()
      }

multiLineComment =
    do { _ <- try (string (commentStart languageDef))
       ; inComment
       }

inComment
    | nestedComments languageDef  = inCommentMulti
    | otherwise                = inCommentSingle

inCommentMulti
    =   do{ _ <- try (string (commentEnd languageDef)) ; return () }
    <|> do{ multiLineComment                     ; inCommentMulti }
    <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
    <|> do{ _ <- oneOf startEnd                  ; inCommentMulti }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

inCommentSingle
    =   do{ _ <- try (string (commentEnd languageDef)); return () }
    <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
    <|> do{ _ <- oneOf startEnd                 ; inCommentSingle }
    <?> "end of comment"
    where
      startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

noSpacesParens::Parser a -> Parser a
noSpacesParens p = between (string "(") (string ")") p

noSpacesBrackets::Parser a -> Parser a
noSpacesBrackets p = between (string "[") (string "]") p