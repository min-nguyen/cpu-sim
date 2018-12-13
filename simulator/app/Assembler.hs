module Assembler where

import Lib
import Data.Word
import Data.Bits
import Data.Void
import Control.Monad 
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Megaparsec.Char.Lexer hiding (space)
import Control.Applicative hiding (many)
import qualified Data.Vector as V
import Debug.Trace

-- data AssemblyExpr' dest source immediate
--                         = ADD  dest source source
--                         | ADDI source source immediate
--                         | BEQ  source source immediate 
--                         | LW   source source immediate
--                         | J    immediate
--                         | BLTZ dest source immediate

-- type AssemblyExpr = AssemblyExpr' RegisterNum RegisterNum Int 

type Parser = Parsec Void String



parseInt :: Parser Int
parseInt = decimal

parseRegister :: Parser RegisterNum
parseRegister = 
    try (R10 <$ string "R10") <|>
    try (R11 <$ string "R11") <|>
    try (R12 <$ string "R12") <|>
    try (R13 <$ string "R13") <|>
    try (R14 <$ string "R14") <|>
    try (R15 <$ string "R15") <|>
    try (R1 <$ string "R1") <|>
    try (R0 <$ string "R0") <|>
    try (R2 <$ string "R2") <|>
    try (R3 <$ string "R3") <|>
    try (R4 <$ string "R4") <|>
    try (R5 <$ string "R5") <|>
    try (R6 <$ string "R6") <|>
    try (R7 <$ string "R7") <|>
    try (R8 <$ string "R8") <|>
    try (R9 <$ string "R9") 


parseADD :: Parser Instruction
parseADD = Add <$ string "Add" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseSUB :: Parser Instruction
parseSUB = Sub <$ string "Sub" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseMUL :: Parser Instruction
parseMUL = Mult <$ string "Mult" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseADDI :: Parser Instruction
parseADDI = AddI <$ string "AddI" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseInt
parseSUBI :: Parser Instruction
parseSUBI = SubI <$ string "SubI" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseInt
parseDIV :: Parser Instruction
parseDIV = Div <$ string "Div" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseAND :: Parser Instruction
parseAND = And <$ string "And" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseEQ :: Parser Instruction
parseEQ = Eq <$ string "Eq" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseOR :: Parser Instruction
parseOR = Or <$ string "Or" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseLT :: Parser Instruction
parseLT = Lt <$ string "Lt" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseNOT :: Parser Instruction
parseNOT = Not <$ string "Not" <* space <*> parseRegister <* space <*> parseRegister
parseMOVE :: Parser Instruction
parseMOVE = Move <$ string "Move" <* space <*> parseRegister <* space <*> parseRegister
parseMOVEI :: Parser Instruction
parseMOVEI = MoveI <$ string "MoveI" <* space <*> parseRegister <* space <*> parseInt
parseLOADIDX :: Parser Instruction
parseLOADIDX = LoadIdx <$ string "LoadIdx" <* space <*> parseRegister <* space <*> parseRegister  <* space  <*> parseInt
parseLOADBASEIDX :: Parser Instruction
parseLOADBASEIDX = LoadBaseIdx <$ string "LoadBaseIdx" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseSTOREIDX :: Parser Instruction
parseSTOREIDX = StoreIdx <$ string "StoreIdx" <* space <*> parseRegister <* space <*> parseRegister  <* space  <*> parseInt
parseSTOREBASEIDX :: Parser Instruction
parseSTOREBASEIDX = StoreBaseIdx <$ string "StoreBaseIdx" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister
parseB :: Parser Instruction
parseB = B <$ string "B" <* space <*> parseInt
parseBT :: Parser Instruction
parseBT = BT <$ string "BT" <* space <*> parseRegister <* space <*> parseInt
parseBF :: Parser Instruction
parseBF = BF <$ string "BF" <* space <*> parseRegister <* space <*> parseInt
parseRET :: Parser Instruction
parseRET = Ret <$ string "Ret" 
parseEND :: Parser Instruction
parseEND = End <$ string "End" 
parseLABEL :: Parser Instruction
parseLABEL = Utils.Label <$ string "Label" <*  space <*> parseInt
-- sc :: Parser ()
-- sc = L.space (() <$ some (char ' ')) lineCmnt blockCmnt
--   where
--     lineCmnt  = L.skipLineComment "//"
--     blockCmnt = L.skipBlockComment "/*" "*/"

parseInstruction :: Parser Instruction
parseInstruction = 
    try parseADDI  <|>
    try parseSUBI  <|>
    try parseADD <|>
    try  parseSUB  <|>
    try  parseMUL  <|>
    try parseDIV  <|>
    try parseAND  <|>
    try parseEND <|>
    try parseOR  <|>
    try parseLT  <|>
    try parseNOT  <|>
    try parseMOVEI  <|>
    try parseMOVE  <|>
    try parseLOADBASEIDX  <|>
    try parseLOADIDX  <|>
    try parseSTOREBASEIDX  <|>
    try parseSTOREIDX  <|>
    try parseLABEL <|>
    try parseBT  <|>
    try parseBF  <|>
    try parseB <|>
    try parseRET <|>
    try parseEQ 

parseAssembly :: Parser [Instruction]
parseAssembly = sepEndBy1 parseInstruction eol <* eof

parseFile :: String -> IO [Instruction]
parseFile file = do 
    program <- readFile file 
    print program
    case parse parseAssembly "" program of 
        Left e -> print e >> fail "parse error"
        Right r -> return r
