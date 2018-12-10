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
parseInt = (decimal) 

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


parseSW :: Parser Instruction 
parseSW = SW <$ string "SW" <* space <*> parseRegister <* space <*> parseRegister

parseSI :: Parser Instruction 
parseSI = SI <$ string "SI" <* space <*> parseRegister <* space <*> parseInt

parseLI :: Parser Instruction
parseLI = LI <$ string "LI" <* space <*> parseRegister <* space <*> parseInt

parseADD :: Parser Instruction
parseADD = ADD <$ string "ADD" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister

parseADDI :: Parser Instruction
parseADDI = ADDI <$ string "ADDI" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseInt

parseBEQ :: Parser Instruction
parseBEQ = BEQ <$ string "BEQ" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseInt

parseLW :: Parser Instruction
parseLW = LW <$ string "LW"  <* space <*> parseRegister <* space <*> parseRegister 

parseLTH :: Parser Instruction
parseLTH = LTH <$ string "LTH" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister

parseJMP :: Parser Instruction
parseJMP = JMP <$ string "JMP" <*> parseInt 

parseBLT :: Parser Instruction
parseBLT = BLT <$ string "BLT" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseInt

parseCMP :: Parser Instruction
parseCMP = CMP <$ string "CMP" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister


parseInstruction :: Parser Instruction
parseInstruction =  try parseLW <|> try parseLI <|> try parseSW <|> try parseADD <|> try parseADDI <|> try parseBEQ <|> 
                    try parseBLT <|> try parseJMP <|> try parseLTH <|> try parseCMP <|> try parseSI

parseAssembly :: Parser [Instruction]
parseAssembly = sepEndBy1 parseInstruction eol <* eof

parseFile :: String -> IO [Instruction]
parseFile file = do 
    program <- readFile file 
    print program
    case parse parseAssembly "" program of 
        Left e -> print e >> fail "parse error"
        Right r -> return r
