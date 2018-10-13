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
import Text.Megaparsec.Char.Lexer
import Control.Applicative
import qualified Data.Vector as V



-- data AssemblyExpr' dest source immediate
--                         = ADD  dest source source
--                         | ADDI source source immediate
--                         | BEQ  source source immediate 
--                         | LW   source source immediate
--                         | J    immediate
--                         | BLTZ dest source immediate

-- type AssemblyExpr = AssemblyExpr' RegisterNum RegisterNum Word32 

type Parser = Parsec Void String

parseWord32 :: Parser Word32
parseWord32 = fromIntegral <$> decimal

parseRegister :: Parser RegisterNum
parseRegister = 
    try (R0 <$ string "R0") <|>
    try (R1 <$ string "R1") <|>
    try (R2 <$ string "R2") <|>
    try (R3 <$ string "R3") <|>
    try (R4 <$ string "R4") <|>
    try (R5 <$ string "R5") <|>
    try (R6 <$ string "R6") <|>
    try (R7 <$ string "R7") <|>
    try (R8 <$ string "R8") 


parseADD :: Parser Instruction
parseADD = ADD <$ string "ADD" <*> parseRegister <*> parseRegister <*> parseRegister

parseADDI :: Parser Instruction
parseADDI = ADDI <$ string "ADDI" <*> parseRegister <*> parseRegister <*> parseWord32

parseBEQ :: Parser Instruction
parseBEQ = BEQ <$ string "BEQ" <*> parseRegister <*> parseRegister <*> parseWord32

parseLW :: Parser Instruction
parseLW = LW <$ string "LW" <*> parseRegister <*> parseRegister <*> parseWord32

parseJ :: Parser Instruction
parseJ = J <$ string "J" <*> parseWord32

parseBLTZ :: Parser Instruction
parseBLTZ = BLTZ <$ string "BLTZ" <*> parseRegister <*> parseRegister <*> parseWord32

parseInstruction :: Parser Instruction
parseInstruction =  try parseADD <|> try parseADDI <|> try parseBEQ <|> 
                    try parseBLTZ <|> try parseJ <|> try parseLW

parseAssembly :: Parser [Instruction]
parseAssembly = sepEndBy1 parseInstruction eol <* eof

parseFile :: String -> IO [Instruction]
parseFile file = do 
    program <- readFile file 
    case parse parseAssembly "" program of 
        Left e -> print e >> fail "parse error"
        Right r -> return r
