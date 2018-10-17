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

-- type AssemblyExpr = AssemblyExpr' RegisterNum RegisterNum Word32 

type Parser = Parsec Void String



parseWord32 :: Parser Word32
parseWord32 = (fromIntegral <$> decimal) 

parseRegister :: Parser RegisterNum
parseRegister = 
    try (trace "R0" $ R0 <$ string "R0") <|>
    try (trace "R1" $ R1 <$ string "R1") <|>
    try (trace "R2" $ R2 <$ string "R2") <|>
    try (R3 <$ string "R3") <|>
    try (R4 <$ string "R4") <|>
    try (R5 <$ string "R5") <|>
    try (R6 <$ string "R6") <|>
    try (R7 <$ string "R7")

parseSW :: Parser Instruction 
parseSW = SW <$ string "SW" <* space <*> parseRegister <* space <*> parseWord32

parseLI :: Parser Instruction
parseLI = trace "here" $ LI <$ string "LI" <* space <*> parseRegister <* space <*> parseWord32

parseADD :: Parser Instruction
parseADD = ADD <$ string "ADD" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseRegister

parseADDI :: Parser Instruction
parseADDI = ADDI <$ string "ADDI" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseWord32

parseBEQ :: Parser Instruction
parseBEQ = BEQ <$ string "BEQ" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseWord32

parseLW :: Parser Instruction
parseLW = LW <$ string "LW" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseWord32

parseJ :: Parser Instruction
parseJ = J <$ string "J" <* space <*> parseWord32

parseBLTZ :: Parser Instruction
parseBLTZ = BLTZ <$ string "BLTZ" <* space <*> parseRegister <* space <*> parseRegister <* space <*> parseWord32

parseInstruction :: Parser Instruction
parseInstruction =  try parseLW <|> try parseLI <|> try parseSW <|> try parseADD <|> try parseADDI <|> try parseBEQ <|> 
                    try parseBLTZ <|> try parseJ

parseAssembly :: Parser [Instruction]
parseAssembly = sepEndBy1 parseInstruction eol <* eof

parseFile :: String -> IO [Instruction]
parseFile file = do 
    program <- readFile file 
    print program
    case parse parseAssembly "" program of 
        Left e -> print e >> fail "parse error"
        Right r -> return r
