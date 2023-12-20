import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO

-- 언어의 추상 구문 트리를 정의합니다.
data Exp = Var String
         | Lit Int
         | Negate Exp
         | Positive Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         deriving (Show)

data Stmt = Assign String Exp deriving (Show)

-- 변수 할당을 추적하는 상태를 정의합니다.
type MyState = Map String Int

-- 언어를 위한 파서를 정의합니다.
parseProgram :: Parser [Stmt]
parseProgram = spaces *> endBy parseAssignment spaces <* eof

parseAssignment :: Parser Stmt
parseAssignment = do
  name <- parseIdentifier
  spaces *> char '=' *> spaces
  exp <- parseExp
  char ';'
  return $ Assign name exp

parseExp :: Parser Exp
parseExp = chainl1 parseTerm parseAddOp

parseTerm :: Parser Exp
parseTerm = chainl1 parseFact parseMulOp

parseFact :: Parser Exp
parseFact = parseParenExp <|> parseNegateExp <|> parsePositiveExp <|> parseLiteral <|> parseVarExp

parseParenExp :: Parser Exp
parseParenExp = char '(' *> spaces *> parseExp <* spaces <* char ')'

parseNegateExp :: Parser Exp
parseNegateExp = char '-' *> spaces *> (Negate <$> parseFact)

parsePositiveExp :: Parser Exp
parsePositiveExp = char '+' *> spaces *> (Positive <$> parseFact)

parseLiteral :: Parser Exp
parseLiteral = Lit <$> parseIntLiteral

parseIntLiteral :: Parser Int
parseIntLiteral = read <$> ((string "0" <* notFollowedBy digit) <|> parseNonZeroDigit)

parseNonZeroDigit :: Parser String
parseNonZeroDigit = fmap (:[]) $ oneOf "123456789"

parseVarExp :: Parser Exp
parseVarExp = Var <$> parseIdentifier

parseIdentifier :: Parser String
parseIdentifier = do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (first:rest)

parseAddOp :: Parser (Exp -> Exp -> Exp)
parseAddOp = (char '+' *> spaces *> pure Plus) <|> (char '-' *> spaces *> pure Minus)

parseMulOp :: Parser (Exp -> Exp -> Exp)
parseMulOp = char '*' *> spaces *> pure Times

-- 현재 상태를 기반으로 표현식을 평가합니다.
evalExp :: Exp -> MyState -> Int
evalExp (Var name) myState = case Map.lookup name myState of
  Just val -> val
  Nothing -> error $ "Uninitialized variable: " ++ name
evalExp (Lit value) _ = value
evalExp (Negate exp) myState = - evalExp exp myState
evalExp (Positive exp) myState = evalExp exp myState
evalExp (Plus exp1 exp2) myState = evalExp exp1 myState + evalExp exp2 myState
evalExp (Minus exp1 exp2) myState = evalExp exp1 myState - evalExp exp2 myState
evalExp (Times exp1 exp2) myState = evalExp exp1 myState * evalExp exp2 myState

-- 새로운 변수 할당으로 상태를 업데이트합니다.
updateMyState :: MyState -> String -> Exp -> MyState
updateMyState myState name exp = Map.insert name (evalExp exp myState) myState

-- 명령문 목록을 해석합니다.
interpretProgram :: [Stmt] -> MyState -> Either String MyState
interpretProgram [] myState = Right myState
interpretProgram (Assign name exp : rest) myState =
  interpretProgram rest (updateMyState myState name exp)

-- 인터프리터를 실행하는 주 함수입니다.
runInterpreter :: String -> IO ()
runInterpreter input = case parse parseProgram "" input of
  Left err -> putStrLn $ "ERROR: " ++ show err
  Right stmts -> case interpretProgram stmts Map.empty of
    Left err -> putStrLn err
    Right finalState -> mapM_ putStrLn $ map (\(var, val) -> var ++ " = " ++ show val) $ Map.toList finalState

-- 샘플 입력 및 출력
main :: IO ()
main = do
  putStrLn "Input:"
  input <- getLine
  putStrLn "\nOutput:"
  runInterpreter input
  putStrLn "\n"
  
{- main :: IO ()
main = do
  putStrLn "입력 1"
  runInterpreter "x = 001;"
  
  putStrLn "\n입력 2"
  runInterpreter "x_2 = 0;"
  
  putStrLn "\n입력 3"
  runInterpreter "x = 0\ny = x;\nz = ---(x+y);"
  
  putStrLn "\n입력 4"
  runInterpreter "x = 1;\ny = 2;\nz = ---(x+y)*(x+-y);" -}
