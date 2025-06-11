-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since it they are parsed.
-- Student 1: Ivan Mandev (s2989190)
-- Student 2: Ivan Gyunderov (s3138585)
-- Student 3: Darius Luca (s3131777)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Prelude hiding (sum) -- sum id duplicating
import Data.Foldable (foldl)

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All

-- import Debug.Trace

{-Currently available: 
    - 3.1 ,3.2, 3.3, 3.4, 5.2, 5.3
    - twice not working correctly because of higher order functions, changed for the sace of testing to work with inc

  To be implemented: 
    - 5.4,5.5
    - partial application
    - change the eval function so it can take higher order functions
    - change the types in the grammar so we can take higher order functions
-}

--FP3.1 by <Ivan Gyunderov>

type Name = String

data Prog = Prog [Func]
  deriving (Show)

data Func = Func Name [Either Name Int] Expr
  deriving (Show)

data Expr
  = Numb Int
  | Var    Name
  | Call   Name [Expr]
  | Add    Expr Expr
  | Sub    Expr Expr
  | Mul    Expr Expr
  | If     Expr CompOp Expr Expr Expr -- If (e1) op (e2) then {e3} else {e4}
   
    deriving (Show)

data CompOp = Lt | Gt | Eq
  deriving (Show)

  --FP5.4 (--! Not finished)
--data Value = VInt Integer | VFun ([Value] -> Value) -- so we can pass a higher order function to use later 

--type Env = [(Name, Value)] --Custom environment type to swap the [(Name, Integer)] on


--FP3.2 by <Ivan Gyunderov>

fibonacci :: Prog
fibonacci = Prog
  [ Func "fibonacci" [Right 0] (Numb 0) -- fibonacci 0 := 0;
  , Func "fibonacci" [Right 1] (Numb 1) -- fibonacci 1 := 1;
  , Func "fibonacci" [Left "n"]         -- fibonacci n := fibonacci (n-1) + fibonacci (n-2);
      ( Add
          (Call "fibonacci" [Sub (Var "n") (Numb 1)])
          (Call "fibonacci" [Sub (Var "n") (Numb 2)])
      )
  ]

fib :: Prog
fib = Prog
  [ Func "fib" [Left "n"] $
      If (Var "n") Lt (Numb 3)                      -- fib n := if (n < 3) then {
         (Numb 1)
         (Add
            (Call "fib" [Sub (Var "n") (Numb 1)])   -- fib (n-1)
            (Call "fib" [Sub (Var "n") (Numb 2)]))  -- + fib (n-2)
  ]

sum :: Prog
sum = Prog
  [ Func "sum" [Right 0] (Numb 0)               -- sum 0 := 0;
  , Func "sum" [Left "a"] $
      Add
        (Call "sum" [Sub (Var "a") (Numb 1)])   -- sum (a-1)
        (Var "a")                               -- + a
  ]

div :: Prog
div = Prog
  [ Func "div" [Left "x", Left "y"] $                        -- div x y :=
      If (Var "x") Lt (Var "y")                              -- if (x < y) then
         (Numb 0)
         (Add
            (Numb 1)
            (Call "div" [Sub (Var "x") (Var "y"), Var "y"])) -- div ((x-y), y)
  ]

-- twice :: Prog
-- twice = Prog
--   [ Func "twice" [Left "f", Left "x"] $
--       Call "f" [Call "f" [Var "x"]]
--   ]

twice :: Prog
twice = Prog
  [ Func "inc" [Left "x"] (Add (Var "x") (Numb 1))
  , Func "twice" [Left "x"] (Call "inc" [Call "inc" [Var "x"]])
  ]

add :: Prog
add = Prog
  [ Func "add" [Left "x", Left "y"] $
      Add (Var "x") (Var "y")
  ]

inc :: Prog
inc = Prog
  [ Func "inc" [] $
      Call "add" [Numb 1]
  ]

eleven :: Prog
eleven = Prog
  [ Func "eleven" [] $
      Call "inc" [Numb 10]
  ]

--FP3.3 by <Ivan Gyunderov>

pretty :: Prog -> String
pretty (Prog funcs) = unlines (map prettyFunc funcs)

prettyFunc :: Func -> String
prettyFunc (Func name args expr) =
  name ++ " " ++ unwords (map prettyArg args) ++ " := " ++ prettyExpr expr ++ ";"

prettyArg :: Either Name Int -> String
prettyArg (Left n)  = n
prettyArg (Right i) = show i

prettyExpr :: Expr -> String
prettyExpr (Numb n) = show n
prettyExpr (Var n) = n
prettyExpr (Call f es) = f ++ "(" ++ commaSep (map prettyExpr es) ++ ")"
prettyExpr (Add e1 e2) = prettyExprWrap e1 ++ " + " ++ prettyExprWrap e2
prettyExpr (Sub e1 e2) = prettyExprWrap e1 ++ " - " ++ prettyExprWrap e2
prettyExpr (Mul e1 e2) = prettyExprWrap e1 ++ " * " ++ prettyExprWrap e2
prettyExpr (If e1 op e2 e3 e4) =
  "if (" ++ prettyExpr e1 ++ " " ++ prettyCompOp op ++ " " ++ prettyExpr e2 ++ ") then { " ++
  prettyExprWrap e3 ++ " } else { " ++ prettyExprWrap e4 ++ " }"

prettyExprWrap :: Expr -> String
prettyExprWrap e@(Add _ _) = "(" ++ prettyExpr e ++ ")"
prettyExprWrap e@(Sub _ _) = "(" ++ prettyExpr e ++ ")"
prettyExprWrap e@(Mul _ _) = "(" ++ prettyExpr e ++ ")"
prettyExprWrap e@(If _ _ _ _ _) = "(" ++ prettyExpr e ++ ")"
prettyExprWrap e = prettyExpr e

prettyCompOp :: CompOp -> String
prettyCompOp Lt = "<"
prettyCompOp Gt = ">"
prettyCompOp Eq = "=="

commaSep :: [String] -> String
commaSep = foldr1 (\a b -> a ++ ", " ++ b)


--FP3.4 by <Ivan Gyunderov>

  --FP5.2 <Ivan Gyunderov>
  -- Helper function for pattern matching arguments to actual pattern, returning bind if succ
  -- Absolutely useless after implementing patmatch
-- matchArgs :: [Either Name Int] -> [Integer] -> Maybe [(Name, Integer)]
-- matchArgs [] [] = Just []
-- matchArgs (Left n : ps) (a:as) = ((n, a):) <$> matchArgs ps as
-- matchArgs (Right i : ps) (a:as)
--   | fromIntegral i == a = matchArgs ps as
--   | otherwise           = Nothing
-- matchArgs _ _ = Nothing


patmatch :: Prog -> Prog
patmatch (Prog funcs) = Prog (map merge (group funcs))
  where
    group [] = []
    group (f@(Func n _ _) : fs) =
      let (same, rest) = partition n (f:fs)
      in same : group rest

    partition _ [] = ([], [])
    partition n (f@(Func n' _ _) : fs)
      | n == n'   = let (yes, no) = partition n fs in (f:yes, no)
      | otherwise = let (yes, no) = partition n fs in (yes, f:no)

    merge (Func fname args body : rest) =
      let restArgs = tail args
          varName = case head args of
                      Left n  -> n
                      Right _ -> "x"
          mkIfs [Func _ (arg:_) b] =
            case arg of
              Left old -> subst old varName b
              Right _  -> b
          mkIfs [Func _ [] b] = b
          mkIfs (Func _ (Right i : _) b : xs) =
            If (Var varName) Eq (Numb i) b (mkIfs xs)
          mkIfs (Func _ (Left old : _) b : xs) =
            subst old varName b
      in Func fname (Left varName : restArgs) (mkIfs (Func fname args body : rest))

subst :: Name -> Name -> Expr -> Expr -- helper function to replace the old variable names with the new one in the body. Used to fix unbound variable error produced by calling the evalExpr with the changed functions.
subst old new expr = case expr of
  Numb n      -> Numb n
  Var n       -> Var (if n == old then new else n)
  Call f es   -> Call f (map (subst old new) es)
  Add a b     -> Add (subst old new a) (subst old new b)
  Sub a b     -> Sub (subst old new a) (subst old new b)
  Mul a b     -> Mul (subst old new a) (subst old new b)
  If e1 op e2 e3 e4 -> If (subst old new e1) op (subst old new e2) (subst old new e3) (subst old new e4)

  
eval :: Prog -> String -> [Integer] -> Integer
eval prog fname args =
  let Prog funcs = patmatch prog
      Func _ params body = head [f | f@(Func n _ _) <- funcs, n == fname]
      env = [ (n, a) | (Left n, a) <- zip params args ]
  in evalExpr (Prog funcs) env body


-- Old eval function, debugging and used for the matchArgs
-- eval :: Prog -> String -> [Integer] -> Integer
-- eval (Prog funcs) fname args =
--   let matchFuncs = [ (body, env)
--                    | Func n params body <- funcs
--                    , Just env <- [matchArgs params args]
--                    , n == fname
--                    ]
--       (body, env) = traceShow (head matchFuncs) (head matchFuncs) -- get the first possible pattern  --! Remove debug output -> (body, env) = head matchFuncs 
--   in evalExpr (Prog funcs) env body

evalExpr :: Prog -> [(Name, Integer)] -> Expr -> Integer
evalExpr prog env expr = case expr of
  Numb n      -> fromIntegral n
  Var n       -> case lookup n env of 
                      Just v -> v
  Add a b     -> evalExpr prog env a + evalExpr prog env b
  Sub a b     -> evalExpr prog env a - evalExpr prog env b
  Mul a b     -> evalExpr prog env a * evalExpr prog env b
  If e1 op e2 e3 e4 ->
    let v1 = evalExpr prog env e1
        v2 = evalExpr prog env e2
        cond = case op of
          Lt -> v1 < v2
          Gt -> v1 > v2
          Eq -> v1 == v2
    in if cond then evalExpr prog env e3 else evalExpr prog env e4
  Call fn es  -> eval prog fn (map (evalExpr prog env) es)




-- QuickCheck: all prop_* tests

prop_fib_eval_10 :: Bool
prop_fib_eval_10 = eval fib "fib" [10] == 55

prop_fib_eval_1 :: Bool
prop_fib_eval_1 = eval fib "fib" [1] == 1

prop_fib_eval_5 :: Bool
prop_fib_eval_5 = eval fib "fib" [5] == 5

prop_add_eval :: Integer -> Integer -> Bool
prop_add_eval x y = eval add "add" [x, y] == x + y

prop_mul_eval :: Integer -> Integer -> Bool
prop_mul_eval x y = 
  let mulProg = Prog [Func "mul" [Left "x", Left "y"] (Mul (Var "x") (Var "y"))]
  in eval mulProg "mul" [x, y] == x * y

prop_sub_eval :: Integer -> Integer -> Bool
prop_sub_eval x y = 
  let subProg = Prog [Func "sub" [Left "x", Left "y"] (Sub (Var "x") (Var "y"))]
  in eval subProg "sub" [x, y] == x - y

prop_inc_eval :: Integer -> Bool
prop_inc_eval x =
  let incProg = Prog
        [ Func "add" [Left "x", Left "y"] (Add (Var "x") (Var "y"))
        , Func "inc" [Left "y"] (Call "add" [Numb 1, Var "y"])
        ]
  in eval incProg "inc" [x] == x + 1

prop_eleven_eval :: Bool
prop_eleven_eval =
  let elevenProg = Prog
        [ Func "add" [Left "x", Left "y"] (Add (Var "x") (Var "y"))
        , Func "inc" [Left "y"] (Call "add" [Numb 1, Var "y"])
        , Func "eleven" [] (Call "inc" [Numb 10])
        ]
  in eval elevenProg "eleven" [] == 11

prop_prettyProg_idempotent :: Bool
prop_prettyProg_idempotent =
  lines (pretty fib) == ["fib n := if (n < 3) then { 1 } else { (fib(n - 1) + fib(n - 2)) };"]


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a -- helper function so we can do left associativity
chainl1 p op =
  let step x (f,y) = f x y
      rest        = many ((,) <$> op <*> p)
  in  foldl step <$> p <*> rest

compOp :: Parser CompOp
compOp =
      (Eq <$ symbol "==")
  <|> (Lt <$ symbol "<")
  <|> (Gt <$ symbol ">")

ifExpr :: Parser Expr
ifExpr =
  let head = symbol "if" *> parens ((,,) <$> expr <*> compOp <*> expr)
      build  = \(e1,o,e2) e3 e4 -> If e1 o e2 e3 e4
  in  build <$> head
            <*> (symbol "then" *> braces expr)
            <*> (symbol "else" *> braces expr)

callOrVar :: Parser Expr
callOrVar =
      Call <$> identifier
           <*> parens (sep expr (symbol ","))
  <|> Var  <$> identifier

factor :: Parser Expr
factor =
      ifExpr
  <|> Numb . fromIntegral <$> integer
  <|> callOrVar
  <|> parens expr

term :: Parser Expr
term = chainl1 factor (Mul <$ symbol "*")

expr :: Parser Expr
expr = chainl1 term ((Add <$ symbol "+") <|> (Sub <$ symbol "-"))

args :: Parser (Either Name Int)
args = Right . fromIntegral <$> integer <|> Left  <$> identifier

funcParser :: Parser Func
funcParser =
  Func
    <$> identifier
    <*> many args
    <*  symbol ":="
    <*> expr
    <*  symbol ";"

progParser :: Parser Prog
progParser = skipSpaces *> (Prog <$> many (funcParser <* skipSpaces))

compile :: String -> Prog
compile s =
  case runParser (progParser <* skipSpaces) (Stream s) of
    [(p, Stream [])] -> p

runFile :: FilePath -> [Integer] -> IO Integer
runFile path args = do
  src         <- readFile path
  let Prog fs       = compile src
      Func f _ _    = last fs -- so we compile only the last (in our case "main")
  pure (eval (Prog fs) f args)

return []
check = $quickCheckAll
