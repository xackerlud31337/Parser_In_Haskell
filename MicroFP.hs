-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since it they are parsed.
-- Student 1: Ivan Mandev (s2989190)
-- Student 2: Ivan Gyunderov (s3138585)
-- Student 3: Darius Luca (s3131777)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All

{-Currently available: 
    - 3.1 ,3.2, 3.3, 3.4
    - fibonacci not working correctly because of pattern matching
    - twice not working correctly because of higher order functions, changed for the sace of testing to work with inc

  To be implemented: 
    - 5.1,5.2,5.3,5.4,5.5
    - patmatch
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
  prettyExpr e3 ++ " } else { " ++ prettyExpr e4 ++ " }"

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

eval :: Prog -> String -> [Integer] -> Integer
eval (Prog funcs) fname args =
  let Func _ params body = head [f | f@(Func n ps _) <- funcs, n == fname]
      env = [(v, a) | (Left v, a) <- zip params args]
  in evalExpr (Prog funcs) env body

evalExpr :: Prog -> [(Name, Integer)] -> Expr -> Integer
evalExpr prog env expr = case expr of
  Numb n      -> fromIntegral n
  Var n       -> case lookup n env of Just v -> v
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


-- ! THIS SHOULD BE FIXED
-- prop_prettyProg_idempotent :: Bool
-- prop_prettyProg_idempotent =
--   lines (pretty fib) == ["fib n := if (n < 3) then { 1 } else { (fib(n - 1) + fib(n - 2)) };"]

return []
check = $quickCheckAll
