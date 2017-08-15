--- Task 4, b)

-- 1. Below is the interpeter extended with a new special form, DLambda.
--    Check the comments that explain the modifications, and run the tests.

-- 2. We need a special type of runtime value to represent dlambda, because
--    it requires a different evaluation rule, given that the environment is
--    extended in a distinct way, not available in ordinary procedures.

-- 3. The evaluation of a dlambda is simple, we just need to capture its parameters
--    and body into a new type of procedure object. The real magic happens when the
--    procedure is applied, that's the moment when the current environment is extended
--    giving rise to a dynamical scope.

-- Expr is the type of abstract syntax trees
data Expr = PrimI   Int                   -- An integer
          | PrimB   Bool                  -- A boolean
          | Ref     String                -- a variable
          | Let     [(String, Expr)] Expr -- a let expression
          | If      Expr Expr Expr        -- an if expression
          | Lambda  [String] Expr         -- a lexical \-expression
          | DLambda [String] Expr         -- a dynamic \-expression
          | Apply   Expr [Expr]           -- a procedure application
          deriving (Show)

-- Proc is the type of all the types of procedures that exist at runtime
data Proc = Proc    Expr [String] Env    -- user procedure
          | DProc   Expr [String]        -- user dynamic procedure
          | PrimF2I (Int -> Int -> Int)  -- primitive
          | PrimF2B (Int -> Int -> Bool) -- primitive

-- A value returned by eval is an integer, a bool, or a procedure
type Value = Either Int (Either Bool Proc)

toProc :: Proc -> Value
toProc = Right . Right

instance Show Proc where
  show (Proc exp par _) = "Proc:"  ++ (show par) ++ (show exp)
  show (DProc exp par)  = "DProc:" ++ (show par) ++ (show exp) -- show DProc
  show (PrimF2I _)      = "Prim: Int -> Int -> Int"
  show (PrimF2B _)      = "Prim: Int -> Int -> Bool"

-- An environment maps variable names to values
type Env = String -> Value

-- The empty environment always fails at mapping
theEmptyEnv :: Env
theEmptyEnv = \key -> error ("Not found: " ++ key)

-- Add more primitives here!
globalEnv :: Env
globalEnv = extendEnv ["+", "-", "*", "=="]
                      (map toProc [PrimF2I (+), PrimF2I (-), PrimF2I (*), PrimF2B (==)])
                      theEmptyEnv

-- Dynamic type checking. Either we get the expected type, or we crash
checkInt :: Value -> Int
checkInt (Left x)           = x
checkInt _                  = error "Integer number expected"

checkBool :: Value -> Bool
checkBool (Right (Left x))  = x
checkBool _                 = error "Boolean expected"

checkProc :: Value -> Proc
checkProc (Right (Right x)) = x
checkProc _                 = error "Procedure expected"

-- The evaluator takes an expression and an environment
eval :: Expr -> Env -> Value
eval (PrimI int)        env = Left int
eval (PrimB bool)       env = Right (Left bool)
eval (Ref var)          env = env var -- lookup = apply the env
eval (Let binds exp)    env = let  (vars, exps) = unzip binds
                              in   eval exp (extendEnv vars (map (flip eval env) exps) env)
eval (If cnd thn els)   env = if   (checkBool (eval cnd env))
                              then eval thn env
                              else eval els env
eval (Lambda  pars exp) env = toProc (Proc  exp pars env)
eval (DLambda pars exp) env = toProc (DProc exp pars) -- handle DLambda
eval (Apply proc args)  env = apply (checkProc (eval proc env))
                                    (map (flip eval env) args)
                                    env -- also pass the current environment

apply :: Proc -> [Value] -> Env -> Value
-- For applying a lexically-scoped lambda, extend the environment
-- captured by the lambda (the one that existed when it was created)
apply (Proc  exp pars env) args _   = eval exp (extendEnv pars args env)
-- For applying a dynamically-scoped lambda, extend the current environment
-- (the one that exists when the lambda is invoked)
apply (DProc exp pars)     args env = eval exp (extendEnv pars args env)
apply prim                 args _   = applyPrim prim args

applyPrim (PrimF2I f) (a1:a2:[]) = Left $ f (checkInt a1) (checkInt a2)
applyPrim (PrimF2I f) _          = error "Wrong # arguments: Int -> Int -> Int"
applyPrim (PrimF2B f) (a1:a2:[]) = Right $ Left $ f (checkInt a1) (checkInt a2)
applyPrim (PrimF2B f) _          = error "Wrong # arguments: Int -> Int -> Bool"
applyPrim _           _          = error "Unknown kind of primitive"

bind :: Env -> String -> Value -> Env
bind old var val = \key -> if key == var then val else old key

extendEnv :: [String] -> [Value] -> Env -> Env
extendEnv [] [] env = env
extendEnv [] _  env = error "Too many arguments"
extendEnv _  [] env = error "Too few arguments"
extendEnv (s:ss) (v:vs) env = bind (extendEnv ss vs env) s v

-- Test programs

-- This wouldn't work when using a lexically-scoped Lambda
-- because the `y` variable was defined after the `f` procedure
prog1 = Let [("f", DLambda ["x"] (Apply (Ref "+") [Ref "x", Ref "y"])),
             ("y", PrimI 10)]
            (Apply (Ref "f") [PrimI 10])

-- When defining `f` as a lexically-scoped Lambda, this program evaluates to `30`
prog2 = Let [("y", PrimI 10)]
            (Let [("f", Lambda ["x"] (Apply (Ref "+") [Ref "x", Ref "y"]))]
                (Let [("g", Lambda []
                                (Let [("y", PrimI 30)] (Apply (Ref "f") [PrimI 20])))]
                    (Apply (Ref "g") [])))

-- When defining `f` as a dynamically-scoped DLambda, the same program evaluates to `50`
prog3 = Let [("y", PrimI 10)]
            (Let [("f", DLambda ["x"] (Apply (Ref "+") [Ref "x", Ref "y"]))]
                (Let [("g", Lambda []
                                (Let [("y", PrimI 30)] (Apply (Ref "f") [PrimI 20])))]
                    (Apply (Ref "g") [])))

-- Evaluate test programs
main = do print (eval prog1 globalEnv)
          print (eval prog2 globalEnv)
          print (eval prog3 globalEnv)
