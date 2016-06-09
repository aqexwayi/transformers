module Main where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

-------------------------- types ------------------------

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

-------------------------- eval0 ------------------------
type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = IntVal $ i1+i2
  where
    IntVal i1 = eval0 env e1
    IntVal i2 = eval0 env e2
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let
  val1 = eval0 env e1
  val2 = eval0 env e2
  in case val1 of
    FunVal env' n body -> eval0 (Map.insert n val2 env') body
                          
-------------------------- eval1 ------------------------

type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal $ i1+i2
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  v1 <- eval1 env e1
  v2 <- eval1 env e2
  case v1 of
    FunVal env' n body -> eval1 (Map.insert n v2 env') body

-------------------------- eval2 ------------------------

type Eval2 a = ExceptT String Identity a
runEval2 v = runIdentity $ runExceptT v

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case  Map.lookup n env of
                      Nothing -> throwError $ "Unbound variable: " ++ n
                      Just v -> return v
eval2 env (Plus e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case (v1,v2) of
    (IntVal i1,IntVal i2) -> return $ IntVal $ i1+i2
    _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  v1 <- eval2 env e1
  v2 <- eval2 env e2
  case v1 of
    FunVal env' n body -> eval2 (Map.insert n v2 env') body
    _ -> throwError "type error in application"

-------------------------- eval3 ------------------------

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Eval3 a -> Env -> Either String a
runEval3 v env = runIdentity $ runExceptT $ runReaderT v env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError $ "Unbound variable: " ++ n
    Just v -> return v
eval3 (Plus e1 e2) = do
  v1 <- eval3 e1
  v2 <- eval3 e2
  case (v1,v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal $ i1+i2
    _ -> throwError "type error in addition"
eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e
eval3 (App e1 e2) = do
  v1 <- eval3 e1
  v2 <- eval3 e2
  case v1 of
    -- FunVal env n body -> local (const (Map.insert n v2 env)) (eval3 body)
    FunVal env n body -> local (Map.insert n v2) (eval3 body)
    _ -> throwError "type error in application."

-------------------------- eval4 ------------------------
type Eval4 a = ReaderT Env (StateT Integer (ExceptT String Identity)) a

runEval4 :: Eval4 a -> Integer -> Env -> Either String (a , Integer)
runEval4 expr st env = runIdentity (runExceptT (runStateT (runReaderT expr env) st))

tick4 :: Eval4 ()
tick4 = do
  st <- get  
  put (st+1) 

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
  tick4
  return $ IntVal i
eval4 (Var n) = do
  tick4
  env <- ask
  case Map.lookup n env of 
    Nothing -> throwError $ "Unbound variable : " ++ n
    Just v -> return v
eval4 (Plus e1 e2) = do
  tick4
  v1 <- eval4 e1
  v2 <- eval4 e2
  case (v1,v2) of
    (IntVal i1,IntVal i2) -> return $ IntVal $ i1+i2
    _ -> throwError "type error in addition"
eval4 (Abs n body) = do
  tick4
  env <- ask
  return $ FunVal env n body
eval4 (App e1 e2) = do
  tick4
  v1 <- eval4 e1
  v2 <- eval4 e2
  case v1 of
    FunVal env n body -> local (Map.insert n v2) (eval4 body)
    _ -> throwError "type error in application."

-------------------------- eval5 ------------------------
type Eval5 a = ReaderT Env (WriterT [String] (StateT Integer (ExceptT String Identity))) a

runEval5 :: Eval5 a -> Integer -> Env -> Either String ((a, [String]), Integer)
runEval5 expr st env = runIdentity (runExceptT (runStateT (runWriterT (runReaderT expr env)) st))

tick5 :: Eval5 ()
tick5 = do
  st <- get
  put (st+1)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
  tick5
  return $ IntVal i
eval5 (Var n) = do
  tick5
  tell ["use var : " ++ n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError $ "Unbound variable : " ++ n
    Just v -> return v
eval5 (Plus e1 e2) = do
  tick5
  v1 <- eval5 e1
  v2 <- eval5 e2
  case (v1,v2) of
    (IntVal i1, IntVal i2) -> return $ IntVal $ i1+i2
    _ -> throwError "type error in addition"
eval5 (Abs n body) = do
  tick5
  env <- ask
  return $ FunVal env n body
eval5 (App e1 e2) = do
  tick5
  v1 <- eval5 e1
  v2 <- eval5 e2
  case v1 of
    FunVal env n body -> local (Map.insert n v2) (eval5 body)
    _ -> throwError "type error in applicaiton"

-------------------------- test ------------------------

exp1 = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exp2 = App (Abs "x" ((Var "x") `Plus` (Var "x"))) (Lit 2)
exp3 = (Lit 1) `Plus` (Var "x") 

main = do
  -- putStrLn $ show $ runEval3 $ eval3 Map.empty exp2 
  -- putStrLn $ show $ runEval3 $ eval3 Map.empty exp3
  putStrLn $ show $ runEval3 (eval3 exp2) Map.empty
  putStrLn $ show $ runEval3 (eval3 exp3) Map.empty
  putStrLn $ show $ runEval4 (eval4 exp2) 0 Map.empty
  putStrLn $ show $ runEval4 (eval4 exp3) 0 Map.empty
  putStrLn $ show $ runEval5 (eval5 exp2) 0 Map.empty
  putStrLn $ show $ runEval5 (eval5 exp3) 0 Map.empty

