module TypeCheck where

import qualified AbsGrammar as G
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad (when)
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Data.Maybe (isNothing, fromJust)
import Data.List

type Position = G.BNFC'Position
showPosition :: Position -> String
showPosition Nothing = "?:?"
showPosition (Just (line, col)) = show line ++ ":" ++ show col

data Type = Int | Str | Bool | VarInt | VarStr | VarBool | Void | Fun Type [Type] deriving (Eq, Show)

type Env = M.Map String Type
type TypeCheckerMonad a = ExceptT String (Reader Env) a

parseVarType :: G.Type -> Type
parseVarType (G.Int _) = VarInt
parseVarType (G.Str _) = VarStr
parseVarType (G.Bool _) = VarBool
parseVarType (G.Void _) = Void

parseType :: G.Type -> Type
parseType (G.Int _) = Int
parseType (G.Str _) = Str
parseType (G.Bool _) = Bool
parseType (G.Void _) = Void
parseType (G.Fun _ rt args) = Fun (parseType rt) (map parseType args)

typeEqual :: Type -> Type -> Bool
typeEqual Int Int = True
typeEqual Str Str = True
typeEqual Bool Bool = True
typeEqual Void Void = True
typeEqual (Fun t1 ts1) (Fun t2 ts2) = t1 == t2 && ts1 == ts2
typeEqual Int VarInt = True
typeEqual VarInt Int = True
typeEqual Str VarStr = True
typeEqual VarStr Str = True
typeEqual Bool VarBool = True
typeEqual VarBool Bool = True
typeEqual VarInt VarInt = True
typeEqual VarStr VarStr = True
typeEqual VarBool VarBool = True
typeEqual _ _ = False

typeNEqual :: Type -> Type -> Bool
typeNEqual a b = not $ typeEqual a b

runTypeCheck :: G.Program -> Except String ()
runTypeCheck (G.Program _ topDefs) = do
    signatures <- mapM parseTopDef topDefs
    env <- createSignatureMap signatures
    typecheckTopDefs topDefs env

createSignatureMap :: [(String, Type, Position)] -> Except String Env
createSignatureMap defs = helper defs initialEnv
    where
        helper :: [(String, Type, Position)] -> Env -> Except String Env
        helper [] m = do
            if not (M.member "main" m) then throwE $ "no " ++ "main" ++ " function"
            else return m
        helper ((name, t, pos) : xs) m = do
            checkRedefinition name pos m
            when (name == "main") $ checkMain (t, pos)
            helper xs (M.insert name t m)
        checkRedefinition name pos env = do
            when (M.member name env) $ throwE $ "redefinition of " ++ name ++ " " ++ showPosition pos
checkMain :: (Type, Position) -> Except String ()
checkMain (t, p) = when (typeNEqual t mainType) $ throwE $ "bad " ++ "main" ++ " signature " ++ showPosition p
    where
        mainType = Fun Void []

initialEnv :: Env
initialEnv = M.fromList [("print", Fun Void [Str]), ("printInt", Fun Void [Int]), ("printBool", Fun Void [Bool])]

parseTopDef :: G.TopDef -> Except String (String, Type, Position)
parseTopDef (G.FnDef pos rt (G.Ident name) args _) = do
    let argNames = map getArgName args
    when (any isNotVariableType argTypes) $ throwE $ "function signature contains an argument of invalid type " ++ showPosition pos
    when (containsDuplicates argNames) $ throwE $ "function signature contains duplicate arguments " ++ showPosition pos
    return (name, Fun (parseType rt) argTypes, pos)
    where
        containsDuplicates argNames = length (nub argNames) /= length argNames
        argTypes = map parseArg args

parseArg :: G.Arg -> Type
parseArg (G.Copy _ t _) = parseType t
parseArg (G.Ref _ t _) = parseVarType t

parseArgType :: Type -> Type
parseArgType Int = VarInt
parseArgType Str = VarStr
parseArgType Bool = VarBool


getArgName :: G.Arg -> String
getArgName (G.Copy _ _ (G.Ident name)) = name
getArgName (G.Ref _ _ (G.Ident name)) = name

functionArgNames :: G.TopDef -> [(String, Type)]
functionArgNames (G.FnDef _ _ _ args _) = zip (map getArgName args) (map (parseArg) args)

isNotVariableType :: Type -> Bool
isNotVariableType x = typeNEqual x Int && typeNEqual x Bool && typeNEqual x Str

returnIdent :: String
returnIdent = "__0"

typecheckTopDefs :: [G.TopDef] -> Env -> Except String ()
typecheckTopDefs [] _ = return ()
typecheckTopDefs (f@(G.FnDef _ rt (G.Ident _) _ (G.Block _ body)):topdefs) env = do
    case runIdentity (runReaderT (runExceptT (typecheckStmt body)) env') of
        Left e -> throwE e
        Right _ -> do
            typecheckTopDefs topdefs env
    where
        env' = M.union (M.insert returnIdent (parseType rt) env) (M.fromList $ functionArgNames f)

typecheckStmt :: [G.Stmt] -> TypeCheckerMonad ()
typecheckStmt (G.Empty _: stmts) = typecheckStmt stmts
typecheckStmt (G.BStmt _ (G.Block _ bstmts): stmts) = do
    typecheckStmt bstmts
    typecheckStmt stmts
typecheckStmt (G.Decl pos itemType ((G.NoInit _ (G.Ident name)) : items) : stmts) = do
    let t = parseVarType itemType
    when (isNotVariableType t) $ throwE $ "declaration contains an item of invalid type " ++ showPosition pos
    local (M.insert name t) (typecheckStmt (G.Decl pos itemType items : stmts))
typecheckStmt (G.Decl pos itemType ((G.Init _ (G.Ident name) exp) : items) : stmts) = do
    t <- evalExpType exp
    let itemType' = parseVarType itemType
    when (isNotVariableType t) $ throwE $ "declaration contains an item of invalid type " ++ showPosition pos
    when (typeNEqual t itemType') $ throwE $ "type mismatch " ++ showPosition pos
    local (M.insert name itemType') (typecheckStmt (G.Decl pos itemType items : stmts))
typecheckStmt (G.Decl {} : stmts) = typecheckStmt stmts
typecheckStmt (G.Ass pos (G.Ident name) expr : stmts) = do
    expType <- evalExpType expr
    varType <- asks $ M.lookup name
    when (isNothing varType) $ throwE $ "undefined variable " ++ name ++ " " ++ showPosition pos
    when (typeNEqual (fromJust varType) expType) $ throwE $ "type mismatch " ++ showPosition pos
    typecheckStmt stmts
typecheckStmt (G.Incr pos (G.Ident name) : stmts) = do
    varType <- asks $ M.lookup name
    when (isNothing varType) $ throwE $ "undefined variable " ++ name ++ " " ++ showPosition pos
    when (typeNEqual (fromJust varType) Int) $ throwE $ "type mismatch " ++ showPosition pos
    typecheckStmt stmts
typecheckStmt (G.Decr pos ident : stmts) = typecheckStmt $ G.Incr pos ident : stmts
typecheckStmt (G.Ret pos exp : stmts) = do
    e <- evalExpType exp
    rt <- asks $ M.lookup returnIdent
    let rt' = fromJust rt
    when (typeNEqual e rt') $ throwE $ "return type mismatch " ++ showPosition pos
    typecheckStmt stmts
typecheckStmt (G.VRet pos : stmts) = do
    rt <- asks $ M.lookup returnIdent
    when (typeNEqual (fromJust rt) Void) $ throwE $ "return type mismatch " ++ showPosition pos
    typecheckStmt stmts
typecheckStmt (G.Cond pos exp stmt : stmts) = do
    b <- evalExpType exp
    when (typeNEqual b Bool) $ throwE $ "condition is not a boolean " ++ showPosition pos
    typecheckStmt [G.BStmt pos (G.Block pos [stmt])]
    typecheckStmt stmts
typecheckStmt (G.CondElse pos expr stmt1 stmt2 : stmts) = do
    typecheckStmt [G.Cond pos expr stmt1]
    typecheckStmt [G.Cond pos expr stmt2]
    typecheckStmt stmts
typecheckStmt (G.While pos exp stmt : stmts) = typecheckStmt $ G.Cond pos exp stmt : stmts
typecheckStmt (G.SExp _ expr : stmts) = do
    _ <- evalExpType expr
    typecheckStmt stmts
typecheckStmt [] = return ()


evalExpType :: G.Expr -> TypeCheckerMonad Type
evalExpType expr = do
    env <- ask
    case runReader (runExceptT $ evalType expr) env of
        Left e -> throwE e
        Right t -> return t

evalType :: G.Expr -> TypeCheckerMonad Type
evalType (G.EVar pos (G.Ident name)) = do
    t <- asks $ M.lookup name
    when (isNothing t) $ throwE $ "undefined variable " ++ name ++ " " ++ showPosition pos
    return $ fromJust t
evalType (G.ELitInt _ _) = return Int
evalType (G.ELitTrue _) = return Bool
evalType (G.ELitFalse _) = return Bool
evalType (G.EApp pos (G.Ident name) args) = do
    ft <- asks $ M.lookup name
    case ft of
        Nothing -> throwE $ "undefined function " ++ name ++ " " ++ showPosition pos
        Just (Fun rt argTypes) -> do
            when (length argTypes /= length args) $ throwE $ "wrong number of arguments " ++ showPosition pos
            argTypes' <- mapM evalType args
            when ((any (==False) [typeEqual a b | (a, b) <- zip argTypes argTypes'])) $ throwE $ "argument types mismatch " ++ showPosition pos
            when (argTypes /= argTypes' && any (==True) [(a == VarInt && b == Int) || (a == VarStr && b == Str) || (a == VarBool && b == Bool) | (a, b) <- zip argTypes argTypes']) $ throwE $ "cannot reference literal value " ++ showPosition pos
            return rt
        _ -> throwE $ "not a function " ++ showPosition pos
evalType (G.EString _ _) = return Str
evalType (G.Neg pos e) = do
    t <- evalType e
    when (typeNEqual t Int) $ throwE $ "type mismatch " ++ showPosition pos
    return Int
evalType (G.Not pos e) = do
    t <- evalType e
    when (typeNEqual t Bool) $ throwE $ "type mismatch " ++ showPosition pos
    return Bool
evalType (G.EMul pos e1 _ e2) = evalType (G.EAdd pos e1 (G.Plus pos) e2)
evalType (G.EAdd pos e1 _ e2) = do
    t1 <- evalType e1
    t2 <- evalType e2
    when (typeNEqual t1 Int || typeNEqual t2 Int) $ throwE $ "type mismatch " ++ showPosition pos
    return Int
evalType (G.ERel pos e1 _ e2) = do
    t1 <- evalType e1
    t2 <- evalType e2
    when (typeNEqual t1 t2) $ throwE $ "type mismatch " ++ showPosition pos
    return Bool
evalType (G.EAnd pos e1 e2) = do
    t1 <- evalType e1
    t2 <- evalType e2
    when (typeNEqual t1 Bool || typeNEqual t2 Bool) $ throwE $ "type mismatch " ++ showPosition pos
    return Bool
evalType (G.EOr pos e1 e2) = evalType (G.EAnd pos e1 e2)
