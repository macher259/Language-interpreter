module Interpreter where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified AbsGrammar as G
import Data.Maybe
import Control.Monad.Trans.Except
import Control.Monad
import AbsGrammar (Stmt'(Empty))
import System.IO
import qualified TypeCheck as T

type InterpreterMonad a = ExceptT String (ReaderT Env (StateT Store IO)) a

type Loc = Integer
type Env = M.Map String Loc
type Store = (M.Map Loc Value, Loc) -- (Value Map, Next avaible location)

alloc :: InterpreterMonad Loc
alloc = do
    (st, l) <- get
    put (st, l+1)
    return l

storeValue :: Value -> Loc -> InterpreterMonad ()
storeValue val loc = do
    (st, l) <- get
    let st' = M.insert loc val st
    put (st', l)

getLoc :: String -> InterpreterMonad Loc
getLoc name = do
    env <- ask
    let val = M.lookup name env
    case val of 
      Nothing -> throwE "no return from function or function is declared under application"
      Just n -> return n

getValue :: String -> InterpreterMonad Value
getValue name = do
    loc <- getLoc name
    (st, _) <- get
    return $ fromJust $ M.lookup loc st -- Same as above.



data Value = VStr String | VInt Integer | VBool Bool | VVoid | VFun Fun deriving (Show, Ord, Eq)
data Fun = Copy (Value -> InterpreterMonad Fun) | Ref (Loc -> InterpreterMonad Fun) | None (InterpreterMonad Value)

instance Show Fun where
    show _ = "fun()"
instance Eq Fun where
    (==) _ _ = False
instance Ord Fun where
    (<=) _ _ = False

appFunc :: Fun -> [G.Expr] -> InterpreterMonad Value
appFunc (Copy f) (arg:args) = do
    env <- ask
    val <- eval arg
    f' <- f val
    appFunc f' args
appFunc (Ref f) (arg:args) = do
    case arg of
        (G.EVar _ (G.Ident name)) -> do
          loc <- getLoc name
          f' <- f loc
          appFunc f' args
        e -> do
            throwE $ "Expected variable, got literal value " ++ show e
            --val <- eval e
            --loc <- alloc
            --storeValue val loc
            ---f' <- f loc
            --appFunc f' args
appFunc (None f) [] = f
appFunc a b = throwE $ "bug: " ++ show a ++ ", " ++ show b

defaultValue :: G.Type -> InterpreterMonad Value
defaultValue (G.Int _) = return $ VInt 0
defaultValue (G.Bool _) = return $ VBool False
defaultValue (G.Str _) = return $ VStr ""
defaultValue (G.Void _) = return VVoid
defaultValue x = throwE $ "bug: " ++ show x ++ " type doesn't have default value"

{-
    = Empty a
    | BStmt a (Block' a)
    | Decl a (Type' a) [Item' a]
    | Ass a Ident (Expr' a)
    | Incr a Ident
    | Decr a Ident
    | Ret a (Expr' a)
    | VRet a
    | Cond a (Expr' a) (Stmt' a)
    | CondElse a (Expr' a) (Stmt' a) (Stmt' a)
    | While a (Expr' a) (Stmt' a)
    | SExp a (Expr' a)
-}
exec :: [G.Stmt] -> InterpreterMonad (Maybe Value)
exec [] = return Nothing
exec (G.Empty _:stmts) = exec stmts
exec (G.BStmt _ (G.Block _ blockStmts):stmts) = do
    ret <- exec blockStmts
    case ret of
        Nothing -> exec stmts
        Just va -> return ret
exec (G.Decl pos t (G.NoInit _ (G.Ident name):items):stmts) = do
    loc <- alloc
    val <- defaultValue t
    storeValue val loc
    local (M.insert name loc) (exec (G.Decl pos t items:stmts))
exec (G.Decl pos t ((G.Init _ (G.Ident name) e):items):stmts) = do
    loc <- alloc
    val <- eval e
    storeValue val loc
    local (M.insert name loc) (exec (G.Decl pos t items:stmts))
exec (G.Decl {}:stmts) = exec stmts
exec (G.Ass _ (G.Ident name) e:stmts) = do
    loc <- getLoc name
    val <- eval e
    storeValue val loc
    exec stmts
exec (G.Incr _ (G.Ident name):stmts) = do
    loc <- getLoc name
    (VInt n) <- getValue name
    storeValue (VInt $ n+1) loc
    exec stmts
exec (G.Decr _ (G.Ident name):stmts) = do
    loc <- getLoc name
    (VInt n) <- getValue name
    storeValue (VInt $ n-1) loc
    exec stmts
exec (G.Ret _ e:stmts) = do
    val <- eval e
    return $ Just val
exec (G.VRet _:stmts) = return $ Just VVoid
exec (G.Cond a e stmt:stmts) = do
    (VBool b) <- eval e
    let ifstmt = G.BStmt a (G.Block a [stmt])
    if b then exec (ifstmt:stmts)
    else exec stmts
exec (G.CondElse a e stmtT stmtF:stmts) = do
    (VBool b) <- eval e
    let tifstmt = G.BStmt a (G.Block a [stmtT])
    let fifstmt = G.BStmt a (G.Block a [stmtF])
    if b then exec (tifstmt:stmts)
    else exec (fifstmt:stmts)
exec while@(G.While a e stmt:stmts) = do
    (VBool b) <- eval e
    let whilestmt = G.BStmt a (G.Block a [stmt])
    if b then do
        x <- exec [whilestmt]
        case x of
          Nothing -> exec while
          Just _ -> return x
    else exec stmts
exec (G.SExp _ e:stmts) = do
    env <- ask
    val <- eval e
    exec stmts

{-
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr' a]
    | EString a String
    | Neg a (Expr' a)
    | Not a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
-}
eval :: G.Expr -> InterpreterMonad Value
eval (G.EVar _ (G.Ident name)) = getValue name
eval (G.ELitInt _ n) = return $ VInt n
eval (G.ELitTrue _) = return $ VBool True
eval (G.ELitFalse _) = return $ VBool False
eval (G.EApp _ (G.Ident name) args) = do
    (VFun f) <- getValue name
    appFunc f args
eval (G.EString _ val) = return $ VStr val
eval (G.Neg _ e) = do
    (VInt val) <- eval e
    return $ VInt $ -val
eval (G.Not _ e) = do
    (VBool val) <- eval e
    return $ VBool $ not val
eval (G.EMul _ e1 (G.Times _) e2) = do
    (VInt val1) <- eval e1
    (VInt val2) <- eval e2
    return $ VInt $ val1 * val2
eval (G.EMul pos e1 op e2) = do
    (VInt val1) <- eval e1
    (VInt val2) <- eval e2
    when (val2 == 0) $ throwE $ "divide by 0 error " ++ T.showPosition pos
    let rv = case op of
            G.Div _ -> val1 `div` val2
            G.Mod _ -> val1 `mod` val2
            _ -> undefined
    return $ VInt rv
eval (G.EAdd _ e1 op e2) = do
    (VInt val1) <- eval e1
    (VInt val2) <- eval e2
    let rv = case op of
            G.Plus _ -> val1 + val2
            G.Minus _ -> val1 - val2
    return $ VInt rv
eval (G.ERel _ e1 op e2) = do
    val1 <- eval e1
    val2 <- eval e2
    return $ VBool (case op of
       G.LTH ma -> val1 < val2
       G.LE ma -> val1 <= val2
       G.GTH ma -> val1 > val2
       G.GE ma -> val1 >= val2
       G.EQU ma -> val1 == val2
       G.NE ma -> val1 /= val2)
eval (G.EAnd _ e1 e2) = do
    (VBool val1) <- eval e1
    if val1 then do
        (VBool val2) <- eval e2
        return $ VBool val2
    else return $ VBool False
eval (G.EOr _ e1 e2) = do
    (VBool val1) <- eval e1
    if val1 then return $ VBool True
    else do
        (VBool val2) <- eval e2
        return $ VBool val2

execProgram :: G.Program -> InterpreterMonad ()
execProgram (G.Program pos topdefs) = do
    let f_types = [ft | (G.FnDef _ ft _ _ _) <- topdefs]
    let (f_idents, funs) = unzip [(ident, (args, body)) | (G.FnDef _ _ (G.Ident ident) args (G.Block _ body)) <- topdefs] in do
            locs <- replicateM (length topdefs) alloc
            let env = M.union (M.fromList $ zip f_idents locs) in do
                local env $ f [(l, makeFnDef body args t) | ((args, body), l, t) <- zip3 funs locs f_types]
   where
    f :: [(Loc, InterpreterMonad Fun)] -> InterpreterMonad ()
    f [] = do
        VFun (None main) <- getValue "main"
        main
        return ()
    f ((l, fn):b) = do
        env <- ask
        f' <- fn
        storeValue (VFun f') l
        f b
makeFnDef :: [G.Stmt] -> [G.Arg] -> G.Type -> InterpreterMonad Fun
makeFnDef body (G.Copy _ _ (G.Ident ident): rest) t = do
        env <- ask
        return $ Copy (\val -> local (const env) $ do
            l <- alloc
            storeValue val l
            local (M.insert ident l) (makeFnDef body rest t))

makeFnDef body (G.Ref _ _ (G.Ident ident): rest) t = do
        env <- ask
        return $ Ref (\loc -> local (const env) $ do
            local (M.insert ident loc) (makeFnDef body rest t))

makeFnDef body [] t = do
    env <- ask
    defaultT <- defaultValue t
    return $ None (fromMaybe defaultT <$> local (const env) (exec body))

runInterpreter :: G.Program -> IO ()
runInterpreter program = do
    let (env, st) = initialState
    (rv, (st, loc)) <- runStateT (runReaderT (runExceptT (execProgram program)) env) st
    case rv of
      Left e -> hPutStrLn stderr $ "runtime error: " ++ e
      Right _ -> return ()

initialState :: (Env, Store)
initialState = (env, (st, 0))
    where
        storeList = [(-1, VFun $ Copy printVal), (-2, VFun $ Copy printVal), (-3, VFun $ Copy printVal)]
        st = M.fromList storeList
        list = [("print", -1), ("printInt", -2), ("printBool", -3)]
        env = M.fromList list
        printVal (VStr v) = return $ None (
            do
                liftIO $ putStr v
                return VVoid
            )
        printVal (VInt v) = return $ None (
            do
                liftIO $ print v
                return VVoid
            )
        printVal (VBool v) = return $ None (
            do
                liftIO $ print v
                return VVoid
            )
        printVal x = throwE $ "bug in print function " ++ show x