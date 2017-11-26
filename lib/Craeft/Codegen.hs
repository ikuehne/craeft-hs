{-|
Module      : Craeft.Codegen
Description : Codegen from the typed AST (@TAST@) to LLVM.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Codegen from the @TAST@ to the llvm (as in @llvm-hs-pure@) AST.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Craeft.Codegen ( codegen ) where

import           Debug.Trace (traceM)

import           Data.Function (on)
import           Data.String (fromString)
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Control.Monad.Trans.State (StateT)
import           Control.Monad.Writer (runWriterT)
import           Data.List (lookup, sortBy, intercalate)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import           Control.Lens
import           Control.Lens.Internal.Zoom ( Focusing )
import           LLVM.AST
import qualified LLVM.AST.FloatingPointPredicate as FPPred
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Type as LLTy
import qualified LLVM.AST.Instruction as LLInstr
import qualified LLVM.AST.Constant as LLConst
import qualified LLVM.AST.Float as LLFloat
import qualified LLVM.AST.Global as LLGlobal
import qualified LLVM.AST.CallingConvention as CConv
import           LLVM.AST.AddrSpace ( AddrSpace (..) )

import qualified Craeft.TemplateFiller as Templ
import qualified Craeft.TypedAST as TAST
import qualified Craeft.TypedAST.Pass as Pass
import qualified Craeft.Types as Types
import qualified Craeft.Scope as Scope
import           Craeft.Utility as Utility

-- | The state for the codegen monad.
--
-- Contains everything needed for codegen up to top-level forms.
data CodegenState = CodegenState {
    -- ^ The @Name@ of the current block.
    _currentBlock :: Name
    -- ^ A list of blocks produced in this Codgen.
 , _blocks       :: Map.Map Name BlockState
    -- ^ A @Scope@ from identifiers to values.
 , _symtab       :: Scope.ScopeState Value
    -- ^ The number of blocks in this @Codegen@.
 , _blockCount   :: Int
    -- ^ The register we are up to.
 , _count        :: Word
   -- ^ A list of globals used in this Codegen.
   --
   -- Necessary in particular for string and array literals.
 , _globals      :: [LLGlobal.Global]
   -- A multiset of @Names@ used thus far in this codegen.
 , _names        :: Map.Map String Int }

type Value = (Types.Type, Operand)

-- | Create a new instance of @CodegenState@.
--
-- Use the given top-level environment.  In the current pattern, a new
-- @CodegenState@ is used for each function, so we need to preserve the
-- @ScopeState@ between functions for top-level definitions.
initCG :: Scope.ScopeState Value -> CodegenState
initCG st = CodegenState { _currentBlock = Name "main"
                         , _blocks = Map.empty
                         , _symtab = st
                         , _blockCount = 0
                         , _count = 0
                         , _globals = []
                         , _names = Map.empty }

-- | The state recorded for a single block.
data BlockState
  = BlockState { _idx   :: Int
               , _stack :: [Named Instruction]
               , _term  :: Maybe (Named Terminator) }

type Specialization = (String, TAST.FunctionSignature, TAST.Block)

data LlvmState = LlvmState {
    _llmod :: Module
      -- ^ A map from identifiers to expandable functions.
    , _functionTab  :: Map.Map String (TAST.FunctionSignature, TAST.Block)
    , _env :: Scope.ScopeState Value }

-- All of these are lensey.
makeLenses ''BlockState
makeLenses ''CodegenState
makeLenses ''LlvmState

type Codegen a = CraeftMonad CodegenState a
type LLVM a = CraeftMonad LlvmState a

-- | The initial @LlvmState@ for a fresh program.
initLLVM :: LlvmState
initLLVM = LlvmState { _llmod = defaultModule
                     , _functionTab = Map.empty
                     , _env = Scope.empty }

-- | The codegen entry point.
--
-- Convert a @TAST@ @Program@ to a pure LLVM @Module.
codegen :: TAST.Program -> CraeftExcept Module
codegen program = _llmod <$> execStateT (mapM toplevelCodegen program) initLLVM

--
-- Generating names.
--

-- | Generate a new, unique name based on the given string.
uniqueName :: String -> Codegen Name
uniqueName n = uses names (Map.lookup n) >>= recordName n . fromMaybe 0
  where recordName :: String -> Int -> Codegen Name
        recordName n i = do let next = i + 1
                            names %= Map.insert n next
                            return $ mkName $ n ++ show next

-- | Get the current @BlockState@.
current :: Codegen BlockState
current = do c <- use currentBlock
             bs <- use blocks
             let err = InternalError $ "No such block: " ++ show c
             liftMaybe err $ Map.lookup c bs

modifyBlock :: (BlockState -> BlockState) -> Codegen ()
modifyBlock f = do active <- use currentBlock
                   blocks %= Map.adjust f active

voidInstr :: Instruction -> Codegen ()
voidInstr ins = modifyBlock $ stack %~ (Do ins :)

instr :: Instruction -> Type -> Codegen Operand
instr i VoidType = do voidInstr i
                      count %= succ
                      ref <- UnName <$> use count
                      return $ LocalReference VoidType ref
instr ins t = do count %= succ
                 ref <- UnName <$> use count
                 modifyBlock $ stack %~ ((ref := ins) :)
                 return $ LocalReference t ref

--
-- Blocks.
--

-- | Create a new block, returning its name.
addBlock :: String -> Codegen Name
addBlock bname = do
    newBlock <- (\i -> BlockState i [] Nothing) <$> use blockCount

    newName <- uniqueName bname
    blockCount += 1
    blocks %= Map.insert newName newBlock

    return newName

-- | Terminate the given block with the given terminator instruciton.
terminate :: Terminator -> Codegen ()
terminate trm = do active <- use currentBlock
                   modifyBlock (term %~ const (Just $ Do trm) )

-- | Switch to the block of the given name.
switchBlock :: Name -> Codegen ()
switchBlock = (currentBlock .=)

-- | Perform the given codegen in the named block, in a new scope.
--
-- After completion, remains in that block but the scope is popped.
inBlock :: Name -> Codegen a -> Codegen a
inBlock n c = switchBlock n >> Scope.nested (zoom symtab) c

-- | Sort the blocks on index.
sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (_idx . snd))

-- | Extract the blocks used in a @Codegen@ to the llvm-hs AST.
createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (_blocks m)

-- | Add all globals used in a @Codegen@ to the current module.
addCgGlobals :: CodegenState -> LLVM ()
addCgGlobals st = do
    defs <- moduleDefinitions <$> use llmod
    llmod %= \s -> s { moduleDefinitions = defs ++ newDefs }
  where newDefs = GlobalDefinition <$> _globals st

-- | Convert a @BlockState@ to an llvm-hs @BasicBlock@.
makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where maketerm = fromMaybe (error $ "Block has no terminator: " ++ show l)

-- | Define a new module-level function.
define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    LLGlobal.name        = mkName label
  , LLGlobal.parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , LLGlobal.returnType  = retty
  , LLGlobal.basicBlocks = body }

-- | Add a definition to the LLVM module.
addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- moduleDefinitions <$> use llmod
  llmod %= \s -> s { moduleDefinitions = defs ++ [d] }

--
-- Templates.
--

-- | Get all of the function calls in a block.
blockFunctionCalls :: MTraversal TAST.Block (Annotated TAST.Expression,
                                             [Annotated TAST.Expression],
                                             [Types.Type])
blockFunctionCalls f = callsInBlock f >=> callsInConditions f
  where calls = TAST.exprContents.Pass.eachExprFunctionCall
        callsInBlock = each!.Pass.eachExpressionInStmt.calls
        callsInConditions = each!.TAST.cond.contents.calls

mangle :: [Types.Type] -> String -> String
mangle ts fname = (fname ++) $ intercalate ".$" $ map makeName ts
  -- TODO
  where makeName _ = "typename"

specialize :: [Types.Type] -> SourcePos -> TAST.FunctionSignature -> TAST.Block
           -> LLVM [Specialization]
specialize ts p sig block = do
      ((nsig@(TAST.Sig name _ _ _), filled), others) <- lift $ runWriterT writer
      return [(mangle ts name, (TAST.name %~ mangle ts) nsig, filled)]
  where writer = Templ.fillFunction ts p (sig, block)

specializeByName :: [Types.Type] -> SourcePos -> String -> LLVM [Specialization]
specializeByName ts p n = uses functionTab (Map.! n)
                      >>= uncurry (specialize ts p)

specializeAll :: TAST.Block -> LLVM TAST.Block
specializeAll = blockFunctionCalls (\(Annotated e p, args, ts) -> do 
    let fnameLens :: Traversal' TAST.Expression String
        fnameLens = TAST.exprContents.TAST.lvalueExpr.TAST.lvalueVarName
    n <- liftMaybe (TypeError "cannot expand function not refered to by name" p)
                 $ e ^? fnameLens
    specializations <- specializeByName ts p n
    forM_ specializations $ \(name, sig, block) ->
        toplevelCodegen (Annotated (TAST.Function sig block) p)
    let e' = (fnameLens %~ mangle ts) e
    return (Annotated e' p, args, ts))

--
-- Expression codegen.
--

-- | Codegen an l-value, returning a pointer to the referenced value.
lvalueCodegen :: Types.Type -> Annotated TAST.LValue -> Codegen Value
lvalueCodegen t a = case a^.contents of
    -- Variables are already stored as pointers.
    TAST.Variable s -> zoom symtab $ Scope.lookup s p
    -- Codegen'ing a pointer to a dereference is just codegen'ing the referand.
    TAST.Dereference expr -> (t,) <$> exprCodegen expr
    -- Get a pointer to the struct, and this is just a GEP.
    TAST.FieldAccess str i -> do
        (_, val) <- case str ^. TAST.exprContents of
            TAST.LValueExpr l -> lvalueCodegen (str ^. TAST.exprType)
                                               (Annotated l p)
            _ -> throw "cannot assign to r-value struct"
        let gep = GetElementPtr False val [constInt i] []
            llt = translateType t
        op <- instr gep llt
        return (t, op)
  where p = a^.pos
        throw = throwError . flip TypeError p

-- | Codegen an expression as an operand/type pair.
exprCodegen :: Annotated TAST.Expression -> Codegen Operand
exprCodegen a = case a ^. contents . TAST.exprContents of
    -- Literals are just constant operands.
    TAST.IntLiteral i -> return $ constInt i
    TAST.UIntLiteral i -> return $ constInt i
    TAST.FloatLiteral f ->
        return $ ConstantOperand $ LLConst.Float $ LLFloat.Double f
    -- String literals need to be represented as globals.
    TAST.StringLiteral s -> do
        globalCount <- length <$> use globals
        let globalName = mkName $ "##strLiteral" ++ show globalCount
            nullTerminated = s ++ "\0"
            cs = LLConst.Int 8 . toInteger . fromEnum <$> nullTerminated
            arrayType = LLTy.ArrayType (fromIntegral $ length nullTerminated)
                                       LLTy.i8
            globalLiteral = LLGlobal.globalVariableDefaults
              { LLGlobal.name = globalName
              , LLGlobal.isConstant = True
              , LLGlobal.type' = arrayType
              , LLGlobal.initializer = Just $ LLConst.Array LLTy.i8 cs }
            globalOp = LLConst.GlobalReference (ptr arrayType) globalName
            gepIdxs = [LLConst.Int 64 0, LLConst.Int 64 0]
            gep = LLConst.GetElementPtr False globalOp gepIdxs
            o = ConstantOperand gep
        globals %= (globalLiteral :)
        return o
    -- L-value codegen gets the address, so just use that.
    TAST.Reference l -> snd <$> lvalueCodegen ty (Annotated l p)
    TAST.Binop l op r -> do
        lhs <- exprCodegen l
        rhs <- exprCodegen r
        let msg = "no such op: " ++ op
            lty = l ^. contents . TAST.exprType
            rty = r ^. contents . TAST.exprType
        -- Use the @ops@ map to lookup that operation.
        f <- liftMaybe (TypeError msg p) $ Map.lookup op ops
        f p ty (lty, lhs) (rty, rhs)
    TAST.FunctionCall f args typeArgs -> do
        func <- exprCodegen f

        -- Codegen each of the arguments.
        args'' <- mapM exprCodegen args
        -- Zip 'em with their metadata (for now, nothing).
        let args' = zip args'' $ repeat []
            inst = LLInstr.Call Nothing CConv.C [] (Right func) args' [] []
        instr inst llty
    -- Just dereference the codegen'ed l-value.
    TAST.LValueExpr lv -> do
        (lvTy, lvOp) <- lvalueCodegen ty (Annotated lv p)
        case lvTy of
            Types.Pointer referandTy ->
                let inst = LLInstr.Load False lvOp Nothing 0 []
                 in instr inst llty
            -- (Functions are a special case.)
            Types.Function _ _ -> return lvOp
    TAST.Cast e -> do
        casted <- exprCodegen e
        cast p (e^.contents.TAST.exprType, casted) ty
  where p = a ^. pos
        ops = Map.fromList [ ("+", addValues)
                           , ("-", subValues)
                           , ("*", mulValues)
                           , ("/", divValues)
                           , ("<", lessComp)
                           , ("<=", lessEqComp)
                           , (">", greaterComp)
                           , (">=", greaterEqComp)
                           , ("==", eqComp)
                           , ("!=", neqComp) ]
        ty = a ^. contents . TAST.exprType
        llty = translateType ty

--
-- Casts.
--

castWithExtTrunc constructor extender truncator old new operand
    | old < new = extender operand (constructor new)
    | old == new = return operand
    | otherwise = truncator operand (constructor new)

cast :: SourcePos -> Value -> Types.Type -> Codegen Operand
cast p (Types.Signed oldbits, o) (Types.Signed newbits) =
    castWithExtTrunc Types.Signed sext trunc oldbits newbits o
cast p (Types.Unsigned oldbits, o) new@(Types.Signed _) =
    cast p (Types.Signed oldbits, o) new
cast p (Types.Unsigned oldbits, o) (Types.Unsigned newbits) =
    castWithExtTrunc Types.Unsigned zext trunc oldbits newbits o
cast p (Types.Signed oldbits, o) new@(Types.Unsigned _) =
    cast p (Types.Unsigned oldbits, o) new
cast p (Types.Floating oldprec, o) (Types.Floating newprec) =
    castWithExtTrunc Types.Floating fext ftrunc oldprec newprec o
cast p (Types.Pointer _, o) ty@(Types.Unsigned _) = bitcast o ty
cast p (Types.Pointer _, o) ty@(Types.Pointer _) = bitcast o ty
cast p _ _ = throwError $ TypeError "invalid cast" p

--
-- Operators.
--

type Operator = SourcePos -> Types.Type -> Value -> Value -> Codegen Operand

-- | Cast two values to a result type.
--
-- Handy for defining binary operators.
casted :: SourcePos -> Value -> Value -> Types.Type
       -> Codegen (Operand, Operand)
casted p lhs rhs resultTy = do
    l <- cast p lhs resultTy
    r <- cast p rhs resultTy
    return (l, r)

-- | The addition operator.
addValues :: Operator
addValues p t l@(lt, lo) r@(rt, ro)
  | Types.integral lt && Types.integral rt = do (lhs, rhs) <- casted p l r t
                                                intAdd lhs rhs t
  | Types.floating lt && Types.floating rt = do (lhs, rhs) <- casted p l r t
                                                floatAdd lhs rhs t
  | Types.integral lt && Types.pointer rt = ptrAdd ro lo t
  | Types.pointer lt && Types.integral rt = ptrAdd lo ro t
  | otherwise = throwError $ TypeError ("cannot add " ++ show lt
                                            ++ " to " ++ show rt) p

-- | The subtraction operator.
subValues :: Operator
subValues p t l@(lt, lo) r@(rt, ro)
  | Types.integral lt && Types.integral rt = do (lhs, rhs) <- casted p l r t
                                                intSub lhs rhs t
  | Types.floating lt && Types.floating rt = do (lhs, rhs) <- casted p l r t
                                                floatSub lhs rhs t
  | Types.pointer lt && Types.integral rt = do 
        asSigned <- cast p r (Types.Signed 64)
        offset <- intNeg asSigned (Types.Signed 64)
        ptrAdd lo offset t
  | Types.pointer lt && Types.pointer rt = ptrSub lo ro t
  | otherwise = throwError $ TypeError ("cannot subtract " ++ show lt
                                               ++ " from " ++ show rt) p

mulValues :: Operator
mulValues p t l@(lt, lo) r@(rt, ro)
  | Types.integral lt && Types.integral rt = do (lhs, rhs) <- casted p l r t
                                                intMul lhs rhs t
  | Types.floating lt && Types.floating rt = do (lhs, rhs) <- casted p l r t
                                                floatMul lhs rhs t
  | Types.integral lt && Types.pointer rt = ptrAdd ro lo t
  | otherwise = throwError $ TypeError ("cannot multiply " ++ show lt
                                                 ++ " by " ++ show rt) p

divValues :: Operator
-- Unsigned division is special.
divValues p t l@(Types.Unsigned _, _) r@(Types.Unsigned _, _) = do
    (lhs, rhs) <- casted p l r t
    uintDiv lhs rhs t
divValues p t l@(lt, lo) r@(rt, ro)
  | Types.integral lt && Types.integral rt = do (lhs, rhs) <- casted p l r t
                                                sintDiv lhs rhs t
  | Types.floating lt && Types.floating rt = do (lhs, rhs) <- casted p l r t
                                                floatDiv lhs rhs t
  | otherwise = throwError $ TypeError ("cannot divide " ++ show lt
                                               ++ " by " ++ show rt) p

makeComparison :: IPred.IntegerPredicate
               -> IPred.IntegerPredicate
               -> FPPred.FloatingPointPredicate
               -> Operator
-- Unsigned comparisons are special.
makeComparison ipred upred fpred p t
               l@(Types.Unsigned lbits, lo) r@(Types.Unsigned rbits, ro) = do
    (lhs, rhs) <- casted p l r $ Types.Unsigned $ max lbits rbits
    icmpFromCode upred lo ro
makeComparison ipred upred fpred p t l@(lt, lo) r@(rt, ro)
  | Types.integral lt && Types.integral rt = do (lhs, rhs) <- casted p l r maxTy
                                                icmpFromCode ipred lo ro
  | Types.floating lt && Types.floating rt = do (lhs, rhs) <- casted p l r maxTy
                                                fcmpFromCode fpred lo ro
  | Types.pointer lt && Types.pointer rt = icmpFromCode ipred lo ro
  | otherwise = throwError $ TypeError ("cannot compare " ++ show lt
                                              ++ " with " ++ show rt) p
  where maxTy = max lt rt

lessComp = makeComparison IPred.SLT IPred.ULT FPPred.OLT
lessEqComp = makeComparison IPred.SLE IPred.ULE FPPred.OLE
greaterComp = makeComparison IPred.SGT IPred.UGT FPPred.OGT
greaterEqComp = makeComparison IPred.SGE IPred.UGE FPPred.OGE
eqComp = makeComparison IPred.EQ IPred.EQ FPPred.OEQ
neqComp = makeComparison IPred.NE IPred.NE FPPred.ONE

--
-- Type codegen.
--

-- | Convert a Craeft type to an LLVM type.
translateType :: Types.Type -> Type
-- Simple types.
translateType (Types.Signed i) = LLTy.IntegerType $ fromIntegral i
translateType (Types.Unsigned i) = LLTy.IntegerType $ fromIntegral i
translateType (Types.Floating Types.SinglePrec) = LLTy.float
translateType (Types.Floating Types.DoublePrec) = LLTy.double
translateType Types.Void = LLTy.VoidType
translateType (Types.Struct fields) =
    LLTy.StructureType False $ map (translateType . snd) fields
translateType (Types.Pointer t) = ptr $ translateType t
translateType (Types.Function ts t) =
    LLTy.FunctionType (translateType t) (map translateType ts) False
translateType (Types.Hole _) =
    error "internal error: attempt to codegen from type hole"

-- | Statement codegen.
stmtCodegen :: Annotated TAST.Statement -> Codegen ()
stmtCodegen (Annotated stmt p) = case stmt of
    TAST.ExpressionStmt e -> void $ exprCodegen (Annotated e p)
    TAST.Return e -> do
        retval <- exprCodegen e
        terminate $ LLInstr.Ret (Just retval) []
    TAST.VoidReturn -> terminate $ LLInstr.Ret Nothing []
    TAST.Assignment lhs rhs -> do
        (_, lhsAddr) <- lvalueCodegen (Types.Pointer $ rhs ^. exprTy) lhs
        rhsExpr <- exprCodegen rhs
        voidInstr $ LLInstr.Store False lhsAddr rhsExpr Nothing 0 []
    TAST.Declaration n ty -> void $ declare n ty
    TAST.CompoundDeclaration n ty init -> do
        -- Declare the new variable,
        space <- declare n ty
        -- codegen the initial value,
        initVal <- exprCodegen init
        -- and do the assignment.
        voidInstr $ LLInstr.Store False space initVal Nothing 0 []
    TAST.IfStatement cond ifB elseB -> do
        -- We need three blocks: one for if the condition is true,
        ifthen <- addBlock "if.then"
        -- one for otherwise,
        ifelse <- addBlock "if.else"
        -- and a third for them to merge at.
        ifmerge <- addBlock "if.merge"

        -- Codegen the predicate.
        test <- exprCodegen cond
        terminate $ LLInstr.CondBr test ifthen ifelse []
        
        inBlock ifthen $
            mapM_ stmtCodegen ifB

        inBlock ifelse $
            mapM_ stmtCodegen elseB

        switchBlock ifmerge
  where exprTy = contents . TAST.exprType
        declare n ty = do
            let llty = translateType ty
            alloca <- instr (LLInstr.Alloca llty Nothing 0 []) (ptr llty)
            zoom symtab $ Scope.insert n (Types.Pointer ty, alloca)
            return alloca

runCgInLlvm :: Codegen a -> LLVM (a, CodegenState)
runCgInLlvm cg = do
    cgState <- initCG <$> use env
    let maybeSt = runStateT cg cgState
    case runIdentity $ runExceptT maybeSt of
        Left err -> throwError err
        Right res -> return res

toplevelCodegen :: Annotated TAST.TopLevel -> LLVM ()
toplevelCodegen (Annotated tl p) = case tl of
    TAST.Function sig body -> do
        when (sig^.TAST.ntargs == 0) $ do
            body' <- specializeAll body
            (args, retty) <- codegenSig sig
            (_, cgState) <- runCgInLlvm $ do
                fnBlock <- addBlock $ sig ^. TAST.name
                inBlock fnBlock $ do mapM_ declareArg (sig ^. TAST.args)
                                     mapM_ stmtCodegen body'
            addCgGlobals cgState
            define retty (sig ^. TAST.name) args (createBlocks cgState)

        when (sig^.TAST.ntargs /= 0) $
            functionTab %= Map.insert (sig^.TAST.name) (sig, body)

    TAST.FunctionDecl sig -> do
        (args, retty) <- codegenSig sig
        define retty (sig ^. TAST.name) args []
  where 
        codegenSig :: TAST.FunctionSignature -> LLVM ([(Type, Name)], Type)
        codegenSig (TAST.Sig name args ntargs retty) = do
            cgState <- initCG <$> use env
            (fty, llretty, argtys) <- fmap fst $ runCgInLlvm $ do
                let llretty = translateType retty
                    argtys = map (translateType . snd) args
                    fty = LLTy.FunctionType llretty argtys False 
                return (ptr fty, llretty, argtys)
            let op = ConstantOperand $ LLConst.GlobalReference fty $ mkName name
                val = (Types.Function (map snd args) retty, op)
            zoom env $ Scope.insert name val
            return (zip argtys (argName . fst <$> args), llretty)
        declareArg (str, ty) = do
            let llty = translateType ty
                argRegister = LocalReference llty $ argName str
            alloca <- instr (LLInstr.Alloca llty Nothing 0 []) (ptr llty)
            zoom symtab $ Scope.insert str (Types.Pointer ty, alloca)
            voidInstr $ LLInstr.Store False alloca argRegister Nothing 0 []
        argName str = mkName $ str ++ "##arg"

--
-- Low-level LLVM utilities.
--

-- | Get a higher-level version of an LLVM AST cast constructor.
fromLlvmCast :: (Operand -> Type -> InstructionMetadata -> Instruction)
             -> Operand -> Types.Type -> Codegen Operand
fromLlvmCast llcast o t = instr (llcast o llty []) llty
  where llty = translateType t

-- | A simple pointer in the default address space.
ptr :: Type -> Type
ptr ty = PointerType ty (AddrSpace 0)

-- | Create an operand from a constant @Integer@.
constInt :: Integer -> Operand
constInt i = ConstantOperand (LLConst.Int 64 i)

fext :: Operand -> Types.Type -> Codegen Operand
fext = fromLlvmCast LLInstr.FPExt

ftrunc :: Operand -> Types.Type -> Codegen Operand
ftrunc = fromLlvmCast LLInstr.FPTrunc

sext :: Operand -> Types.Type -> Codegen Operand
sext = fromLlvmCast LLInstr.SExt

zext :: Operand -> Types.Type -> Codegen Operand
zext = fromLlvmCast LLInstr.ZExt

trunc :: Operand -> Types.Type -> Codegen Operand
trunc = fromLlvmCast LLInstr.Trunc

bitcast :: Operand -> Types.Type -> Codegen Operand
bitcast = fromLlvmCast LLInstr.BitCast

intAdd :: Operand -> Operand -> Types.Type -> Codegen Operand
intAdd l r t = instr (LLInstr.Add False False l r []) (translateType t)

floatAdd :: Operand -> Operand -> Types.Type -> Codegen Operand
floatAdd l r t = instr (LLInstr.FAdd NoFastMathFlags l r []) (translateType t)

ptrAdd :: Operand -> Operand -> Types.Type -> Codegen Operand
ptrAdd ptr offset t = instr (LLInstr.GetElementPtr False ptr [offset] [])
                            (translateType t)

intSub :: Operand -> Operand -> Types.Type -> Codegen Operand
intSub l r t = instr (LLInstr.Sub False False l r []) (translateType t)

intNeg :: Operand -> Types.Type -> Codegen Operand
intNeg = intSub (constInt 0)

floatSub :: Operand -> Operand -> Types.Type -> Codegen Operand
floatSub l r t = instr (LLInstr.FSub NoFastMathFlags l r []) (translateType t)

ptrSub :: Operand -> Operand -> Types.Type -> Codegen Operand
ptrSub l r t = do lhsAsInt <- bitcast l (Types.Signed 64)
                  rhsAsInt <- bitcast r (Types.Signed 64)
                  intSub l r t

intMul :: Operand -> Operand -> Types.Type -> Codegen Operand
intMul l r t = instr (LLInstr.Mul False False l r []) (translateType t)

floatMul :: Operand -> Operand -> Types.Type -> Codegen Operand
floatMul l r t = instr (LLInstr.FMul NoFastMathFlags l r []) (translateType t)

uintDiv :: Operand -> Operand -> Types.Type -> Codegen Operand
uintDiv l r t = instr (LLInstr.UDiv False l r []) (translateType t)

sintDiv :: Operand -> Operand -> Types.Type -> Codegen Operand
sintDiv l r t = instr (LLInstr.SDiv False l r []) (translateType t)

floatDiv :: Operand -> Operand -> Types.Type -> Codegen Operand
floatDiv l r t = instr (LLInstr.FDiv NoFastMathFlags l r []) (translateType t)

fcmpFromCode :: FPPred.FloatingPointPredicate -> Operand -> Operand
             -> Codegen Operand
fcmpFromCode code o1 o2 = instr (LLInstr.FCmp code o1 o2 []) LLTy.i1

icmpFromCode :: IPred.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmpFromCode code o1 o2 = instr (LLInstr.ICmp code o1 o2 []) LLTy.i1
