module SymbolTable where

import qualified Data.Map as M
import qualified Control.Monad.State as S

import AST

type MemSegLookup = M.Map Keyword Int
type MemSegCountState = S.State MemSegLookup

data SymbolInfo = SymbolInfo
  {
    typ :: Type
  , kind :: Keyword
  , occurrence :: Int
  } deriving (Show, Eq)

buildSymInfo :: [(VarName, Int)] -> Type -> Keyword -> [(VarName, SymbolInfo)]
buildSymInfo xs t k = map (buildSym t k) xs
  where buildSym t' k' (vn, oc) =
          (vn, SymbolInfo {typ = t', kind = k', occurrence = oc })

clsSymElms :: ClassVarDec -> MemSegCountState [(VarName, SymbolInfo)]
clsSymElms (ClassVarDec segKind tp vn vns) = do
  -- return a list of pairs ready to be used in M.fromList to make symbol table
  memSegCount <- S.get
  let kindCount = getCount memSegCount segKind
      inputPairs = zip (vn:vns) [kindCount..]
      output = buildSymInfo inputPairs tp segKind
  S.put $ M.adjust ((length output)+) segKind memSegCount
  return output
  where getCount msc sk = case M.lookup sk msc of
          Nothing -> -1 -- will cause VM error and alert that inital state isn't correct
          (Just c) -> c

classVarSymTable :: Class -> (M.Map VarName SymbolInfo)
classVarSymTable (Class _ _ cVarDecs _) =
{-
  TODO: maybe add error handling around duplicate var declarations
  if you had something like:

  static int foo, bar
  field int foo, bar

  the last instance of foo and bar win and overwrite the values
  from the static declaration when the symbol table map is created
-}
  M.fromList
  $ concat
  $ S.evalState (mapM clsSymElms cVarDecs)
  $ M.fromList [((Keyword "static"),  0), ((Keyword "field"), 0)]

subArgSyms :: ParameterList -> Type -> [(VarName, SymbolInfo)]
subArgSyms (ParameterList params) cls = thisEntry:paramSymInfo params
  where thisEntry = ((Identifier "this"),
                     SymbolInfo { typ = cls,
                                  kind = (Keyword "argument"),
                                  occurrence = 0})
        paramSymInfo ps = concatMap symInfos $ zip ps [1..]
        symInfos ((tp, vn), oc) = buildSymInfo [(vn, oc)] tp (Keyword "argument")

subBodySyms :: SubroutineBody -> [(VarName, SymbolInfo)]
subBodySyms (SubroutineBody vars _) =
  concatMap symInfos $ zip (concatMap joinVars vars) [0..]
  where symInfos ((tp, vn), oc) = buildSymInfo [(vn, oc)] tp (Keyword "local")
        joinVars (VarDec t vn vns) = map (\v -> (t, v)) (vn:vns)

subVarSymTable :: Type -> SubroutineDec -> (M.Map VarName SymbolInfo)
subVarSymTable cls (SubroutineDec _ _ _ params subBody) =
  M.fromList $ subArgSyms params cls ++ subBodySyms subBody
