{-# LANGUAGE BangPatterns #-}
module Generate
  ( debug
  , dev
  , prod
  , repl
  )
  where


import Prelude hiding (cycle, print)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Monad (liftM2)
import qualified Data.ByteString.Builder as B
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as N
import qualified Data.NonEmptyList as NE
import qualified Data.List as List

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Type as Type
import qualified Build
import qualified Canonicalize.Effects as Effects
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified File
import qualified Generate.JavaScript as JS
import qualified Generate.Mode as Mode
import qualified Nitpick.Debug as Nitpick
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff

import Debug.Trace
-- NOTE: This is used by Make, Repl, and Reactor right now. But it may be
-- desireable to have Repl and Reactor to keep foreign objects in memory
-- to make things a bit faster?



-- GENERATORS


type Task a =
  Task.Task Exit.Generate a

toStr :: (ModuleName.Raw, N.Name) -> String
toStr (r, n) =
  ModuleName.toChars r ++ "." ++ N.toChars n

debug :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
debug root details artifacts@(Build.Artifacts pkg ifaces roots modules) =
  do  loading <- loadObjects root details modules
      types   <- loadTypes root ifaces modules
      objects <- finalizeObjects loading
      exports <- gatherTopLevelFns root pkg modules (Build.getRootNames artifacts)
      let mode = Mode.Dev (Just types)
      let graph = objectsToGlobalGraph objects
      return $ JS.generateForNodeJs mode graph exports


dev :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
dev root details artifacts@(Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      exports <- gatherTopLevelFns root pkg modules (Build.getRootNames artifacts)
      let mode = Mode.Dev Nothing
      let graph = objectsToGlobalGraph objects
      return $ JS.generateForNodeJs mode graph exports


prod :: FilePath -> Details.Details -> Build.Artifacts -> Task B.Builder
prod root details artifacts@(Build.Artifacts pkg _ roots modules) =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      checkForDebugUses objects
      exports <- gatherTopLevelFns root pkg modules (Build.getRootNames artifacts)
      let graph = objectsToGlobalGraph objects
      let mode = Mode.Prod (Mode.shortenFieldNames graph)
      return $ JS.generateForNodeJs mode graph exports


repl :: FilePath -> Details.Details -> Bool -> Build.ReplArtifacts -> N.Name -> Task B.Builder
repl root details ansi (Build.ReplArtifacts home modules localizer annotations) name =
  do  objects <- finalizeObjects =<< loadObjects root details modules
      let graph = objectsToGlobalGraph objects
      return $ JS.generateForRepl ansi localizer graph home name (annotations ! name)



-- CHECK FOR DEBUG


checkForDebugUses :: Objects -> Task ()
checkForDebugUses (Objects _ locals) =
  case Map.keys (Map.filter Nitpick.hasDebugUses locals) of
    []   -> return ()
    m:ms -> Task.throw (Exit.GenerateCannotOptimizeDebugValues m ms)



-- GATHER MAINS


gatherMains :: Pkg.Name -> Objects -> NE.List Build.Root -> Map.Map ModuleName.Canonical Opt.Main
gatherMains pkg (Objects _ locals) roots =
  Map.fromList $ Maybe.mapMaybe (lookupMain pkg locals) (NE.toList roots)


lookupMain :: Pkg.Name -> Map.Map ModuleName.Raw Opt.LocalGraph -> Build.Root -> Maybe (ModuleName.Canonical, Opt.Main)
lookupMain pkg locals root =
  let
    toPair name (Opt.LocalGraph maybeMain _ _) =
      (,) (ModuleName.Canonical pkg name) <$> maybeMain
  in
  case root of
    Build.Inside  name     -> toPair name =<< Map.lookup name locals
    Build.Outside name _ g -> toPair name g



-- GATHER FUNCTIONS IN ROOT

gatherTopLevelFns :: FilePath -> Pkg.Name -> [Build.Module] -> NE.List ModuleName.Raw -> Task JS.ExportedValues
gatherTopLevelFns root pkg modules roots =
  do
    topLevelFns <- gatherAllTopLevelFns root pkg modules roots

    case Map.keys (Map.filter hasWrongType topLevelFns) of
      [] -> return topLevelFns
      xs -> Task.throw $
        Exit.GenerateWrongExposedPayload $
          List.map (\(ModuleName.Canonical _ m, n) -> (m,n)) xs


hasWrongType :: Can.Annotation -> Bool
hasWrongType annotation =
  case Type.toFunctionType annotation of
    Type.FunctionType _ res args ->
      not $
        all checkPayload args
        && checkPayload res

checkPayload :: Can.Type -> Bool
checkPayload tipe =
  case Effects.checkPayload tipe of
    Left _ -> False
    Right _ -> True


gatherAllTopLevelFns :: FilePath -> Pkg.Name -> [Build.Module] -> NE.List ModuleName.Raw -> Task JS.ExportedValues
gatherAllTopLevelFns root pkg modules roots =
  fmap  (Map.mapKeys (\( moduleName, name) -> (ModuleName.Canonical pkg moduleName, name) )) $
    fmap Map.unions (sequence $ List.map (findFns root modules) (NE.toList roots))


findFns :: FilePath -> [Build.Module] -> ModuleName.Raw -> Task (Map.Map (ModuleName.Raw, N.Name) Can.Annotation)
findFns root modules rootName =
  fmap Map.unions $
    sequence $ List.map (extractValues root rootName) $ 
      List.filter  (\m -> rootName == moduleName m) modules 
  

extractValues :: FilePath -> ModuleName.Raw -> Build.Module -> Task (Map.Map (ModuleName.Raw, N.Name) Can.Annotation)
extractValues root rootName modul =
   case modul of
        Build.Fresh _ iface  _ ->
              interfaceToValues rootName iface

        Build.Cached _ _ mvar ->
            do
              cachedInterface <- Task.io $ readMVar mvar
              case cachedInterface of 
                Build.Loaded iface ->
                  interfaceToValues rootName iface

                Build.Unneeded ->
                  do 
                    maybeIface <- Task.io $ File.readBinary (Stuff.elmi root rootName)
                    case maybeIface of
                      Just iface ->
                        interfaceToValues rootName iface

                      Nothing ->
                        Task.throw (Exit.GenerateCannotLoadArtifacts)

                Build.Corrupted ->
                  Task.throw (Exit.GenerateCannotLoadArtifacts)

interfaceToValues :: ModuleName.Raw -> I.Interface -> Task (Map.Map (ModuleName.Raw, N.Name) Can.Annotation)
interfaceToValues rootName (I.Interface _ values _ _ _) =
  return $ Map.mapKeys ((,) rootName) (values) 

moduleName :: Build.Module -> ModuleName.Raw
moduleName modul =
    case modul of
        Build.Fresh n _ _  -> n
        Build.Cached n _ _  -> n




-- LOADING OBJECTS


data LoadingObjects =
  LoadingObjects
    { _foreign_mvar :: MVar (Maybe Opt.GlobalGraph)
    , _local_mvars :: Map.Map ModuleName.Raw (MVar (Maybe Opt.LocalGraph))
    }


loadObjects :: FilePath -> Details.Details -> [Build.Module] -> Task LoadingObjects
loadObjects root details modules =
  Task.io $
  do  mvar <- Details.loadObjects root details
      mvars <- traverse (loadObject root) modules
      return $ LoadingObjects mvar (Map.fromList mvars)


loadObject :: FilePath -> Build.Module -> IO (ModuleName.Raw, MVar (Maybe Opt.LocalGraph))
loadObject root modul =
  case modul of
    Build.Fresh name _ graph ->
      do  mvar <- newMVar (Just graph)
          return (name, mvar)

    Build.Cached name _ _ ->
      do  mvar <- newEmptyMVar
          _ <- forkIO $ putMVar mvar =<< File.readBinary (Stuff.elmo root name)
          return (name, mvar)



-- FINALIZE OBJECTS


data Objects =
  Objects
    { _foreign :: Opt.GlobalGraph
    , _locals :: Map.Map ModuleName.Raw Opt.LocalGraph
    }


finalizeObjects :: LoadingObjects -> Task Objects
finalizeObjects (LoadingObjects mvar mvars) =
  Task.eio id $
  do  result  <- readMVar mvar
      results <- traverse readMVar mvars
      case liftM2 Objects result (sequence results) of
        Just loaded -> return (Right loaded)
        Nothing     -> return (Left Exit.GenerateCannotLoadArtifacts)


objectsToGlobalGraph :: Objects -> Opt.GlobalGraph
objectsToGlobalGraph (Objects globals locals) =
  foldr Opt.addLocalGraph globals locals



-- LOAD TYPES


loadTypes :: FilePath -> Map.Map ModuleName.Canonical I.DependencyInterface -> [Build.Module] -> Task Extract.Types
loadTypes root ifaces modules =
  Task.eio id $
  do  mvars <- traverse (loadTypesHelp root) modules
      let !foreigns = Extract.mergeMany (Map.elems (Map.mapWithKey Extract.fromDependencyInterface ifaces))
      results <- traverse readMVar mvars
      case sequence results of
        Just ts -> return (Right (Extract.merge foreigns (Extract.mergeMany ts)))
        Nothing -> return (Left Exit.GenerateCannotLoadArtifacts)


loadTypesHelp :: FilePath -> Build.Module -> IO (MVar (Maybe Extract.Types))
loadTypesHelp root modul =
  case modul of
    Build.Fresh name iface _ ->
      newMVar (Just (Extract.fromInterface name iface))

    Build.Cached name _ ciMVar ->
      do  cachedInterface <- readMVar ciMVar
          case cachedInterface of
            Build.Unneeded ->
              do  mvar <- newEmptyMVar
                  _ <- forkIO $
                    do  maybeIface <- File.readBinary (Stuff.elmi root name)
                        putMVar mvar (Extract.fromInterface name <$> maybeIface)
                  return mvar

            Build.Loaded iface ->
              newMVar (Just (Extract.fromInterface name iface))

            Build.Corrupted ->
              newMVar Nothing
