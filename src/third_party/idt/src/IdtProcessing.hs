module IdtProcessing
  ( CyclicDependencyError(..)
  , Declaration(..)
  , DeclarationSequence
  , asDeclarationSequence
  , isSimpleEnum
  )
where

import           Idt
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Data.Map.Strict               as Map
import           Data.List                     as List

deriving instance Eq NamedType
deriving instance Show NamedType
deriving instance Eq Type
instance Show Type where
  show (Concrete       (NamedType n _)) = "Concrete \"" ++ n ++ "\""
  show (Reference      (NamedType n _)) = "Reference \"" ++ n ++ "\""
  show (NamedPrimitive n              ) = "NamedPrimitive \"" ++ n ++ "\""
  show (List           t              ) = "List (" ++ show t ++ ")"
  show (Tuple          x              ) = "Tuple " ++ show x
  show (FixedSizeArray t s) =
    "(FixedSizeArray " ++ show t ++ " " ++ show s ++ ")"
  show (Struct      x) = "Struct " ++ show x
  show (Enum        x) = "Enum " ++ show x
  show (TaggedUnion x) = "TaggedUnion " ++ show x

data Declaration = TypeDeclaration NamedType | ForwardDeclaration NamedType
                   deriving (Show)
-- A DeclarationSequence is intended to have the properties that for any
-- NamedType in the list, it only refers to types that have already appeared
-- previously in the DeclarationSequence.  This format makes it easy to
-- digest for target outputs.
type DeclarationSequence = [Declaration]

type NamedTypeMap = Map.Map String NamedType
data DeclarationSequenceBuilderState = DeclarationSequenceBuilderState {
  declarationSequence :: DeclarationSequence,
  forwardDeclarations :: NamedTypeMap,
  typeDeclarations :: NamedTypeMap
} deriving (Show)
emptyDeclarationSequence = DeclarationSequenceBuilderState [] empty empty

newtype CyclicDependencyError = CyclicDependencyError NamedType

instance Show CyclicDependencyError where
  show (CyclicDependencyError nt) = show $ name nt

type DeclarationSequenceBuilder
  = State (Either CyclicDependencyError DeclarationSequenceBuilderState)

addTypeDeclaration
  :: DeclarationSequenceBuilderState
  -> NamedType
  -> DeclarationSequenceBuilderState
addTypeDeclaration (DeclarationSequenceBuilderState ds fd td) nt@(NamedType n t)
  = DeclarationSequenceBuilderState (ds ++ [TypeDeclaration nt])
                                    (Map.delete n fd)
                                    (Map.insert n nt td)

addForwardDeclaration
  :: DeclarationSequenceBuilderState
  -> NamedType
  -> DeclarationSequenceBuilderState
addForwardDeclaration (DeclarationSequenceBuilderState ds fd td) nt@(NamedType n t)
  = DeclarationSequenceBuilderState (ds ++ [ForwardDeclaration nt])
                                    (Map.insert n nt fd)
                                    (assert (not $ member n td) td)

ensureTypeDefined :: NamedTypeMap -> NamedType -> DeclarationSequenceBuilder ()
ensureTypeDefined under_construction nt@(NamedType n t)
  | member n under_construction =
      -- This type is currently being built above us in the call stack, which
      -- means we'd loop forever if we recursed into it, so return an error.
                                  put $ Left (CyclicDependencyError nt)
  | otherwise = do
    original_state <- get
    case original_state of
      Left  _  -> return ()
      Right ds -> unless (member n (typeDeclarations ds)) $ do
          -- The type has not been seen before, so add it to the list of
          -- definitions before returning to our parent.
        buildDeclarationSequence (Map.insert n nt under_construction) t
        after_child_state <- get
        case after_child_state of
          Left  _  -> return ()
          Right ds -> put $ Right (addTypeDeclaration ds nt)

ensureTypeDefinedOrForwardDeclared
  :: NamedTypeMap -> NamedType -> DeclarationSequenceBuilder ()
ensureTypeDefinedOrForwardDeclared under_construction nt@(NamedType n t) = do
  original_state <- get
  case original_state of
    Left  _  -> return ()
    Right ds -> do
      ensureTypeDefined under_construction nt
      new_state <- get
      case new_state of
        Right _ -> return ()  -- If there's no problem, there's no problem!
        Left (CyclicDependencyError (NamedType en et)) ->
          -- In the case of a cyclic dependency, create a forward declaration
          -- if we're the cyclic dependency and a forward declaration has
          -- not already been declared.
          when ((en == n) || member en under_construction)
            $ put
            $ if member n (forwardDeclarations ds)
                then original_state
                else Right $ addForwardDeclaration ds nt


buildDeclarationSequence
  :: NamedTypeMap -> Type -> DeclarationSequenceBuilder ()
buildDeclarationSequence under_construction (Concrete nt) =
  ensureTypeDefined under_construction nt

buildDeclarationSequence under_construction (Reference nt) =
  ensureTypeDefinedOrForwardDeclared under_construction nt

buildDeclarationSequence under_construction (NamedPrimitive _) = return ()
buildDeclarationSequence under_construction (List (Concrete nt)) =
  -- Since a list is variable length, it will always be allocated at runtime,
  -- which means it's possible to have a list of forward declared types.
  ensureTypeDefinedOrForwardDeclared under_construction nt
buildDeclarationSequence under_construction (List t) =
  buildDeclarationSequence under_construction t
buildDeclarationSequence under_construction (Tuple ts) =
  mapM_ (buildDeclarationSequence under_construction) ts
buildDeclarationSequence under_construction (FixedSizeArray t s) =
  buildDeclarationSequence under_construction t
buildDeclarationSequence under_construction (Struct nts) =
  mapM_ (\(n, t) -> buildDeclarationSequence under_construction t) nts
buildDeclarationSequence under_construction (Enum nts) = mapM_
  (\(n, ts) -> mapM_ (buildDeclarationSequence under_construction) ts)
  nts
buildDeclarationSequence under_construction (TaggedUnion ts) =
  mapM_ (buildDeclarationSequence under_construction) ts


defineTypeIfPossible :: NamedType -> DeclarationSequenceBuilder ()
defineTypeIfPossible nt = do
  original_state <- get
  buildDeclarationSequence empty $ Concrete nt
  after_state <- get
  case after_state of
    Left  _ -> put original_state
    Right _ -> return ()

defineRemainingForwardDeclarations :: DeclarationSequenceBuilder ()
defineRemainingForwardDeclarations = do
  original_state <- get
  case original_state of
    Left _ -> return ()
    Right (DeclarationSequenceBuilderState ds fd td) ->
      unless (Map.null fd) $ do
        mapM_ defineTypeIfPossible (elems fd)
        after_state <- get
        case after_state of
          Left _ -> return ()
          Right (DeclarationSequenceBuilderState _ afd _) ->
            if size fd /= size afd
              then
-- Iterate until we stabilize.
                   defineRemainingForwardDeclarations
              else
-- If the size is not zero, return the results from evaluating an
-- arbitrary element in the remaining list, which will produce
-- an error.
                when (size fd /= 0) $ buildDeclarationSequence empty $ Concrete
                  (head $ elems fd)

toDeclarationSequence
  :: [Type] -> Either CyclicDependencyError DeclarationSequence
toDeclarationSequence tl =
  let fullSequenceBuilder = do
        -- Start by defining everything we can from the initial set of types,
        -- and aggregate the results.
        mapM_ (buildDeclarationSequence empty) tl
        -- Next, ensure that any yet-undefined forward declarations eventually
        -- get defined.
        defineRemainingForwardDeclarations
  in  case execState fullSequenceBuilder (Right emptyDeclarationSequence) of
        Right ds -> Right (declarationSequence ds)
        Left  e  -> Left e

asDeclarationSequence
  :: [NamedType] -> Either CyclicDependencyError DeclarationSequence
asDeclarationSequence = toDeclarationSequence . List.map Concrete

-- Returns true if the given Enum parameters all have zero parameters.
-- In other words, this is a "C++ enum" as opposed to a Haskell enum where
-- the constructors take parameters.
isSimpleEnum :: [(String, [Type])] -> Bool
isSimpleEnum = all (\(_, ts) -> List.null ts)
