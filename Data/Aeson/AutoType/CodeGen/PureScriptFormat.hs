{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGuaGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGuaGE DeriveGeneric       #-}
{-# LANGuaGE FlexibleContexts    #-}

-- | Formatting type declarations and class instances for inferred types.

module Data.Aeson.AutoType.CodeGen.PureScriptFormat(
  -- displaySplitTypes,
  normalizeTypeName) where
import           Control.Arrow             ((&&&))
import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens.TH
import           Control.Lens
import           Control.Monad             (forM)
import           Control.Exception(assert)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Text                 (Text)
import           Data.Set                  (Set )
import           Data.List                 (foldl1')
import           Data.Char                 (isAlpha, isDigit)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)
import qualified Data.Graph          as Graph
import           GHC.Generics              (Generic)
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Split
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Util    ()

--------------------------------------------------------------------------------
trace _ x = x

-- | Explanatory type alias for making declarations
-- First element of the triple is original JSON identifier,
-- second element of the triple is the mapped identifier name in Haskell.
-- third element of the triple shows the type in a formatted way
type MappedKey = (Text, Text, Text, Bool)

data DeclState = 
  DeclState 
    { _decls   :: [Text]
    , _counter :: Int
    }
    deriving (Eq, Show, Ord, Generic)

makeLenses ''DeclState

type DeclM = State DeclState

type Map k v = Map.HashMap k v 

fst3 ::  (t, t1, t2) -> t
fst3 (a, _b, _c) = a

stepM :: DeclM Int
stepM = counter %%= (\i -> (i, i+1))

tShow :: (Show a) => a -> Text
tShow = T.pack . show 

-- | Clean up and normalize field names, 
--   
normalizeFieldName ::  Text -> Text -> Text
normalizeFieldName identifier = 
  escapeKeywords 
    . uncapitalize 
      . (normalizeTypeName identifier `T.append`) 
        . normalizeTypeName

-- | Language specific keywords that already defined as a part of language 
--   
keywords :: Set Text
keywords = 
  Set.fromList 
  [ "data"
  , "type"
  , "foreign"
  , "import"
  , "infixl"
  , "infixr"
  , "infix"
  , "class"
  , "instance"
  , "module"
  , "case"
  , "of"
  , "if"
  , "then"
  , "else"
  , "do"
  , "let"
  , "true"
  , "false"
  , "in"
  ]

-- |
-- 
escapeKeywords ::  Text -> Text
escapeKeywords k | k `Set.member` keywords = k `T.append` "_"
escapeKeywords k                           = k


-- | Format the type within DeclM monad, that records
-- the separate declarations on which this one is dependent.
formatType :: Type -> DeclM Text
formatType  TString                          = return "Text"
formatType  TNum                             = return "Double"
formatType  TBool                            = return "Bool"
formatType (TLabel l)                        = return $ normalizeTypeName l
formatType (TUnion u)                        = wrap <$> case length nonNull of
                                                          0 -> return emptyTypeRepr
                                                          1 -> formatType $ head nonNull
                                                          _ -> T.intercalate ":|:" <$> mapM formatType nonNull
  where
    nonNull       = Set.toList $ Set.filter (TNull /=) u
    wrap                                :: Text -> Text
    wrap   inner  | TNull `Set.member` u = T.concat ["(Maybe (", inner, "))"]
                  | otherwise            =                          inner
formatType (TArray a)                        = do inner <- formatType a
                                                  return $ T.concat ["[", inner, "]"]
formatType (TObj   o)                        = do ident <- genericIdentifier
                                                  newDecl ident d
  where
    d = Map.toList $ unDict o 
formatType  e | e `Set.member` emptySetLikes = return emptyTypeRepr
formatType  t                                = return $ "ERROR: Don't know how to handle: " `T.append` tShow t

emptyTypeRepr :: Text
emptyTypeRepr = "(Maybe Value)" -- default, accepts future extension where we found no data

-- | Makes a generic identifier name.
genericIdentifier :: DeclM Text
genericIdentifier = do
  i <- stepM
  return $! "Obj" `T.append` tShow i

-- | Printing a single data type declaration
newDecl :: Text -> [(Text, Type)] -> DeclM Text
newDecl identifier kvs = do attrs <- forM kvs $ \(k, v) -> do
                              formatted <- formatType v
                              return (k, normalizeFieldName identifier k, formatted, isNullable v)
                            let decl = T.unlines 
                                         [ wrapDecl identifier $ fieldDecls attrs
                                         , ""
                                         , makeEncoder identifier attrs
                                         ,""
                                         , makeDecoder identifier attrs
                                         ]
                            addDecl decl
                            return identifier
  where
    fieldDecls attrList = T.intercalate ",\n" $ map fieldDecl attrList
    fieldDecl :: (Text, Text, Text, Bool) -> Text
    fieldDecl (_jsonName, haskellName, fType, _nullable) = T.concat [
                                                             "    ", haskellName, " :: ", fType]
-- | Adds declaration
addDecl decl = decls %%= (\ds -> ((), decl:ds))

-- | Run declaration
runDecl ::  DeclM a -> Text
runDecl decl = T.unlines $ finalState ^. decls
  where
    initialState    = DeclState [] 1
    (_, finalState) = runState decl initialState

formatObjectType ::  Text -> Type -> DeclM Text
formatObjectType identifier (TObj o) = 
  newDecl identifier d
    where 
      d = Map.toList $ unDict o
formatObjectType identifier other = newAlias identifier other

-- | Add new type alias for Array type
newAlias :: Text -> Type -> DeclM Text
newAlias identifier content = do 
  formatted <- formatType content
  addDecl $ T.unlines [wrapAlias identifier formatted]
  return identifier

-- | Wrap a type alias.
wrapAlias :: Text -> Text -> Text
wrapAlias identifier contents = 
  T.unwords 
    [ "type"
    , identifier
    , "="
    , contents
    ]

-- | Wrap a data type declaration
wrapDecl ::  Text -> Text -> Text
wrapDecl identifier contents = 
  T.unlines 
    [ header
    , contents
    , " "
    , wrapDeriver identifier
    ]
  where
    header = T.concat ["data ", identifier, " = ", identifier, " { "]

-- | There is no easy deriving we need actually
--   declare instnaces, like `derive instance name :: Generic MyType`
--   for automatic derivation
wrapDeriver :: Text -> Text
wrapDeriver identifier = 
  T.concat
    [ "derive instance name :: Generic "
    ,  identifier
    ]

makeEncoder :: Text -> [MappedKey] -> Text
makeEncoder identifier contents = ""

makeDecoder :: Text -> [MappedKey] -> Text
makeDecoder identifier contents = ""

--------------------------------------------------------------------------------
-- -- | 
-- encoderIdent ident = 
--   "encodeJson" <> capitalize (normalizeTypeName ident)

-- -- | Make Encoder declaration, given identifier (object name in Haskell) 
-- --   and mapping of its keys from JSON to Haskell identifiers in the same
-- --   order as in declaration
-- makeEncoder :: Text         -- ^
--             -> [MappedKey]  -- ^
--             -> Text
-- makeEncoder identifier contents =
--   ext.unlines 
--     [ T.unwords [ encoderIdent identifier
--                 , ":"
--                 , identifier
--                 , "->"
--                 , "Json.Encode.Value"
--                 ]
--     , encoderIdent identifier <> " record ="
--     , "    Json.Encode.object ["
--     , "        " <> (joinWith "\n      , " (makeEncoder <$> contents))
--     , "    ]"
--     ]
--   where
--     makeEncoder (jsonId, haskellId, _typeText, ty, _nullable) = 
--       T.concat 
--         [ "("
--         , tShow jsonId
--         , ", ("
--         , getEncoder ty
--         , ") record."
--         , normalizeFieldName identifier jsonId
--         , ")"
--         ]
--     --"answers",  Json.Encode.list <| List.map encodeAnswer <| record.answers
--     escapeText = T.pack . show . T.unpack

-- getEncoder :: Type -> Text
-- getEncoder  TString   = "A.encodeJsonJString"
-- getEncoder  TNum      = "A.encodeJsonJNumber"
-- getEncoder  TBool     = "A.encodeJsonJBoolean"
-- getEncoder  TNull     = "A.encodeJsonUnit"
-- getEncoder (TLabel l) = encoderIdent l
-- getEncoder (TArray e) = "A.encodeJsonArray"
-- getEncoder (TObj   o) = error $ "Seeing direct object encoder: "         <> show o
-- getEncoder (TUnion u) = 
--   case nonNull of
--     []  -> "encodeJsonUnit"
--     [x] -> getDecoder x
--     _   -> foldl1' altEncoder $ map getEncoder nonNull
--   where
--     nonNull = nonNullComponents u

-- altEncoder a b = "Either.unpack (" <> a <> ") (" <> b <> ")"


-------------------------------------------------------------------------------


-- | Display an environment of types split by name.
-- 
displaySplitTypes ::  Map Text Type -> Text
displaySplitTypes dict = 
  trace ("displaySplitTypes: " ++ show (toposort dict)) $ runDecl declarations
    where
      declarations =
        forM (toposort dict) $ \(name, typ) ->
          formatObjectType (normalizeTypeName name) typ

-- | Normalize type name by:
--   1. Treating all characters that are not acceptable in Haskell variable name as end of word.
--   2. Capitalizing each word, but a first (camelCase).
--   3. Adding underscore if first character is non-alphabetic.
--   4. Escaping Haskell keywords if the whole identifier is such keyword.
--   5. If identifier is empty, then substituting "JsonEmptyKey" for its name.
normalizeTypeName :: Text -> Text
normalizeTypeName s  = 
  ifEmpty "JsonEmptyKey"                  
    . escapeKeywords                          
      . escapeFirstNonAlpha                     
        . T.concat                             
          . map capitalize                          
            . filter (not . T.null)            
              . T.split (not . acceptableInVariable) 
                $ s
  where
    ifEmpty x ""       = x
    ifEmpty _ nonEmpty = nonEmpty
    
    acceptableInVariable c = isAlpha c || isDigit c
    
    escapeFirstNonAlpha cs               | T.null cs  =                cs
    escapeFirstNonAlpha cs@(T.head -> c) | isAlpha c  =                cs
    escapeFirstNonAlpha cs                            = "_" `T.append` cs
