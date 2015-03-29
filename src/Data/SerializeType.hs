module Data.SerializeType where

data Type
    = PlainType { typeName :: String }
    | List { elementType :: Type }
    | Object [ObjectEntry]
    deriving (Show, Eq)

data ObjectEntry = ObjectEntry { key :: String, valueType :: Type }
    deriving (Show, Eq)

data TypeDef = TypeDef { typeDefName :: String, typeDefType :: Type }
    deriving (Show, Eq)
