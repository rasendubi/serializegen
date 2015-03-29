{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Emitter.QJson(emitQJson) where

import Data.SerializeType

import Data.ByteString.Char8 as BS
import Text.Literal.TH (litFile)

emitQJson :: [TypeDef] -> ByteString
emitQJson typedefs = BS.concat
    [ emitHeader
    , emitStructs typedefs
    , emitDefs typedefs
    ]

emitHeader = BS.pack $ [litFile|src/Emitter/QJson_header.h|]

emitStructs :: [TypeDef] -> ByteString
emitStructs =  BS.concat . fmap emitStruct

emitStruct (TypeDef name type_def) = BS.concat
    [ "typedef "
    , emitType type_def
    , " "
    , BS.pack name
    , ";"
    ]

emitType (PlainType name) = BS.pack name
emitType (List elem_type) = BS.concat
    [ "QList<"
    , emitType elem_type
    , ">"
    ]
emitType (Object fields) = BS.concat
    [ "struct {\n"
    , BS.concat (fmap (flip BS.append ";\n" . emitField) fields)
    , "}"
    ]

emitField (ObjectEntry key valueType) = BS.concat
    [ emitType valueType
    , " "
    , BS.pack key
    ]

emitDefs = BS.concat . fmap emitDef

emitDef (TypeDef name def) = BS.concat
    [ "\n\ntemplate<>\n"
    , "struct JsonEmitter<", BS.pack name, "> {\n"
    , "\tstatic QJsonValue toJson(const ", BS.pack name, "& value) {\n"
    , "\t\treturn ", emitSerialize "value" def, ";\n"
    , "\t}\n"
    , "\n"
    , "\tstatic ", BS.pack name, " fromJson(QJsonValue value) {\n"
    , "\t\treturn ", emitDeserialize "value" def, ";\n"
    , "\t}\n"
    , "};\n\n"
    ]

emitSerialize value (PlainType name) = BS.concat
    [ "toJsonValue<", BS.pack name, ">(", value, ")" ]
emitSerialize value (Object fields) = BS.concat
    [ "QJsonObject { ", emitSerializeFields (value `BS.append` ".toObject()") fields, "}" ]
emitSerialize value (List (PlainType name)) = BS.concat
    [ "fromJsonList<", BS.pack name, ">(", value, ".toArray())" ]
emitSerialize value (List _) = error "arbitrary lists are not supported"

emitSerializeFields base = BS.intercalate ", " . fmap serializeField
    where
        serializeField (ObjectEntry key valueType) =
            emitSerialize (BS.concat [ base, "[\"", BS.pack key, "\"]" ]) valueType

emitDeserialize value (PlainType name) = BS.concat
    [ "fromJsonValue<", BS.pack name, ">(", value, ")" ]
emitDeserialize value (Object fields) = BS.concat
    [ "{ ", emitDesrializeFields (value `BS.append` ".toObject()") fields, "}" ]
emitDeserialize value (List (PlainType name)) = BS.concat
    [ "toJsonList<", BS.pack name, ">(", value, ".toArray())" ]
emitDeserialize _ _ = error "arbitrary lists are not supported"

emitDesrializeFields base = BS.intercalate ", " . fmap deserializeField
    where
        deserializeField (ObjectEntry key valueType) =
            emitDeserialize (BS.concat [ base, "[\"", BS.pack key, "\"]" ]) valueType
