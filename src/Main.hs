module Main(main) where

import Data.Either
import qualified Data.ByteString.Char8 as BS

import Data.SerializeType
import Parser

import Emitter.QJson

example = "my_type : { x: [double], y: {z:double, y:double}}"

main = interact $ either show (BS.unpack . emitQJson) . parseTypeDefs
