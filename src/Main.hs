module Main(main) where

import Data.SerializeType
import Parser

example = "my_type : { x: [double], y: {z:double, y:double}}"

-- main = interact $ show . parseTypeDefs
main = print $ parseTypeDefs example
