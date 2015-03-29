module Parser(parseTypeDefs) where

import Control.Applicative ((<$>), (<*))

import Data.SerializeType

import Text.Parsec
import Text.ParserCombinators.Parsec

parseTypeDefs :: String -> Either ParseError [TypeDef]
parseTypeDefs = parse typeDefs "(unknown)"

typeDefs = spaces >> many (typeDef <* spaces) <* eof

typeDef = do
    name <- identifier
    spaces
    char ':'
    spaces
    t <- type_parser
    return $ TypeDef name t

identifier = many1 (alphaNum <|> char '_')

type_parser =
        plain_type
    <|> list_type
    <|> object_type
    <?> "type"

plain_type = PlainType <$> identifier

list_type = List <$> between (char '[') (char ']') (spaces >> type_parser)

object_type = Object <$> between (char '{') (char '}') (object_entry `sepBy` char ',')

object_entry = do
    spaces
    name <- identifier
    spaces
    char ':'
    spaces
    t <- type_parser
    spaces
    return $ ObjectEntry name t

