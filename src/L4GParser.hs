module L4GParser where 

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

l4gdef = emptyDef {
    commentLine = "%",
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> char '_',
    caseSensitive = True
}

lexer = makeTokenParser l4gdef

sortsParser = many sortDeclarationParser

sortDeclarationParser = numDeclarationParser <|> localSortDeclarationParser

numDeclarationParser = try $ do
    name <- numSortNameParser 
    declaration <- cardinalitySpecListParser
    return (name, declaration)

numSortNameParser = identifier lexer

localSortDeclarationParser = do
    name <- localSortNameParser
    sortSpec <- sortSpecParser
    return (name, sortSpec)

localSortNameParser = identifier lexer

cardinalitySpecListParser = ((:[]) <$> cardinalitySpecParser) <|> (braces lexer $ many cardinalitySpecParser)

oneOfSymbol x = choice $ fmap (symbol lexer) x

cardinalitySpecParser = do
    symbol lexer "cardinality"
    relation <- oneOfSymbol ["<", "=", "!=", ">"]
    posint <- natural lexer
    if posint == 0 then 
        fail "Cardinality must be greater than 0" 
    else return $ show (relation, posint)

sortSpecParser = cardinalitySpecListParser <|> enumerationParser <|> sortAliasParser 

enumerationParser = do
    symbol lexer "enum"
    colon lexer
    enumList <- sepBy1 enumItemNameParser (comma lexer)
    symbol lexer "."
    return enumList

enumItemNameParser = identifier lexer 

sortAliasParser = (symbol lexer "alias") >> return <$> sortNameParser 

sortNameParser = symbol lexer "bool" <|> numSortNameParser <|> localSortNameParser

