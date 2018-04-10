module L4GParser where 

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

l4gdef = emptyDef {
    commentLine = "%",
    identStart = letter,
    identLetter = alphaNum,
    caseSensitive = True
}

lexer = makeTokenParser l4gdef

sortsParser = many sortDeclarationParser

sortDeclarationParser = numDeclarationParser <|> localSortDeclarationParser

numDeclarationParser = do
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
    posint <- natural lexer -- TODO Fail if zero
    return $ show (relation, posint)

sortSpecParser = cardinalitySpecListParser <|> enumerationParser <|> sortAliasParser 

enumerationParser = do
    symbol lexer "enum"
    colon lexer
    enumList <- sepBy1 enumItemNameParser (comma lexer)
    return enumList

enumItemNameParser = identifier lexer 

sortAliasParser = (symbol lexer "alias") >> return <$> sortNameParser 

sortNameParser = symbol lexer "bool" <|> numSortNameParser <|> localSortNameParser