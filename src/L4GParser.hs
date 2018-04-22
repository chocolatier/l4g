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
    relation <- parseSizeRelation
    posint <- natural lexer
    if posint == 0 then 
        fail "Cardinality must be greater than 0" 
    else return $ show (relation, posint)

parseSizeRelation = oneOfSymbol ["<", "=", "!=", ">"]

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

vocabularyParser = many symbolDeclarationParser

symbolDeclarationParser =   (symbol lexer "name" >> constantDeclarationListParser)
                        <|> (symbol lexer "predicate" >> predicateDeclarationListParser)
                        <|> (symbol lexer "function" >> functionDeclarationListParser)

listParser f = ((:[]) <$> f) <|> (braces lexer $ many f)

constantDeclarationListParser = listParser constantDeclarationParser

predicateDeclarationListParser = listParser predicateDeclarationParser

functionDeclarationListParser = listParser functionDeclarationParser

constantDeclarationParser = do
    constName <- localConstantNameParser 
    symbol lexer ":"
    sortName <- sortNameParser
    specList <- functionSpecListParser
    return (constName, [sortName], specList) 

predicateDeclarationParser = do 
    predName <- localPredicateNameParser
    sortList <- parens lexer $ sepBy1 sortNameParser (comma lexer)
    specList <- functionSpecListParser
    return (predName, sortList, specList)

functionDeclarationParser = do 
    predName <- localFunctionNameParser
    sortList <- parens lexer $ sepBy1 sortNameParser (comma lexer)
    specList <- functionSpecListParser
    return (predName, sortList, specList)
    
localConstantNameParser = identifier lexer

localPredicateNameParser = identifier lexer 

localFunctionNameParser = identifier lexer

parsePredicateName = parseSizeRelation <|> oneOfSymbol ["EST","NOT","AND","OR","IMP"] <|> localPredicateNameParser

--TODO: Should not drop the the sort name.
functionNameParser = oneOfSymbol ["LEN", "DIF"] <|> (oneOfSymbol ["PRED", "SUCC", "+", "-"] >>= (\x -> optionalSortNameParser >> return x))

--TODO: Should not drop the the sort name.
constantParser = oneOfSymbol ["FALSE", "TRUE"] <|> (oneOfSymbol ["MAX", "MIN"] >>= (\x -> optionalSortNameParser >> return x)) <|> (show <$> natural lexer) <|> enumItemNameParser <|> localConstantNameParser

--IDK if spaces are allowed to the left/right of the underscore. Assuming are.  
optionalSortNameParser = try $ symbol lexer "_" >> sortNameParser


functionSpecListParser = listParser functionSpecParser

functionSpecParser = (\x -> (x,"")) <$> oneOfSymbol ["all_different", "surjective", "surjective", "total", "partial", "commutative", "priority", "no_priority", "hidden", "cut", "no_cut"]
                     <|> scopeParser 
                     <|> printParser 
                     where
                        scopeParser = symbol lexer "scope" >> (\x -> ("scope", show x)) <$> natural lexer 
                        printParser = symbol lexer "print" >> try (colon lexer) >>  (\x -> ("print", show x)) <$> verbosityLevelParser

verbosityLevelParser = oneOfSymbol ["none", "brief", "full"]

constraintsParser = many clauseParser 

clauseParser = do
    ant <- option [] antParser 
    cons <- consParser 
    symbol lexer "."
    return (ant,cos)

antParser = sepBy1 (comma lexer) formulaParser >> symbol lexer "->"

consParser = sepBy1 (semi lexer) formulaParser

formulaParser = parens lexer formulaParser <|> constantParser <|> qfParser <|> fnfParser <|> ffnfparser <|> variableParser

qfParser = do
    q <- quantifierParser 
    f <- formulaParser 
    return $ show (q,f)

quantifierParser = do 
    q <- quantityIndicatorParser 
    v <- variableParser
    return (q,v)

quantityIndicatorParser = oneOfSymbol ["ALL", "SOME"]
    
fnfParser = do
    name <- functionNameParser
    formulae <- sepBy1 (comma lexer) formulaParser 
    return $ show (name, formulae)

ffnfparser = try $ do 
    formula1 <- formulaParser 
    name <- functionNameParser
    formula2 <- formulaParser
    return $ show (formula1, name, formula2)
    
variableParser = identifier lexer


