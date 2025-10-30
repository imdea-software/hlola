module Interpreter.LolaParsing where

-- Importing position from Parsec
import Text.Parsec.Pos
-- Importing parser combinators
import Text.Parsec
import Text.Parsec.String
-- Importing tokenizer
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as Ex
import Interpreter.AST hiding (number)
import Interpreter.Theory (ParsingInfo(..), Precedence)
import Data.Fix (Fix(..))
import Data.List
import qualified Data.Map.Strict as Map
import Prelude hiding ((<*>))

-- | Simple function to test parsers
strParse :: ParsingInfo -> String -> Spec
strParse parsinginfo str = case parse specParser "" str of
  Right s -> s
  Left err -> error (show err)
  where
  ----------------------------------------
  -- Lexer
  ----------------------------------------
  reservedOpNames = Map.keys$operators parsinginfo
  -- [
  --     "*" -- Multiplication
  --     ,"/" -- Division
  --     ,"+" -- Addition
  --     ,"-" -- Substraction
  --     ,"<" -- RelationalLT
  --     ,"<=" -- RelationalLTE
  --     ,">" -- RelationalGT
  --     ,">=" -- RelationalGTE
  --     ,"==" -- RelationalEqual
  --     ,"!=" -- RelationalNotEqual
  --     ,"&&" -- LogicalAnd
  --     ,"||" -- LogicalOr
  --   ]

  lexer :: Tok.TokenParser ()
  lexer = Tok.makeTokenParser langDef
    where
      reservedNames =
        -- Globals
        ["input","output","define"]
        ++ -- Ite
        ["if", "then", "else"]
        ++ -- Over
        ["instantiate", "over", "withInit", "updating"]

      langDef =
        Lang.emptyDef{ Tok.commentStart = "/*"
                    , Tok.commentEnd = "*/"
                    , Tok.commentLine = "//"
                    , Tok.nestedComments = False
                    -- | Here we define that identifiers being with a letter
                    , Tok.identStart = letter
                    -- | Rest of identifiers accepted characters
                    , Tok.identLetter = alphaNum <|> char '_'
                    -- | Operators begin with
                    , Tok.opStart = oneOf (operatorstarts parsinginfo)
                    , Tok.reservedNames = reservedNames
                    , Tok.reservedOpNames = reservedOpNames
                    -- | Is the language case sensitive? It should be
                    , Tok.caseSensitive = True
                    }

  -- Lexemes

  wspcs :: Parsec String () ()
  wspcs = Tok.whiteSpace lexer

  braces :: Parsec String () a -> Parsec String () a
  braces = Tok.braces lexer

  brackets :: Parsec String () a -> Parsec String () a
  brackets = Tok.brackets lexer

  parens :: Parser a -> Parser a
  parens = Tok.parens lexer

  angles :: Parser a -> Parser a
  angles = Tok.angles lexer

  comma :: Parser String
  comma = Tok.comma lexer

  semi :: Parser String
  semi = Tok.semi lexer

  stringLit :: Parser String
  stringLit = Tok.stringLiteral lexer

  charLit :: Parser Char
  charLit = Tok.charLiteral lexer

  reserved :: String -> Parsec String () ()
  reserved = Tok.reserved lexer

  reservedOp :: String -> Parsec String () ()
  reservedOp = Tok.reservedOp lexer

  identifierParser :: Parser String
  identifierParser = Tok.identifier lexer

  hexa :: Parser Integer
  hexa = char '0' >> Tok.hexadecimal lexer

  number :: Parser Int
  number = fromInteger <$> Tok.integer lexer

  ----------------------------------------
  -- Parser
  ----------------------------------------

  typeTermParser :: Parser Type
  typeTermParser = Ex.buildExpressionParser
    [[Ex.Infix spacef Ex.AssocLeft]]
    typeParser
    where
    spacef :: Parser (Type->Type->Type)
    spacef = wspcs
      >> return (\t0 t1 -> Fix (TApp t0 t1))

  literalTypeParser :: Parser Type
  literalTypeParser = do
    tyname <- identifierParser
    return (Fix (TBase tyname))

  setTypeParser :: Parser Type
  setTypeParser = do
    texpr <- braces typeParser
    return $ Fix (TApp (Fix (TBase "Set")) texpr)

  listTypeParser :: Parser Type
  listTypeParser = do
    texpr <- brackets typeParser
    return $ Fix (TApp (Fix (TBase "[]")) texpr)

  pairTypeParser :: Parser Type
  pairTypeParser = try $ parens $ do
    t0 <- typeParser
    comma
    t1 <- typeParser
    return $ Fix (TApp (Fix (TApp (Fix (TBase "(,)")) t0)) t1)

  typeParser :: Parser Type
  typeParser = literalTypeParser
    <|> setTypeParser
    <|> listTypeParser
    <|> pairTypeParser
    <|> parens typeTermParser

  inputParser :: Parser StrDec
  inputParser = do
    reserved "input"
    ty <- typeParser
    identifier <- identifierParser
    let strdec = StrDec {
      isoutput = False,
      name = identifier,
      typ = ty,
      params = [],
      body = Nothing
      }
    return strdec

  binaryInfix s f = Ex.Infix (do
    reservedOp s
    return $ \x y -> wapp (wapp f x) y)

  expressionParser :: Parser Body
  expressionParser = Ex.buildExpressionParser
    ([[nowPostfix, accessPostfix]
    ,[Ex.Infix spacef Ex.AssocLeft]
  --   ,[binaryInfix "+" suma Ex.AssocLeft]
  --   ,[binaryInfix "||" (Symbol "or") Ex.AssocLeft
  --    ,binaryInfix "&&" (Symbol "and") Ex.AssocLeft]
    ] ++ processOperators (Map.toList$operators parsinginfo))
    termParser
    where
    now x = Symbol "now" <*> x
    at strm offs dflt = Symbol "at" <*> strm <*> offs <*> dflt
    nowPostfix = Ex.Postfix (do
      reservedOp "[now]"
      return $ \parent -> now parent)
    accessParser :: Parser (Body, Body)
    accessParser = brackets (do
      offset <- expressionParser
      reservedOp "|"
      dflt <- expressionParser
      return (offset, dflt))
    accessPostfix = Ex.Postfix (do
      (offset, dflt) <- accessParser
      return $ \strm -> at strm offset dflt)
    spacef :: Parser (Body->Body->Body)
    spacef = wspcs
      *> notFollowedBy (choice . map reservedOp $ reservedOpNames)
      >> return wapp

  constExprParser :: Parser Body
  constExprParser = parseLitInt <|> parseLitSym <|> parseSet <|> parseList
    where
      parseLitInt = do
        num <- number
        return (leafsym (show num))
      parseLitSym = do
        strname <- identifierParser
        return (leafsym strname)
      parseSet = do
        elems <- braces (expressionParser `sepBy` comma)
        let ls = foldr (\e -> wapp (wapp (leafsym "listcons") e)) (leafsym "emptylist") elems
        return $ wapp (leafsym "setFromList") ls
      parseList = do
        elems <- brackets (expressionParser `sepBy` comma)
        return $ foldr (\e -> wapp (wapp (leafsym "listcons") e)) (leafsym "emptylist") elems

  iteParser :: Parser Body
  iteParser = do
    reserved "if"
    ie <- expressionParser
    reserved "then"
    te <- expressionParser
    reserved "else"
    ee <- expressionParser
    return $ wapp (wapp (wapp (leafsym "ite") ie) te) ee

  overParser :: Parser Body
  overParser = do
    reserved "instantiate"
    strm <- expressionParser
    reserved "over"
    set <- expressionParser
    return $ over strm set
    where
    over strm set = Symbol "over" <*> strm <*> set

  pairParser :: Parser Body
  pairParser = parens $ do
    p0 <- expressionParser
    comma
    p1 <- expressionParser
    return $ wapp (wapp (leafsym "mkpair") p0) p1

  funcParser :: Parser Body
  funcParser = do
    name <- identifierParser
    args <- parens (sepBy termParser comma)
    return $ foldl wapp (leafsym name) args

  termParser :: Parser Body
  termParser = 
    try overParser
    <|> try funcParser
    <|> try constExprParser
    <|> try iteParser
    <|> try pairParser
    <|> parens expressionParser

  bodyParser :: Parser Body
  bodyParser = expressionParser

  argParser :: Parser (String, Type)
  argParser = angles (do
    ty <- typeParser
    identifier <- identifierParser
    return (identifier, ty))

  checkoutput :: Parser Bool
  checkoutput =
    try (reserved "output" >> return True)
    <|> try (reserved "define" >> return False)

  outputParser :: Parser StrDec
  outputParser = do
    -- reserved "output" -- Or "define"
    isoutput <- checkoutput
    ty <- typeParser
    identifier <- identifierParser
    args <- many argParser
    reservedOp "="
    body <- bodyParser
    let strdec = StrDec {
      isoutput = isoutput,
      name = identifier,
      typ = ty,
      params = args,
      body = Just body
      }
    return strdec

  -- | Top Level parser
  topLevel :: Parser Spec
  topLevel = many1 $
    try inputParser <|> try outputParser

  specParser :: Parser Spec
  specParser = do
    spec <- topLevel
    eof
    return spec


  -- Our functions
  -- processOperators :: [(String, (String, Precedence))] -> _
  processOperators ls = let
    reols = map (\(op, (fun, pr)) -> (pr, (op,fun))) ls
    sorted = sortBy (\(a,_) (b,_) -> compare a b) reols
    grouped = groupBy (\(a,_) (b,_) -> a==b) sorted
    lists = map (map snd) grouped
    res = map (map (\(op,fun) -> binaryInfix op (leafsym fun) Ex.AssocLeft)) lists
    in res

(<*>) = Application
leafsym sym = Symbol "leaf" <*> Symbol sym
wapp x y = Symbol "app" <*> x <*> y
