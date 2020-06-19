{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

module Parser (parseScene) where

import           Common
import           Env                      
import           Object.Light             
import           Object.Material          
import           Object.Primitive         
import           Object.Scene             

import           Control.Lens             hiding (assign, lens)
import           Control.Monad.State.Lazy
import qualified Data.Map                 as M
import           Data.Void                (Void)
import           Linear
import           Text.Megaparsec          hiding (State)
import           Text.Megaparsec.Char

type Vector = V3 Double

data Memory = Memory
    { _vectors   :: M.Map String Vector
    , _materials :: M.Map String Material
    , _numbers   :: M.Map String Double
    , _shapes    :: [Shape]
    , _lights    :: [Light]
    , _env       :: Maybe Env
    }
makeLenses ''Memory

type Parser = ParsecT Void String (State Memory)

clampf :: (Ord a, Num a) => a -> a
clampf = min 1 . max 0

-- Primitives

parseNumber :: Parser Double
parseNumber = do
    sign <- optional $ char '-'
    num <- (try parseFloat <|> parseInt)
    let result = read num in return $ case sign of
        Just _  -> -result
        Nothing -> result

parseInt :: Parser String
parseInt = some digitChar

parseFloat :: Parser String
parseFloat = do
    ones <- parseInt
    char '.'
    after <- parseInt
    return $ ones ++ "." ++ after

parseVector :: Parser Vector
parseVector = do
    char '('
    a <- parseOrGetNumber
    char ',' >> space
    b <- parseOrGetNumber
    char ',' >> space
    c <- parseOrGetNumber
    char ')'
    return $ V3 a b c

parseIdentifier :: Parser String
parseIdentifier = (:) <$> lowerChar <*> (many (char '_' <|> lowerChar))

-- Materials

parseDiffuse, parseReflective, parseRefractive :: Parser MaterialType
parseDiffuse = string "Diffuse" >> return Diffuse

parseReflective = do
    string "Reflective" >> space
    v <- clampf <$> parseOrGetNumber
    return $ Reflection v

parseRefractive = do
    string "Refractive" >> space
    v1 <- parseOrGetNumber
    space
    v2 <- clampf <$> parseOrGetNumber
    return $ Refraction v1 v2

parseMatType :: Parser MaterialType
parseMatType = parseDiffuse <|> try parseReflective <|> parseRefractive

parseMaterial :: Parser Material
parseMaterial = do
    t <- parseMatType
    space
    color <- clamp <$> parseOrGetVector
    shine <- max 0 <$> (try (space >> parseOrGetNumber) <|> pure 0)
    return $ Material (Phong color shine) t

-- State getters

readDict :: _lens -> String -> Parser _
readDict lens key = lens `uses` M.lookup key >>= \case
    Nothing -> fail $ "Variable not set: " ++ key
    Just x  -> return x

getNumber :: String -> Parser Double
getNumber = readDict numbers

getVector :: String -> Parser Vector
getVector = readDict vectors

getMaterial :: String -> Parser Material
getMaterial = readDict materials

-- Parse or get

parseOrGetNumber :: Parser Double
parseOrGetNumber = parseNumber <|> (parseIdentifier >>= getNumber)

parseOrGetVector :: Parser Vector
parseOrGetVector = parseVector <|> (parseIdentifier >>= getVector)

parseOrGetMaterial :: Parser Material
parseOrGetMaterial = parseMaterial <|> (parseIdentifier >>= getMaterial)

-- Assignments

assign :: _lens -> String -> a -> Parser ()
assign lens = (lift . (lens %=)) .: M.insert

assignFloat, assignVector, assignMaterial :: String -> Parser ()
assignFloat i = assign numbers i =<< parseNumber
assignVector i = assign vectors i =<< parseVector
assignMaterial i = assign materials i =<< parseMaterial

parseAssignment :: Parser ()
parseAssignment = do
    ident <- parseIdentifier
    space >> char '=' >> space
    foldr (<|>) empty $ map ($ ident) [assignFloat, assignVector, assignMaterial]

-- Shapes and Lights

append :: _lens -> a -> Parser ()
append lens = lift . (lens %=) . (:)

parsePlane :: Parser Shape
parsePlane = do
    string "Plane" >> space
    pos <- parseOrGetVector
    space
    dir <- normalize <$> parseOrGetVector
    space
    plane pos dir <$> parseOrGetMaterial

parseSphere :: Parser Shape
parseSphere = do
    string "Sphere" >> space
    pos <- parseOrGetVector
    space
    r <- parseOrGetNumber
    space
    sphere pos r <$> parseOrGetMaterial

parseShape :: Parser ()
parseShape = (parsePlane <|> parseSphere) >>= append shapes

parseAmbient :: Parser Light
parseAmbient = do
    string "Ambient" >> space
    ambientLight <$> parseOrGetVector

parseDirectional :: Parser Light
parseDirectional = do
    string "Directional" >> space
    dir <- normalize <$> parseOrGetVector
    space
    (dirLight dir . clamp) <$> parseOrGetVector

parsePoint :: Parser Light
parsePoint = do
    string "Point" >> space
    pos <- parseOrGetVector
    space
    (pointLight pos . clamp) <$> parseOrGetVector

parseLight :: Parser ()
parseLight = (parseAmbient <|> parseDirectional <|> parsePoint) >>= append lights

-- High level
parseEnv :: Parser ()
parseEnv = do
    string "Camera" >> space
    t_eye <- parseOrGetVector
    space
    t_lookAt <- parseOrGetVector
    space
    t_up <- normalize <$> parseOrGetVector
    let e = Env 1920 1080 $ Camera t_eye t_lookAt t_up 60 in lift $ env .= Just e

parseComment, parseEmpty, parseExpression, parseFile :: Parser ()
parseComment = char '#' >> takeWhileP Nothing (/= '\n') >> pure ()
parseEmpty = pure ()
parseExpression = parseComment <|> parseAssignment <|> parseShape <|> parseLight <|> parseEnv <|> parseEmpty
parseFile = parseExpression >> many (eol >> parseExpression) >> eof >> pure ()

initialState :: Memory
initialState = Memory M.empty M.empty M.empty [] [] Nothing

parseScene :: FilePath -> IO (Maybe (Scene, Maybe Env))
parseScene file = do
    text <- readFile file
    let (result, (Memory _ _ _ sh li t_env)) = runState (runParserT parseFile "scene.txt" text) initialState
    case result of
        Left err -> putStrLn (errorBundlePretty err) >> return Nothing
        Right _  -> return $ Just (Scene sh li, t_env)
