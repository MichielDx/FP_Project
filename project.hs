module Main where
import Data.List
import Data.Ord
import System.IO
import Data.List.Split
import Data.Maybe

type FileName = String
type AttributeName = String
type DomainValue = String
type TargetValue = String
type Instance = [String]

data Attribute = Attribute (AttributeName, [DomainValue]) deriving (Show)
data Set = DataSet {dataset :: [Attribute]} deriving (Show)
data Tree = Leaf {leaf :: TargetValue} | Node { tuple :: (AttributeName, DomainValue), el :: [Tree]} deriving (Show)

unique :: (Ord a) => [a] -> [a]
unique = foldl (\y x -> if x `elem` y
    then y
    else y ++ [x]) []

argmax :: (Ord a) => [a] -> Int
argmax xs = snd(maximumBy (comparing fst) (zip xs [0..]))

getAttributeNames :: Set -> [AttributeName]
getAttributeNames x = unique [ fst y | (Attribute y) <- (dataset x)]

getDomainValues :: Set -> AttributeName -> [DomainValue]
getDomainValues x y = [snd z | (Attribute z) <- (dataset x), (fst z) == y]!!0

readCsv :: FileName-> IO Set
readCsv x = do
    handle <- openFile x ReadMode
    contents <- hGetContents handle
    return (parseCsv contents)

parseCsv :: String -> Set
parseCsv x =
    let rows = lines x
        headers = splitOn "," (rows!!0)
        instances = [createInstance y | y <- (drop 1 rows)]
        attributes = createAttributes headers instances
    in DataSet{dataset = attributes}

createInstance :: String -> Instance
createInstance x =
    let temp = (splitOn "," x)
    in temp

createAttributes :: [String] -> [Instance] ->  [Attribute]
createAttributes x y =
    
    let tuples = [(a,[(b!!(fromJust $ (elemIndex a x))) | b <- y]) | a <- x]
        attributes = [ Attribute a | a <- tuples]
    in attributes

-- Main function type = IO().
main :: IO ()
main = do
    putStrLn("Begin of program")
    
    --let att = Node {tuple = ("lol","lel"), el = [Leaf {leaf = "lol"}]}
    --print att

    let test = "abacdbbdcc"
    print (unique test)

    let test = argmax [2,5,6,9,10,72]
    print test

    let x = DataSet{dataset = [Attribute ("windy",["jep","nep"]), Attribute ("overcast",["nep"])]}
    print (getAttributeNames x)

    let a = getDomainValues x "windy"
    print a

    let temp = (readCsv "data/weather.csv")
    set <- temp
    let attributeNames = (getAttributeNames set)
    print attributeNames
    print (getDomainValues set (attributeNames!!1))

    putStrLn("End of program")

