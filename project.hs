module Main where
import Data.List
import Data.Ord
import System.IO
import Data.List.Split
import Data.Maybe
import GHC.Float

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

entropy :: Int -> Int -> Float
entropy a b =
    let total = a+b
        fracA = (fromIntegral a) / (fromIntegral total) :: Float
        fracB = (fromIntegral b) / (fromIntegral total) :: Float
        ent = (-fracA*(logBase 2 fracA)-fracB*(logBase 2 fracB))
    in if isNaN ent then (double2Float 0.0) else ent

purity :: Set -> AttributeName -> Float
purity a b =
    let setData = dataset a
        setDataResultIndex = (length setData)-1
        valuesResult = getDomainValues a ((getAttributeNames a)!!setDataResultIndex)
        uniqueValuesDomain = unique domainValues
        uniqueValuesResult = unique valuesResult
        sizeResults = length valuesResult
        entropyResult = entropy (length [x | x <- valuesResult, x == uniqueValuesResult!!0]) (length [x | x <- valuesResult, x == uniqueValuesResult!!1])
        occurences = getOccurences domainValues valuesResult uniqueValuesResult uniqueValuesDomain
        entropyValues = [entropy (fst x) (snd x) | x <- occurences]
        in (entropyResult - entropyValues!!0*(((fromIntegral ((fst (occurences!!0)) + (snd (occurences!!0))))/(fromIntegral sizeResults))::Float) - entropyValues!!1*(((fromIntegral ((fst (occurences!!1)) + (snd (occurences!!1))))/(fromIntegral sizeResults))::Float) - entropyValues!!2*(((fromIntegral ((fst (occurences!!2)) + (snd (occurences!!2))))/(fromIntegral sizeResults))::Float))
    where domainValues = getDomainValues a b

getOccurences :: [DomainValue] -> [DomainValue] -> [DomainValue] -> [DomainValue] -> [(Int,Int)]
getOccurences domainValues valuesResult uniqueValuesResult [] = []
getOccurences domainValues valuesResult uniqueValuesResult (uniqueDomainValue:uniqueValuesDomain) =
    let temp = zip domainValues valuesResult
        a = length [y | y <- temp, (fst y) == uniqueDomainValue && (snd y) == uniqueValuesResult!!0]
        b = length [y | y <- temp, (fst y) == uniqueDomainValue && (snd y) == uniqueValuesResult!!1]
    in [(a, b)]++ getOccurences domainValues valuesResult uniqueValuesResult uniqueValuesDomain

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
    print set
    let attributeNames = (getAttributeNames set)
    print attributeNames
    print (getDomainValues set (attributeNames!!1))

    let ent = entropy 9 5
    print ent

    let temp = getOccurences ["overcast","overcast","overcast","overcast","rainy","rainy","rainy","rainy","rainy","sunny","sunny","sunny","sunny","sunny"] ["yes","yes","yes","yes","yes","yes","no","yes","no","no","no","no","yes","yes"] ["yes","no"] ["overcast","rainy","sunny"]
    print temp

    let arthur = purity set "Outlook"
    print arthur

    putStrLn("End of program")

