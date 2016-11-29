module Main where
import Data.List
import Data.Ord
import System.IO
import Data.List.Split
import Data.Maybe
import GHC.Float
import Data.Tuple

type FileName = String
type AttributeName = String
type DomainValue = String
type TargetValue = String
type Instance = [String]

data Attribute = Attribute {attribute :: (AttributeName, [DomainValue])} deriving (Show, Eq)
data Set = DataSet {dataset :: [Attribute]} deriving (Show, Eq)
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
        occurences = getOccurences domainValues valuesResult uniqueValuesResult uniqueValuesDomain
        entropyValues = [entropy (fst x) (snd x) | x <- occurences]
    in (sum [entropyValues!!x*(((fromIntegral ((fst (occurences!!x)) + (snd (occurences!!x))))/(fromIntegral sizeResults))::Float) | x <- [0..((length occurences)-1)]])
    where domainValues = getDomainValues a b

getOccurences :: [DomainValue] -> [DomainValue] -> [DomainValue] -> [DomainValue] -> [(Int,Int)]
getOccurences domainValues valuesResult uniqueValuesResult [] = []
getOccurences domainValues valuesResult uniqueValuesResult (uniqueDomainValue:uniqueValuesDomain) =
    let temp = zip domainValues valuesResult
        a = length [y | y <- temp, (fst y) == uniqueDomainValue && (snd y) == "yes"]
        b = length [y | y <- temp, (fst y) == uniqueDomainValue && (snd y) == "no"]
    in [(a, b)]++ getOccurences domainValues valuesResult uniqueValuesResult uniqueValuesDomain

setPurity :: Set -> Float
setPurity set = 
    let attributeNames = [fst x | (Attribute x ) <- setData]
        occurences = getResultOccurences (getDomainValues set (last attributeNames))
        puritySet = entropy (fst occurences) (snd occurences)
    in puritySet
    where setData = dataset set

getResultOccurences :: [DomainValue] -> (Int,Int)
getResultOccurences domainValues =
    let uniqueValues = unique domainValues
        a = length [y | y <- domainValues, y == "yes"]
        b = length [y | y <- domainValues, y == "no"]
    in (a, b)

bestSplit :: Set -> AttributeName
bestSplit set =
    let attributeNames = [fst x | (Attribute x) <- setData]
        purityAttributes = [purity set x | x <- attributeNames]
        puritySet = setPurity set
        informationGains = [puritySet - x | x <- (init purityAttributes)]
        indexBestSplit = argmax informationGains
    in attributeNames!!indexBestSplit
    where setData = dataset set

splitSet :: Set -> [Set]
splitSet set =
    let puritySet = setPurity set
        bestAttributeName = bestSplit set
        domainValues = getDomainValues set bestAttributeName
        uniqueDomainValues = unique domainValues
    in if puritySet == 0 then [] else [createSet set bestAttributeName x | x<- uniqueDomainValues]
    where createSet :: Set -> AttributeName -> DomainValue -> Set
          createSet set attributeName domainValue =
            let domainValues = getDomainValues set attributeName
                attributes = [Attribute { attribute = ((fst x),[((snd x)!!y) | y <- [0..((length(domainValues))-1)], ((domainValues)!!y) == domainValue])} | (Attribute x) <- setData]
            in DataSet {dataset = attributes}
            where setData = dataset set

buildTree :: AttributeName -> DomainValue -> Set -> Tree
buildTree attributeName domainValue set@(setData) = 
    let bestAttribute = bestSplit set
        sets = splitSet set
        uniqueValues = unique (getDomainValues set bestAttribute)
    in if (length sets) == 0 then Node { tuple = (attributeName, domainValue), el = [Leaf{ leaf = ((unique (snd (attribute (last (dataset set)))))!!0) }]} else Node { tuple = (attributeName, domainValue), el = [buildTree bestAttribute (uniqueValues!!x) (sets!!x) | x <- [0..((length sets)-1)]]}

id3Tree :: Set -> Tree
id3Tree set =
    buildTree (bestSplit set) "" set

-- Main function type = IO().
main :: IO ()
main = do 
    set <- readCsv "data/weather.csv"
    let final = id3Tree set
    print final