-- By Theis Erik Jendal, tjenda15@student.aau.dk

import Data.List

---------------------------------------
-- Types and instances
---------------------------------------

-- Synonym
type Encoding = (Char, [Int])

-- Datatype
data NTree = Leaf Char Int | Branch [NTree] Int deriving (Show)

-- Must be exhaustive to insure no exceptions. Meaning all combinations of NTree. 
instance Eq NTree where
    (Leaf _ a) == (Leaf _ b) = a == b
    (Branch _ a) == (Branch _ b) = a == b
    (Leaf _ a) == (Branch _ b) = a == b
    (Branch _ a) == (Leaf _ b) = a == b

instance Ord NTree where
    (Leaf _ a) `compare` (Leaf _ b) = a `compare` b
    (Branch _ a) `compare` (Branch _ b) = a `compare` b
    (Leaf _ a) `compare` (Branch _ b) = a `compare` b
    (Branch _ a) `compare` (Leaf _ b) = a `compare` b


---------------------------------------
-- I/O handling
---------------------------------------

main = do
    putStrLn "Enter text to encode"
    string <- getLine

    putStrLn "\nEnter encode alphabet size. Must be greater than 1."
    
    -- Read a number. Makes exeption in not a number. 
    n <- do 
        num <- getLine
        return (read num)

    -- Ensure that the alphabet size is greater than 1. 
    if n <= 1
        then
            putStrLn "Invalid alphabet size"
        else
            do
                -- Set to a tuple of int and huffman tree.
                let encoded = encode string (n :: Int)

                print (fst encoded) -- Is able to print any type deriving from Show. Just putStrLn with show function. 

                putStrLn "\nPress 1 to decode previous message or anything else to exit"
                choice <- getLine

                if choice == "1" 
                    then do 
                        -- Decoded previous encoded message. 
                        let decoded = uncurry decode encoded
                        putStrLn ("\n" ++ decoded)
                    else return ()
                
    putStrLn "\nThe program is now done. Exiting.."


---------------------------------------
-- Auxillary functions
---------------------------------------   

-- Gets the frequency of a node in a NTree.
freq :: NTree -> Int                            
freq (Leaf _ b) = b
freq (Branch _ a) = a

-- Add frequency of a tree with a number. Used in treemerge.
add :: NTree -> Int -> Int
add (Branch _ f1) f2 = f1 + f2 
add (Leaf _ f1) f2 = f1 + f2 

-- Merges two NTrees and returns the merged.
treemerge ::  [NTree] -> NTree
treemerge children = Branch children (foldr add 0 children)

-- My own lookup as every letter in the string is present in the table and Maybe is therefore not necessary. 
-- If a character for some reason is not present, in encoding then it is ignored.
lookup' :: Char -> [Encoding] -> [Int]
lookup' _ [] = []
lookup' c (e:es)  = if c == fst e then snd e else lookup' c es

---------------------------------------
-- Processing functions
---------------------------------------

-- Returns a list of leaves.
makeleafs :: String -> [NTree]
makeleafs [] = []
makeleafs xs =
    let
        h = head xs
        different = [x | x <- xs, x /=  h]              -- Finds all characters that are not equal.
        leaf = Leaf h (length xs - length different)    -- The difference total length and length in different is the frequency.
    in
        leaf : makeleafs different

-- Given a list of NTrees merges N trees with lowest frequency until a single tree exist. 
construct :: [NTree] -> Int -> Maybe NTree
construct ts n = construct' (sort ts) -- Sorts to ensure ordering.
    where
        -- Actually constructs the NTree.
        construct' :: [NTree] -> Maybe NTree
        construct' [] = Nothing
        construct' [tree] = Just tree
        construct' trees = 
            let
                merged = treemerge (take n trees)
                rest = drop n trees
            in
                construct' (insert merged rest) -- Inserts merged and calls again.

-- Make generate encodings from a NTree.
encodings :: Maybe NTree -> [Encoding]
encodings tree =
    case tree of 
        Just t -> encodings' [] t
        Nothing -> []
    where 
        -- Calculates the int sequence for each character.  
        encodings' :: [Int] -> NTree -> [Encoding]
        encodings' is (Leaf a _) = [(a, is)]
        encodings' is (Branch children _) =
            let
                zipped = zip [is ++ [i] | i <- [0..]] children -- Pairs a child with a sequence of integers, representing the order from left to right. 
            in
                foldr (\(ints, child) -> (++) (encodings' ints child)) [] zipped -- Calls encoding' on all pairs and appends the result. 

---------------------------------------
-- Central functions
---------------------------------------

-- Encodes a string and return the int sequence and the huffman tree. 
encode :: String -> Int -> ([Int], Maybe NTree)
encode string n = 
    let
        leafs = makeleafs string            -- Gets the frequencies of a the characters.
        huffmanTree = construct leafs n     -- Construct a huffman tree given the frequencies.
        ecs = encodings huffmanTree         -- Make encodings from huffman tree.
        intrepresentation = foldr (\c -> (++) (lookup' c ecs)) [] string -- Appends int representation for each char in string.
    in
        (intrepresentation, huffmanTree)                

-- Decodes a int sequence given a NTree.
decode :: [Int] -> Maybe NTree -> String
decode _ (Just (Leaf a b)) = replicate b a      -- If only a single character encoded message, return b replicas of a.
decode _ Nothing = ""                           -- If no NTree exist return nothing.
decode ints (Just root) = decode' ints root
            where
                -- Takes a Int from the list and decodes a branch until a leaf appears. 
                -- Then adds the character of the leaf and either decodes the rest of the int sequence or stops no more int in the list.
                decode' :: [Int] -> NTree -> String
                decode' [] (Leaf a _) = [a]
                decode' ints (Leaf a _) = a : decode' ints root
                decode' (i:ints) (Branch children _) = decode' ints (children !! i) -- Calls decode' on the i'th child
