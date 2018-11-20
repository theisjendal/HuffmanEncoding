-- By Theis Erik Jendal, tjenda15@student.aau.dk

import Data.List

---------------------------------------
-- Types and instances
---------------------------------------

data Bit = Zero | One deriving Show
data BTree = Leaf Char Int | Branch BTree BTree Int deriving (Show)

-- Must be exhaustive to insure no exceptions. Meaning all combinations of BTree. 
instance Eq BTree where
    (Leaf _ a) == (Leaf _ b) = a == b
    (Branch _ _ a) == (Branch _ _ b) = a == b
    (Leaf _ a) == (Branch _ _ b) = a == b
    (Branch _ _ a) == (Leaf _ b) = a == b

instance Ord BTree where
    (Leaf _ a) `compare` (Leaf _ b) = a `compare` b
    (Branch _ _ a) `compare` (Branch _ _ b) = a `compare` b
    (Leaf _ a) `compare` (Branch _ _ b) = a `compare` b
    (Branch _ _ a) `compare` (Leaf _ b) = a `compare` b

type Frequency = (Char, Int)
type Encoding = (Char, [Bit])

---------------------------------------
-- I/O handling
---------------------------------------

main = do
    putStrLn "Enter text to encode"
    string <- getLine

    -- Set to a tuple of bits and huffman tree.
    let encoded = encode string 

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

-- Adds values of a pair and returns the key of the first and their combined value.
addvalue :: Frequency -> Frequency -> Frequency
addvalue (a,b) (_,c) =  (a, b + c)

-- Gets the frequency of a node in a BTree.
freq :: BTree -> Int                            
freq (Leaf _ b) = b
freq (Branch _ _ a) = a

-- Merges two BTrees and returns the merged.
treemerge :: BTree -> BTree -> BTree
treemerge t1 t2 = Branch t1 t2 (freq t1 + freq t2) 

-- My own lookup as every letter in the string is present in the table and Maybe is therefore not necessary. 
-- If a character for some reason is not present, in encoding then it is ignored.
lookup' :: Char -> [Encoding] -> [Bit]
lookup' _ [] = []
lookup' c (e:es)  = if c == fst e then snd e else lookup' c es

---------------------------------------
-- Main functions
---------------------------------------

-- Returns a list of leaves.
charFrequency :: String -> [BTree]
charFrequency [] = []
charFrequency xs =
    let
        h = head xs
        different = [x | x <- xs, x /=  h]              -- Finds all characters that are not equal.
        leaf = Leaf h (length xs - length different)    -- The difference total length and length in different is the frequency.
    in
        leaf : charFrequency different

-- Given a list of BTrees (probably Leafs) merges two trees with lowest frequency until a single tree exist. 
construct :: [BTree] -> Maybe BTree
construct ts = construct' (sort ts) -- Sorts to ensure ordering.
    where
        -- Actually constructs the BTree.
        construct' :: [BTree] -> Maybe BTree
        construct' [] = Nothing
        construct' [tree] = Just tree
        construct' (left:right:trees) = 
            let
                merged = treemerge left right 
            in
                construct' (insert merged trees) -- Inserts merged and calls again.

-- Make generate encodings from a BTree.
encodings :: Maybe BTree -> [Encoding]
encodings tree =
    case tree of 
        Just t -> encodings' [] t
        Nothing -> []
    where 
        -- Goes through the tree and saves a bit value dependend on left or right branch. If a leaf save the character and bit sequence.  
        encodings' :: [Bit] -> BTree -> [Encoding]
        encodings' bs (Leaf a _) = [(a, bs)]
        encodings' bs (Branch left right _) = encodings' (bs ++ [Zero]) left ++ encodings' (bs ++ [One]) right

-- Encodes a string and return the bits and the huffman tree. 
encode :: String -> ([Bit], Maybe BTree)
encode string = 
    let
        frequencies = charFrequency string      -- Gets the frequencies of a the characters.
        huffmanTree = construct frequencies     -- Construct a huffman tree given the frequencies.
        ecs = encodings huffmanTree             -- Make encodings from huffman tree.
        bitrepresentation = foldr (\c -> (++) (lookup' c ecs)) [] string -- Appends bit representation for each char in string.
    in
        (bitrepresentation, huffmanTree)

-- Decodes a bit sequence given a BTree.
decode :: [Bit] -> Maybe BTree -> String
decode _ (Just (Leaf a b)) = replicate b a      -- If only a single character encoded message, return b replicas of a.
decode _ Nothing = ""                           -- If no BTree exist return nothing.
decode bits (Just root) = decode' bits root
            where
                -- Takes a bit from the list and either take the left or right branch until a leaf appears. 
                -- Then adds the character of the leaf and either decodes the rest of the bit sequence or stops no more bits.
                decode' :: [Bit] -> BTree -> String
                decode' [] (Leaf a _) = [a]
                decode' bits (Leaf a _) = a : decode' bits root
                decode' (bit:bits) (Branch left right _) =
                    case bit of
                        Zero -> decode' bits left
                        One -> decode' bits right