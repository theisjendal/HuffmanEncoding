-- By Theis Erik Jendal, tjenda15@student.aau.dk

import Data.List

data Bit = Zero | One deriving Show
data NTree = Leaf Char Int | Branch [NTree] Int deriving (Show)

-- Must be exhaustive to insure no exceptions. Meaning all combinations of BTree. 
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

type Encoding = (Char, [Int])

---------------------------------------
-- Auxillary functions
---------------------------------------   

-- Gets the frequency of a node in a BTree.
freq :: NTree -> Int                            
freq (Leaf _ b) = b
freq (Branch _ a) = a

append :: NTree -> Int -> Int
append (Branch _ f1) f2 = f1 + f2 
append (Leaf _ f1) f2 = f1 + f2 


-- Merges two BTrees and returns the merged.
treemerge ::  [NTree] -> NTree
treemerge children = Branch children (foldr append 0 children)

-- My own lookup as every letter in the string is present in the table and Maybe is therefore not necessary. 
-- If a character for some reason is not present, in encoding then it is ignored.
lookup' :: Char -> [Encoding] -> [Int]
lookup' _ [] = []
lookup' c (e:es)  = if c == fst e then snd e else lookup' c es

---------------------------------------
-- Main functions
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

construct :: [NTree] -> Int -> Maybe NTree
construct ts n = construct' (sort ts) -- Sorts to ensure ordering.
    where
        -- Actually constructs the BTree.
        construct' :: [NTree] -> Maybe NTree
        construct' [] = Nothing
        construct' [tree] = Just tree
        construct' trees = 
            let
                merged = treemerge (take n trees)
                rest = drop n trees
            in
                construct' (insert merged rest) -- Inserts merged and calls again.

-- Make generate encodings from a BTree.
encodings :: Maybe NTree -> [Encoding]
encodings tree =
    case tree of 
        Just t -> encodings' [] t
        Nothing -> []
    where 
        -- Goes through the tree and saves a bit value dependend on left or right branch. If a leaf save the character and bit sequence.  
        encodings' :: [Int] -> NTree -> [Encoding]
        encodings' is (Leaf a _) = [(a, is)]
        encodings' is (Branch children _) =
            let
                zipped = zip [is ++ [i] | i <- [1..]] children
            in
                foldr (\(ints, child) -> (++) (encodings' ints child)) [] zipped

-- Encodes a string and return the bits and the huffman tree. 
encode :: String -> Int -> ([Int], Maybe NTree)
encode string n = 
    let
        leafs = makeleafs string      -- Gets the frequencies of a the characters.
        huffmanTree = construct leafs n     -- Construct a huffman tree given the frequencies.
        ecs = encodings huffmanTree           -- Make encodings from huffman tree.
        bitrepresentation = foldr (\c -> (++) (lookup' c ecs)) [] string -- Appends bit representation for each char in string.
    in
        (bitrepresentation, huffmanTree)                

-- Decodes a bit sequence given a BTree.
decode :: [Int] -> Maybe NTree -> String
decode _ (Just (Leaf a b)) = replicate b a      -- If only a single character encoded message, return b replicas of a.
decode _ Nothing = ""                           -- If no BTree exist return nothing.
decode ints (Just root) = decode' ints root
            where
                -- Takes a bit from the list and either take the left or right branch until a leaf appears. 
                -- Then adds the character of the leaf and either decodes the rest of the bit sequence or stops no more bits.
                decode' :: [Int] -> NTree -> String
                decode' [] (Leaf a _) = [a]
                decode' ints (Leaf a _) = a : decode' ints root
                decode' (i:ints) (Branch children _) =
                    let
                        child = children !! (i - 1)
                    in
                        decode' ints child 



