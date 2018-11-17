import Data.List

---------------------------------------
-- Types
---------------------------------------

data Bit = Zero | One deriving Show
data BTree = Leaf Char Int | Branch BTree BTree Int deriving (Show, Eq, Ord)

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

    print (fst encoded) -- Is able to print any type deriving from Show.

    putStrLn "\nPress 1 to decode previous message or anything else to exit"
    choice <- getLine

    if choice == "1" 
    then do 
        -- Decoded previous encoded message. 
        let decoded = uncurry decode encoded
        putStrLn ("\n" ++ decoded)
    else putStrLn "Exiting"

---------------------------------------
-- Auxillary funcions
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

-- A function describing how to order BTrees.
ordering :: BTree -> BTree -> Ordering
ordering t1 t2 = compare (freq t1) (freq t2)

-- My own lookup as every letter in the string is present in the table and Maybe is therefore not necessary. 
lookup' :: Char -> [Encoding] -> [Bit]
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
construct :: [BTree] -> BTree
construct ts = construct' (sortBy ordering ts) -- Sorts to ensure ordering.
    where
        -- Actually constructs the BTree.
        construct' :: [BTree] -> BTree
        construct' [tree] = tree
        construct' trees = 
            let
                left = head trees
                right = trees !! 1              -- Takes the second element of trees.
                merged = treemerge left right 
                rest = drop 2 trees             -- Removes two first elements of tree.
            in
                construct' (insertBy ordering merged rest) -- Inserts merged and calls again.

-- Make generate encodings from a BTree.
encodings :: BTree -> [Encoding]
encodings = encodings' []
    where 
        -- Goes through the tree and saves a bit value dependend on left or right branch. If a leaf save the character and bit sequence.  
        encodings' :: [Bit] -> BTree -> [Encoding]
        encodings' bs (Leaf a _) = [(a, bs)]
        encodings' bs (Branch left right _) = encodings' (bs ++ [Zero]) left ++ encodings' (bs ++ [One]) right

-- Encodes a string and return the bits and the huffman tree. 
encode :: String -> ([Bit], BTree)
encode string = 
    let
        frequencies = charFrequency string      -- Gets the frequencies of a the characters.
        huffmanTree = construct frequencies     -- Construct a huffman tree given the frequencies.
        ecs = encodings huffmanTree             -- Make encodings from huffman tree.
        bitrepresentation = foldr (\c -> (++) (lookup' c ecs)) [] string -- Appends bit representation for each char in string.
    in
        (bitrepresentation, huffmanTree) -- Note huffmanTree might be an exception, but wont interfere with the main program. 

-- Decodes a bit sequence given a BTree.
decode :: [Bit] -> BTree -> String
decode [] _ = [] -- Return nothing if empty as no BTree will exist. 
decode bits root = decode' bits root
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