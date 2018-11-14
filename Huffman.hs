import Data.List
test = "Mississippi river"

data Bit = Zero | One deriving Show
data BTree = Leaf Char Int | Branch BTree BTree Int deriving Show

type Frequency = (Char, Int)

type Encoding = (Char, [Bit])

-- Convert the Bit type to int
bit2int :: Bit -> Int
bit2int One = 1
bit2int Zero = 0

-- Converts Int to Bit. Everything Int except 0 is One.
int2bit :: Int -> Bit
int2bit x = if x == 0 then Zero else One

-- Adds values of a pair and returns the key of the first and their combined value
addvalue :: Frequency -> Frequency -> Frequency
addvalue (a,b) (_,c) =  (a, b + c)

makeLeaf :: Frequency -> BTree
makeLeaf f = Leaf (fst f) (snd f)

-- Returns a list of leaves.
charFrequency :: String -> [BTree]
charFrequency [] = []
charFrequency xs =
    let
        h = head xs
        different = [x | x <- xs, x /=  h]
        leaf = Leaf h (length xs - length different)
    in
        leaf : charFrequency different

freq :: BTree -> Int                            
freq (Leaf _ b) = b
freq (Branch _ _ a) = a

treemerge :: BTree -> BTree -> BTree
treemerge t1 t2 = Branch t1 t2 (freq t1 + freq t2) 

construct :: [BTree] -> BTree
construct [tree] = tree
construct trees =
    let
        ts = sortBy (\t1 t2 -> compare (freq t1) (freq t2)) trees
        left = head ts
        lrest = tail ts
        right = head lrest
        rrest = tail lrest
        z = treemerge left right
    in
        construct (z:rrest)
  
-- My own lookup as every letter in the string is present in the table and Maybe is therefore not necessary. 
lookup' :: Char -> [Encoding] -> [Bit]
lookup' c (e:es)  = if c == fst e then snd e else lookup' c es

encodings :: BTree -> [Encoding]
encodings = encodings' []
    where 
        encodings' :: [Bit] -> BTree -> [Encoding]
        encodings' bs (Leaf a _) = [(a, bs)]
        encodings' bs (Branch left right _) = encodings' (bs ++ [Zero]) left ++ encodings' (bs ++ [One]) right

encode :: String -> [Bit]
encode string = 
    let
        frequencies = charFrequency string
        huffmanTree = construct frequencies
        ecs = encodings huffmanTree
    in
        foldr (\c -> (++) (lookup' c ecs)) [] string


 