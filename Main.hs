#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts   #-}

import System.Environment(getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO(stderr, hPutStrLn)
import System.Directory(doesFileExist)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.String

import Text.Regex.TDFA((=~))
import Data.List(isSuffixOf, isInfixOf, findIndex, findIndices,
	             elem, nub, maximumBy, groupBy, intersect, transpose, 
				 intercalate)
import Data.List.Split(splitOn, splitWhen)
import Data.Char(isLetter, isNumber, isSymbol, isSeparator, isPunctuation)
import Data.Map(fromListWith, toList)
import Data.Function(on)

import Control.Monad(when, unless)
import Data.Either(rights)

import Text.Read(readMaybe)

-- | Chapter 1. Functions for parsing the rule

data Rule = Rule [Char] [Char]
	deriving Show

parseRule :: Parser Rule
parseRule = do
	left  <- many' anyChar
	string "->"
	right <- many' anyChar
	return $ Rule left right

-- | Guess the delimiter for several known formats

knownFormats = [".csv", ".tsv", ".ssv", ".list"]

decideF :: FilePath -> Maybe Char
decideF flnm
	| ".csv"  `isSuffixOf` flnm = Just ','
	| ".tsv"  `isSuffixOf` flnm = Just '\t'
	| ".ssv"  `isSuffixOf` flnm = Just ' '
	| ".list" `isSuffixOf` flnm = Just ' '
	| ".sam"  `isSuffixOf` flnm = Just '\t'
	| ".vcf"  `isSuffixOf` flnm = Just '\t'
	| ".bed"  `isSuffixOf` flnm = Just '\t'
	| ".gff"  `isSuffixOf` flnm = Just '\t'
	| ".gtf"  `isSuffixOf` flnm = Just '\t'
	| ".gff3" `isSuffixOf` flnm = Just '\t'
	| otherwise 		   	    = Nothing

guessF :: String -> Char
guessF fstLine = mostFreqSymbol
	where
		mostFreqSymbol = fst $ maximumBy (compare `on` snd) symbolCount
		symbolCount    = filter (\(s,n) -> isSymbol s || isSeparator s || isPunctuation s) freqMap
		freqMap        = toList $ fromListWith (+) [(c, 1) | c <- fstLine]


tuplify2 :: [t] -> (t, t)
tuplify2 (a:b:_) = (a,b)

isEmpty :: Char -> Bool
isEmpty x = (x == ' ') || (x == '\t')

lstrip :: [Char] -> [Char]
lstrip []       = []
lstrip w@(x:xs) = if (isEmpty x) then (lstrip xs) else w

rstrip :: [Char] -> [Char]
rstrip [] = []
rstrip xs = if (isEmpty $ last xs) then rstrip (init xs) else xs

strip :: [Char] -> [Char]
strip = lstrip . rstrip


isFieldVar :: [Char] -> Bool
isFieldVar = all (\x -> isLetter x || isNumber x)

isFieldChar :: Char -> Bool
isFieldChar x = isLetter x || isNumber x


interDivise x = splitWhen (not . isFieldChar) x

fi :: Eq a => [a] -> a -> [Int]
fi ar el = findIndices (==el) ar

findFstInSnd :: Eq a => [a] -> [a] -> [[Int]]
findFstInSnd a b = map (fi b) a

sole :: Eq a => [a] -> [a] -> Bool
sole x xs = length (splitOn x xs) == 2

sole' :: Eq a => [a] -> [a] -> Bool
sole' x xs = length (splitOn x xs) <= 2

splitMargins :: Eq a => a -> [a] -> (Int, Int)
splitMargins x xs = (pos', length xs - pos' - 1)
	where
		Just pos'
			| pos == Nothing = Just 0
			| otherwise      = pos
		pos       = findIndex (==x) xs


-- | Check the rule correctness
-- 1. Every variable on the right must be present on the left

checkRule :: [Char] -> [Char] -> Bool
checkRule list1 list2
	| ell `isInfixOf` list1 = (all (flip elem list1') list2') 
							  && (sole  ell list1)
	| otherwise             = all (flip elem list1') list2'
		where
			ell = "..."
			list1' = splitOn " " list1
			list2' = splitOn " " list2


-- | Chapter 2. Functions for parsing the records

recordWithSep sep field = sepBy field sep

anyField sep = takeTill (==sep)

parseLine line sep = feed (parse (recordWithSep (char sep) (anyField sep)) $ B.pack line) B.empty

parseLine' line sep = y
	where Done x y = case sep of
		Nothing   -> parseLine line (guessF line)
		Just sep' -> parseLine line sep'

packEither oldDelim newDelim line = case newDelim of
	Nothing -> tryOld oldDelim line
	Just x  -> B.pack [x]

tryOld oldDelim line = case oldDelim of 
	Nothing -> B.pack [guessF line]
	Just y  -> B.pack [y]

maybeEnumerate enumBool oldDelim newDelim
	| enumBool  = zipWith (++) (map (\x -> show x ++ [delim]) [1..])
	| otherwise = id
		where
			Just old' = oldDelim
			Just new' = newDelim
			delim
				| oldDelim == Nothing && newDelim == Nothing = error "Provide a new delimiter to enumerate lines."
				| newDelim == Nothing = old'
				| otherwise = new'

-- | Chapter 3. Parse options and flags

data Options = Options  {
	optVersion  :: Bool,
	optHelp     :: Bool,

	optVerbose  :: Bool,
	optDelim    :: Maybe Char,
	optNewDelim :: Maybe Char,

	optSkip   :: Maybe Int,
	optOmit   :: Maybe Int,
	optEnum   :: Bool,
	optColnum :: Bool
	}

-- Handy if only version or help is needed
defaultOptions' = defaultOptions "example.ssv"

defaultOptions :: FilePath -> Options
defaultOptions filename = Options {
	optVersion  = False,
	optHelp     = False,

	optVerbose  = False,
	optDelim    = decideF filename,
	optNewDelim = decideF filename,

	optSkip   = Just 0,
	optOmit   = Just 0,
	optEnum   = False,
	optColnum = False
}

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['V'] ["version"]  (NoArg showVersion)  	   	"Show version number",
			Option ['h'] ["help"]     (NoArg showHelp)  	   	"Show help message",
			Option ['v'] ["verbose"]  (NoArg setVerbose)  	   	"Enable verbose messages",
			Option ['f'] ["delim"]    (ReqArg setDelim " ")     "Define delimiter in the input file",
			Option ['g'] ["newdelim"] (ReqArg setNewDelim " ")  "Define delimiter in the output file",
			Option ['s'] ["skip"] 	  (ReqArg setSkip "0")     	"Set number of lines to skip",
			Option ['t'] ["omit"] 	  (ReqArg setOmit "0")     	"Set number of lines to omit in the end of the file",
			Option ['n'] ["enum"] 	  (NoArg setEnum)     		"Treat the line number as the first column of the input",
			Option ['a'] ["colnum"]   (NoArg setColnum)   		"Access the columns by their number"
		  ]

setDelim    arg opt = return opt { optDelim = if length (readLiteral arg) > 0 
												then Just (readLiteral arg !! 0) 
												else error "The delimiter should have non-zero length"
								 }

setNewDelim arg opt = return opt { optNewDelim = if length (readLiteral arg) > 0 
													then Just (readLiteral arg !! 0) 
													else error "The new delimiter should have non-zero length" }


setSkip arg opt = return opt { optSkip = let argParsed = readMaybe arg 
											in if (argParsed == Nothing)
												   then error "The number of lines to skip should be an integer" 
												   else argParsed
							 }

setOmit arg opt = return opt { optOmit = let argParsed = readMaybe arg 
											in if (argParsed == Nothing)
												   then error "The number of lines to omit should be an integer" 
												   else argParsed
							 }

setEnum opt = return opt { optEnum = True }

setColnum opt = return opt { optColnum = True }

setVerbose opt = return opt { optVerbose = True }

elem' :: Eq a => [a] -> a -> Bool
elem' = flip elem

readLiteral :: [Char] -> String
readLiteral s = read ('"' : s ++ "\"") :: String

parseArgs argv = case getOpt Permute options argv of
 
		(args,fs,[]) -> do
			let files = if null fs then ["-"] else fs
			return (args, files)
 
		(_,_,errs)	 -> do
			hPutStrLn stderr (concat errs ++ usageInfo header options)
			exitWith (ExitFailure 1)
 
		where header = "Usage: rei [options] rule file"

showVersion _ = do
	hPutStrLn stderr "rei: process lists easily. Version 0.4.0 (alpha). November 2016."
	exitWith ExitSuccess

showHelp _    = do
	let header = "\n\trei is designed to process lists easily\n"
	let magich = "\n  Magic rules:\n"
	let magic  = "  merge, unite, join, subtract, transpose, filter, reduce, distinct\n"
	hPutStrLn stderr (usageInfo (header ++ magich ++ magic) options)
	exitWith ExitSuccess



-- | Chapter 4. Working with the file

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (\t -> unless t s)



-- | Chapter 5. Magic functions

condense :: Eq a => [[a]] -> [[a]]
condense input = zipWith (:) keys values
	where
		grouped = groupBy ((==) `on` fst) . map tuplify2 $ input
		keys    = map (fst . (!! 0)) grouped
		values  = map (map snd) grouped

condense2 :: Eq a => [[a]] -> [[a]]
condense2 input = zipWith (++) keys values
	where 
		grouped = groupBy ((==) `on` (Prelude.take 2)) input
		keys    = map (Prelude.take 2 . (!! 0)) grouped
		values  = map (map (!! 2)) grouped

condense3 :: Eq a => [[a]] -> [[a]]
condense3 input = zipWith (++) keys values
	where 
		grouped = groupBy ((==) `on` (Prelude.take 3)) input
		keys    = map (Prelude.take 3 . (!! 0)) grouped
		values  = map (map (!! 3)) grouped

melter :: [t] -> [[t]]
melter x = case x of
	[]        -> []
	(a:b:[])  -> [ [a,b] ]
	(a:b:xs)  -> ( [a,b]:(melter (a:xs)) )
	otherwise -> error "There might be not enough columns to melt the table"

melt :: [[t]] -> [[t]]
melt input = foldr (++) [] . map melter $ input

melter2 :: [t] -> [[t]]
melter2 x = case x of
	[]         -> []
	(a:b:c:[]) -> [ [a,b,c] ]
	(a:b:c:xs) -> ( [a,b,c]:(melter2 (a:b:xs)) )
	otherwise  -> error "Not enough columns to melt the table. You may want to use `melt`"


melt2 :: [[t]] -> [[t]]
melt2 input = foldr (++) [] . map melter2 $ input

melter3 :: [t] -> [[t]]
melter3 x = case x of
	[]           -> []
	(a:b:c:d:[]) -> [ [a,b,c,d] ]
	(a:b:c:d:xs) -> ( [a,b,c,d]:(melter3 (a:b:c:xs)) )
	otherwise    -> error "Not enough columns to melt the table. You may want to use `melt2`"


melt3 :: [[t]] -> [[t]]
melt3 input = foldr (++) [] . map melter3 $ input

mzip :: [[a]] -> [[a]] -> [[a]]
mzip a b = map (\(x,y) -> x++y) ( zip a b )

subtrList :: Eq a => [a] -> [a] -> [a]
subtrList xs ys = filter (\x -> notElem x ys) xs

filterUnique :: (Eq a) => Int -> [[a]] -> [[a]]
filterUnique n xs = foldl (\xs x -> if xs == []
	then [x]
	else if (x!!n)/=(xs!!0!!n) then x:xs else xs) [] xs

slice :: [Int] -> [a] -> [a]
slice is xs = map (xs !!) is

filterUniqueMulti :: (Eq a) => [Int] -> [[a]] -> [[a]]
filterUniqueMulti ns xs = foldl(\xs x -> if xs == []
	then [x]
	else if (slice ns x)/=(slice ns (xs !! 0)) then x:xs else xs) [] xs 

toInt :: String -> Int
toInt x = read x :: Int

main = do
	(opts, args) <- getArgs >>= parseArgs
	-- putStrLn $ "Flags: " ++ show opts

	-- Check several special options: help and version
	optsSp <- foldl (>>=) (return defaultOptions') opts
	let Options {
			  optVersion = versionOnly,
			  optHelp    = helpOnly     } = optsSp

	when helpOnly    $ showHelp ""
	when versionOnly $ showVersion ""

	-- Check the rule
	let (rule:files) = args
	let filename
		| rule == "-"        = error "Provide a rule and a filename\n--  Example: `rei \"a b -> b a\" example.ssv`"
		| rule == "filter"   = files !! 1
		| rule == "reduce"   = files !! 1
		| rule == "distinct" = files !! 1
		| length files > 0   = files !! 0
		| otherwise          = error "Provide a filename\n--  Example: `rei \"a b -> b a\" example.ssv`"

	-- Parse options
	opts' <- foldl (>>=) (return $ defaultOptions filename) opts
	let Options { 
			  optVerbose  = verbose
			, optVersion  = versionOnly
			, optDelim    = fieldSep
			, optNewDelim = fieldSep'
			, optSkip     = linesToSkip
			, optOmit     = linesToOmit
			, optEnum     = ifEnumerate
			, optColnum   = ifColnum } = opts'

	-- Check if the delimiter provided if the file format is unknown
	if fieldSep == Nothing 
		then hPutStrLn stderr "Warning: the delimiter is not provided and can not be deduced from the file extension."
		else return ()

	-- Parse the rule
	let pat = "(.*[^\\])->(.*)" :: String

	-- Definitions for magic functions
	let finalDelim
		| fieldSep' /= Nothing = B.pack [d2]
		| fieldSep /= Nothing  = B.pack [d1]
		| otherwise            = B.pack " "
			where
				Just d1 = fieldSep
				Just d2 = fieldSep'
	let open x = if x == "-" then getContents else readFile x
	let drop' (Just n) = drop n
	let take' m xs
		| m == Just 0 = id xs
		| otherwise   = Prelude.take (length xs - m') xs
			where
				Just m' = m
	let getParsed x = parseLine' x fieldSep

	let rule' = strip rule

	-- Create a union of several files, column-wise
	when (rule' == "merge") $ do
		when verbose $ hPutStrLn stderr "Merging files..."
		let several_files
			| length files > 1 = files
			| otherwise        = error "Provide at least two files to merge\n--  Example: `rei merge f1.ssv f2.ssv`"
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		let parseActions = map getParsed . take' linesToOmit . drop' linesToSkip . lines
		filesContents <- mapM readFile several_files
		mapM_ print' $ foldl1 mzip ( map parseActions filesContents )
		exitWith ExitSuccess

	-- Condense considering first two/three/four columns of the file as keys
	when (rule' == "condense" || rule' == "condense2" || rule' == "condense3") $ do
		when verbose $ hPutStrLn stderr "Condensing a file..."
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		mapM_ print' 
			. (if rule' == "condense3" then condense3 else if rule' == "condense2" then condense2 else condense)
			. map getParsed . take' linesToOmit . drop' linesToSkip . lines =<< open filename
		exitWith ExitSuccess

	-- Melt the file into two/three/four-column file
	when (rule' == "melt" || rule' == "melt2" || rule' == "melt3") $ do
		when verbose $ hPutStrLn stderr "Melting a file..."
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		mapM_ print' 
			. (if rule' == "melt3" then melt3 else if rule' == "melt2" then melt2 else melt)
			. map getParsed . take' linesToOmit . drop' linesToSkip . lines =<< open filename
		exitWith ExitSuccess

	-- Concatenate files, row-wise
	when (rule' == "unite" || rule' == "concatenate" || rule' == "concat") $ do
		when verbose $ hPutStrLn stderr "Uniting files..."
		let several_files
			| length files > 1 = files
			| otherwise        = error "Provide at least two files to unite\n--  Example: `rei unite f1.ssv f2.ssv`"
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		let parseActions = map getParsed . take' linesToOmit . drop' linesToSkip . lines
		filesContents <- mapM readFile several_files
		mapM_ print' $ concatMap parseActions filesContents
		exitWith ExitSuccess

	-- Find common rows for several files.
	when (rule' == "join" || rule' == "intersect") $ do
		when verbose $ hPutStrLn stderr "Joining files..."
		let several_files
			| length files > 1 = files
			| otherwise        = error "Provide at least two files to join\n--  Example: `rei join f1.ssv f2.ssv`"
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		let parseActions = map getParsed . take' linesToOmit . drop' linesToSkip . lines
		filesContents <- mapM readFile several_files
		let joined = foldl1 intersect ( map parseActions filesContents )
		when verbose $ hPutStrLn stderr ("There are " ++ 
										 show (length joined) ++ 
										 " common rows.")
		mapM_ print' $ joined
		exitWith ExitSuccess

	-- Find common rows for several files.
	when (rule' == "subtract" || rule' == "subtr") $ do
		when verbose $ hPutStrLn stderr "Subtracting files..."
		let several_files
			| length files > 1 = files
			| otherwise        = error "Provide at least two files to subtract\n--  Example: `rei subtr f1.ssv f2.ssv`"
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		let parseActions = map getParsed . take' linesToOmit . drop' linesToSkip . lines
		filesContents <- mapM readFile several_files
		let elemsUniq = foldl1 subtrList ( map parseActions filesContents )
		when verbose $ hPutStrLn stderr ("There are " ++ 
										 show (length elemsUniq) ++ 
										 " common rows.")
		mapM_ print' $ elemsUniq
		exitWith ExitSuccess

	-- Transpose a list.
	when (rule' == "transpose") $ do
		when verbose $ hPutStrLn stderr "Transposing a file..."
		let print' x = B.putStrLn $ B.intercalate finalDelim $ x
		mapM_ print' . transpose . map getParsed . take' linesToOmit . drop' linesToSkip . lines =<< open filename
		exitWith ExitSuccess

	-- Filter a list.
	when (rule' == "filter" || rule' == "reduce") $ do
		when verbose $ hPutStrLn stderr "Filtering a file..."
		let (pattern:several_files)
			| length files > 1 = files
			| otherwise        = error "Provide a filtering pattern and a file\n -- Example: `rei filter 'chr source type => type ~ gene' f1.bed`"
		let single_file    = several_files !! 0
		let regex_filter   = "(.*[^\\])=>(.*)~(.*)"         :: String      -- Filtering pattern
		let pattern_parts  = (pattern =~ regex_filter)      :: [[String]]
		when (length pattern_parts < 1 || length (pattern_parts !! 0) /= 4) $ error "Something's wrong with the filtering pattern. The pattern should consist of a set of column descriptors, the fat arrow, the target field name, and a regular expression.\n-- Example: `rei filter 'chr source type => type ~ gene' f1.bed`"
		let pattern_parts' = pattern_parts !! 0
		when (not $ checkRule (pattern_parts' !! 1) (pattern_parts' !! 2)) $ error "Something's wrong with the filtering pattern. There might be a variable which is unique to the right part."
		let pattern_regex  = strip $ pattern_parts' !! 3
		let pattern_field  = findFstInSnd (words $ pattern_parts' !! 2) (words $ pattern_parts' !! 1) !! 0 !! 0
		let print'' x = B.putStrLn $ B.intercalate finalDelim $ x
		let parseActions = map getParsed . take' linesToOmit . drop' linesToSkip . lines
		fileContent <- if single_file == "-" then getContents else readFile single_file
		let filtered
			| rule' == "filter" = filter (\x -> (x !! pattern_field =~ pattern_regex :: Bool)) (parseActions fileContent)
			| rule' == "reduce" = filter (\x -> not (x !! pattern_field =~ pattern_regex :: Bool)) (parseActions fileContent)
		when verbose $ hPutStrLn stderr ("There are " ++ 
										 show (length filtered) ++ 
										 " filtered rows.")
		mapM_ print'' $ filtered
		exitWith ExitSuccess

	-- Get unique records by field.
	when (rule' == "distinct") $ do
		when verbose $ hPutStrLn stderr "Getting distinct records..."
		let (pattern:several_files)
			| length files > 1 = files
			| otherwise        = error "Provide a pattern to select distinct lines and a file\n -- Example: `rei distinct 'chr source type => type' f1.bed`"
		let single_file    = several_files !! 0
		let regex_filter   = "(.*[^\\])=>(.*)"         :: String      -- Pattern
		let pattern_parts  = (pattern =~ regex_filter) :: [[String]]
		when (length pattern_parts < 1 || length (pattern_parts !! 0) /= 3) $ error "Something's wrong with the filtering pattern. The pattern should consist of a set of column descriptors, the fat arrow, the target field name, and a regular expression.\n-- Example: `rei distinct 'chr source type => chr' f1.bed`"
		let pattern_parts' = pattern_parts !! 0
		when (not $ checkRule (pattern_parts' !! 1) (pattern_parts' !! 2)) $ error "Something's wrong with the filtering pattern. There might be a variable which is unique to the right part."
		let pattern_fields  = concat $ findFstInSnd (words $ pattern_parts' !! 2) (words $ pattern_parts' !! 1)
		let print'' x = B.putStrLn $ B.intercalate finalDelim $ x
		let parseActions = map getParsed . take' linesToOmit . drop' linesToSkip . lines
		fileContent <- if single_file == "-" then getContents else readFile single_file
		let unique = reverse $ filterUniqueMulti pattern_fields (parseActions fileContent)
		when verbose $ hPutStrLn stderr ("There are " ++ 
										 show (length unique) ++ 
										 " distinct rows.")
		mapM_ print'' $ unique
		exitWith ExitSuccess

	let (before, after) 
		| matchResult == [] = error "Something's wrong with the rule. The rule must have two sets of column descriptors separated by the arrow.\n-- Example: `rei \"a b -> b a\" example.ssv`"
		| otherwise         = tuplify2 . map strip . drop 1 $ matchResult !! 0
			where 
				matchResult = (rule =~ pat :: [[String]])
	-- print $ splitOn " " rule
	-- print $ strip before
	-- print $ strip after
	let ell = "..."

	let afterInt = map (toInt . strip) $ splitOn " " after

	let order
		| ifColnum && length before == 0      = afterInt
		| checkRule before after              = concat $ findFstInSnd (words after) (words before)
		| sole' ell before && sole' ell after = error "Something's wrong with the rule. There might be a variable which is unique to the right part."
		| otherwise = error "Something's wrong with the rule. There might be multiple ellipsis signs."


	let correctOrder ns xs = ifEl
		where
			(prex, sufx) = splitMargins ell (words before)
			ifEl
				| rightEllipsis  = concat $ map (proceed shift) ns
				| otherwise      = map (proceedSkip prex) ns 
			proceed s x     = if x > prex then [x+s] 
								else if x == prex then [ x .. x + shift ]
									else [x]
			shift           = lxs - prex - sufx - 1
			proceedSkip a x = if x <= a then x else x + shift
			lxs             = length xs
			rightEllipsis   = ell `isInfixOf` after

	-- Parse the records
	let 
		print' x
			| length order == 0 = error "The right part is empty. That doesn't make sense..."
			| maximum order >= length (getParsed x) = error "Too many fields requested. There might be extra fields in the rule, or the wrong delimiter might have been used."
			| otherwise = B.putStrLn $ B.intercalate (packEither fieldSep fieldSep' x) $ getFields x

		getFields x
			| ell_pos == Nothing = map (pre_x !!) order
			| otherwise          = map (pre_x !!) $ correctOrder order pre_x
				where
					ell_pos       = findIndex (=="...") (words before)
					pre_x         = getParsed x


	-- Read the file and apply the rule
	let withFile s = mapM_ print' . maybeEnumerate ifEnumerate fieldSep fieldSep'
								  . take' linesToOmit
								  . drop' linesToSkip
								  . lines =<< open s
		where
			open x = if x == "-" then getContents else readFile x
			drop' (Just n) = drop n
			take' m xs
				| m == Just 0 = id xs
				| otherwise   = Prelude.take (length xs - m') xs
					where
						Just m' = m

	withFile filename

	-- Verbose messages

	when verbose $ hPutStrLn stderr (case fieldSep of
				Nothing    -> "Delimiter of the input file is not defined."
				Just delim -> "-- Delimiter of the input file: \'" ++ [delim] ++ "\'.")

	when verbose $ hPutStrLn stderr (case fieldSep' of
				Nothing    -> "-- Delimiter of the output file is not defined."
				Just delim -> "-- Delimiter of the output file: \'" ++ [delim] ++ "\'.")


	when verbose $ hPutStrLn stderr (case linesToSkip of
				Just 0 -> "-- No lines skipped in the beginning of the file."
				Just x -> "-- Lines skipped in the beginning: \'" ++ show linesToSkip ++ "\'.")

	when verbose $ hPutStrLn stderr (case linesToOmit of
				Just 0 -> "-- No lines skipped in the end of the file."
				Just x -> "-- Lines omitted in the end: \'" ++ show linesToOmit ++ "\'.")

	
	when verbose $ hPutStrLn stderr (case ifEnumerate of
				True  -> "-- Line enumeration is enabled."
				False -> "-- Lines are not enumerated." )


	when verbose $ hPutStrLn stderr (case ifColnum of
				True  -> "-- Accessing columns by their number is enabled."
				False -> "-- Accessing columns by user-defined names." )
