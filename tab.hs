import Data.List.Split
import System.Environment
import System.IO
import Text.Regex (splitRegex, mkRegex)
import Data.Char

main :: IO ()
main = getArgs >>= parse


-- parse command line args
-- handles several different options:
-- -d to specify delimeter
-- -w to specify the maximum width of the columns
parse :: [String] -> IO ()

-- if no args, read from stdin and pass to tablify
parse [] = getContents >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] "," 100)
parse ["-h"] = putStrLn $ unlines ["tab utility usage:",
                                   "\ttab [option] <filename>",
                                   "\twhere options are:",
                                   "\t\t-d <regex>  -- specify the regex by which each column is separated",
                                   "\t\t-w <integer> -- specify the maximum width of the columns in the table",
                                   "\t\t\tNB. If working with bsv files and using the bar to delimit columns,",
                                   "\t\t\t\tthen remember to escape the bar in the regex following -d,",
                                   "\t\t\t\te.g. tab -d '\\|' file rather than tab -d '|' file",
                                   "\t\t\t\tthe same goes for using any other regex special char as a delimeter"
                                  ]

-- if -d specified, read from stdin and pass delimeter to tablify
parse ["-d",d] = getContents >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] d 100)
-- if only fp is specified, read that file to tablify
parse [fp] = readFile fp >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] "," 100)
-- read file and pass delimeter to tablify
parse ["-d",d,fp] = readFile fp >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] d 100)
-- read stdin and pass width to tablify
parse ["-w",w] = getContents >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] "," (read w))
-- read stdin and pass width and delimeter to tablify
parse ["-d",d,"-w",w] = getContents >>= putStrLn .
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] d (read w))
-- read stdin and pass width and delimeter to tablify
parse ["-w",w,"-d",d] = getContents >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] d (read w))
-- read file and pass width and delimeter to tablify
parse ["-d",d,"-w",w,fp] = readFile fp >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] d (read w))
-- read file and pass width to tablify
parse ["-w",w,fp] = readFile fp >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] "," (read w))
-- read file and pass width and delimeter to tablify
parse ["-w",w,"-d",d,fp] = readFile fp >>= putStrLn . 
  (\s -> tablify [line | line <- lines' s, not $ all isSpace line, line /= ""] d (read w))


-- print error message and usage
parse _ = putStrLn "incorrectly formatted arguments. Usage is:\n\ttab [option] <filename>\n\twhere options are:\n\t\t-d <string>  -- specify the string by which each column is separated (e.g. \"|\" for bsv, \",\" for csv)\n\t\t-w <integer> -- specify the maximum width of any given column\n"

lines' = splitRegex (mkRegex "\\s*\n\\s*")

-- centers the given String in a string of a given width,
-- made up of the given Char
center :: [a] -> Int -> a -> [a]
center [] width pad = replicate width pad
center s width pad =
  left ++ s' ++ right
  where s' = trunk s width
        diff = width - length s'
        leftLength = diff `div` 2
        rightLength = diff - leftLength
        left = replicate leftLength pad
        right = replicate rightLength pad

-- truncate a string to the given length if it is longer 
-- than the given length
trunk :: [a] -> Int -> [a]
trunk s n 
  | length s > n = take n s
  | otherwise = s


-- turn a list of lines into a table,
-- considering the max length and column delimeter
tablify :: [String] -> String -> Int -> String
tablify strList delim maxWidth =
  let table = map (splitRegex (mkRegex $ "\\s*" ++ delim ++ "\\s*")) $ strList
      widths = foldl
               (\acc row -> (map maximum (zipWith (\a b -> [a,length b]) acc row)))
               (replicate (length table) (-2147483648))
               table
      stdWidths = map (\row -> map (\(word, width) -> center word (min width maxWidth) ' ') $ zip row widths) table
      makeLine ln = foldr (\str acc -> '|' : str ++ acc) "|\n" ln
      lines = map makeLine stdWidths
  in "+" ++ replicate ((length $ head lines) - 3) '-' ++ "+" ++ "\n" ++ 
     head lines ++ 
     "+" ++ replicate ((length $ head lines) - 3) '-' ++ "+" ++ "\n" ++
     (foldr1 (++) $ tail lines) ++ 
     "+" ++ replicate ((length $ head lines) - 3) '-' ++ "+\n"

