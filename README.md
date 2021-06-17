# tabulator-utility

A command line tool to turn csv, bsv, or any other \*sv files into a table made up of ascii characters.
It utilizes Haskell's Text.Regex and Data.Char libraries to match and split lines on the specified
delimeters and to clean out empty lines from input.

## Usage
````
$ tab [option]... FILE
````
Options include:
- -d <regex> specify a regular expression on which to split columns
- -w <int>   specify a maximum column with
- -h         get usage information
e.g. 
````
$ tab -d '\|' -w 10 file
````
or
````
$ cat file | tab -w 30
````
