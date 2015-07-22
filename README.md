# Process lists easily with `rei`

While originally *rei* was not intended to be an abbreviation, one may think of it as of the *Row Editing Interface*. Working with lists is an important part of many people, including data-scientists and bioinformaticians, and `rei` aims to make that experience more pleasant.

## Installation

`rei` can be installed from `hackage` using `cabal`:

```
cabal update
cabal install rei

# $PATH should contain ~/.cabal/bin directory
# (example for bash):
PATH=$PATH:~/.cabal/bin
export PATH
```

## Getting started

```
rei "rname x y -> rname y x" example.csv
rei unite   example_left.csv example_right.csv
rei melt2  example_condensed.csv
rei condense2 example_melted.csv
```

### Defining the rule 

The main idea of the `rei` is to apply the **rule** over the lines of the file. The rule should consist of two parts — *before* and *after* — separated by the arrow sign («->»). The *before* part of the rule describes the fields (columns) in one record (line) in the initial file. The *after* part of the rule describes the desired format of the output.

The arrow sign should be surrounded with spaces. Like this: `.. -> ..`. The fields in the rule should be surrounded with spaces too. The field delimiter in the output file is the same as in the input file by default, however it's possible to change it via the option `-g`, or `--newdelim`. (It's easy to remember, since `-f` is the flag to set the delimiter in the input file.)

### Providing the file

There's several ways to provide `rei` with the content of the file. The first one is the-most-obvious-way-you-can-think-of: just provide the path to the file. Sometimes it is helpful to use process substitution (? TODO). And if there's a need to pipe the content, just write a dash («-»). Well, here's the code:

```sh
> rei "x -> x" 0.ssv
...
> rei "x -> x" <(cat 0.ssv)
...
> cat 0.ssv | rei "x -> x" -
...
```

### Simple examples

Let's use a small sample file with spaces as delimiters for these examples (saved as `0.ssv`):

```
A B C
D E F
G H I
J K L
M N O
P Q R
S T U
V W X
Y Z 0
```

This is how easily we can address the columns:

```sh
> rei "a b c -> c b a" 0.ssv
C B A
F E D
I H G
L K J
O N M
R Q P
U T S
X W V
0 Z Y
```

The columns are now in the reversed order.

We can extract the columns that we need:

```sh
> rei "a b c -> b" 0.ssv
B
E
H
K
N
Q
T
W
Z
```

It is possible to define only columns needed:

```sh
> rei "a b -> a b" 0.ssv
A B
D E
G H
J K
M N
P Q
S T
V W
Y Z
```

You may want to keep the rest of the columns, here's how you can accomplish that:

```sh
> rei "a b ... -> a ..."
A C
D F
G I
J L
M O
P R
S U
V X
Y 0
```

The beauty is that one may give columns descriptive titles. And that is great in so many ways, as it increases readability, productivity, descriptiveness, maintainability and awareness of what's happening with all that list processing. See some real-world examples below.

### Keywords

You can define a delimiter (`-f`, or `--delim`, for the input file and  `-g`, or `--newdelim`, for the output file). It's important to emphasize that only one-character long delimiters are used. Tabulation («\t») is considered one-character too. If multicharacter literal is provided, `rei` uses its first symbol as a delimiter.

For some common file formats `rei` doesn't require a delimiter to be provided individually:

* .ssv &rarr; space (' '),
* .csv &rarr; comma (','),
* .tsv &rarr; tab ('\t'),
* .txt &rarr; space (' ').

TODO

The flag `-g` is powerful as it allows for fast format conversion. That's how `rei` may be used to convert from .ssv to .csv:

```sh
> rei -g ","  "... -> ..." 0.ssv
A,B,C
D,E,F
G,H,I
J,K,L
M,N,O
P,Q,R
S,T,U
V,W,X
Y,Z,0
```

As you see, `rei` guessed the delimiter in the input file by its extension — *space-separated values*. The output won't change in the example above if we provide `-f " "`.


TODO

There's also flags to define the number of lines to skip in the beginning (`--skip`, or `-s`) or in the end (`--omit`, or `-t`) of the file.

TODO

### Skipping lines

Sometimes there is a need to cut out the header of the file or several lines in its end. It's generally accomplished by combining `head` and/or `tail` programs, piping, etc. Since `rei` is designed for easy list processing, such feature is implemented here.



### Magic rules


There's are some common tasks that one may want to do with lists and tables, and it seems convenient to include them in `rei`: melt2, condense2, unite. Each *magic rule* has its own syntax.

TODO

#### **Melt**ing and **condens**ing

TODO



#### **Unit**ing

TODO

### Outputting to file

The default behaviour for `rei` is to give the output to `stdout`. That allows `rei` to be easily embedded  into workflows and pipelines. 

However there's a common case when files are given suffixes or prefixes while mining the data, like `brain_data.csv` and `brain_data_adults_only.csv`. There's an option to shorten the way it can be written in `rei`:

```sh
rei "x y z -> z" testfile_in.csv > testfile_out.csv
rei "x y z -> z" "testfile_(in -> out).csv"
```

The two examples above are almost equivalent, but when using *naming pattern* `(in -> out)` the output file should not exist, otherwise `rei` throws an error with the corresponding message. *Naming pattern* also makes the command shorter and the idea behind it easier to understand:

```sh
rei "rn _ _ expr3 -> rn expr3" brain_expression(_all -> _adults).csv
```

No space characters, or `<` / `>`, or parenthesis are allowed in file names if *naming pattern* is used.

### Sophisticated examples

Skip rownames and colnames, than calculate mean of every row, than append the new column to the file from the second row, and to the first row the title "mean" is added:

```sh
> rei --skip 1 "rownames ... -> ... => mean" | \
       rei --magic "unite" --colnames "<< Mean" example.ssv -
```

It's easy to merge several files, and turn the output to .csv:

```sh
> cat <(rei "a b c -> a c" 0.ssv) <(rei "x y z -> y z" 1.ssv) | rei -f " " -g "," "1 2 -> 1 2" -
TODO
```

### Real-world examples

- .bam files stats
- date and smth else
- uniting data
- merging data

## Some more notes

### Errors and warnings

`rei` tries to be friendly to the user. For example, when there's a field variable in the *right* part of the rule that is not present in the *left* part, `rei` hides implementation details behind the user-friendly message, trying to guess that *Something's wrong with the rule...*

TODO

## Dev

`Rei` is written in Haskell, uses [regular expressions](http://hackage.haskell.org/package/regex-posix) to parse the rule and [Attoparsec](https://hackage.haskell.org/package/attoparsec) to parse the file provided.

### Requests

* guessing delimiters for "bioinformatic" formats, like .sam, .vsf, etc.

### Sources

* [Parsec examples](https://github.com/JakeWheat/intro_to_parsing)
* [Parsec to parse files](https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec)
* [Faster than C++](http://newartisans.com/2012/08/parsing-with-haskell-and-attoparsec/)
* [Argument handling in Haskell](https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling)
* [Command line options in Haskell](http://leiffrenzel.de/papers/commandline-options-in-haskell.html)
