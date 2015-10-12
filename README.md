# Process lists easily with `rei`

While originally *rei* was not intended to be an abbreviation, one may think of it as of the *Row Editing Interface*. Working with lists is an important part of many people, including data-scientists and bioinformaticians, and `rei` aims to make that experience more pleasant.

## Installation

`rei` can be easily installed from `hackage` with `cabal` :

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
rei merge example_left.csv example_right.csv
rei unite example_top.ssv example_bottom.ssv
rei melt2 example_condensed.csv
rei condense2 example_melted.csv
rei join example_foo.ssv example_bar.ssv
rei subtract minuend.csv subtrahend.csv
rei transpose example_matrix.ssv
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
A B C D E
F G H I J
K L M N O
P Q R S T
U V W X Y
```

This is how easily we can address the columns:

```sh
> rei "a b c -> c b a" 0.ssv
C B A
H G F
M L K
R Q P
W V U
```

The columns are now in the reversed order.

We can extract the columns that we need:

```sh
> rei "a b c -> b" 0.ssv
B
G
L
Q
V
```

It is possible to define only columns needed:

```sh
> rei "a b -> a b" 0.ssv
A B
F G
K L
P Q
U V
```

You may want to keep the rest of the columns, here's the code for that:

```sh
> rei "a b ... -> a ..."
A C D E
F H I J
K M N O
P R S T
U W X Y
```

The beauty is that one may (and sometimes should) give columns descriptive titles. And that is great in so many ways, as it increases readability, productivity, descriptiveness, maintainability and awareness of what's happening with all that list processing. See some real-world examples below.

### Keywords

#### Delimiters

You can define a delimiter (`-f`, or `--delim`, for the input file and  `-g`, or `--newdelim`, for the output file). It's important to emphasize that only one-character long delimiters are used. Tabulation («\t») is considered one-character too. If multicharacter literal is provided, `rei` uses its first symbol as a delimiter.

For some common file formats `rei` doesn't require a delimiter to be provided individually:

* .ssv &rarr; space (' '),
* .csv &rarr; comma (','),
* .tsv &rarr; tab ('\t'),
* .txt &rarr; space (' '),
* .list &rarr; space (' '),
* .sam, .vcf, .bed, .gff, .gtf &rarr; tab ('\t'),

The flag `-g` is powerful as it allows for fast format conversion. That's how `rei` may be used to convert from .ssv to .csv:

```sh
> rei -g ","  "... -> ..." 0.ssv
A,B,C,D,E
F,G,H,I,J
K,L,M,N,O
P,Q,R,S,T
```

As you see, `rei` guessed the delimiter in the input file by its extension — *space-separated values*. The output won't change in the example above if `-f " "` is provided.

#### Skipping lines

Sometimes there is a need to cut out the header of the file or several lines in its end. It's generally accomplished by combining `head` and/or `tail` programs, piping, etc. Since `rei` is designed for easy list processing, such feature is implemented here. There are flags to define the number of lines to skip in the beginning (`--skip`, or `-s`) or in the end (`--omit`, or `-t`) of the file.

```sh
> rei -s 1 -t 2 "f g h i j -> f h j" 0.ssv
F H J
K M O
```

#### Enumerating lines

Sometimes it's handy to have line numbers in the data file. For that purpose `rei` offers `-n` flag (or `--enum`) which let the user treat the first variable in the rule as a line number (enumeration starts with `1`):

```sh
> rei -n "# _ _ _ d -> d d #" 0.ssv
D D 1
I I 2
N N 3
S S 4
X X 5
```


### Magic rules

There's are some common tasks that one may want to do with lists and tables, and it seems convenient to include them in `rei`: *melt2, condense2, merge,  unite, join, subtract*. Each *magic rule* has its own syntax.

#### *Melt*ing and *condens*ing

TODO

#### *Merg*ing

Here, to *merge* several (typically two) lists means to get the data together. With *merge* one can add new columns. If the length of two lists (or tables) differs, the shortest possible list is returned. `rei` cares, as usually, about the delimiters, but not about finding and reassorting rows when data is being merged.

```sh
> rei merge 0.ssv <(rei -s 1 "a -> a" 0.ssv)
A B C D E F
F G H I J K
K L M N O P
P Q R S T U
```


#### *Unit*ing

Uniting, or concatenating, several files can be achieved with `unite` rule. This rule has a synonym: `concatenate`, or `concat` for short. While simple file concatenation can be achieved using UNIX  `cat` tool, `rei unite <...>` has to acknowledge the delimiter symbol (which should be the same for all input files) and can change the delimiter symbol for the whole output or skip / omit lines.


```sh
> rei unite 0.ssv <(head -n 1 0.ssv)
A B C D E
F G H I J
K L M N O
P Q R S T
U V W X Y
A B C D E
```


#### *Join*ing

Another useful thing is finding common elements in multiple lists. `rei` allows that with `join`. (In most cases the order of the files provided does not matter. However, if the first file contains duplicates, so will the result.)

Let's prepare a file to join with `0.ssv` and save it as `01.ssv`.

```
A B C D E
K L M N O
X X X X X
```

The code for *join* is straightforward:

```sh
> rei -g ',' join 0.ssv 01.ssv
A,B,C,D,E
K,L,M,N,O
```


#### Retrieving unique data with *subtr*

Finding differences between multiple lists with a clear and concise syntax is not a trivial task. To deal with this, `rei` offers a *magic rule* called *subtract* (or *subtr* for short). It behaves exactly as it is titled: takes the first file and removes each row in it only if the row is present in any of the following files.

```sh
> tail -n 1 0.ssv > 02.ssv
> rei subtr 0.ssv 01.ssv 02.ssv
F G H I J
P Q R S T
```


#### *Tranpos*ing data

When you need to transpose the list, you can just do it with `rei`. It can be beautifully demonstrated for the following matrix (`1.ssv`):

```sh
11 12 13 14 15
21 22 23 24 25
31 32 33 34 35
41 42 43 44 45
51 52 53 54 55
```

```sh
> rei -g ',' transpose 1.ssv
11,21,31,41,51
12,22,32,42,52
13,23,33,43,53
14,24,34,44,54
15,25,35,45,55
```

### Sophisticated examples

Skip rownames and colnames:

```sh
> rei --skip 1 "rownames ... -> ..." example.ssv
```

It's easy to merge several files, and turn the output to .csv:

```sh
> rei -f ' ' -g ',' unite <(rei "a b c -> a c" 0.ssv) <(rei "x y z -> y z" 1.ssv)
TODO
```

### Real-world examples

TODO

- .bam files stats
- .bed files: counting elements
- date and smth else
- uniting data
- merging data

## Notes

### Errors and warnings

`rei` tries to be friendly to the user. For example, when there's a field variable in the *right* part of the rule that is not present in the *left* part, `rei` hides implementation details behind the user-friendly message, trying to guess that *Something's wrong with the rule...*

TODO

## Dev

`rei` is written in Haskell, uses [regular expressions](http://hackage.haskell.org/package/regex-posix) to parse the rule and [Attoparsec](https://hackage.haskell.org/package/attoparsec) to parse the file provided.

### Requests

* [x] guessing delimiters for "bioinformatic" formats, like: .sam, .vsf, etc.
* [x] guessing delimiters for more formats: .bed, .gff, .gtf
