
# cmdparseR

<!-- badges: start -->
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/jperkel/cmdparseR?include_prereleases)](https://github.com/jperkel/cmdparseR/releases/latest)
<!-- badges: end -->

`cmdparseR` provides simple command-line parsing for R scripts. It supports optional commands and subcommands, and both Boolean and value-based arguments. 

Suppose you want to write a script for creating and maintaining a check book, called `MyCheckbook.R`. You might call it like so: `Rscript MyCheckbook.R withdraw cash --amount=100` to indicate that you took out some cash at the ATM. `cmdparseR` interprets the arguments to `MyCheckbook.R` so you don't have to. 

To use, call `init_command_line_parser()` to initialize the parsing tool. Then call `reg_argument_list()` to register expected command line arguments, `reg_command_list()` to register expected commands, and `reg_subcmd_list()` for subcommands. 

Given a command line such as `Rscript MyCheckbook.R withdraw cash --amount=100 --msg='birthday gift'`, `withdraw` is the command, `cash` is a subcommand and `--amount=100` and `--msg='birthday gift'` are arguments. You might also add a `deposit` command, and a `check` subcommand, eg `MyCheckbook.R withdraw check --number=123 --amount=50 --payee='Electric Co.'` to log a check to the electric company.)

Note that the user need not provide all registered arguments on the command line. The default parameter passed to `reg_argument_list()` preloads the default. 

When registering arguments, you must indicate the type of value you expect to receive. Valid parameter types are: 
- `argsType$TypeBool` for Boolean values. Using the argument flips the default value. Thus, if the default value of `--plot` is `FALSE`, including `--plot` on the command line will set its value to `TRUE`. Arguments of the form `--plot=TRUE` and `--plot=F` are also allowed. 
- `argsType$TypeCount` allows for parameters whose value increments each time the argument is used. Thus `-v -v -v` returns a value of 3. Note that short parameters can be combined, so `-vvv` is equivalent to `-v -v -v`.
- `argsType$TypeMultiVal` for parameters where multiple values can be supplied, such as keywords: `-k key1 -k key2`
- `argsType$TypeRange` separates two strings separated by a colon into substrings, each of which is stored in `<var>1` and `<var>2`. For instance, an argument `--range` with variable name `range`: `--range 2020:2022` yields `range1` (2020) and `range2` (2022).
- `argsType$TypeValue` for arguments such as `--outfile=file`, `--outfile file`, and `-o file`

"Positional" arguments -- i.e., required arguments at the far right of the command line -- are supported using `reg_positionals_list()`. Multiple positional arguments can be supplied; they will be filled (from left-to-right) in the order given in the call to `reg_positionals_list()`.

The return value of `parse_command_line()` is a list (say, `mydata`) in which each entry is named according to the variable name (i.e., the third element) passed to `reg_argument_list()`. Commands and subcommands are stored in `mydata$command` and `mydata$subcmd` respectively. Unrecognized arguments are stored in `mydata$unknowns`.

## Installation

Install cmdparseR from [GitHub](https://github.com/jperkel/cmdparseR) with:

``` r
# install.packages("devtools")
devtools::install_github('jperkel/cmdparseR')
```

## Example

The file `test_cmdparseR.R` provides a simple example:

``` r
library(cmdparseR)

main <- function() {
  # script name, script description, version number
  init_command_line_parser('test_cmdparseR','Test cmdparseR package','0.1.0')

  cmds <- list(
    # command, help string
    c('add', 'Add something'),
    c('delete', 'Delete something')
  )
  reg_command_list(cmds)
  
  subcmds <- list(
    # subcommand, parent command, help string
    c('name','add','Add a name'),
    c('file','add','Add a file'),
    c('name','delete','Delete a name'),
    c('file','delete','Delete a file')
  )
  reg_subcmd_list(subcmds)
  
  args <- list(
    # long parameter form, short parameter form, variable name, default value, argument type, help string
    c('--config','-c','config','~/myconfigfile.txt',argsType$TypeValue,'Configuration file'),
    c('--debug','-d','debug',FALSE,argsType$TypeBool,'Display debug messages'),
    c('--keywords','-k','keywords',NA,argsType$TypeMultiVal,'Search keywords'),
    c('--daterange','-r','daterange',NA,argsType$TypeRange,'Date range'),
    c('--verbose','-v','verbose',0,argsType$TypeCount,'Verbosity level')
  )
  reg_argument_list(args)

  pos <- list(
    # variable name, help string
    c('outfile','Output filename'),
    c('infiles','Input filename(s)')
  )
  reg_positionals_list(pos)

  args <- commandArgs(trailingOnly = TRUE)
  mydata <- parse_command_line(args)

  print(mydata)
}

main()
```

Invoked like so:

```
Rscript test_cmdparseR.R add name -dvvv -r 2020:2022 -z -k key1 -k key2 outfile.txt infile1.txt infile2.txt infile3.txt
```

you should see the following:
```
$ Rscript test_cmdparseR.R add name -dvvv -r 2020:2022 -z -k key1 -k key2 outfile.txt infile1.txt infile2.txt infile3.txt
Warning: parse_command_line(): unknown param: -z
$help
[1] "FALSE"

$config
[1] "~/myconfigfile.txt"

$debug
[1] TRUE

$keywords
[1] "key1" "key2"

$daterange
[1] "2020:2022"

$verbose
[1] 3

$outfile
[1] "outfile.txt"

$infiles
[1] "infile1.txt" "infile2.txt" "infile3.txt"

$command
[1] "add"

$subcmd
[1] "name"

$daterange1
[1] "2020"

$daterange2
[1] "2022"

$unknowns
[1] "-z"

```

`cmdparseR` provides a `usage()` function to create a formatted help message based on the `desc` strings passed to `reg_argument_list()`, `reg_command_list()` and `reg_subcmd_list()`. By default, `--help` or `-?` on the command line will call this function:

```
$ Rscript test_cmdparseR.R -?

test_cmdparseR: Test cmdparseR package
  USAGE: Rscript test_cmdparseR [COMMAND] [SUBCOMMAND] <optional arguments> [outfile] [infiles]
      Ver: 0.1.0

  COMMANDS:
      add    : Add something
          SUBCOMMANDS:
              file : Add a file
              name : Add a name
      delete : Delete something
          SUBCOMMANDS:
              file : Delete a file
              name : Delete a name

  REQUIRED ARGUMENTS: 
      outfile         : Output filename
      infiles         : Input filename(s)

  OPTIONAL ARGUMENTS:
      --config    (-c): Configuration file
                           default: ~/myconfigfile.txt
      --daterange (-r): Date range
      --debug     (-d): Display debug messages
                           default: FALSE
      --help      (-?): Display help message
                           default: FALSE
      --keywords  (-k): Search keywords
      --verbose   (-v): Verbosity level
                           default: 0
Error: 
Execution halted
```

A `parse_date()` function takes a string formatted as `YYYY-MM-DD`, `YYYY-MM` or `YYYY` and returns a vector of integers. For instance, `parse_date('2022-01-31')` returns `c(2022, 1, 31)`; `parse_date('2022-01')` returns `c(2022, 1, NA)`, and `parse_date('2021')` returns `c(2021, NA, NA)`.


