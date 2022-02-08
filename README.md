
# cmdparseR

<!-- badges: start -->
<!-- badges: end -->

`cmdparseR` provides simple command-line parsing for R scripts. It supports optional commands and subcommands, and both Boolean and value-based arguments. 

Suppose you want to write a script for creating and maintaining a check book, called `MyCheckbook.R`. You might call it like so: `Rscript MyCheckbook.R withdraw cash --amount=100` to indicate that you took out some cash at the ATM. `cmdparseR` interprets the arguments to `MyCheckbook.R` so you don't have to. 

To use, call `init_command_line_parser()` to initialize the parsing tool. Then call `reg_argument_list()` to register expected command line arguments, `reg_command_list()` to register expected commands, and `reg_subcmd_list()` for subcommands. 

Given a command line such as `Rscript MyCheckbook.R withdraw cash --amount=100 --msg='birthday gift'`, `withdraw` is the command, `cash` is a subcommand and `--amount=100` and `--msg='birthday gift'` are arguments. You might also add a `deposit` command, and a `check` subcommand, eg `MyCheckbook.R withdraw check --number=123 --amount=50 --payee='Electric Co.'` to log a check to the electric company.)

Note that the user need not provide all registered arguments on the command line. The default parameter passed to `reg_argument_list()` preloads the default. Thus, you could have an argument whose default value is FALSE. When `parse_command_line()` is called, the variable will be set to FALSE, unless the user includes in the command line, in which case it flips to TRUE.

When registering arguments, you must indicate the type of value you expect to receive. Valid parameter types are `argsType$TypeBool` for Boolean values; `argsType$TypeValue` for arguments such as `--outfile=file`, `--outfile file`, and `-o file`; and `argsType$TypeMultiVal` for parameters where multiple values can be supplied, such as keywords: `-k key1 -k key2`. 

`argsType$TypeCount` allows for parameters whose value increments each time the argument is used. Thus `-v -v -v` would return a value of 3. Short parameters can be combined, so `-vvv` is equivalent to `-v -v -v`.

"Positional" arguments are supported using `reg_positionals_list()`. Multiple positional arguments can be supplied and will be filled (from left-to-right) in the order given in the call to `reg_positionals_list()`.

If `argsType$TypeBool` is used, using the argument flips the default Boolean value. So for instance, if you call `reg_argument_list(c("--plot","-p","plot",FALSE,argsType$TypeBool,'plot output'))`, the default value of `plot` is `FALSE`. If `--plot` (or `-p`) is included in the argument list, `plot` will be set to `TRUE`. Arguments of the form `--plot=TRUE` are also allowed.

## Installation

You can install cmdparseR from [GitHub](https://github.com/jperkel/cmdparseR) with:

``` r
# install.packages("devtools")
devtools::install_github('jperkel/cmdparseR')
```

## Example

This is a basic example:

``` r
library(cmdparseR)

init_command_line_parser('MyCheckbook.R','My checkbook program', '1.0.0')

# register arguments
arguments <- list(
  # example TypeBool argument. Use as '--lparam' or '-sparam'
  c("--debug","-x","debug",FALSE,argsType$TypeBool,'print debug messages'),
  
  # example TypeValue arguments. Use as '--lparam=val', '--lparam val', or '-l val'
  c("--infile","-i","infile",NA,argsType$TypeValue,'location of your checkbook file'),
  c("--date","-d","date",NA,argsType$TypeValue,'specify date'),
  c("--msg","-m","msg",NA,argsType$TypeValue,'memo line message'),
  c("--amount","-a","amount",NA,argsType$TypeValue,'specify dollar amount'),
  c("--payee","-p","payee",NA,argsType$TypeValue,'specify payee'),
  c("--number","-n","cknum",NA,argsType$TypeValue,'specify check number'),
  
  # an example TypeMultiVal, where all supplied params are stored
  c("--keyword","-k","keyword",NA,argsType$TypeMultiVal,'keyword search terms'),
  
  # an example TypeCount, where each use of the param increments a variable
  c("--verbose","-v","verbose",0,argsType$TypeCount,'verbose level')
)
reg_argument_list(arguments)

# register commands
cmds <- list(
  c("withdraw", "add a withdrawal"),
  c("plot", "graph output"),
  c("deposit", "add a deposit"),
  c("edit", "update a record"),
  c("find", "find a record")
)
reg_command_list(cmds)

# register subcommands
subcmds <- list(
  c("cash", "withdraw", "add a cash withdrawal")
  c("check", "withdraw", "add a check withdrawal")
  c("paycheck", "deposit", "add a paycheck deposit"),
  c("reimbursement", "deposit", "add a reimbursement"),
  c("bankfee", "withdraw", "add a bank fee")
)
reg_subcmd_list(subcmds)

pos <- list(
  c("content",NA,"Content to scrape: archive type (e.g., 'technology-feature:1:3'), article URL or CSV")
)
reg_positionals_list(pos)

# get command line arguments from the system
args <- commandArgs(trailingOnly = TRUE)
# parse them
mydata <- parse_command_line(args)
```

`mydata` is a list in which each entry corresponds to a registered argument. Commands and subcommands are stored in `mydata$command` and `mydata$subcmd` respectively. Unrecognized arguments are stored in `mydata$unknowns`.

``` r
# get values as mydata$<variable_name>, eg: 
writeLines ("\nAfter parse_command_line()...")
writeLines (paste("debug mode:",debug))
writeLines (paste("command:",mydata$command))
writeLines (paste("subcommand:",mydata$subcmd))
writeLines (paste("infile:", mydata$infile))
writeLines (paste("date:",mydata$date))
writeLines (paste("msg:",mydata$msg))
writeLines (paste("amount:",mydata$amount))
writeLines (paste("payee:",mydata$payee))
writeLines (paste("cknum:",mydata$cknum))
writeLines (paste("keywords:",mydata$keyword))
writeLines (paste("unknowns:",mydata$unknowns))
writeLines (paste("verbose level:", mydata$verbose))
``` 

`cmdparseR` provides a `usage()` function to create a formatted help message based on the `desc` strings passed to `reg_argument_list()`, `reg_command_list()` and `reg_subcmd_list()`. By default, `--help` or `-?` on the command line will call this function. `--ver` or `-V` prints out version information. 


