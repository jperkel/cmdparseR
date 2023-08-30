##########################
## parse_command_line.R: functions for parsing command line parameters
##
## (c) 2019 Jeffrey M. Perkel
##########################


pkg.globals <- new.env()
pkg.globals$initialized <- FALSE

#' Define an enum for different modes; access with argsType$<enum_element>
#' ht https://stackoverflow.com/questions/33838392/enum-like-arguments-in-r
#'
#' TypeBool: TRUE/FALSE
#' TypeValue: any value expressed as "--arg=Value", "--arg Value", or "-a Value"
#' TypeMultiVal: TypeValue, but allowing multiple values to be stored (ie, keywords)
#' TypeCount: value increments each time the param is used. eg, -v -v yields 2
#' TypeRange: splits a TypeValue like "1:3" into two variables, "1" and "3"
#' TypePositional: required, final argument
#'
#'
argsEnum <- function() {
  list (TypeBool = 1, TypeValue = 2, TypeMultiVal = 3, TypeCount = 4, TypeRange = 5, TypePositional = 6)
}
#' enum for different param types; access with argsType$<enum_element>
#' @export
argsType <- argsEnum()


#' usage: Create a help message; adds --help and --ver params if not provided.
#'
#' @export
#' @importFrom stringr str_pad
#'
usage <- function() {
  # number of spaces for each indentation level
  lvl1_indent <- 2
  lvl2_indent <- 6
  lvl3_indent <- 10
  lvl4_indent <- 14

  buffer_str <- function(spacer) {
    return (paste0(rep(' ', spacer), collapse = ''))
  }

  # remove first row of the tables, which is all NA
  args_table <- pkg.globals$args_table[-1,]
  cmds_table <- pkg.globals$cmds_table[-1,]
  subcmds_table <- pkg.globals$subcmds_table[-1,]

  positionals <- NULL
  pos_string <- ""
  if (any(args_table$argType == argsType$TypePositional)) {
    positionals <- args_table$var[args_table$argType == argsType$TypePositional]
    pos_string <- paste0('[', positionals, ']', collapse = ' ')
  }

  writeLines(paste0('\n', pkg.globals$script, ': ', pkg.globals$desc_str))
  writeLines(paste0(buffer_str(lvl1_indent), 'USAGE: Rscript ', pkg.globals$script, ' ',
                    ifelse(nrow(cmds_table) > 0, '[COMMAND] ', ''),
                    ifelse(nrow(subcmds_table) > 0, '[SUBCOMMAND] ', ''),
                    ifelse(nrow(args_table) > 0, '<optional arguments> ', ''),
                    pos_string
  ))
  if (!is.na(pkg.globals$ver)) {
    writeLines(paste0(buffer_str(lvl2_indent), 'Ver: ', pkg.globals$ver))
  }
  writeLines('')

  # sort the tables alphabetically
  args_table <- args_table[order(args_table$lparam),]
  # args_table$scope <- ifelse(args_table$scope == "NA", NA, args_table$scope)

  if (nrow(cmds_table) > 0) {
    cmds_table <- cmds_table[order(cmds_table$cmd),]

    writeLines(paste0(buffer_str(lvl1_indent), 'COMMANDS:'))
    for (r in 1:nrow(cmds_table)) {
      myrow <- cmds_table[r,]
      writeLines(paste0(buffer_str(lvl2_indent),stringr::str_pad(myrow$cmd, max(nchar(cmds_table$cmd)), "right"),
                        ' : ', myrow$help))
      if (nrow(subcmds_table) > 0) {
        subtable <- subcmds_table[subcmds_table$parent == myrow$cmd, ]
        if (nrow(subtable) > 0) {
          writeLines(paste0(buffer_str(lvl3_indent), "SUBCOMMANDS:"))
          subtable <- subtable[order(subtable$subcmd),]
          writeLines(paste0(buffer_str(lvl4_indent),stringr::str_pad(subtable$subcmd, max(nchar(subtable$subcmd)), "right"),
                            ' : ', subtable$help))
        } # if (nrow(subtable) > 0)
      } # if (nrow(subcmds_table) > 0)
    } # for
    writeLines('')
  } # if (nrow(cmds_table) > 0)

  if (!is.null(positionals[1])) {
    writeLines(paste0(buffer_str(lvl1_indent), "REQUIRED ARGUMENTS: "))
    for (p in positionals) {
      writeLines(paste0(buffer_str(lvl2_indent),stringr::str_pad(p, max(nchar(args_table$lparam), na.rm = TRUE), "right"),
                        buffer_str(5), ": ", args_table$help[args_table$var == p]))
    }
  }

  # remove positionals from the table
  args_table <- args_table[args_table$argType != argsType$TypePositional,]

  writeLines('')
  writeLines(paste0(buffer_str(lvl1_indent), 'OPTIONAL ARGUMENTS:'))
  for (r in 1:nrow(args_table)) {
    myrow <- args_table[r,]
    writeLines(paste0(
      buffer_str(lvl2_indent),stringr::str_pad(myrow$lparam, max(nchar(args_table$lparam)), "right"),
      # need 5 spaces to account for sparam if none provided, eg ' (-m)'
      ifelse (!is.na(myrow$sparam), paste0(' (', myrow$sparam, ')'), buffer_str(5)),
      ifelse (myrow$help == '', '', ': '),
      myrow$help,
      ifelse(is.na(myrow$default), '',
             paste0('\n', buffer_str(lvl2_indent + max(nchar(args_table$lparam)) + 10),
                    'default: ',
                    ifelse (myrow$argType == argsType$TypeBool, as.logical(myrow$default), myrow$default))
      ) #,
      # ifelse(is.na(myrow$scope), '',
      #        paste0('\n', buffer_str(lvl2_indent + max(nchar(args_table$lparam)) + 10),
      #               "Valid for: ", gsub('_', ', ', myrow$scope)))
    ))
  }
} # usage


#' Initialize command-line parsing
#'
#' @param script name of the R script
#' @param desc description of the script
#' @param ver tool version number (string)
#'
#' @export
#'
#' @examples
#' init_command_line_parser('MyCheckbook.R','My checkbook program', '1.0.0')
init_command_line_parser <- function (script, desc, ver = NA) {
  pkg.globals$initialized <- TRUE
  pkg.globals$script <- script
  pkg.globals$desc_str <- desc
  pkg.globals$ver <- ver
  # tables to hold the possible command line params
  pkg.globals$args_table <- data.frame(lparam = NA, sparam = NA, var = NA, default = NA, argType = NA,
                                       help = NA, stringsAsFactors = FALSE)
  pkg.globals$cmds_table <- data.frame(cmd = NA, help = NA, stringsAsFactors = FALSE)
  pkg.globals$subcmds_table <- data.frame(subcmd = NA, parent = NA, help = NA, stringsAsFactors = FALSE)

  # add a 'help' param
  reg_argument(lparam = '--help', sparam = '-h', var = 'help', default = FALSE, argType = argsType$TypeBool,
               help = 'Display help message')

  # add a 'version' param
  reg_argument(lparam = '--ver', sparam = '-V', var = 'ver', default = FALSE, argType = argsType$TypeBool,
               help = "Display version information")

} # init_command_line_parser


##
## Register required commands. Use for programs with syntax:
##    myprog.R <COMMAND> [optional-params]
##
##    Call reg_command() for each allowed command. Commands are assumed to be the first
##       argument after the script name, and only one command is allowed.
##
##       cmd: expected command
##       help: help string for the param, for usage()
##
reg_command <- function(cmd, help = '') {
#  if (is.na(pkg.globals$desc_str)) {
  if (!pkg.globals$initialized) {
    stop("Error: reg_command(): Command line parser not initialized.", call. = FALSE)
  }

  if (cmd %in% pkg.globals$cmds_table$cmd) {
    stop(paste0("Error: reg_command(): duplicated command: ", cmd), call. = FALSE)
  }

  my_df <- data.frame(cmd = cmd, help = help, stringsAsFactors = FALSE)
  pkg.globals$cmds_table <- rbind(pkg.globals$cmds_table, my_df)
} # reg_command


#' Register commands using a list
#'
#' @param clist list of lists of commands: command, help string
#'
#' @export
#'
#' @examples
#' cmds <- list(
#'   list("withdraw", "add a withdrawal"),
#'   list("plot", "graph output"),
#'   list("deposit", "add a deposit"),
#'   list("edit", "update a record"),
#'   list("find", "find a record")
#' )
#' reg_command_list(cmds)
reg_command_list <- function(clist) {
  ids <- c("cmd","help")
  for (c in clist) {
    stopifnot(length(c) == length(ids))
    reg_command(cmd = c[[1]], help = c[[2]])
  }
} # reg_command_list


##
## Register required subcommands. Use for programs with syntax:
##    myprog.R <COMMAND> <SUBCOMMMAND> [optional-params]
##
##    Call reg_subcmd() for each allowed subcommand. Subcommands are assumed to be the second
##       argument after the script name, and only one subcommand is allowed.
##
##       subcmd: expected command
##       parent: parent command
##       help: help string for the param, for usage()
##
reg_subcmd <- function(subcmd = subcmd, parent = parent, help = '') {
#  if (is.na(pkg.globals$desc_str)) {
  if (!pkg.globals$initialized) {
    stop("Error: reg_subcmd(): Command line parser not initialized.", call. = FALSE)
  }

  subtable <- pkg.globals$subcmds_table[pkg.globals$subcmds_table$parent == parent,]
  if (subcmd %in% subtable$subcmd) {
    stop(paste0("Error: reg_subcmd(): duplicated subcommand: ", subcmd), call. = FALSE)
  }

  my_df <- data.frame(subcmd = subcmd, parent = parent, help = help, stringsAsFactors = FALSE)
  pkg.globals$subcmds_table <- rbind(pkg.globals$subcmds_table, my_df)
} # reg_subcmd


#' Register subcommands using a list
#'
#' @param slist list of lists of subcommands: subcmd, parent, help string
#'
#' @export
#'
#' @examples
#' subcmds <- list(
#' list("paycheck", "deposit", "add a paycheck deposit"),
#' list("reimbursement", "deposit", "add a reimbursement"),
#' list("bankfee", "withdraw", "add a bank fee"),
#' list("check", "deposit", "add a check deposit")
#' )
#' reg_subcmd_list(subcmds)
reg_subcmd_list <- function(slist) {
  ids <- c("subcmd","parent","help")
  for (s in slist) {
    stopifnot(length(s) == length(ids))
    reg_subcmd(subcmd = s[[1]], parent = s[[2]], help = s[[3]])
  }
} # reg_subcmd_list


##
## Register an expected command line argument. Use for programs with syntax:
##    myprog.R [optional-args]
##
##    Call reg_argument() for each allowed parameter.
##       lparam: long-form arg (eg '--outfile')
##       sparam: short-form arg (eg '-o); use NA for none.
##       var: variable name to hold the value
##       default: default value for var
##       argType: argsType$TypeBool for logical values (TRUE/FALSE)
##                argsType$TypeValue for params of type '--outfile=myfile.txt', '--outfile myfile.txt'
##                  or '-o outfile.txt'
##                argsType$TypeMultiVal to store multiple values (ie, keywords)
##       help: help string for the arg, for usage()
##       scope: a list of commands & subcmds for which the arg is valid, given as a vector,
##                eg, "c("command1|subcmd1", "command2")
##
reg_argument <- function(lparam, sparam, var, default, argType, help) {
  # if (is.na(pkg.globals$desc_str)) {
  if (!pkg.globals$initialized) {
    stop("Error: reg_argument(): Command line parser not initialized.", call. = FALSE)
  }

  if (sparam %in% pkg.globals$args_table$sparam[!is.na(pkg.globals$args_table$sparam)] ||
      lparam %in% pkg.globals$args_table$lparam[!is.na(pkg.globals$args_table$lparam)]) {
    stop(paste("Error: reg_argument(): duplicated param:", lparam, sparam), call. = FALSE)
  }

  if (!argType %in% c(argsType$TypeBool, argsType$TypeValue, argsType$TypeMultiVal,
                      argsType$TypeCount, argsType$TypeRange, argsType$TypePositional))
    stop(paste("Error: reg_argument(): invalid argType:", argType))
  if (argType == argsType$TypeBool) default <- as.logical(default)

  my_df <- data.frame(lparam = lparam, sparam = sparam, var = var, default = default, argType = argType,
                      help = help, stringsAsFactors = FALSE)
  pkg.globals$args_table <- rbind(pkg.globals$args_table, my_df)
} # reg_argument


#' Register command line arguments
#'
#' @param plist list of lists of arguments: lparam, sparam, var, default, argType, help string
#'
#' @export
#'
#' @examples
#' arguments <- list(
#'   list("--outfile","-o","outfile",NA,argsType$TypeValue,'location of output file'),
#'   list("--date","-d","date",NA,argsType$TypeValue,'specify date'),
#'   list("--msg","-m","msg",NA,argsType$TypeValue,'memo line message'),
#'   list("--amount","-a","amount",NA,argsType$TypeValue,'specify dollar amount'),
#'   list("--payee","-p","payee",NA,argsType$TypeValue,'specify payee'))
#' reg_argument_list(arguments)
reg_argument_list <- function(plist) {
  # scope is not required. So, check for the 6 required params, and if no scope provided, set to NA
  ids <- c("lparam","sparam","var","default","argType","help")

  for (p in plist) {
    stopifnot (length(p) == length(ids))
    reg_argument (lparam = p[[1]], sparam = p[[2]], var = p[[3]], default = p[[4]],
                  argType = p[[5]], help = p[[6]])
  }
} # reg_argument_list


#
# Register a 'positional' command line argument (ie, the last argument in the list)
reg_positionals <- function(var, help) {
  if (!pkg.globals$initialized) {
    stop("Error: reg_positionals(): Command line parser not initialized.", call. = FALSE)
  }

  reg_argument (lparam = NA, sparam = NA, var = var, default = NA, argType = argsType$TypePositional, help = help)
} # reg_positionals


#' Register a list of 'positional' arguments
#'
#' @param plist list of lists of positional arguments: variable name, help text
#'
#' @export
#'
#' @examples
#' args <- list(list("infile","input file"))
reg_positionals_list <- function(plist) {
  ids <- c("var","help")

  for (p in plist) {
    stopifnot(length(p) == length(ids))
    reg_positionals(var = p[[1]], help = p[[2]])
  }
} # reg_positionals_list


#' Parses a date in YYYY-MM-DD, YYYYMMDD, YYYY-MM or YYYY format
#'
#' @param d the date to parse (string)
#'
#' @return A vector: c(y, m, d)
#' @export
#'
#' @examples
#' parse_date("2019-12-31")
#' parse_date("2019-12")
#' parse_date("2019")
#' # parse_date("2019-13-31") # bad date!
parse_date <- function(d) {
  year <- NA
  month <- NA
  day <- NA

  if (grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$', d) == TRUE) {
    myDate <- try(as.Date (d, format = "%Y-%m-%d"))
    if (class (myDate) == "try-error" || is.na(myDate)) {
      stop(paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
    year <- as.integer(format(myDate, "%Y"))
    month <- as.integer(format(myDate, "%m"))
    day <- as.integer(format(myDate, "%d"))
  }
  else if (grepl('^[0-9]{8}$', d) == TRUE) {
    myDate <- try(as.Date (d, format = "%Y%m%d"))
    if (class (myDate) == "try-error" || is.na(myDate)) {
      stop(paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
    year <- as.integer(format(myDate, "%Y"))
    month <- as.integer(format(myDate, "%m"))
    day <- as.integer(format(myDate, "%d"))
  }
  else if (grepl('^[0-9]{4}-[0-9]{2}$', d) == TRUE) {
    year <- as.integer(substr(d, 1, 4))
    month <- as.integer(substr(d, 6, 7))
    if ( (is.na(year)) ||
         (is.na(month)) ||
         !(month %in% 1:12)) {
      stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
  }
  else if (grepl('^[0-9]{4}$', d) == TRUE) {
    year <- as.integer(d)
    if (is.na(year)) {
      stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
    }
  }
  else {
    stop (paste("parse_date(): Bad date format:", d), call. = FALSE)
  }
  return(c(year, month, day))
} # parse_date


#' Parse the command line
#'
#' @param args command line arguments
#' @param default_help use built-in help generation (set to FALSE to provide eg subcmd-specific help)
#'
#' @return a list of parsed data
#' @export
#'
#' @examples
#' args <- commandArgs(trailingOnly = TRUE)
#' mydata <- parse_command_line(args)
#' # After parse_command_line(), access values as mydata$<var_name>, eg
#' # writeLines (paste("command:",mydata$command))
#' # writeLines (paste("subcommand:",mydata$subcmd))
#' # writeLines (paste("infile:", mydata$infile))
#' # writeLines (paste("outfile:",mydata$outfile))
parse_command_line <- function(args, default_help=TRUE) {
  if (!pkg.globals$initialized) {
    stop("Error: parse_command_line(): Command line parser not initialized.", call. = FALSE)
  }

  if(any(args %in% c('--help', '-h'))) {
    usage()
    stop(call. = FALSE)
  }

  if(any(args %in% c('--ver', '-V'))) {
    writeLines(paste0(pkg.globals$script, ': ', pkg.globals$desc_str, '\n\tVer: ', pkg.globals$ver, '\n'))
    stop(call. = FALSE)
  }

  # remove the first line of the tables, which are all NA
  args_table <- pkg.globals$args_table[-1,]
  cmds_table <- pkg.globals$cmds_table[-1,]
  subcmds_table <- pkg.globals$subcmds_table[-1,]
  # placeholder for positional args, if any
  positionals <- NA

  # if neither reg_arguments() nor reg_command() has been called, there's no table to process;
  # return the args as a list under the name 'unknowns'
  if (nrow(args_table) == 0 && nrow(cmds_table) == 0) {
    warning ("Warning: parse_command_line(): no cmdline params or commands registered.")
    return (list(unknowns = args))
  }

  # if default_help is TRUE, show default usage() if --help or -? anywhere in the cmdline
  # if not, we can catch --help/-? lower down and return by setting mydata$help to TRUE
  if (default_help && any(args %in% c("--help", "-?"))) {
    usage()
    stop(call. = FALSE)
  }

  # if any 'concatenated' sparams -- eg '-abc' for '-a -b -c', resolve them
  while (any(is_concatenated_sparam(args))) {
    index <- which(is_concatenated_sparam(args))[1]
    sparam <- args[index]
    args <- args[-index]
    spl <- strsplit(sparam, '')[[1]]
    # insert the new sparams in their original position
    args <- append(args, paste0('-', spl[2:length(spl)]), after = (index-1))
  }

  # create an empty list to store results, name each entry by its var name, & store defaults
  mydata <- vector("list", nrow(args_table))
  names(mydata) <- args_table$var
  for (name in names(mydata)) {
    myrow <- args_table[args_table$var == name,]
    mydata[[name]] <- myrow$default
    # ensure TypeBool is in fact a Bool
    if (myrow$argType == argsType$TypeBool) mydata[[name]] <- as.logical(mydata[[name]])
  }

  # counter
  i <- 1

  # process commands if any
  if (nrow(cmds_table) > 0) {
    if (args[i] %in% cmds_table$cmd) {
      mydata[["command"]] <- args[i]

      # filter subcmds_table to include only entries where parent == command
      subcmds_table <- subcmds_table[subcmds_table$parent == mydata$command,]
    }
    else if (args[i] %in% c("--help","-?")) {
      mydata[["help"]] <- TRUE
      return (mydata)
    }
    else if (is.na(args[i])) {
      usage()
      stop("parse_command_line(): command required", call. = FALSE)
    }
    else {
      stop (paste("parse_command_line(): unknown command:", args[i]), call. = FALSE)
    }
    i <- i + 1
  }

  # process subcommands if any
  if (nrow(subcmds_table) > 0) {
    if (args[i] %in% subcmds_table$subcmd) {
      mydata[["subcmd"]] <- args[i]
    }
    else if (args[i] %in% c("--help","-?")) {
      mydata[["help"]] <- TRUE
      return (mydata)
    }
    else if (is.na(args[i])) {
      stop("parse_command_line(): subcommand required", call. = FALSE)
    }
    else {
      stop (paste0("parse_command_line(): \'", args[i], "\' is not a subcommand of parent \'",
                   mydata$command, "\'"), call. = FALSE)
    }
    i <- i + 1
  }

  # process arguments
  unk <- 0 # number of unknown params found
  while (i <= length(args)) {
    p <- args[i]
    myrow <- NULL
    index <- NULL
    has_equals <- FALSE

    if (is_lparam(p)) {
      if (p %in% args_table$lparam) {
        index <- which(args_table$lparam == p)
      }
      else if (strsplit(p, "=")[[1]][1] %in% args_table$lparam) {
        index <- which(args_table$lparam == strsplit(p, "=")[[1]][1])
        has_equals <- TRUE
      }
      else {
        # unrecognized argument
        unk <- unk + 1
        mydata[["unknowns"]][unk] <- p
        warning (paste("Warning: parse_command_line(): unknown param:", p))
        i <- i + 1
        next
      }
    }
    else if (is_sparam(p)) {
      if (p %in% args_table$sparam) index <- which(args_table$sparam == p)
      else {
        # unrecognized argument
        unk <- unk + 1
        mydata[["unknowns"]][unk] <- p
        warning (paste("Warning: parse_command_line(): unknown param:", p))
        i <- i + 1
        next
      }
    }
    else if (any(args_table$argType == argsType$TypePositional)) {
      # possible positional argument
      # if none of the following args start w/ '^-', assume all are positionals
      if (all(!grepl('^-', c(args[i:length(args)])))) positionals[length(positionals)+1] <- p
      else {
        # unrecognized argument
        unk <- unk + 1
        mydata[["unknowns"]][unk] <- p
        warning (paste("Warning: parse_command_line(): unknown param:", p))
      }
      i <- i + 1
      next
    }
    else {
      # unrecognized argument
      unk <- unk + 1
      mydata[["unknowns"]][unk] <- p
      warning (paste("Warning: parse_command_line(): unknown param:", p))
      i <- i + 1
      next
    }

    myrow <- args_table[index,]

    if(myrow$argType == argsType$TypeBool) { # if the param is a logical type, save the opposite logical type
      if (has_equals) {
        val <- strsplit(p, "=")[[1]][2]
        if (!is.na(as.logical(val))) mydata[[myrow$var]] <- as.logical(val)
        else warning(paste("Warning: parse_command_line(): non-Boolean value provided to Boolean argument ignored:", p))
      }
      else mydata[[myrow$var]] <- !as.logical(myrow$default)
    }

    else if (myrow$argType == argsType$TypeCount) {
      mydata[[myrow$var]] <- ifelse(is.na(mydata[[myrow$var]]), 1, as.integer(mydata[[myrow$var]]) + 1)
    }

    # TypeValue, TypeMultiVal, TypeRange: store the next argument, or whatever is after the '='
    else if (myrow$argType %in% c(argsType$TypeValue, argsType$TypeMultiVal, argsType$TypeRange)) {
      if (!has_equals) {
        if (i == length(args)) { # ie, there is no args[i+1]
          stop(paste("parse_command_line(): Expected value missing after param:", p), call. = FALSE)
        }
        if (myrow$argType == argsType$TypeValue) {
          mydata[[myrow$var]] <- args[i+1]
        }
        # if the same arg is passed multiple times, collect all responses (ie, for keywords)
        else if (myrow$argType == argsType$TypeMultiVal) {
          idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          mydata[[myrow$var]][idx] <- args[i+1]
        }
        else if (myrow$argType == argsType$TypeRange) {
          mydata[[myrow$var]] <- args[i+1]
          s <- strsplit(args[i+1], ':')[[1]]
          mydata[[paste0(myrow$var, 1)]] <- s[1]
          mydata[[paste0(myrow$var, 2)]] <- s[2]
        }
        i <- i + 1 # increment the counter to ignore the next param
      }
      else { # has_equals == TRUE
        val <- strsplit(p, "=")[[1]][2]
        if (myrow$argType == argsType$TypeValue) {
          mydata[[myrow$var]] <- val
        }
        else if (myrow$argType == argsType$TypeMultiVal) {
          idx <- ifelse(is.na(mydata[[myrow$var]][1]), 1, length(mydata[[myrow$var]])+1)
          mydata[[myrow$var]][idx] <- val
        }
        else if (myrow$argType == argsType$TypeRange) {
          mydata[[myrow$var]] <- val
          s <- strsplit(val, ':')[[1]]
          mydata[[paste0(myrow$var, 1)]] <- s[1]
          mydata[[paste0(myrow$var, 2)]] <- s[2]
        }
      }
    }
    i <- i + 1 # advance to next param
  }

  # process positionals
  # if (any(!is.na(positionals))) {
  if (any(args_table$argType == argsType$TypePositional)) {
    # lose the first element, which is NA
    positionals <- positionals[which(!is.na(positionals))]
    # get indices of positions from args_table
    index <- which(args_table$argType == argsType$TypePositional)
    for (i in seq_along(index)) {
      myrow <- args_table[index[i],]
      mydata[[myrow$var]] <- positionals[i]
    }
    if (length(index) < length(positionals)) {
      # ie, if more positionals provided than expected, copy the remainder into the last positional variable
      mydata[[myrow$var]] <- c(mydata[[myrow$var]], positionals[(i+1):length(positionals)])
    }
  } # positionals
  return (mydata)
} # new_parse_command_line


# HELPER FUNCTIONS
is_lparam <- function(arg) {
  return (grepl('^--', arg))
} # is_lparam

is_sparam <- function(arg) {
  return(grepl('^-[a-zA-Z?]{1}$', arg))
}

is_concatenated_sparam <- function(arg) {
  return(grepl('^-[a-zA-Z?]{2,}$', arg))
}
