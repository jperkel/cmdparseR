library(cmdparseR)

main <- function() {
  init_command_line_parser('test_cmdparseR','Test cmdparseR package','0.1.0')

  cmds <- list(
    list('add', 'Add something'),
    list('delete', 'Delete something')
  )
  reg_command_list(cmds)

  subcmds <- list(
    list('name','add','Add a name'),
    list('file','add','Add a file'),
    list('name','delete','Delete a name'),
    list('file','delete','Delete a file')
  )
  reg_subcmd_list(subcmds)

  args <- list(
    list('--config','-c','config','~/myconfigfile.txt',argsType$TypeValue,'Configuration file'),
    list('--debug','-d','debug',FALSE,argsType$TypeBool,'Display debug messages'),
    list('--keywords','-k','keywords',NA,argsType$TypeMultiVal,'Search keywords'),
    list('--daterange','-r','daterange',NA,argsType$TypeRange,'Date range'),
    list('--username','-u','username',NA,argsType$TypeValue,'User name'),
    list('--verbose','-v','verbose',0,argsType$TypeCount,'Verbosity level')
  )
  reg_argument_list(args)

  pos <- list(
    list('outfile','Output filename'),
    list('infiles','Input filename(s)')
  )
  reg_positionals_list(pos)

  args <- commandArgs(trailingOnly = TRUE)
  mydata <- parse_command_line(args)

  if (is.na(mydata$outfile)) stop("Error: no outfile provided.")
  # read input from stdin if no input files provided on the cmd line.
  if (is.na(mydata$infiles[1])) {
    stdin <- file("stdin")
    lines <- readLines(stdin)
    close(stdin)
    mydata$infiles <- lines

  }
  print(mydata)
}

main()
