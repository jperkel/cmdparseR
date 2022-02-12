library(cmdparseR)

main <- function() {
  init_command_line_parser('test_cmdparseR','Test cmdparseR package','0.1.0')

  cmds <- list(
    c('add', 'Add something'),
    c('delete', 'Delete something')
  )
  reg_command_list(cmds)

  subcmds <- list(
    c('name','add','Add a name'),
    c('file','add','Add a file'),
    c('name','delete','Delete a name'),
    c('file','delete','Delete a file')
  )
  reg_subcmd_list(subcmds)

  args <- list(
    c('--config','-c','config','~/myconfigfile.txt',argsType$TypeValue,'Configuration file'),
    c('--debug','-d','debug',FALSE,argsType$TypeBool,'Display debug messages'),
    c('--keywords','-k','keywords',NA,argsType$TypeMultiVal,'Search keywords'),
    c('--daterange','-r','daterange',NA,argsType$TypeRange,'Date range'),
    c('--verbose','-v','verbose',0,argsType$TypeCount,'Verbosity level')
  )
  reg_argument_list(args)

  pos <- list(
    c('outfile','Output filename'),
    c('infiles','Input filename(s)')
  )
  reg_positionals_list(pos)

  args <- commandArgs(trailingOnly = TRUE)
  mydata <- parse_command_line(args)

  print(mydata)
}

main()
