library(cmdparseR)

main <- function() {
  init_command_line_parser('test_cmdparseR','Test cmdparseR package','0.1.0')

  args <- list(
    c('--config','-c','config','~/myconfigfile.txt',argsType$TypeValue,'Configuration file'),
    c('--debug','-d','debug',FALSE,argsType$TypeBool,'Display debug messages'),
    c('--keywords','-k','keywords',NA,argsType$TypeMultiVal,'Search keywords'),
    c('--daterange','-r','daterange',NA,argsType$TypeRange,'Date range'),
    c('--verbose','-v','verbose',0,argsType$TypeCount,'Verbosity level')
  )
  reg_argument_list(args)

  pos <- list(
    c('outfile',NA,'Output filename'),
    c('infiles',NA,'Input filename(s)')
  )
  reg_positionals_list(pos)

  args <- commandArgs(trailingOnly = TRUE)
  mydata <- parse_command_line(args)

  print(mydata)
}

main()
