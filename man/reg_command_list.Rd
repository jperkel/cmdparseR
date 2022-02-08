% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_command_line.R
\name{reg_command_list}
\alias{reg_command_list}
\title{Register commands using a list}
\usage{
reg_command_list(clist)
}
\arguments{
\item{clist}{list of commands}
}
\description{
Register commands using a list
}
\examples{
cmds <- list(
  c("withdraw", "add a withdrawal"),
  c("plot", "graph output"),
  c("deposit", "add a deposit"),
  c("edit", "update a record"),
  c("find", "find a record")
)
reg_command_list(cmds)
}