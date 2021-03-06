% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/read.list.R
\name{read.list}
\alias{read.list}
\title{Read list}
\usage{
read.list(filename, outdir = ".", RData = FALSE, tag = "list2csv")
}
\arguments{
\item{filename}{The filename of the STATA log file}

\item{outdir}{Directory for output}

\item{RData}{store output in RData}

\item{tag}{specifies what tag to look for in the STATA log file}
}
\description{
Reads STATA tables created by \code{list x y}. For each \code{list} command found, this function will create a CSV or RData file.
For this function to work there must be a statement
\code{di "{tag} filename.csv"} preceding every \code{list} command, where
\code{{tag}} is defined in the argument to this function.
}

