% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_lengths.R
\name{get_lengths}
\alias{get_lengths}
\title{Extract Length data from obdbs}
\usage{
get_lengths(channel, species = "all", year = 1994, sex = "all")
}
\arguments{
\item{channel}{an RODBC object (see \code{\link{connect_to_database}})}

\item{species}{a specific species code (NESPP3) or set of codes. Either numeric or character vector. Defaults to "all" species.
#' Numeric codes are converted to VARCHAR2(3 BYTE) when creating the sql statement. Character codes are short character strings.}

\item{year}{a numeric vector containing the years to search over}

\item{sex}{character vector. Default = "all". options "M" (male), "F" (female), "U" (unsexed)}
}
\value{
A list is returned:

  \item{data}{containing the result of the executed \code{$sql} statement}

  \item{sql}{containing the sql call}

  \item{colNames}{a vector of the table's column names}
}
\description{
Extract a list of length for speices sampled by observers on commercial fishing boats
This data is extracted from oblen
}
\section{Reference}{

Use the data dictionary (\url{http://nova.nefsc.noaa.gov/datadict/}) for field name explanations.
Note: species codes (nespp4) are stored in the database as VARCHAR2(4 BYTE)
}

\examples{
\dontrun{
# extracts info for american flounder (plaice) (124)
channel <- connect_to_database(server="name_of_server",uid="individuals_username")
get_lengths(channel, species=124, year = "all")
get_lengths(channel, species="124", year = 2000, sex = "M")

}

}
\seealso{
\code{\link{connect_to_database}}
}
