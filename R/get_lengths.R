#' Extract Length data from obdbs
#'
#'Extract a list of length for speices sampled by observers on commercial fishing boats
#'This data is extracted from oblen
#'
#' @param channel an RODBC object (see \code{\link{connect_to_database}})
#' @param species a specific species code or set of codes. Either numeric or character vector. Defaults to "all" species.
#' Numeric codes are converted to VARCHAR2(4 BYTE) when creating the sql statement. Character codes are short character strings.
#' @param sex character vector. Default = "all". options "M" (male), "F" (female), "U" (unsexed)
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{$sql} statement}
#'
#'   \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'@section Reference:
#'Use the data dictionary (\url{http://nova.nefsc.noaa.gov/datadict/}) for field name explanations.
#'Note: species codes (nespp4) are stored in the database as VARCHAR2(4 BYTE)
#'
#' @seealso \code{\link{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts info for cod (73)
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_length_age(channel,species=73)
#'
#' # extracts info for cod ("COD")
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_length_age(channel,"cod") or
#' get_length_age(channel,"co") or
#' get_length_age(channel,"COD")
#'
#'}
#'
#' @export


get_lengths <- function(channel, year=1994, species="all", sex="all", marketCode = T){

  if ((year == "all") & (species == "all")) stop("Can not pull all species and all years. Too much data!!")

  # create an SQL query to extract all relavent data from tables
  # list of strings to build where clause in sql statement
  whereVec <- list()

  if (marketCode == T) {
    whereVec[[1]] <-  createString(itemName="nespp4",species,convertToCharacter=TRUE,numChars=4)
  } else { # makes last digit wildcard
    whereVec[[3]] <-  createStringSpecies(itemName="nespp4",species,convertToCharacter=TRUE,numChars=4)
  }

  # sex conversion
  if (tolower(sex) == "all") {
    sex <- c(0,1,2)
  } else if (!is.numeric(sex)) {
    sex <- gsub("M",1,sex)
    sex <- gsub("F",2,sex)
    sex <- as.numeric(gsub("U",0,sex))
  }

  whereVec[[4]] <-  paste("sex in (",toString(sex),")")

  # build where clause of SQL statement based on input above
  whereStr <- "where"
  for (item in whereVec) {
    if (is.null(item)) {next}
    if (which(item == whereVec) == length(whereVec)) {
      whereStr <- paste(whereStr,item)
      next
    }
    whereStr <- paste(whereStr,item,"and")
  }



  # eventually user will be able to pass these variables
  sqlStatement <- "select YEAR, MONTH, NEGEAR, NESPP4, LATHBEG, LONHBEG, AREA, SEX, LENANML, NUMLEN
                    from obdbs.oblen"

  sqlStatement <- paste(sqlStatement,whereStr)
  # call database
  query <- RODBC::sqlQuery(channel,sqlStatement,errors=TRUE,as.is=TRUE)

  # column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'OBLEN' and owner='OBDBS';"
  colNames <- RODBC::sqlQuery(channel,sqlcolName,errors=TRUE,as.is=TRUE)

  return (list(data=query,sql=sqlStatement, colNames=colNames))
}


