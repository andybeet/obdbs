#' Extract absence-presence data from obdbs
#'
#' For a specified region of interest, extracts all hauls and indicates presence or absence for
#' species of interest. Sampled by observers on commercial fishing boats
#' This data is extracted from obspp
#'
#' @param channel RODBC object (see \code{\link{connect_to_database}})
#' @param species Numeric Vector. a specific species code (NESPP3) or set of codes. Defaults to "all" species.
#' #' Numeric codes are converted to VARCHAR2(3 BYTE) when creating the sql statement. Character codes are short character strings.
#' @param year Numeric vector. Years to search over
#' @param sex Character string. Default = "all". options "M" (male), "F" (female), "U" (unsexed)
#' @param area Character vector. Statistical areas. Default = "all".
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
#' # extracts info for american flounder (plaice) (124) in stat areas 512 to 514 for years 2000 - 2017
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_absence_presence(channel,species=124, area = c(512:514), year=c(2000:2017))
#'
#'}
#'
#' @export


get_absence_presence <- function(channel, species="all", year=1994,  sex="all", area ="all"){

  if ((length(year) == 1) & (length(species) == 1)) {
    if ((year == "all") & (species == "all")) stop("Can not pull all species and all years. Too much data!!")
  }

  message("This could take a couple of minutes to complete. Please be patient ..." )

  # create an SQL query to extract all relavent data from tables
  # list of strings to build where clause in sql statement
  whereVec <- list()

  whereVec[[1]] <-  dbutils::createStringSpecies(itemName="nespp4",species,convertToCharacter=TRUE,numChars=3)
  whereVec[[2]] <-  dbutils::createString(itemName="year",year,convertToCharacter=TRUE,numChars=4)
  whereVec[[3]] <-  dbutils::createString(itemName="area",area,convertToCharacter=TRUE,numChars=3)

  # # sex conversion
  # if (tolower(sex) == "all") {
  #   sex <- c(0,1,2)
  # } else if (!is.numeric(sex)) {
  #   sex <- gsub("M",1,sex)
  #   sex <- gsub("F",2,sex)
  #   sex <- as.numeric(gsub("U",0,sex))
  # }
  #
  # whereVec[[4]] <-  paste("sex in (",toString(sex),")")


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

  # build where clause of SQL statement based on input above from Trip pull
  whereStrTrip <- NULL
  for (item in whereVec) {
    if (is.null(item)) {next}
    if (grepl("nespp4",item)){next}
    if (which(item == whereVec) == length(whereVec)) {
      whereStrTrip <- paste(whereStrTrip,item)
      next
    }
    whereStrTrip <- paste(whereStrTrip,item,"and")
  }

  # find unique trips in area and time of interest

  sqlTrip <- paste0("select distinct YEAR, MONTH, TRIPID, HAULNUM, AREA, NEGEAR, LATHBEG, LONHBEG from obdbs.obspp ")
  if (!is.null(whereStrTrip)) {  sqlTrip <- paste(sqlTrip," where ",whereStrTrip)}
  uniqueTrips <- DBI::dbGetQuery(channel,sqlTrip)

  # eventually user will be able to pass these variables
  # sql to get species data
  sqlStatement <- "select distinct YEAR, MONTH, TRIPID, HAULNUM, NEGEAR, NESPP4, LATHBEG, LONHBEG, AREA
                    from obdbs.obspp"

  sqlStatement <- paste(sqlStatement,whereStr)

  # call database to get species data
  query <-  DBI::dbGetQuery(channel,sqlStatement)

  #return (list(speciesOnly = dplyr::as_tibble(query),tripOnly=dplyr::as_tibble(uniqueTrips),sql=sqlStatement))

  # process the data to include absence/presence (0/1) for species listed.
  # left join
  join <- dplyr::left_join(uniqueTrips,query,by=c('YEAR', 'MONTH', 'TRIPID', 'HAULNUM', 'NEGEAR', "LATHBEG", "LONHBEG", "AREA"))
  join$NESPP4[!is.na(join$NESPP4)] <- as.numeric(1)
  join$NESPP4[is.na(join$NESPP4)] <- as.numeric(0)
  # rename new column
  colnames(join)[colnames(join) == "NESPP4"] <- "Presence"

  # column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'OBLEN' and owner='OBDBS';"
  colNames <-  DBI::dbGetQuery(channel,sqlcolName)

  return (list(data=dplyr::as_tibble(join),speciesOnly = dplyr::as_tibble(query),tripOnly=dplyr::as_tibble(uniqueTrips),sql=sqlStatement, colNames=colNames))
}



