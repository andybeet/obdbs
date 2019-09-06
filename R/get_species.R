#' Extract SPECIES information from OBDBS
#'
#' Extract a list of speices names, code, etc from OBDBS. These are same codes as in CFDBS
#'
#' @inheritParams cfdbs::get_species
#'
#' @section Database servers
#' CFDBS is on sole but ODBS is on nova.
#'
#' @export
#'

get_species <- function(channel,species="all"){

  return(cfdbs::get_species(channel,species="all"))

}



