#' @title idPad
#' @description This function converts an employee ID into a specified format
#' that includes leading zeroes. HR systems often create employee IDs that have leading zeroes.
#' The leading zeroes can be lost when they are worked on in Excel or other tools. It's usually
#' helpful to convert the IDs back the correct format when performing lookups.
#'
#' @param id An employee ID.
#' @param len The standard length requirement for each employee ID. Leading zeroes
#' will be added to fill up the space to meet the requirement.
#' @import data.tree
#' @export
#' @return character value
#' @examples
#' # Using a string input
#' id = "12345"
#' len = 9
#' idPad(id,len)
#'
#' "000012345"
#'
#' # Using a numeric input
#' id = 54321
#' len = 9
#' idPad(id,len)
#'
#' "000054321"

idPad = function(id,len){
  if(!is.integer(len)){
    stop("'length' must be an integer'")
  }else{
    id = sprintf(paste0("%0",len,"d"),id)
  }
  return(id)
}

