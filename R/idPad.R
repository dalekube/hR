#' @title idPad
#' @description This function converts an all-integer employee ID into a specified format
#' that includes leading zeroes. HR systems often create employee IDs that have leading zeroes.
#' The leading zeroes can be lost when they are worked on in Excel or other tools. It's usually
#' helpful to convert the IDs back the correct format when performing lookups.
#' @param id An employee ID.
#' @param len The standard length requirement for each employee ID. Leading zeroes
#' will be added to fill up the space to meet the requirement.
#' @export
#' @return character value
#' @examples
#' idPad(id="12345",len=9)

idPad = function(id,len){
  id = as.integer(id)
  len = as.integer(len)
  sprintf(paste0("%0",len,"d"),id)
}

