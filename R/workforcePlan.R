
#' @title workforcePlan
#' @description Launch a simple, interactive workforce planning worksheet that helps managers and
#' team leaders to execute basic workforce planning tasks and plan ahead for hiring, turnover, and other
#' factors that influence a team's talent structure. Data analysts can use this alongside
#' team leaders to convey change and proactively think about recruitment, etc.
#' @import shiny
#' @import rhandsontable
#' @import data.table
#' @import knitr
#' @importFrom shinyjs useShinyjs show hide removeClass addClass hidden html
#' @import shinyFiles
#' @export
#' @examples
#' workforcePlan()

workforcePlan = function(){
  
  appDir = system.file("shiny-workforcePlan",package="hR")
  if(appDir==""){
    stop("Could not find example directory. Try re-installing `hR`.", call. = FALSE)
  }
  
  runApp(appDir,display.mode="normal")
}
