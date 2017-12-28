#' @title visRoleChanges
#' @description This function requires one data frame with two columns representing
#' the titles of employees from two different time periods.This function is useful
#' for visualizing "before-and-after" role changes.
#'
#' @param df A data frame with two columns. Column 1 should be the "before" job titles. Column 2 should be the "after" job titles".
#' @param supv A list of values representing the supervisors of the employees. These values should be
#' of the same type as the employee values.
#' @import visNetwork
#' @import dplyr
#' @export
#' @return visNetwork
#' @examples
#' df = data.frame(from=c(rep("builder",10),"tester","tester","builder","manager","builder","recruiter"),
#'                to=c("tester",rep("builder",10),"tester","builder","manager","builder","builder"))
#' visRoleChanges(df)

visRoleChanges = function(df){
  edges = as.data.frame(lapply(df,as.character),stringsAsFactors=F)
  edges = edges %>%
    group_by(from,to) %>%
    summarise(label=n()) %>%
    ungroup() %>%
    mutate(arrows="to")
  nodes = unique(c(edges$from,edges$to))
  nodes = data.frame(id=nodes,label=nodes,stringsAsFactors=F)
  total = edges %>%
    group_by(to) %>%
    summarise(value=sum(label)) %>%
    ungroup() %>%
    rename(id=to)
  nodes = merge(nodes,total)
  visNetwork(nodes,edges) %>%
    visOptions(highlightNearest=T)
}
