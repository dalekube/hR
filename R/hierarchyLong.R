#' @title hierarchyLong
#' @description This function takes employee and supervisor
#' identifiers (name, ID, etc.) and returns a long data frame consisting of
#' one row per employee for every supervisor above them, up to the CEO.
#'
#' The resulting format is useful for quickly filtering to a specific
#' part of the workforce that directly and indirectly reports up through
#' a particular leader.
#'
#' @param ee A list of values representing employees (e.g. employee IDs).
#' @param supv A list of values representing the supervisors of the employees. These values should be
#' of the same type as the employee values.
#' @export
#' @return data frame
#' @examples
#' ee = c("Dale","Bob","Julie","Susan")
#' supv = c("Julie","Julie","Susan","George")
#' hierarchyLong(ee,supv)

hierarchyLong = function(ee,supv){
  require(data.tree)
  require(dplyr)
  if(is.factor(ee)) ee = as.character(ee)
  if(is.factor(supv)) supv = as.character(supv)
  if(class(ee)!=class(supv)){
    stop("Employee and supervisor inputs are different data types.")
  }else if(length(ee)!=length(supv)){
    stop("Employee and supervisor inputs are of different lengths.")
  }else{
    df = data.frame(ee,supv,stringsAsFactors=F)
    tree = FromDataFrameNetwork(df)
    print("The hierarchy structure:")
    lev = max(print(tree,by="level")[,2])
    if(lev>2){
      for(i in 3:lev){
        df[,i] = ""
      }
      for(w in 3:lev){
        for(i in 1:nrow(df)){
          x = df$supv[df$ee==df[i,w-1]]
          df[i,w] = ifelse(length(x)>0,x,NA)
        }
      }
    }
    z = 2:ncol(df)
    colnames(df)[z] = z-1
    df = reshape2::melt(df,id=1) %>%
      select(Employee=1,Level=2,Supervisor=3) %>%
      filter(!is.na(Supervisor)) %>%
      arrange(Employee,Level)
    return(df)
  }
}
