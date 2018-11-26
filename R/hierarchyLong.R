
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
#' @import data.tree data.table
#' @export
#' @return data frame
#' @examples
#' ee = c("Dale","Bob","Jill","Mark","Julie","Andrea","Susan")
#' supv = c("Julie","Julie","Julie","Andrea","Susan","Susan","George")
#' hierarchyLong(ee,supv)

hierarchyLong = function(ee,supv){

  if(is.factor(ee)) ee = as.character(ee)
  if(is.factor(supv)) supv = as.character(supv)
  if(class(ee)!=class(supv)){
    stop("Employee and supervisor inputs are different data types.")
  }else if(length(ee)!=length(supv)){
    stop("Employee and supervisor inputs are of different lengths.")
  }else{
    df = data.frame(ee,supv,stringsAsFactors=F)
    tryCatch(
      {tree = FromDataFrameNetwork(df)},
      error=function(cond){
        message("The network is not a tree! Make sure the data reflects complete, unbroken tree of employees and supervisors.")
        },
      finally={
        if(tree$height>2){
          x = 3:tree$height
          df[,x] = ""
          for(w in x){
            for(i in 1:nrow(df)){
              y = df$supv[df$ee==df[i,w-1]]
              df[i,w] = ifelse(length(y)>0,y,NA)
            }
          }
        }
        z = 2:ncol(df)
        colnames(df)[z] = z-1
        df = as.data.table(df)
        df = melt.data.table(df,id.vars=1)
        colnames(df) = c("Employee","Level","Supervisor")
        df = df[!is.na(df$Supervisor)]
        df = df[order(df$Employee,df$Level)]
        df = as.data.frame(df)
        return(df)
      }
      )
  }
}
