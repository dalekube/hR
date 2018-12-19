
#' @title hierarchyLong
#' @description The hierarchyLong function transforms a standard set of employee and
#' supervisor identifiers into a long format that can be used to aggregate employee data
#' by a particular line of leadership (i.e. include everyone who rolls up to Susan). The
#' function returns a long data frame consisting of one row per employee for every supervisor
#' above them, up to the top of the tree (i.e. the CEO in your organization). The levels represent
#' the number of supervisors from the employee (starting with "1" for an employee's direct supervisor).
#' @param ee A list of values representing employees (e.g. employee IDs).
#' @param supv A list of values representing the supervisors of the
#' employees. These values should be of the same data type as the employee values.
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

  # Ensure the inputs are the same type
  if(class(ee)!=class(supv)){

    stop("Employee and supervisor inputs are different data types.")

  # Ensure the inputs are of equal length
  }else if(
    sum(is.na(ee)) > 0 |
    sum(is.na(supv)) >0
  ){

    stop("Missing values exist.")

  # Ensure the inputs are of equal length
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
