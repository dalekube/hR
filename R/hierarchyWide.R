#' @title hierarchyWide
#' @description This function takes two lists representing employee and supervisor
#' identifiers (name, ID, etc.) and returns a wide data frame consisting of
#' a single row per employee and their respective reporting hierarchy in a wide format.
#'
#' The resulting format is very useful for subsequent aggregation of employee data
#' for a particular leadership tree.
#'
#' @param ee A list of values representing employees (e.g. employee IDs).
#' @param supv A list of values representing the supervisors of the employees. These values should be
#' of the same type as the employee values.
#' @export
#' @return data frame
#' @examples
#' ee = c("Dale","Bob","Julie","Susan")
#' supv = c("Julie","Julie","Susan","George")
#' hierarchyWide(ee,supv)

hierarchyWide = function(ee,supv){
  if(is.factor(ee)) ee = as.character(ee)
  if(is.factor(supv)) supv = as.character(supv)
  if(class(ee)!=class(supv)){
    stop("Employee and supervisor inputs are different data types.")
  }else if(length(ee)!=length(supv)){
    stop("Employee and supervisor inputs are of different lengths.")
  }else{
    df = data.frame(ee,supv,stringsAsFactors=F)
    tree = FromDataFrameNetwork(df)
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
    df.new = t(apply(df,1,function(x){c(x[is.na(x)],x[!is.na(x)])}))
    df.new = data.frame(cbind(df$ee,df.new),stringsAsFactors=F)
    for(i in 2:ncol(df.new)){
      df.new[,i] = ifelse(df.new[,i]==df.new[1],NA,df.new[,i])
    }
    df.new = df.new[colSums(!is.na(df.new)) > 0]
    df.new = cbind(df.new[1],rev(df.new[2:ncol(df.new)]))
    colnames(df.new)[2:ncol(df.new)] = paste0("Supv",seq(1,ncol(df.new)-1))
    colnames(df.new)[1] = "Employee"
    return(df.new)
  }
}
