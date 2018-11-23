
#' @title hierarchyWide
#' @description This function takes employee and supervisor
#' identifiers (name, ID, etc.) and returns a wide data frame consisting of
#' a single row per employee and their respective reporting hierarchy in a wide format.
#'
#' The resulting format is very useful for subsequent aggregation of employee data
#' for a particular leadership tree.
#'
#' @param ee A list of values representing employees (e.g. employee IDs).
#' @param supv A list of values representing the supervisors of the employees. These values should be
#' of the same type as the employee values.
#' @import data.tree
#' @export
#' @return data frame
#' @examples
#' ee = c("Dale","Bob","Jill","Mark","Julie","Andrea","Susan")
#' supv = c("Julie","Julie","Julie","Andrea","Susan","Susan","George")
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
        df.names = df$ee
        df = t(apply(df,1,function(x){c(x[is.na(x)],x[!is.na(x)])}))
        df = as.data.frame(
          cbind(df.names,df),
          stringsAsFactors=F
          )
        
        # Remove supervisor names that equal the employee name
        n = ncol(df)
        df[2:n] = lapply(df[2:n],function(x) ifelse(x==df[,1],NA,x))
        df = df[colSums(!is.na(df)) > 0]
        
        # Reorder and set column names
        df = cbind(df[1],rev(df[2:ncol(df)]))
        supv.cols = paste0("Supv",seq(1,ncol(df)-1))  
        colnames(df) = c("Employee",supv.cols)
        return(df)
        
      }
    )
    
  }
}
