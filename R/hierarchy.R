
#' @title hierarchy
#' @description The hierarchy function transforms a standard set of unique employee and
#' supervisor identifiers (employee IDs, email addresses, etc.) into a wide or elongated format that
#' can be used to aggregate employee data by a particular line of leadership (i.e. include
#' everyone who rolls up to Susan).
#' @param ee A vector containing unique identifiers for employees.
#' @param supv A vector containing unique identifiers for supervisors. These values should be
#' of the same type as the employee values.
#' @param format character string; either "long" or "wide"; default = "long".
#' @param descending logical; default = TRUE. Should the hierarchy levels be descending 
#' (i.e. the top person in the hierarchy is represented at level 1)?
#' @import data.table
#' @export
#' @return data table
#' @examples
#' ee = c("Dale@hR.com","Bob@hR.com","Julie@hR.com","Andrea@hR.com")
#' supv = c("Julie@hR.com","Julie@hR.com","Andrea@hR.com","Susan@hR.com")
#' hierarchy(ee,supv,format="long",descending=TRUE)

hierarchy = function(ee, supv, format="long", descending=TRUE){
  
  Level = NULL
  Employee = NULL
  
  # Coerce to character data type
  ee = as.character(ee)
  supv = as.character(supv)
  
  if(!(format %in% c("long","wide"))){
    
    stop('Format must be "long" or "wide"')
    
  }
  
  # Validate the input vectors for completeness and quality
  hierarchyValid(ee, supv)
  
  # Recursively look up hierarchy
  df = data.table(ee,supv)
  setnames(df,c("Employee",1))
  tmp = copy(df)
  supv_count = 0
  valid = T
  while(valid){
    
    supv_count = supv_count+1
    prev_col = as.character(supv_count)
    new_col = as.character(supv_count+1)
    setnames(tmp,c(prev_col,new_col))
    df = merge(df,tmp,by=prev_col,all.x=T)
    valid = nrow(df[!is.na(get(new_col))]) > 0
    
  }
  df[,(new_col):=NULL]
  
  if(format=="long"){
    
    df = melt.data.table(
      df,
      id.vars="Employee",
      variable.name="Level",
      value.name="Supervisor",
      na.rm=T,
      variable.factor=F
      )
    df[,Level:=as.integer(Level)]
    
    if(descending){
      
      setorder(df,Employee,-Level)
      df$Level = df[,(1:.N),by=Employee]$V1
      return(df)
      
    } else {
      
      setorder(df,Employee,Level)
      return(df)
      
    }
    
  } else {
    
    if(descending){
      
      setcolorder(df,"Employee")
      df = cbind(
        df[,1],
        t(apply(df[,-1], 1, function(x) c(x[!is.na(x)], x[is.na(x)])))
        )
      supv_cols = paste0("Supv",seq(1,ncol(df)-1))
      setnames(df,c("Employee",supv_cols))
      return(df)
      
    } else {
      
      setcolorder(df,rev(colnames(df)))
      cols = colnames(df)[2:ncol(df)]
      cols = paste0("Supv",cols)
      colnames(df)[2:ncol(df)] = cols
      return(df)
      
    }
    
  }
  
}
