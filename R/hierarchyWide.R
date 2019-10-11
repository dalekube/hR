
#' @title hierarchyWide
#' @description The hierarchyWide function transforms a standard set of unique employee
#' and supervisor identifiers (employee IDs, email addresses, etc.) into a wide format
#' that can be used to aggregate employee data by a particular line of leadership (i.e. include
#' everyone who rolls up to Susan). The function returns a wide data.table with a column for
#' every level in the hierarchy, starting from the top of the tree (i.e. "Supv1" is likely
#' the CEO in your organization).
#' @param ee An array containing unique identifers for employees.
#' @param supv An array containing unique identifiers for supervisors. These values should be
#' of the same type as the employee values.
#' @import data.tree data.table
#' @export
#' @return data table
#' @examples
#' ee = c("Dale@hR.com","Bob@hR.com","Julie@hR.com","Andrea@hR.com")
#' supv = c("Julie@hR.com","Julie@hR.com","Andrea@hR.com","Susan@hR.com")
#' hierarchyWide(ee,supv)

hierarchyWide = function(ee,supv){

  # Validate character type for inputs
  ee = as.character(ee)
  supv = as.character(supv)

  # Ensure the inputs are the same type
  if(class(ee)!=class(supv)){

    stop("Employee and supervisor inputs are different data types.")

  }
  
  # Point out NA values
  if(sum(is.na(ee)) > 0 | sum(is.na(supv)) >0){

    stop("Missing values exist.")
    
  }
  
  # Ensure the inputs are of equal length
  if(length(ee)!=length(supv)){

    stop("Employee and supervisor inputs are of different lengths.")

  }

  df = data.frame(ee,supv,stringsAsFactors=F)

  tryCatch({tree = FromDataFrameNetwork(df)},

    error=function(cond){

      message("The network is not a tree. Validate the data and ensure it reflects a complete, unbroken tree of employees and supervisors.")

    },

    finally={

      # Traverse the tree and create a full table
      if(tree$height>2){
        
        x = 3:tree$height
        df[,x] = ""
        for(w in x){
          for(i in 1:nrow(df)){
            y = df$supv[df$ee==df[i,w-1]]
            df[i,w] = ifelse(length(y)>0,y,NA)
          }
        }
        
        # Shift rows with NAs to vertically align hierarchy values
        df = t(apply(df,1,function(x){c(x[is.na(x)],x[!is.na(x)])}))
        df = data.table(cbind(ee,df))
        
        # Remove supervisor names that equal the employee name
        df = cbind(df[,ee],df[,lapply(.SD,function(x) ifelse(x==ee,NA,x)),.SDcols=!"ee"])
        df = df[,colSums(!is.na(df))>0,with=F]
        
        # Reorder and set column names
        df = cbind(df[,(1),with=F],rev(df[,(2:ncol(df)),with=F]))
        supv.cols = c("Employee",paste0("Supv",seq(1,ncol(df)-1)))
        setnames(df,supv.cols)
        return(df[])
        
      }else{
        
        return(df[])
        
      }

    }
  )
  
}
