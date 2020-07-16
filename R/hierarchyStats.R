
#' @title hierarchyStats
#' @description The hierarchyStats function computes summary statistics and span of control measures
#' from a standard set of unique employee and supervisor identifiers (employee IDs, email addresses, etc.).
#' @param ee An array containing unique identifiers for employees.
#' @param supv An array containing unique identifiers for supervisors. These values should be
#' of the same type as the employee values.
#' @import data.tree data.table
#' @export
#' @return list
#' @examples
#' ee = c("Dale@hR.com","Bob@hR.com","Julie@hR.com","Andrea@hR.com")
#' supv = c("Julie@hR.com","Julie@hR.com","Andrea@hR.com","Susan@hR.com")
#' hierarchyStats(ee,supv)

hierarchyStats = function(ee,supv){
  
  # Validate character type for inputs
  ee = as.character(ee)
  supv = as.character(supv)
  
  # Validate the input vectors for completeness and quality
  stopifnot(hierarchyValid(ee,supv))
  
  # Construct the tree and compute the statistics
  df = data.frame(ee,supv,stringsAsFactors=F)
  tryCatch({tree = FromDataFrameNetwork(df)},
           
           error=function(cond){
             
             message("The network is not a tree. Validate the data and ensure it reflects a complete, unbroken tree of employees and supervisors.")
             
           },
           
           finally={
             
             if(tree$height>2){
               
               # Store the resulting objects in a list
               output = list()
               
               # Total levels in the hierarchy
               output["levelsCount"] = list(
                 list(
                   value=tree$height,
                   description="Total levels in the hierarchy"
                 )
               )
               
               # Total number of individual contributors
               output["individualContributorCount"] = list(
                 list(
                   value=tree$leafCount,
                   description="Total number of individual contributors in the hierarchy"
                 )
               )
               
               # Total number of people managers
               output["peopleManagerCount"] = list(
                 list(
                   value=tree$totalCount-tree$leafCount,
                   description="Total number of people managers in the hierarchy"
                   )
               )
               
               # Recursively calculate the number of direct reports and total reports
               tree$Do(function(node) node$directReports = ifelse(node$isLeaf,0,length(node$children)))
               tree$Do(function(x) x$spanOfControl = sum(x$directReports,Aggregate(x,"directReports",sum)), traversal="post-order")
               tree$Do(function(x) x$spanOfControl = sum(x$directReports,Aggregate(x,"spanOfControl",sum)), traversal="post-order")
               
               # Compile the traversal statistics into a data table
               directReports = as.data.table(tree$Get("directReports"),keep.rownames=T)
               setnames(directReports,c("Employee","directReports"))
               spanOfControl = as.data.table(tree$Get("spanOfControl"),keep.rownames=T)
               setnames(spanOfControl,c("Employee","spanOfControl"))
               dt = merge.data.table(directReports,spanOfControl,by="Employee")
               output["spanOfControlTable"] = list(dt)
               
               # Median number of direct reports across the hierarchy
               output["medianDirectReports"] = list(
                 list(
                   value=median(dt$directReports),
                   description="Median number of direct reports across the hierarchy"
                 )
               )
               
               # Median span of control (total number of direct and indirect reports) across the hierarchy
               output["medianSpanOfControl"] = list(
                 list(
                   value=median(dt$spanOfControl),
                   description="Median span of control across the hierarchy. Span of control is defined as the total number of direct and indirect reports for a given person in the hierarchy."
                 )
               )
               
               return(output)
               
             }else{
               
               return(df[])
               
             }
             
           }
  )
  
}
