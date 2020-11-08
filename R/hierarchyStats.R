
#' @title hierarchyStats
#' @description The hierarchyStats function computes summary statistics and span of control metrics
#' from a standard set of unique employee and supervisor identifiers (employee IDs, email addresses, etc.).
#' @param ee A vector containing unique identifiers for employees.
#' @param supv A vector containing unique identifiers for supervisors. These values should be
#' of the same type as the employee values.
#' @import data.table stats
#' @export
#' @return list
#' @examples
#' ee = c("Dale@hR.com","Bob@hR.com","Julie@hR.com","Andrea@hR.com")
#' supv = c("Julie@hR.com","Julie@hR.com","Andrea@hR.com","Susan@hR.com")
#' hierarchyStats(ee,supv)

hierarchyStats = function(ee,supv){
  
  . = NULL
  PeopleManager = NULL
  Employee = NULL
  Supervisor = NULL
  Level = NULL
  
  # Construct the tree and compute the statistics
  dt = hierarchy(ee,supv,format="long",descending=F)
  dt[,PeopleManager:=ifelse(Employee %in% Supervisor,T,F)]
  supv_list = data.table(Employee=supv,PeopleManager=T)
  ee_list = unique(dt[,.(Employee,PeopleManager)])
  ee_list = rbindlist(list(ee_list,supv_list))
  ee_list = unique(ee_list)
  
  # Store the resulting objects in a list
  output = list()
  
  # Total levels in the hierarchy
  output["levelsCount"] = list(
    list(
      value=max(dt$Level)+1,
      description="Total levels in the hierarchy"
    )
  )
  
  # Total number of individual contributors
  output["individualContributorsCount"] = list(
    list(
      value=nrow(ee_list[PeopleManager==F]),
      description="Total number of individual contributors in the hierarchy"
    )
  )
  
  # Total number of people managers
  output["peopleManagersCount"] = list(
    list(
      value=nrow(ee_list[PeopleManager==T]),
      description="Total number of people managers in the hierarchy"
    )
  )
  
  # Compile  statistics into a data table
  directReports = dt[Level==1,.(directReports=.N),by=Supervisor]
  setnames(directReports,"Supervisor","Employee")
  spanOfControl = dt[,.(spanOfControl=.N),by=Supervisor]
  setnames(spanOfControl,"Supervisor","Employee")
  spanOfControl = merge(directReports,spanOfControl,by="Employee")
  output["spanOfControlTable"] = list(spanOfControl)
  
  # Median number of direct reports across the hierarchy
  output["medianDirectReports"] = list(
    list(
      value=median(spanOfControl$directReports),
      description="Median number of direct reports across the hierarchy"
    )
  )
  
  # Median span of control (total number of direct and indirect reports) across the hierarchy
  output["medianSpanOfControl"] = list(
    list(
      value=median(spanOfControl$spanOfControl),
      description="Median span of control across the hierarchy. Span of control is defined as the total number of direct and indirect reports for a given person in the hierarchy."
    )
  )
  
  return(output)
  
}
