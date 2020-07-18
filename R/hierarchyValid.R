
#' @title hierarchyValid
#' @description The hierarchyValid function considers a standard set of unique employee and 
#' supervisor identifiers (employee IDs, email addresses, etc.) and validates the completeness
#' and quality of the two input vectors representing the overall hierarchy.
#' @param ee A vector containing unique identifiers for employees.
#' @param supv A vector containing unique identifiers for supervisors. These values should be
#' of the same type as the employee values.
#' @return logical
#' @export
#' @examples
#' ee = c("Dale@hR.com","Bob@hR.com","Julie@hR.com","Andrea@hR.com")
#' supv = c("Julie@hR.com","Julie@hR.com","Andrea@hR.com","Susan@hR.com")
#' hierarchyValid(ee,supv)

hierarchyValid = function(ee,supv){
  
  # Ensure the inputs are the same type
  if(class(ee)!=class(supv)){
    
    stop("Employee and supervisor inputs are different data types.")
    
  }
  
  # Point out NA values
  ee_na = sum(is.na(ee))
  supv_na = sum(is.na(supv))
  if(ee_na+supv_na > 0){
    
    stop(paste0("Missing values exist: ",ee_na," Employees, ",supv_na," Supervisors"))
    
  }
  
  # Ensure the inputs are of equal length
  if(length(ee)!=length(supv)){
    
    stop("Employee and supervisor inputs are of different lengths.")
    
  }
  
  # Check for a broken tree
  if(sum(!(supv %in% ee))>1){
    
    stop("The tree is broken. Make sure all employees roll up to a single person.")
    
  }
  
  # Check for employees reporting to themselves
  if(sum(supv==ee)>0){
    
    stop("At least one employee is reporting to himself/herself in the data.")
    
  }
  
  # If everything else passes, return TRUE
  return(TRUE)
  
}
