
#' Job history data for a sample team of employees and contractors.
#'
#' Artificial data that reflects the job history structure often used
#' to manage employment records in a human capital management system.
#'
#' @docType data
#' @usage data(jobHistory)
#' @format A data frame with 42 rows and 10 variables:
#' \describe{
#'   \item{DATE}{Effective date of the record}
#'   \item{SEQ}{Effective sequence of the record (used to manage multiple records for the same effective date)}
#'   \item{ACTION}{Action}
#'   \item{EMPLID}{Employee ID}
#'   \item{SUPVID}{Supervisor ID}
#'   \item{TYPE}{Employee type (employee or contractor)}
#'   \item{REGTEMP}{Regular, temporary, or contract employment}
#'   \item{TITLE}{Job title}
#'   \item{STATUS}{Employment status}
#'   \item{NAME}{Employee name}
#'   ...
#' }
"jobHistory"
