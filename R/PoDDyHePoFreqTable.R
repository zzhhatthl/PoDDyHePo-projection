#' Description of PoDDyHePoFreqTable()
#' @title Frequency table Function Developed for PoDDy-Hepo Project
#'
#' @description PoDDyHePoFreqTable returns a table arranged by Gender and Year.
#'
#' @details The table includes Total, Missing and %.
#'
#' @param data A data frame with items that need to be projected.
#' @param colName A column that you want to have a frequency table.
#'
#'
#' @return a table
#' @export
#' 
#' @importFrom qwraps2 summary_table
#' @importFrom dplyr group_by


PoDDyHePoFreqTable <- function(data, colName){
  
  options(qwraps2_markup = "markdown")
  summary_statistic <- list("Male" =
                              list(
                                "Total" = ~ sum(!is.na(get("year")[which(sex == 1)])),
                                "Missing" = ~ sum(is.na(get(colName)[which(sex == 1)])),
                                "%" = ~ round(100 * mean(is.na(get(colName)[which(sex == 1)])), 1)
                              ),
                            "Female" =
                              list(
                                "Total" = ~ sum(!is.na(get("year")[which(sex == 2)])),
                                "Missing" = ~ sum(is.na(get(colName)[which(sex == 2)])),
                                "%" = ~ round(100 * mean(is.na(get(colName)[which(sex == 2)])), 1)
                              )
  )

  print(qwraps2::summary_table(
    group_by(data, get("year")),
    summary_statistic
  ))

}
    
