 #' Description of PoDDyHePoProjection()
#' 
#' @title Projection Function Developed for PoDDy-HePo Project
#'
#' @description PoDDyHePoProjection is the main body for projection. 
#' 
#' @details It allows people to set up a variable that to be separated and returns a \code{\link[=mids-class]{mids}} obeject.
#' 
#' @param data A data frame with items that need to be projected.
#' @param m Number of multiple imputations. The default is \code{m=5}
#' @param sep_col A variable (usually has 3 levels or more) to be seperated. 
#' @param ... Formulas. Formulas 
#'
#' @return Returns an S3 object of class \code{\link[=mids-class]{mids}}
#'        (multiply imputed data set)
#'        
#' @export
#' 
#' @import mice
#' @importFrom fastDummies dummy_cols
#' @importFrom magrittr %>% 

PoDDyHePoProjection <- function(data, m = 5, sep_col = NULL, ...){   # ...
  
  # Imputation
  imp <- mice(data,
              m,
              maxit = 10,
              formulas = name.formulas(list(...)),
              printFlag = F
  )
  
  # If sep_col is NULL, wchih means do not have to separate the varaible proportionally.
  if(!is.null(sep_col)){

    # Long format
    imp.complete <- complete(imp, action = "long", include = T) 
    
    # Create dummies - if necesssary
    imp.complete <- imp.complete %>% 
      fastDummies::dummy_cols(select_columns = sep_col, ignore_na = T)
    
    # Transform back to mids
    imp <- as.mids(imp.complete)
  }
  
  return(imp)
}
