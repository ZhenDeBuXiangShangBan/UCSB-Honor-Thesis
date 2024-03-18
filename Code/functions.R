library(Hmisc)


replace_labels <- function(dt_from, dt_to) {
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Hmisc package is not installed. Please install it using install.packages('Hmisc')")
  }
  
  for (col_name in names(dt_from)) {
    if (col_name %in% names(dt_to)) {
      label_to_apply <- label(dt_from[[col_name]])
      label(dt_to[[col_name]]) <- label_to_apply
    }
  }
  
  return(dt_to)
}
