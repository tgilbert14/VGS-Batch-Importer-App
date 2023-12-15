## assertthat library ----
library(assertthat)

data_quality_data_frame <- function(data) {
  assert_that(is.data.frame(data), msg = "Data not a Data Frame")
  #assert_that(all(colSums(!is.na(data)) > 0), msg = "Some columns are entirely NA.")
  # ... [rest of the function]
}

