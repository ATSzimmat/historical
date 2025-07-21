hist_include <- function(new_data) {
  required_cols <- names(the_sources)
  if (!identical(names(new_data), required_cols)) {
    stop("Error: The column names or order of the columns in this dataset do not match those in 'the_sources'. You can rework the column names using dplyr::mutate() or the order of the columns using dplyr::select.")}
  classes_the_sources <- sapply(the_sources, class)
  classes_new_data <- sapply(new_data, class)
  if (!identical(classes_new_data, classes_the_sources)) {
    stop("Error: The data types of the columns do not match those in 'the_sources'. You can rework them using as.factor() and as.character().")
  }
  the_sources <<- bind_rows(the_sources, new_data)}
