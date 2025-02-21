transform_to_base_index <- function(data, variables = NULL, base_year) {
  
  require(dplyr)
  
  # Ensure the input data has a date column
  if (!"date" %in% names(data)) {
    stop("The dataset must contain a 'date' column.")
  }
  
  # Set default variables to all columns except the date column
  if (is.null(variables)) {
    variables <- setdiff(names(data), "date")
  }
  
  # Extract the average values for the base year
  base_row <- data %>%
    filter(format(date, "%Y") == as.character(base_year)) %>%
    summarize(across(all_of(variables), \(x) mean(x, na.rm = TRUE)))
  
  if (nrow(base_row) == 0) {
    stop("No data found for the specified base year.")
  }
  
  # Extract the base year values for the selected variables
  base_values <- base_row %>%
    select(all_of(variables)) %>%
    unlist()
  
  # Transform the selected variables
  data_transformed <- data %>%
    mutate(across(all_of(variables), ~ . / base_values[deparse(substitute(.))] * 100))
  
  return(data_transformed)
}
