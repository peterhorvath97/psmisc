to_quarter_start <- function(dates) {
  # Ensure we’re working with Date objects
  d <- as_date(dates)
  
  # Floor each date to the start of its quarter
  qstart <- floor_date(d, unit = "quarter")
  
  # Return as character in YYYY-mm-dd format
  format(qstart, "%Y-%m-%d")
}

to_month_start <- function(dates) {
  # Ensure Date objects
  d <- as_date(dates)
  
  # Snap to first day of month
  mstart <- floor_date(d, unit = "month")
  
  # Return as character YYYY-mm-dd
  format(mstart, "%Y-%m-%d")
}