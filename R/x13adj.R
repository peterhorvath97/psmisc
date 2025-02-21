x13adj <- function(value, freq, date){
  require(forecast)
  require(seasonal)
  require(tidyverse)
  require(lubridate)
  tryCatch(
    value %>% 
      ts(frequency = freq,
         start = c(date %>% min %>% year, 
                   date %>% min %>% month)) %>% 
      seas() %>% 
      seasadj() %>% 
      as.numeric(),
    error = function(error){value}
  )
}