# https://github.com/ropensci/targets/discussions/850
# modularise target pipeline


# READ IN SCRAPED CGIAR DATA ----------------------------------------------

get_data_from_csv <- function(file) {
  read_csv(file)
}

remove_na <- function(data) {
  data <- data %>% 
  replace_with_na_all(
    condition = ~.x %in% c("NA", "N/A", -9999, -9998)
  )
}

# first functions to work with monfreda data

# next, functions to work with agimpacts data in one function