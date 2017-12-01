# First time, run:
# install.packages(c("sp", "eurostat", "dplyr", "plm"))

library(eurostat)
library(dplyr)

# -----------------------------------------------------------------------------.
# Download data and format
# -----------------------------------------------------------------------------.
income    <- get_eurostat(id = "tgs00005")

# a quick look at the data
head(income)

# format data to our needs
income_2015 <- income %>%
  filter(time == "2015-01-01") %>%
  select(-c(unit, time))

# another look
head(income_2015)

# the 'normal' way to calculate this in R
income_2015_v2 <-
  select(filter(income, time == "2015-01-01"), -c(unit, time))

# Are they identical?
identical(income_2015, income_2015_v2)
