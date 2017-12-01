
## First time, run:
# install.packages(c("sp", "eurostat", "dplyr", "plm"))

library(sp)
library(eurostat)
library(dplyr)
library(plm)

# -----------------------------------------------------------------------------.
# Download data and format
# -----------------------------------------------------------------------------.
income    <- get_eurostat(id = "tgs00005")
broadband <- get_eurostat(id = "isoc_r_broad_h")


income_2015 <- income %>%
  filter(time == "2015-01-01",
         grepl(pattern = "FR.", x = geo)) %>%
  select(-c(unit, time)) %>%
  mutate(cat = cut_to_classes(values, 7, style = "quantile"))


broadband_2014 <- broadband %>%
  filter(time == "2014-01-01",
         grepl(pattern = "FR.", x = geo),
         unit == "PC_HH") %>%
  select(-c(unit, time)) %>%
  mutate(cat = cut_to_classes(values, 7, style = "quantile"))

# -----------------------------------------------------------------------------.
# Plot
# -----------------------------------------------------------------------------.
xlim <- c(-5, 10); ylim <- c(40, 52)

income_2015 %>%
  merge_eurostat_geodata(output_class = "spdf") %>%
  spplot("cat", xlim = xlim, ylim = ylim)

# broadband_2014 %>%
#   eurostat::merge_eurostat_geodata(output_class = "spdf") %>%
#   sp::spplot("cat", xlim = xlim, ylim = ylim)

# -----------------------------------------------------------------------------.
# Merge the two data frames
# -----------------------------------------------------------------------------.

data <-
  inner_join(broadband_2014, income_2015, by = "geo") %>%
  rename(broadband = values.x, income = values.y)

# -----------------------------------------------------------------------------.
# Plot again
# -----------------------------------------------------------------------------.

ols_model <- lm(broadband ~ income, data = data)
(results <- summary(ols_model))

plot(y = data$broadband,
     x = data$income,
     main = "The Relation between Internet Access and Income",
     ylab = "Broadband access in 2014",
     xlab = "Income in 2015")
abline(ols_model, col = "red")


# -----------------------------------------------------------------------------.
# Merge the two original data frames and estimate a panel
# -----------------------------------------------------------------------------.

data <-
  broadband %>%
  dplyr::filter(unit == "PC_HH") %>%
  inner_join(income, by = c("geo", "time")) %>%
  rename(broadband = values.x, income = values.y) %>%
  select(-c(unit.x, unit.y))

panel_model <- plm(formula = diff(log(income)) ~ lag(diff(log(broadband))),
                   data = data,
                   effect = "time")
panel_model_summary <- summary(panel_model)
print(panel_model_summary)

panel_model_summary$coefficients[1, 1]


