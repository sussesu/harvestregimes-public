################################################################'
##
## MS table 1
##
################################################################'

rm(list=ls())

library(tidyverse)

load("./outputs/basemaps_europe.RData")

## Set things ----

# Read files
data_annual <- readRDS("./data/processed/data_annual_22-08-23_RFready.rds")
census_stats_final <- readRDS("./data/processed/census_stats_final_22-08-23_RFready.rds")

table(census_stats_final$db_country)
table(data_annual$db_country)

dim(census_stats_final)
dim(data_annual)


n_table <- census_stats_final %>%
  group_by(country, database.code) %>%
  summarise(n = n(),
            n_harvest = sum(harvest01 == "HARVEST"))


#### Years of included data ----
yrs <- census_stats_final %>% 
  group_by(database.code, country) %>%
  summarize(n = n(),
             n_harvest = sum(harvest01 == "HARVEST"),
             min.date0 = floor(min(census.date0, na.rm=TRUE)),
             max.date0 = floor(max(census.date0, na.rm=TRUE)),
             min.date = floor(min(census.date, na.rm=TRUE)),
             max.date = floor(max(census.date, na.rm=TRUE)),
            interval_mean = round(mean(census_interval), 1)
            )


# yrs  %>% 
#   as.data.frame()

tbl1 <- yrs %>% 
  ungroup() %>%
  arrange(country) %>%
  mutate(meas1 = paste0(min.date0, "-", max.date0),
         meas2 = paste0(min.date, "-", max.date)) %>%
  select(country, meas1, meas2, interval_mean, n, n_harvest)

write.csv(tbl1, "./outputs/table1_details.csv", row.names=FALSE)

sum(tbl1$n)
sum(tbl1$n_harvest)

# total number of observed trees
sum(census_stats_final$n_stems_obs0)


