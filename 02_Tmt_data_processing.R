################################################################'
##
## Processing the TMt-data for the management analysis
##
################################################################'

rm(list=ls())

library(tidyverse)
library(DescTools)
library(sf)
library(viridis)
library(patchwork)
library(ggcorrplot)
source("./src/read_tmt_data.R")
source("./src/basic_stats_nplot.R")
source("./src/sp_outside_range.R")

load("./outputs/basemaps_europe.RData")

###################################################################'
## Set things ----
###################################################################'

## Run for a subset of tree data only?
run_subset <- FALSE

## run native species part?
run_nativesp <- FALSE

## exclude data sets, give db_country (database.code + . + 3-letters of country)
exclude_db <- c("NFL.Bel", "FUN.Fin", "NSP.Spa") #"FUN.Spa")

## take harvest.status from mode.death for datasets (instead of defining it separately from original data)
db_harvest_from_mode.death <- c("NSW", "NCZ", "NFL", "NFR", "NPO1", "NSP", "NOR") #, "IRL")


# Functions ---------------------------------------------------------------

# Function for census summaries
summarize_census <- function(df) {
  # calculate census level summary statistics (used in all subsets of census_stats)
  df_out <- df %>%
    # mutate(w = ifelse(tree.status == 0 | is.na(n.ha), 1/n.ha, NA)) %>%
    summarize(gini_d_nplot = DescTools::Gini(d, n = round(n.ha)),
              n_stems_obs = n(),
              n_uniq_d = n_distinct(d),
              ba_total = sum(ba * n.ha),
              ba_species_max = max(ba_species),
              ba_percent_dom = ba_species_max/ba_total,
              species_dom = species.cor[ba_species == ba_species_max][1],
              genus_dom = genus.cor[ba_species == ba_species_max][1],
              family_dom = family.cor[ba_species == ba_species_max][1],
              d_mean_ht = mase::horvitzThompson(d, pi = w)$pop_mean,
              d_mean_nha = mean_nplot(d, n.ha),
              n_stems_ha = sum(n.ha), 
              Dq_mean = qmean_nplot(d, n.ha),
              SDI = SDI(n_stems_ha, Dq_mean))
  return(df_out)
}


###################################################################'
## 1. Preps and data in ----
###################################################################'

# give database.codes for data coming from the TMt database
db_tmt_ready <- c("FUN", "NCZ", "NFG", "NFL", "NFR", "NNL", "NSI") #, "IRL") #, "NSP")
# db_tmt_ready <- c("FUN") #, "IRL") #, "NSP")

db_no_processed <- c() #"IRL" # if no tree_biomass file availables

##### Small definitions ----

# data string for file names
fl_date <-format(Sys.Date(), "%d-%m-%y")

# projection def
proj_lambert_conic <- "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs"

##### Read TMt data ----

# tree
tree <- read_tmt("treedata_biomass", datatype="Processed_data", database.code = db_tmt_ready)
tree_noProcessed <- read_tmt("tree", database.code = db_no_processed)

common_colnames <- Reduce(intersect, lapply(list(tree, tree_noProcessed), colnames))
tree <- tree %>% select(any_of(common_colnames)) %>%
  rbind(tree_noProcessed %>% select(any_of(common_colnames)))

# census_info
census_info <- read_tmt("census", database.code = c(db_tmt_ready, db_no_processed))

# plot_info
plot_info <- read_tmt("plot", database.code = c(db_tmt_ready, db_no_processed))

# stand_dynamics
stand_dynamics <- read_tmt("stand_dynamics", datatype="Processed_data", database.code = db_tmt_ready)

# # traits
# traits <- read_tmt("CWMalive", datatype="Processed_data", database.code =  c("FUN", "NCZ", "NFG", "NFL", "NNL", "NSI", "NPO"))

tree <- tree %>% select(!any_of("X"))
census_info <- census_info %>% select(!any_of("X"))
plot_info <- plot_info %>% select(!any_of("X"))
stand_dynamics <- stand_dynamics %>% select(!any_of("X"))
# traits <- traits %>% select(!any_of(c("X", "g.plot", "n.plot")))

#### Fixes ----
tree$species.cor[tree$species.cor == "Taxus baccata "] <- "Taxus baccata"

#### Add/update data sets ----
###### a. Poland ----
data_path_NPO <- "./data/processed/Poland_filtered/"
tree_NPO <- read.csv(paste0(data_path_NPO, "01_treedata_NPO1.csv"), row.names = 1)
census_info_NPO <- read.csv(paste0(data_path_NPO, "03_census_info_NPO1.csv"), row.names = 1)
plot_info_NPO <- read.csv(paste0(data_path_NPO, "04_plot_info_NPO1.csv"), row.names = 1)

tree_NPO$d <- ifelse(tree_NPO$tree.status == 1, 0, tree_NPO$d)

# alive trees with diameter as NA --> remove(?)
tree_NPO <- tree_NPO %>%
  filter(!(tree.status == 0 & is.na(d)))

tree_NPO %>% 
  group_by(database.code) %>% 
  mutate(census1 = min(census.n)) %>% 
  select(tmt.tree.id, database.code, census.n, census1, d) %>% 
  filter(census.n == census1 & database.code != "IRL") %>% 
  with(summary(d))

tree <- rbind(tree, tree_NPO[,intersect(colnames(tree_NPO), colnames(tree))])
census_info <- rbind(census_info, census_info_NPO[,intersect(colnames(census_info_NPO), colnames(census_info))])
plot_info <- rbind(plot_info, plot_info_NPO[,intersect(colnames(plot_info_NPO), colnames(plot_info))])

###### b. Sweden -----
data_path_NSW <- "/Users/suvantss/Documents/ForMMI/TMt_data_workshop/Sweden_update/output/NSW/"
tree_NSW <- read.csv(paste0(data_path_NSW, "01_qc-treedata_TMt_NSW.csv"), row.names = 1)
census_info_NSW <- read.csv(paste0(data_path_NSW, "03_census-info_TMt_NSW.csv"), row.names = 1)
plot_info_NSW <- read.csv(paste0(data_path_NSW, "04_plot-info_TMt_NSW.csv"), row.names = 1)

tree_NSW[,setdiff(colnames(tree), colnames(tree_NSW))] <- NA
tree_NSW <- tree_NSW[, colnames(tree)]
census_info_NSW <- census_info_NSW[, colnames(census_info)]

tree <- rbind(tree, tree_NSW)
census_info <- rbind(census_info, census_info_NSW)
plot_info <- rbind(plot_info, plot_info_NSW)


###### c. Finland ----
tree_fin <- read.csv("./data/raw/VMI/TMt_format/01_qc-treedata_TMt_FIN_18-03-22.csv")
census_info_fin <- read.csv("./data/raw/VMI/TMt_format/03_census-info_TMt_FIN_18-03-22.csv")
plot_info_fin <- read.csv("./data/raw/VMI/TMt_format/04_plot-info_TMt_FIN_18-03-22.csv")

# colnames(tree_fin)

# setdiff(colnames(tree), colnames(tree_fin))
# setdiff(colnames(census_info), colnames(census_info_fin))
# setdiff(colnames(plot_info), colnames(plot_info_fin))

tree_fin <- tree_fin %>%
  mutate(biomass = NA,
         biomass.plot = NA,
         biomass.ha = NA,
         ba.ha = g.plot) %>%
  select(any_of(colnames(tree)))

census_info_fin <- census_info_fin %>%
  select(any_of(colnames(census_info)))

plot_info_fin <- plot_info_fin %>%
  select(any_of(colnames(plot_info)))

tree <- rbind(tree, tree_fin)
census_info <- rbind(census_info, census_info_fin)
plot_info <- rbind(plot_info, plot_info_fin)

# check duplicates
summary(duplicated(tree$tmt.obs.id))
summary(duplicated(census_info$tmt.census.id))
summary(duplicated(plot_info$tmt.plot.id))


###### e. Norway ----
datapath_NOR <- "./data/raw/Norway/processed_susanne/"

tree_NOR <- read.csv(paste0(datapath_NOR, "01_qc-treedata_TMt_NOR.csv"), row.names = 1)
census_info_NOR <- read.csv(paste0(datapath_NOR, "03_census-info_TMt_NOR.csv"), row.names = 1)
plot_info_NOR <- read.csv(paste0(datapath_NOR, "04_plot-info_TMt_NOR.csv"), row.names = 1)

tree_NOR[,setdiff(colnames(tree), colnames(tree_NOR))] <- NA
tree_NOR <- tree_NOR[, colnames(tree)]
census_info_NOR <- census_info_NOR[, colnames(census_info)]
plot_info_NOR <- plot_info_NOR[, colnames(plot_info)]

# combine
tree <- rbind(tree, tree_NOR)
census_info <- rbind(census_info, census_info_NOR)
plot_info <- rbind(plot_info, plot_info_NOR)

table(tree$database.code)

rm(tree_NOR, census_info_NOR, plot_info_NOR,
   tree_NSW, census_info_NSW, plot_info_NSW,
   tree_NPO, census_info_NPO, plot_info_NPO,
   # tree_NSP, census_info_NSP, plot_info_NSP,
   tree_fin, census_info_fin, plot_info_fin)


#### Harvest status ----

fls_names <- list.files("./data/processed/harvest_status/",
                        full.names = TRUE)
fls_names <- setdiff(fls_names, list.dirs("./data/processed/harvest_status/")) # remove subdirectory
fls_names <- fls_names[!grepl(pattern="NFL", x = fls_names)] # exclude NFL - use data in TMt mode-of-death

harvest_lst <- lapply(fls_names, read.csv)

common_colnames <- Reduce(intersect, lapply(harvest_lst, colnames))
harvest_lst <- lapply(harvest_lst, function(x) x[,common_colnames])
harvest_df <- do.call(rbind, harvest_lst)
harvest_df$in_harvestdf <- TRUE

## CHECK DUPLICATES! Should be all FALSE
table(duplicated(harvest_df$tmt.obs.id), harvest_df$database.code) 

dim(harvest_df)
dim(tree)

tree <- tree %>%
  left_join(harvest_df) %>%
  # for some db's derive harvest.status from mode.death
  mutate(harvest.mode.death = case_when(
    substr(mode.death, 1, 1) == "2" ~ 1,
    substr(mode.death, 1, 1) == "1" ~ 0,
    mode.death == "3" ~ 1,
    mode.death == "4" ~ as.numeric(NA)),
    harvest.status = ifelse(database.code %in% db_harvest_from_mode.death, 
                            harvest.mode.death,
                            harvest.status),
    harvest.status = ifelse(tree.status == 1, harvest.status, 0)) # set harvest status of living trees to 0

with(tree%>% filter(tree.status == 1), table(database.code, harvest.status, useNA = "always") )
with(tree %>% filter(tree.status == 0), table(database.code, harvest.status, useNA = "always") )

# NAs in harvest.status for trees with mode.death = 2 in NFG?? These are all census.n = 2 -> not included in this work, so no worries!
tree %>% filter(database.code == "NFG" & mode.death == 2 & is.na(harvest.status) & census.n == 3)

round(prop.table(table(tree$database.code, tree$harvest.status), 1),2)

dim(tree)

rm(harvest_lst)

##### Check: trees without census info?

tree_no_census <- tree %>%
  filter(!(tmt.census.id %in% census_info$tmt.census.id) )
with(tree_no_census, table(database.code))  #many NSI and a few NSW

tree %>%
  mutate(missing_census = !(tmt.census.id %in% census_info$tmt.census.id)) %>%
  group_by(tmt.census.id) %>%
  mutate(all_or_none_missing = all(missing_census) | all(!missing_census)) %>%
  filter(missing_census) %>%
  arrange(tmt.census.id) %>%
  with(table(mode.death))
  # as.data.frame %>% head()
  # with(table(all_or_none_missing))

tree %>%
  filter(!(tmt.plot.id %in% plot_info$tmt.plot.id) ) %>%
  with(table(database.code))

# Check that all trees d > 100mm on first measurement ----
# - Removing trees with d < 100mm in the first (used) measurement
#   This can happen typically, if tree was >100mm in the inventory before,
#   but has now shrunk for some reason for the 1st measurement used here.
tree %>% 
  group_by(database.code) %>% 
  mutate(census1 = max(census.n),
         census0 = census1-1) %>% 
  select(tmt.tree.id, database.code, census.n, census1, census0, d, tree.status) %>% 
  filter(census.n == census0 & tree.status == 0 & d < 100) %>%
  dim()

idx_remove <- tree %>%
  group_by(database.code) %>% 
  mutate(census1 = max(census.n),
         census0 = census1-1,
         tooSmall1 = census.n == census0 & d < 100 & tree.status == 0) %>%
  filter(tooSmall1) %>%
  pull(tmt.tree.id)

# check how many trees this is (now 497)
length(idx_remove)

tree <- tree %>%
  filter(!(tmt.tree.id %in% idx_remove))

#### Subsample? ----

if(run_subset) {
  print("subsetting tree data...")
  set.seed(48)
  plotid_subsample <- plot_info %>% sample_n(1e4)
  tree_subset <- tree %>% 
    filter(tmt.plot.id%in%plotid_subsample$tmt.plot.id) 
  tree <- tree_subset
}

#### Years of included data ----
yrs <- census_info %>% 
  left_join(plot_info %>% select(tmt.plot.id, country)) %>%
  mutate(db_country = paste(database.code, substr(country, 1, 3), sep=".")) %>%
  filter(!db_country %in% exclude_db) %>%
  group_by(database.code, country, census.n) %>%
  summarize(min.date = floor(min(census.date, na.rm=TRUE)),
            max.date = floor(max(census.date, na.rm=TRUE))) # %>%
  # filter(db_country %in% c("NSW.Sweden", "NSI.Switzerland", "NPO.Poland",
  #                          "NNL.Netherlands", "NFG.Germany", "FUN.Spain",
  #                          "FUN.Belgium", "FIN.Finland"))


yrs_n <- census_info %>% 
  left_join(plot_info %>% select(tmt.plot.id, country)) %>%
  mutate(db_country = paste(database.code, country, sep=".")) %>%
  rowwise() %>%
  mutate(year = floor(min(census.date, na.rm=TRUE))) %>% ungroup() %>%
  group_by(db_country, census.n, year) %>%
  summarize(n = n())


yrs %>% 
  arrange(country, database.code, desc(census.n)) %>%
  group_by(country, database.code) %>%
  slice_head(n=2) %>% 
  mutate(census_status = ifelse(census.n == max(census.n), 
                         "harvest",
                         "preharvest"),
         years = paste(min.date, max.date, sep="-")) %>%
  select(country, database.code, census_status, years) %>%
  pivot_wider(names_from = census_status, 
              values_from = years) %>%
  select(country, database.code, preharvest, harvest) %>%
  write.csv("./outputs/years_table.csv")

###################################################################'
##
## 2. Tree data per census ----
## - harvesting / no harvesting check
## - size structure (gini)
## - species composition (ba% of dominant species)
##
###################################################################'

### Tree stats, census1 ----

tree1 <- tree %>%
  group_by(database.code) %>%
  mutate(max.census = max(census.n),
         ba = ba/1e6) %>%
  group_by(tmt.plot.id) %>%
  mutate(total.n.census = n_distinct(census.n)) %>%
  filter(census.n == max.census & total.n.census > 1) %>%
  mutate(w = ifelse(tree.status == 0, 1/n.ha, NA)) %>%
  ungroup()

with(tree, table(database.code, census.n))
with(tree1, table(database.code, census.n))

census1_stats <- tree1 %>%
  # filter(tree.status == 0) %>%
  mutate(tree.status = ifelse(tree.status == 0, "", "DEAD")) %>%
  group_by(tmt.census.id, species.cor) %>%
  mutate(ba_species = sum(ba * n.ha)) %>%
  group_by(database.code, tmt.plot.id, tmt.census.id, census.n, tree.status) %>%
  summarize_census() 

census1_stats <- census1_stats %>% # convert to wide form
  pivot_wider(names_from = tree.status,
              names_sep="",
              values_from = gini_d_nplot:last_col()) %>%
  rename(n_stems_dead_obs = n_stems_obsDEAD) %>%
  select(!ends_with("DEAD")) %>%
  filter(!tmt.census.id%in%c("NSI.143880.4", "NSI.45341.4"))  #weird duplicates of tmt.plot.id + census.n, ask A.

# Test: are all ba-NAs cases with no living trees? (Should be empty tibble)
census1_stats %>% 
  filter(is.na(ba_total)) %>%
  filter(!is.na(n_stems_obs)) %>%
  select(tmt.census.id, ba_total, n_stems_obs) 


if(any(duplicated(tree1$tmt.tree.id))) {
  stop("Duplicated tree IDs in tree1 !!!!")
}

### Tree stats, census0 ----

tree0 <- tree %>%
  group_by(database.code) %>%
  mutate(max.census = max(census.n),
         ba = ba/1e6) %>%
  group_by(tmt.plot.id) %>%
  mutate(total.n.census = n_distinct(census.n)) %>%
  ungroup() %>%
  filter(census.n == (max.census -1) & total.n.census > 1) %>%
  filter(tree.status == 0) %>% # only living trees from the first census!
  mutate(w = ifelse(tree.status == 0, 1/n.ha, NA)) %>%
  ungroup()

with(tree, table(database.code, census.n))
with(tree0, table(database.code, census.n))

census0_stats <- tree0 %>%
  group_by(tmt.census.id, species.cor) %>%
  mutate(ba_species = sum(ba * n.ha)) %>%
  group_by(database.code, tmt.plot.id, tmt.census.id, census.n) %>%
  summarize_census()  %>%
  rename_at(vars(!c("database.code", "tmt.census.id", "tmt.plot.id", "census.n")), ~ str_c(., "0")) %>%
  ungroup()

head(census0_stats)
summary(is.na(census0_stats$ba_total0))

### Harvested and dead trees stats ----
### stats from census0, status from census1

tree_status1 <- tree1 %>%
  select(database.code, tmt.plot.id, tmt.tree.id, census.n,
         tree.status, harvest.status) %>%
  rename(tree.status1 = tree.status, 
         harvest.status1 = harvest.status) %>%
  mutate(census.n.old = census.n,
         census.n = census.n - 1) # this to join with the prev. census tree data

if(any(is.na(tree_status1$tree.status1))) {
  stop("NAs in tree.status1 !!!")
}

tree0_status1 <- tree0 %>%
  left_join(tree_status1) 

dim(tree0)
dim(tree0_status1)

### CHECKS ----
# Last run: Total of 4222 trees missing in the last census. See comments on database notes to A.
# These are mainly plots that were not measured in the last census (?)
# (a lot more in NSP, but not using that right now)
n_status_NA <- sum(is.na(tree0_status1$tree.status1))
warning(paste("Total of", n_status_NA, "trees missing in the last census. Plots not measured? These are removed."))

table(tree0_status1$database.code, is.na(tree0_status1$tree.status1))

tree0_status1 %>%
  group_by(tmt.census.id) %>%
  summarise(any_status1_NA = any(is.na(tree.status1)),
            all_status1_NA = all(is.na(tree.status1)) ) %>%
  filter(any_status1_NA & !all_status1_NA)

# census_stats for harvested vs non-harvested trees
census_stats_harvested <- tree0_status1 %>%
  filter(!is.na(tree.status1)) %>% # remove trees that were not measured in the latest census
  mutate(harvest.status1 = ifelse(harvest.status1 == 0 | is.na(harvest.status1), 
                                  "NONHARVEST", "HARVEST")) %>%
  group_by(tmt.census.id, harvest.status1, species.cor) %>%
  mutate(ba_species = sum(ba * n.ha)) %>%
  group_by(database.code, tmt.plot.id, tmt.census.id, census.n, harvest.status1) %>%
  summarize_census() %>%
  pivot_wider(names_from = harvest.status1,
              values_from = gini_d_nplot:last_col(),
              names_sep= ".") %>%
  ungroup()

# stats for dead trees (DEAD) and trees that are still alive in the last census (SURVIVOR)
census_stats_dead <- tree0_status1 %>%
  filter(!is.na(tree.status1)) %>% # remove trees that were not measured in the latest census
  mutate(tree.status1 = ifelse(tree.status1 == 1, "DEAD", "SURVIVOR")) %>%
  group_by(tmt.census.id, tree.status1, species.cor) %>%
  mutate(ba_species = sum(ba * n.ha)) %>%
  group_by(database.code, tmt.plot.id, tmt.census.id, census.n, tree.status1) %>%
  summarize_census() %>%
  pivot_wider(names_from = tree.status1,
              values_from = gini_d_nplot:last_col(),
              names_sep= ".") %>%
  ungroup()

### Combine ----

census0_stats <- census0_stats %>% # rename here to get census-dependent IDs from census1
  rename_at(vars(c("tmt.census.id", "census.n")), ~ str_c(., "0")) 

census_stats_final <- census0_stats %>% # stats calculated from trees in the previous census (census0)
  left_join(census1_stats %>% ungroup()) %>% # stats calculated from trees in the latest census (census1)
  left_join(census_stats_harvested %>% select(!any_of(c("tmt.census.id", "tmt.obs.id", "census.n")))) %>% # stats calculated from census0 for trees that were harvested/non-harvested in census1
  left_join(census_stats_dead %>% select(!any_of(c("tmt.census.id", "tmt.obs.id", "census.n")))) %>% # stats calculated from census0 for trees that were dead in census1
  left_join(plot_info %>% select(!plot.area)) %>%
  left_join(census_info %>% select(!plot.id)) %>%
  mutate(db_country = factor(paste(database.code, substr(country, 1, 3), sep=".")) )

dim(census0_stats)
dim(census_stats_final)
dim(census1_stats)
dim(census_stats_harvested)

colnames(census_stats_final)

summary(census_stats_final$n_stems_obs.HARVEST)

### New harvest variables ----      

census_stats_final <- census_stats_final %>%
  mutate(n_stems_obs.HARVEST = ifelse(!is.na(n_stems_obs.HARVEST), n_stems_obs.HARVEST, 0), #assuming harvest.status NAs are not harvest
         harvest_any = n_stems_obs.HARVEST > 0,
         harvest_percent_stems = n_stems_obs.HARVEST/n_stems_obs0,
         harvest_percent_ba = ba_total.HARVEST/ba_total0,
         harvest_d_cm = d_mean_nha.HARVEST/10,
         harvest_size_diff = d_mean_nha.HARVEST/10 - d_mean_nha.NONHARVEST/10,
         harvest_size_rel = harvest_size_diff/ (d_mean_nha.NONHARVEST/10),
         harvest_3class = factor(case_when(
           !harvest_any ~ "NO_HARVEST",
           harvest_any & harvest_percent_stems < 1 ~ "PARTIAL_CUT",
           harvest_any & harvest_percent_stems == 1 ~ "ALL_CUT"),
           levels = c("NO_HARVEST", "PARTIAL_CUT", "ALL_CUT")) )


### Checks ----

# tmt.census.id is NA? Mainly NNL, these seem to be plots that are not measured in
# census 7, but rows are created here through the 6th meas.
# --> remove
census_stats_final %>%
  filter(is.na(tmt.census.id)) %>%
  select(tmt.plot.id, tmt.census.id, tmt.census.id0)

census_info %>% filter(tmt.plot.id == "NNL.11137")

census_stats_final <- census_stats_final %>%
  filter(!is.na(tmt.census.id))

# NAs in census1 stats 
# These are cases where all trees have died -- stats only calculated from living trees!
# But: if regrowth after census0, these are not necessarily NA, even if the trees in census0 are dead.
table(census_stats_final$database.code, is.na(census_stats_final$n_stems_obs))
table(census_stats_final$n_stems_obs.DEAD == census_stats_final$n_stems_obs0,
        is.na(census_stats_final$n_stems_obs))

census_stats_final %>%
  filter(n_stems_obs.DEAD == n_stems_obs0 & !is.na(n_stems_obs)) %>%
  select(tmt.plot.id,  n_stems_obs.DEAD, n_stems_obs0, n_stems_obs) 

tree %>% filter(tmt.plot.id == "FIN.1534054_3") %>%
  select(tmt.plot.id, census.n, tmt.tree.id, tree.status) %>%
  arrange(tmt.tree.id, census.n)
  
# + 401 NAs in census1 stats needing n.ha for Finland
# these are most likely trees that are no longer part of sample plot in vmi12 -> n.ha is NA and everything that uses it is too
# --> no need for action, these are currently not used anyway. If these are needed later, check the notes in the 0X_process_VMI_data.R script.
table(census_stats_final$database.code, is.na(census_stats_final$d_mean_ht) & !is.na(census_stats_final$n_stems_obs))

census_stats_final %>%
  filter(is.na(d_mean_ht) & !is.na(n_stems_obs)) %>%
  select(tmt.census.id, d_mean_ht, n_stems_obs)

#proportion of harvested plots in all plots (latest census only)
sum(census_stats_final$harvest_any, na.rm=TRUE) / nrow(census_stats_final)*100
#proportion of harvested plots in plots with no NAs in harvest_any (latest census only)
sum(census_stats_final$harvest_any, na.rm=TRUE) / sum(!is.na(census_stats_final$harvest_any))*100

#proportion of NA harvest plots  (latest census only)
round(sum(is.na(census_stats_final$harvest_any)) / nrow(census_stats_final)*100, 1)

###################################################################'
##
## 3. Species ----
##
###################################################################'

### Conifers & species fixes ----

gymno_fam <- read.table("./data/processed/species_class/gymnosperm_families.txt", 
                        skip=1, header=1)

# fix NAs in family (3 x Larix indet in FIN)
census_stats_final %>% filter(is.na(family_dom0)) %>% select(tmt.census.id, species_dom0, genus_dom0, family_dom0)

# Fix species
census_stats_final$species_dom0[census_stats_final$species_dom0 == "Eucalyptus gomphocephalus"] <- "Eucalyptus gomphocephala"
census_stats_final$species_dom0[census_stats_final$species_dom0 == "Salix elaeagnos"] <- "Salix eleagnos"
census_stats_final$species_dom0[census_stats_final$species_dom0 == "Fagus silvatica"] <- "Fagus sylvatica"
census_stats_final$species_dom0[census_stats_final$species_dom0 == "Malus silvestris"] <- "Malus sylvestris"

# Fix family information
sp_table <- census_stats_final %>%
  select(species_dom0) %>%
  distinct()

sp_check <- Taxonstand::TPL(sp_table$species_dom0)

fam_table <- sp_check %>% select(Taxon, Family) %>%
  mutate(genus_dom0 = word(Taxon, 1)) %>%
  select(genus_dom0, Family) %>%
  filter(Family != "") %>%
  distinct() %>%
  arrange(genus_dom0) 

if(any(duplicated(fam_table$genus_dom0))) {
  stop("Duplicates of genus in fam_table!!")
}

# saveRDS(fam_table, file="./data/processed/family_table.rds")

census_stats_final <- census_stats_final %>%
  left_join(fam_table) %>%
  mutate(family_dom0 = ifelse(is.na(family_dom0), Family, family_dom0))

census_stats_final %>% filter(is.na(family_dom0)) %>% select(tmt.census.id, species_dom0, genus_dom0, Family, family_dom0)

census_stats_final <- census_stats_final %>%
  mutate(conifer = family_dom0 %in% gymno_fam$family)

################################################################'
##
##  4. Socio-economic variables ----
## - average share of forestry on GDP over 1990-2011 (FAO)
## - share of protected forests (for 2015, other years also available!) 
## - forest ownership
## - workforce in forestry
##
################################################################'

dir_foresteurope <- "./data/raw/FORESTEUROPE2020/"
# list.files(dir_foresteurope)

#### GDP Forestry share ----

gdp_share <- read.csv("./data/raw/economy/GDP_share_forestry/gdp_share_FAO.csv")

gdp_share <- gdp_share %>%
  pivot_longer(cols = X1990:X2011,
               names_to = "year",
               names_prefix = "X",
               values_to = "GDP_share") %>%
  mutate(year = as.numeric(year))

gdp_share_av <- gdp_share %>%
  group_by(Country) %>%
  summarise(gdp_share = mean(GDP_share)) %>%
  rename(country = Country) 

#### Protected areas share ----

# forest area as 1000 ha
forestarea2015 <- read.csv(paste0(dir_foresteurope, "panEuropean-forestArea_forests_2015.csv"),
                           skip = 1)
forestarea2015 <- forestarea2015 %>% rename(country = X, forest_area = Area) %>%
  slice(1:(n()-2)) 

protectedarea2015 <- read.csv(paste0(dir_foresteurope, "panEuropean-protectedForests_2015.csv" ),
                              skip=1)
protectedarea2015 <- protectedarea2015 %>%
  slice(1:(n()-2)) %>% 
  rename(country = X) %>%
  slice(1:(n()-2)) %>%
  left_join(forestarea2015) %>%
  rowwise() %>%
  mutate(total_class1 = ifelse(is.na(MCPFE.Class.1.1) &
                                 is.na(MCPFE.Class.1.2) &
                                 is.na(MCPFE.Class.1.3),
                               NA, 
                               sum(MCPFE.Class.1.1, MCPFE.Class.1.2, MCPFE.Class.1.3, na.rm=TRUE)),
         protected_share_class1 = total_class1 / forest_area)
  
    
protectedarea2015 <- protectedarea2015 %>% select(country, total_class1, forest_area, protected_share_class1)

# total forest area covered
forestarea2015 %>%
  filter(country %in% c("Sweden", "Finland", "Spain", "France", "Germany",
                        "Poland", "Czechia","Netherlands", "Switzerland",
                        "Norway", "Belgium")) %>%
  with(sum(forest_area) - 155503.0/1000) # taking out Flanders from Belgium

#### Ownership -----

private2015 <- read.csv(paste0(dir_foresteurope, "panEuropean-forestHoldings_private_2015.csv"), skip=1) %>%
  slice(1:(n()-2)) %>% 
  rename(country = X,
         area = Total.forest.area..1000.ha.) %>%
  mutate(ownership = "private") %>%
  select(country, ownership, area)

public2015 <- read.csv(paste0(dir_foresteurope, "panEuropean-forestHoldings_public_2015.csv"), skip=1) %>%
  slice(1:(n()-2)) %>% 
  rename(country = X,
         area = Total.forest.area..1000.ha.) %>%
  mutate(ownership = "public") %>%
  select(country, ownership, area)

other2015 <- read.csv(paste0(dir_foresteurope, "panEuropean-forestHoldings_other_2015.csv"), skip=1) %>%
  slice(1:(n()-2)) %>% 
  rename(country = X,
         area = Total.forest.area..1000.ha.) %>%
  mutate(ownership = "other") %>%
  select(country, ownership, area)

ownership2015 <- rbind(private2015, public2015, other2015) %>%
  left_join(forestarea2015) %>%
  filter(!is.na(area))

ownership_shares2015 <- ownership2015 %>%
  mutate(owner_share = area / forest_area) %>%
  pivot_wider(names_from=ownership, values_from = c(owner_share, area)) %>%
  select(!forest_area)

#### Employment in forestry ----
## - share of total laborforce
## - share of forest area?

employment_forestry <- read.csv(paste0(dir_foresteurope, 
                "panEuropean-employmentByGenderAndAge_forestry.csv"),
                skip = 1) %>%
  slice(1:(n()-2)) %>% 
  rename(country = X,
         employment_forestry = Total)

laborforce_total <- read.csv("./data/raw/Labor_force_WorldBank/API_SL.TLF.TOTL.IN_DS2_en_csv_v2_2713561.csv",
                             skip=4) %>%
  mutate(laborforce2015 = X2015/1000,
         country_fix= case_when(
           Country.Name == "Czech Republic" ~ "Czechia",
           Country.Name == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
           Country.Name == "Moldova" ~ "Republic of Moldova",
           Country.Name == "Slovak Republic" ~ "Slovakia"),
         country = ifelse(!is.na(country_fix), country_fix, Country.Name)) %>%
  select(country, laborforce2015)

employment_forestry <- employment_forestry %>%
  filter(!is.na(employment_forestry)) %>%
  left_join(laborforce_total) %>%
  left_join(forestarea2015) %>%
  mutate(employment_share_laborforce = employment_forestry/laborforce2015,
         employment_share_area = employment_forestry / forest_area) %>%
  select(country, employment_forestry, laborforce2015, employment_share_laborforce, employment_share_area)
  # select(forest_area, laborforce2015, area_private, area_public, area_other)
  

#### Combine ----
# Check country names!!!
soceco2015 <- ownership_shares2015 %>%
  left_join(protectedarea2015) %>%
  left_join(employment_forestry) %>%
  mutate(country = ifelse(country == "Czechia", "Czech Republic", country)) %>%
  right_join(gdp_share_av)


census_stats_final %>% dim()
soceco2015 %>% dim()
census_stats_final %>%
  left_join(soceco2015) %>%
  dim()

census_stats_final <- census_stats_final %>%
  left_join(soceco2015)

dim(census_stats_final)

################################################################'
## 6. Stand dynamics ----
## - Add stand dynamics table
## - Check if harvest calculations match with stand dynamics table
##
## Missing for Finland! Not used in the final analysis!
##
################################################################'

# Calculate stuff within the stand_dynamics table
dim(stand_dynamics)
dim(census_stats_final)
census_stats_final %>% left_join(stand_dynamics) %>% dim()

census_stats_final <- census_stats_final %>%
  left_join(stand_dynamics %>%
              select(-any_of(c("census.date", "census.n", "tmt.plot.id",
                               "plot.area", "latitude", "longitude", "country"))) )



################################################################'
## 10. NA checks and fixes  ----
################################################################'
  
### NAs in census.interval  ----
# Even if census.data for prev. census exists??
# --> recalculate

if(any(is.na(census_stats_final$census.interval))) {
  message("NAs in census.interval. Will recalculate if census dates available.
            New colname: census_interval")
  census_stats_final %>%
    dplyr::filter(is.na(census.interval)) %>%
    with(table(database.code, census.n))

  census_info %>% filter(tmt.census.id == "NFL.105130_1.1")
  census_info %>% filter(tmt.census.id == "NFL.105130_1.2")

  #recalculate census interval: census_interval vs (old) census.interval
  census_info0 <- census_info %>%
    mutate(census.n = census.n + 1) %>%
    rename(census.date0 = census.date) %>%
    select(tmt.plot.id, census.n, census.date0)

  c_interval <- census_info %>%
    select(tmt.plot.id, census.n, census.date) %>%
    left_join(census_info0) %>%
    mutate(census_interval = census.date - census.date0)

  ci_latest <- census_stats_final %>%
    left_join(c_interval %>% select(!census.date))

  summary(ci_latest$census_interval)

  ci_latest %>% filter(is.na(census_interval)) %>%
    select(tmt.census.id, census.date, census.date0)

  ci_latest %>% filter(is.na(census_interval) & !is.na(census.date)) %>%
    select(tmt.census.id, census.date, census.date0)

  census_info %>% filter(tmt.census.id == "NNL.31710.7")
  tree %>% filter(tmt.census.id == "NNL.31710.7")

  # 224 NAs left, most with NA in census.date, 15 with NA in census.date0

  census_stats_final <- ci_latest
} else {
  message ("All ok!
        No NAs in census interval, creating identical col: census_interval")
  census_stats_final$census_interval <- census_stats_final$census.interval
}

dim(census_stats_final)


################################################################'
## 12. Final edits harvest + predictors ----
## 
################################################################'

# census_stats_final <- readRDS("./data/processed/census_info2_latest_21-01-22.rds")

### Final edits and definitions ----
census_stats_final <- census_stats_final %>%
  mutate(
    #predictors
    non_native = factor(non_native, levels=c("native", "exotic", "outside-range")),
    ba0_m2 = ba_total0,
    d0_cm = d_mean_ht0/10,
    db_country = factor(paste(database.code, substr(country, 1, 3), sep="."))
  ) 

### Filter ----
# Exclude observations with census interval > 15 years or NA
# Exclude Flanders
# Exclude Finland in FUNDIV data, because more recent NFI data added in FIN.Fin

with(census_stats_final, summary(census_interval))
with(census_stats_final, summary(census_interval > 15))
with(census_stats_final, summary(n_stems_obs0))
table(census_stats_final$db_country)

census_info2_latest_prefilter <- census_stats_final

dim(census_stats_final)
census_stats_final <- census_stats_final %>%
  filter(!is.na(census_interval)) %>%
  filter(census_interval < 15) %>%
  filter(!is.na(n_stems_obs0) ) %>%
  filter(!db_country%in%exclude_db)  %>%
  filter(!(db_country == "NSI.Swi" & ba0_m2 > 100)) %>% # remove these outliers, see emails with Golo
  mutate(db_country = droplevels(db_country))
dim(census_stats_final)


################################################################'
## 15. Represented forest area ----
################################################################'

countries_w_plotvalues <- c("Finland", "Germany", "Sweden") 

## Finland, calculated per sampling region and specified per plot in 0X_process_VMI_data.R
area_rep_fin <- read.csv("./data/raw/VMI/processed/represented_area_per_plot_vmi12_16-03-22.csv")

## Germany, values from Wageningen team
area_rep_ger <- read.csv("./data/raw/WUR/NFIplots_with_reprArea_GER.csv")
area_rep_ger <- area_rep_ger %>%
  mutate(database.code = "NFG",
         plot.id.ger = map_chr(strsplit(plot, split="_"), 3),
         census.n = as.numeric(map_chr(strsplit(plot, split="_"), 2)))

head(area_rep_ger)

## Sweden, values from Jonas

sweden_update <- read.csv2("./data/raw/Sweden_addition/Sweden_Census_Update_220208_to_Susanne.csv")

sweden_update <- sweden_update  %>%
  mutate(database.code = "NSW",
         Area.Exp.Factor..ha. = as.numeric(Area.Exp.Factor..ha.),
         plot.id = as.character(PlotID),
         census.n = case_when(
           inv_year <= 2007 ~ 1,
           inv_year > 2007 & inv_year < 2013 ~ 2,
           inv_year > 2012 ~ 3))  %>%
  select(database.code,ClusterID_census, plot.id, census.n, inv_year, County_ID,  Total.county.area..ha., Region_ID, Area.Exp.Factor..ha.) %>%
  group_by(database.code, plot.id, inv_year) %>%
  mutate(forest_area_repSWE = sum(Area.Exp.Factor..ha.)) %>%
  select(!Area.Exp.Factor..ha.) %>%
  distinct()
  
area_rep_swe <- sweden_update %>%
  ungroup() %>%
  select(database.code, plot.id, census.n, forest_area_repSWE)


## Other countries as forest_area/number of plots
## -  Belgium Walloon (FUN) & Flanders (NFL) separately, areas based on Table 2 in
##   https://www.cnc-nkc.be/sites/default/files/report/file/national_forest_accounting_plan_-_belgium.pdf

plots_per_region <- census_stats_final %>%
  group_by(country, database.code) %>%
  tally()

forestArea_Bel <- data.frame(country = rep("Belgium", 2),
                             database.code = c("FUN", "NFL"),
                             forest_area_ha = c(16844 * 0.312 * 100,
                                                13522 * 0.115 * 100))

area_rep <- plots_per_region %>%
  left_join(soceco2015 %>% 
              select(country, forest_area) %>%
              rbind(forestarea2015 %>% filter(country == "Norway")) %>%
              filter(!country%in%countries_w_plotvalues)) %>%
  left_join(forestArea_Bel) %>%
  mutate(forest_area_ha = ifelse(is.na(forest_area_ha),
                                   forest_area * 1000,
                                   forest_area_ha),
         forest_area_rep = forest_area_ha / n)

# combine

census_stats_final_arearep <- census_stats_final %>%
  mutate(plot.id.ger = ifelse(database.code == "NFG",
                              gsub(pattern="_", replacement = "00", x=plot.id), 
                              NA)) %>%
  left_join(area_rep %>% select(!forest_area)) %>%
  left_join(area_rep_fin %>% rename(forest_area_repFIN = forest_area_rep)) %>%
  left_join(area_rep_ger %>% rename(forest_area_repGER = reprArea_ha)) %>%
  left_join(area_rep_swe) %>%
  mutate(forest_area_rep = ifelse(database.code == "NFG",
                                  forest_area_repGER,
                                  forest_area_rep),
         forest_area_rep = ifelse(database.code == "FIN",
                                  forest_area_repFIN,
                                  forest_area_rep),
         forest_area_rep = ifelse(database.code == "NSW",
                                  forest_area_repSWE,
                                  forest_area_rep))

nrow(census_stats_final_arearep) == nrow(census_stats_final)

summary(census_stats_final_arearep$forest_area_repGER)


# Take rep.area values from nearest neighbours for the NAs in the German data
ger_NAs <- census_stats_final_arearep %>%
  filter(database.code == "NFG" & is.na(forest_area_repGER)) %>%
  select(database.code, tmt.plot.id, tmt.census.id, latitude, longitude) %>%
  st_as_sf(coords=c("longitude", "latitude"),
                 crs="EPSG:4326", remove=FALSE)  %>%
  st_transform(proj_lambert_conic)

ger_nonNAs <- census_stats_final_arearep %>%
  filter(database.code == "NFG" & !is.na(forest_area_repGER)) %>%
  select(database.code, tmt.plot.id, tmt.census.id, latitude, longitude, forest_area_repGER) %>%
  st_as_sf(coords=c("longitude", "latitude"),
           crs="EPSG:4326", remove=FALSE) %>%
  st_transform(proj_lambert_conic) %>%
  st_buffer(dist=5000)

ger_join <- ger_NAs %>%
  st_join(ger_nonNAs, join=st_nearest_feature)


idx_na <- census_stats_final_arearep$tmt.plot.id%in%ger_join$tmt.plot.id.x
census_stats_final_arearep$forest_area_rep[idx_na] <- ger_join$forest_area_repGER

with(census_stats_final_arearep,
     table(database.code, is.na(forest_area_rep)))


# Combine with the final data frame
census_stats_final_arearep <- census_stats_final_arearep %>%
  select(database.code, tmt.plot.id, tmt.census.id, forest_area_rep)

census_stats_final <- census_stats_final %>%
  select(!any_of("forest_area_rep")) %>%
  left_join(census_stats_final_arearep)

# Plot and check
census_stats_final %>%
  group_by(database.code, country) %>%
  summarize(nas = sum(is.na(forest_area_rep)))

################################################################'
## 16. One obs per year ----
################################################################'

# Generate one row for each year
# If harvest_any = TRUE, assign harvest to the first of the generated rows

# census_stats_final <- readRDS("./data/processed/census_stats_final_21-02-22.rds")

find_harvest_year <- function(x) {
  # find harvest year when harvest in the middle of the census interval
  y_out <- ifelse(x %% 2 == 0, 
                  ifelse(sample(c(TRUE, FALSE)),  # even integers, randomize if going up or down from middle
                         x/2, x/2 + 1),
                  ceiling(x/2) )
  return(y_out)
}

data_annual <- census_stats_final %>%
  rowwise() %>%
  mutate(interval_years = as.integer(round(census_interval)),
         mid_interval = find_harvest_year(interval_years)) %>%
  uncount(weights = interval_years, .id="ann.id", .remove=FALSE) %>%
  mutate(status = case_when(
    !harvest_any ~ "pre-harvest",
    harvest_any & ann.id < mid_interval ~ "pre-harvest",
    harvest_any & ann.id == mid_interval ~ "harvest",
    harvest_any & ann.id > mid_interval ~ "post-harvest"))
  
if (any(data_annual$mid_interval %% 1 > 0) ) {
  summary(data_annual$mid_interval %% 1)
  stop("Non-integer mid_interval values present, check where is the problem!")
}


# For post-harvest rows, use variables calculated without the harvested trees
data_annual <- data_annual %>%
  mutate(harvest_any = ifelse(status == "harvest",
                              harvest_any,
                               FALSE),
         harvest_3class = factor(ifelse(status == "harvest",
                                 as.character(harvest_3class),
                                 "NO_HARVEST"), levels = c("NO_HARVEST", "PARTIAL_CUT", "ALL_CUT")),
         harvest_percent_stems = ifelse(status == "harvest",
                                        harvest_percent_stems,
                                        NA),
         harvest_percent_ba = ifelse(status == "harvest",
                                     harvest_percent_ba,
                                        NA),
         harvest_size_diff = ifelse(status == "harvest",
                                    harvest_size_diff,
                                     NA),
         harvest_d_cm = ifelse(status == "harvest",
                               harvest_d_cm,
                               NA),
         harvest_size_rel = ifelse(status == "harvest",
                                   harvest_size_rel,
                                   NA),
         gini_d_nplot0 = ifelse(status != "post-harvest", 
                                gini_d_nplot0,              # for status pre-harvest or harvest use the original values
                                ifelse(is.na(gini_d_nplot.NONHARVEST), 0, gini_d_nplot.NONHARVEST)), # for post-harvest use the values without harvested trees. if no trees left, these variables will be NA and some values need to be defined
         ba0_m2 = ifelse(status != "post-harvest", 
                         ba_total0, 
                         ifelse(is.na(ba_total.NONHARVEST), 0, ba_total.NONHARVEST)),
         ba_percent_dom0 = ifelse(status != "post-harvest", ba_percent_dom0, 
                                  ifelse(is.na(ba_percent_dom.NONHARVEST), 1, ba_percent_dom.NONHARVEST)),
         species_dom0 = ifelse(status != "post-harvest", species_dom0, 
                               ifelse(is.na(species_dom.NONHARVEST), species_dom0, species_dom.NONHARVEST)),
         genus_dom0 = ifelse(status != "post-harvest", genus_dom0, 
                             ifelse(is.na(genus_dom.NONHARVEST), genus_dom0, genus_dom.NONHARVEST)),
         family_dom0 = ifelse(status != "post-harvest", family_dom0, 
                              ifelse(is.na(family_dom.NONHARVEST), family_dom0, family_dom.NONHARVEST)),
         d0_cm = ifelse(status != "post-harvest", d0_cm, 
                        ifelse(is.na(d_mean_ht.NONHARVEST), 0, d_mean_ht.NONHARVEST/10)),
         n_stems_ha0 = ifelse(status != "post-harvest", n_stems_ha0, 
                              ifelse(is.na(n_stems_ha.NONHARVEST), 0, n_stems_ha.NONHARVEST))
       )

dim(data_annual)

table(data_annual$harvest_3class)
table(data_annual$country, data_annual$harvest_3class)

#### Remove updated plots not matching sampling criteria ----

idx_include <- which(data_annual$n_stems_ha0 > 0)
data_annual <- data_annual[idx_include,]

################################################################'
## Add new species variable ----
################################################################'

census_stats_final <- census_stats_final %>%
  mutate(conifer0 = family_dom0 %in% gymno_fam$family,
         species_classes = case_when(
           genus_dom0 %in% c("Picea") ~ "Picea",
           genus_dom0 %in% c("Pinus") ~ "Pinus",
           genus_dom0 %in% c("Fagus", "Quercus") ~ "FagusQuercus",
           # genus_dom0 %in% c("Eucalyptus") ~ "PlantationSp",
           genus_dom0 %in% c("Eucalyptus") | species_dom0 == "Pinus pinaster" ~ "PlantationSp",
           TRUE & !conifer0 ~ "OtherBroadleaved",
           TRUE & conifer0 ~ "OtherConifer"
  ))

# with(census_stats_final, table(species_dom0, species_classes))
data_annual <- data_annual %>%
  mutate(conifer0 = family_dom0 %in% gymno_fam$family,
         species_classes = case_when(
           genus_dom0 %in% c("Picea") ~ "Picea",
           genus_dom0 %in% c("Pinus") ~ "Pinus",
           genus_dom0 %in% c("Fagus", "Quercus") ~ "FagusQuercus",
           # genus_dom0 %in% c("Eucalyptus")  ~ "PlantationSp",
           genus_dom0 %in% c("Eucalyptus") | species_dom0 == "Pinus pinaster" ~ "PlantationSp",
           TRUE & !conifer0 ~ "OtherBroadleaved",
           TRUE & conifer0 ~ "OtherConifer"
  ))

# with(data_annual, table(species_dom0, species_classes))

################################################################'
# Define harvest01  ----
################################################################'
# (= harvest_any but factor)
data_annual$harvest01 <- as.factor(ifelse(data_annual$harvest_any, "HARVEST", "NO-HARVEST"))
census_stats_final$harvest01 <- as.factor(ifelse(census_stats_final$harvest_any, "HARVEST", "NO-HARVEST"))

table(census_stats_final$db_country)

################################################################'
## 17. Write files ----
################################################################'

fl_name1 <- paste0("./data/processed/census_stats_final_", fl_date, ".rds")
saveRDS(census_stats_final, file=fl_name1)

fl_name2 <- paste0("./data/processed/data_annual_", fl_date, ".rds")
saveRDS(data_annual, file=fl_name2)

