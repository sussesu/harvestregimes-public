#########################################################'
##
## !!! These files not used anymore, processed again within the TMt workflow from the original files !!!
##
## Explore and process the Norwegian NFI data to combine with the harvest work
## - processed by Ajdin @ WUR
##
## Processing done to Ajdin's files:
## - add rows to census_info table if tmt.census.id in tree file not found there
## - set DBH threshold to 10 cm
## - make species names compatible with the TMt data (spp -> indet etc.)
##
#########################################################'

library(dplyr)
library(ggplot2)

source("./src/treemort-database-master/01_Format-data/functions/04_QualityControl.R")

# Read data ----
input_dir <- "./data/raw/Norway/DataNOR/"
tree <- read.csv(paste0(input_dir, "01_qc-treedata_TMt_NOR.csv"), row.names = 1)
tree_noqc <- read.csv(paste0(input_dir, "01_treedata_TMt_NOR.csv"), row.names = 1)
census_info <- read.csv(paste0(input_dir, "03_census-info_TMt_NOR.csv"), row.names = 1)
plot_info <- read.csv(paste0(input_dir, "04_plot-info_TMt_NOR.csv"), row.names = 1)

# Basic exploration ----
dim(tree)
dim(tree_noqc)
dim(census_info)
dim(plot_info)

table(census_info$census.n)
table(floor(census_info$census.date), census_info$census.n)

summary(tree$tmt.census.id %in% census_info$tmt.census.id) # 295 trees don't have their census.id's in census_info table!
summary(tree$tmt.plot.id %in% census_info$tmt.plot.id) # ok
summary(tree$tmt.plot.id %in% plot_info$tmt.plot.id) # ok

summary(tree$n.ha)
table(tree$n.ha[tree$tree.status == 0])

# Modify IDs to combine the split plot parts to one
# tree_NEW <- tree %>%
#   mutate(tmt.plot.id = sapply(strsplit(tmt.plot.id, split="-"), "[", 1),
#          tmt.census.id = paste0(tmt.plot.id, ".", census.n),
#          census.id = gsub(tmt.census.id, pattern = "NOR.", replacement = ""),
#          plot.id = gsub(tmt.plot.id, pattern = "NOR.", replacement = ""))

tree_noqc_NEW <- tree_noqc %>%
  mutate(tmt.plot.id = sapply(strsplit(tmt.plot.id, split="-"), "[", 1),
         tmt.census.id = paste0(tmt.plot.id, ".", census.n),
         census.id = gsub(tmt.census.id, pattern = "NOR.", replacement = ""),
         plot.id = gsub(tmt.plot.id, pattern = "NOR.", replacement = ""))

census_info_NEW <- census_info %>%
  mutate(tmt.plot.id = sapply(strsplit(tmt.plot.id, split="-"), "[", 1),
         tmt.census.id = paste0(tmt.plot.id, ".", census.n),
         census.id = gsub(tmt.census.id, pattern = "NOR.", replacement = ""),
         plot.id = gsub(tmt.plot.id, pattern = "NOR.", replacement = "")) %>%
  distinct()

plot_info_NEW <- plot_info %>%
  mutate(tmt.plot.id = sapply(strsplit(tmt.plot.id, split="-"), "[", 1),
         # splitID = sapply(strsplit(tmt.plot.id, split="-"), "[", 2),
         plot.id = gsub(tmt.plot.id, pattern = "NOR.", replacement = "") ) %>%
  group_by(tmt.plot.id) %>%
  mutate(plot.area = max(plot.area)) %>%
  distinct()

# dim(tree)
# dim(tree_NEW)
# tree_NEW %>% select(tmt.plot.id, plot.id, tmt.census.id, census.id) %>% head()
# tree <- tree_NEW

dim(tree_noqc)
dim(tree_noqc_NEW)
tree_noqc_NEW %>% select(tmt.plot.id, plot.id, tmt.census.id, census.id) %>% head()
tree_noqc <- tree_noqc_NEW

dim(census_info)
dim(census_info_NEW)
census_info_NEW %>% select(tmt.plot.id, plot.id, tmt.census.id, census.id) %>% head()
census_info <- census_info_NEW

dim(plot_info)
dim(plot_info_NEW)
plot_info_NEW %>% select(tmt.plot.id, plot.id) %>% head()
plot_info <- plot_info_NEW

# Handle duplicates in the plot_info table

# Explore the missing tmt.census.id's ----
# 81 trees
# Explanation: these are plots where all trees are dead in the said census
tree_census_missing <- tree %>%
  filter(!(tmt.census.id %in% census_info$tmt.census.id))

all(tree_census_missing$d == 0)
all(tree_census_missing$tree.status == 1)

# Solution: create rows for these plots in the census_info table
colnames(census_info)
shared_colnames <- intersect(colnames(census_info), colnames(tree))
not_shared <- setdiff(colnames(census_info), shared_colnames)

new_census_rows <- tree_census_missing %>%
  select(all_of(shared_colnames)) %>%
  distinct()

any(duplicated(new_census_rows$tmt.census.id)) # check that no duplicates for census.id

new_census_rows[, not_shared] <- NA # create missing cols
new_census_rows <- new_census_rows %>% select(any_of(colnames(census_info))) # arrange cols
colnames(new_census_rows) == colnames(census_info) # check that colnames match
census_info <- rbind(census_info, new_census_rows) # add to census_info
summary(tree$tmt.census.id %in% census_info$tmt.census.id) # check, should now be all TRUE - ok!

# # Fix tmt.census.id ----
## --- redefined censusIDs above so this is no longer needed
# census_info$tmt.census.id <- gsub(pattern = "NOR ", replacement = "NOR.", x = census_info$tmt.census.id)
# tree$tmt.census.id <- gsub(pattern = "NOR ", replacement = "NOR.", x = tree$tmt.census.id)

# Check harvest ----
table(tree$mode.death)

any(tree$tree.status == 1 & is.na(tree$mode.death)) # all dead trees have a mode of death, should be FALSE - ok!

summary(tree$mode.death == 2)

summary(census_info$management) # no census level management info here

# Quality control -----
# - DBH threshold
# - Missing dead

tree_d10 <- ThresholdD(tree=tree, d_threshold = 100)
# tree_missingDead <- MissingDead (tree_d10, database.code = "NOR")

tree <- tree_d10

# Check species ----
tree %>% group_by(species.id, species.cor) %>% tally() %>% as.data.frame()

sp_fix <- c(spp.="indet", 
            Other = "Indet", 
            hardwoods = "indet",
            conifers = "indet")

for(i in 1:length(sp_fix)) {
  old_str <- names(sp_fix)[i]
  new_str <- sp_fix[i]
  print(paste(old_str, "=>", new_str))
  tree$species.cor <- gsub(pattern = old_str, 
                           replacement = new_str,
                           x = tree$species.cor)
}

tree %>% group_by(species.id, species.cor) %>% tally() %>% as.data.frame()

# Add empty cols to match the rest of the files ----
# (tree cols need to match the tree-biomass format!!)
tree[,c("biomass","biomass.plot", "biomass.ha", "ba.ha")] <- NA
census_info <- census_info %>% select(!any_of(c("disturbance")))
tree <- tree %>% select(!any_of(c("qc.missing1stmeasurement", "qc.missing.d")))

# save the updated files
outdir <- "./data/raw/Norway/processed_susanne/"

write.csv(tree, file=paste0(outdir, "01_qc-treedata_TMt_NOR.csv"), row.names=FALSE )
write.csv(census_info, file=paste0(outdir, "03_census-info_TMt_NOR.csv"), row.names=FALSE )
write.csv(plot_info, file=paste0(outdir, "04_plot-info_TMt_NOR.csv"), row.names=FALSE )

