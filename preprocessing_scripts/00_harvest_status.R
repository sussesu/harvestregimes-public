################################################################'
##
## Defining harvest status from the original data files
## - harvest = tree not present in the forest 
##   (i.e., includes also naturally dead trees that were collected afterwards)
##
################################################################'

rm(list=ls())

library(tidyverse)
source("./src/read_tmt_data.R")

datadir <- "./data/raw/TreeMort-database/Original_data/NFI-Europe/"
outdir <- "./data/processed/harvest_status/"



# TMt processed data ---------------------------------------------------
# to restrict the work to those trees that are considered in the first place

tree_tmt <- read_tmt("treedata_biomass", datatype="Processed_data")

# Switzerland -------------------------------------------------------------------
# Q's:
# - what does TREE_STATUS = -1 (not determined) mean in practise? Tree not found? (now classified as dead)

tree_org_NSI1 <- read.csv(paste0(datadir, "Switzerland/Tree_level_NFI1_NFI4_20210120.csv"))
tree_org_NSI <- tree_org_NSI1 %>% 
  mutate(database.code = "NSI",
         tree.id = TREEID,
         tmt.plot.id = paste (database.code, PLOTID, sep = '.'),
         tmt.tree.id = paste (database.code, PLOTID, TREEID, sep = '.'),
         census.n = NA,
         census.n = ifelse (CENSUSID == 150,1, census.n),
         census.n = ifelse (CENSUSID == 250,2, census.n),
         census.n = ifelse (CENSUSID == 350,3, census.n),
         census.n = ifelse (CENSUSID == 450,4, census.n),
         tmt.census.id = paste (database.code, PLOTID, CENSUSID, sep = '.'),
         tmt.obs.id = paste (tmt.tree.id, census.n,sep = '.'),
         tree.status = TREE_STATUS,
         tree.status = ifelse (tree.status == 1, 0,tree.status),
         tree.status = ifelse (tree.status %in% c(2,-1),1,tree.status)# vanished trees are also considered dead) %>%
  )  %>% 
  mutate(harvest.status = case_when(
    TREE_STATUS == -1 & HISTORY %in% c(4) ~ 1,
    TREE_STATUS == -1 & HISTORY %in% c(8, 5, 6, 14, 15) ~ 0,
    TREE_STATUS == -1 & HISTORY == -1 ~ 0, #as.numeric(NA),
    TREE_STATUS == 2 & HISTORY %in% c(-1, 1, 2, 3, 9) ~ 0,
    TREE_STATUS == 1 ~ 0) )

with(tree_org_NSI, table(HISTORY, TREE_STATUS, harvest.status))
with(tree_org_NSI, table(HISTORY, TREE_STATUS, is.na(harvest.status)))
with(tree_org_NSI, table(is.na(harvest.status)))

with(tree_org_NSI, table(is.na(HISTORY)))
with(tree_org_NSI, table(is.na(TREE_STATUS)))

tree_org_NSI %>%
  filter(census.n > 1) %>%
  group_by(TREE_STATUS, HISTORY) %>%
  tally()

# FUNDIV  ------------------------------------------------------------------

tree_org_FUN <- read.csv("./data/raw/TreeMort-database/Original_data/FUNDIV/FunDivEUROPE_trees_99mm_Tom.csv")
plot_FUN <- read.csv("./data/raw/TreeMort-database/Original_data/FUNDIV/FunDivEUROPE_plots_Tom.csv")

table(plot_FUN$management2, plot_FUN$country) # management info for Spain 0/1s
table(tree_org_FUN$country, tree_org_FUN$treestatus_th)

# To long format (census as column)
tree_org_FUN$census.n1 <- 1
tree_org_FUN$census.n2 <- 2

c1 <- grepl(pattern="1$", x=colnames(tree_org_FUN))
c2 <- grepl(pattern="2$", x=colnames(tree_org_FUN))
c0 <- !(c1 | c2)

fundiv1 <- tree_org_FUN[, c0 | c1]
fundiv2 <- tree_org_FUN[, c0 | c2]

colnames(fundiv1) <- gsub(pattern="1$", replacement = "",  x=colnames(fundiv1))
colnames(fundiv2) <- gsub(pattern="2$", replacement = "",  x=colnames(fundiv2))

tree_FUNDIV <- rbind(fundiv1, fundiv2)

#### Tree status by region in Spain ----

# # write ES plots in file for Paloma to check the problematic regions
# write.csv(plot_FUN %>% 
#             filter(country == "ES") %>%
#             select(plotcode, country), file="./data/processed/Spain_plotIDs_FUNDIV.csv",
#           row.names = FALSE)

# Spanish region codes from Paloma, add to FUNDIV plot data

spain_regions <- read.csv("./data/raw/Spain_paloma/management23.csv",
                          row.names = 1) %>%
  mutate(country = "ES") #%>%  select(country, plotcode, Provincia)


# for 3 provinces treestatus_th for dead trees 
# (15 = Galicia/A Coruna, 27 = Galicia/Lugo, 36 = Galicia/Pontevedra)
# is always 4 (dead, stem present)
tree_FUNDIV %>%
  filter(country == "ES") %>%
  filter(treestatus_th %in% 3:5) %>% # only dead trees
  left_join(spain_regions) %>%
  group_by(Provincia) %>%
  summarize(status3 = sum(treestatus_th == 3),
            status4 = sum(treestatus_th == 4),
            status5 = sum(treestatus_th == 5),
            all_4 = all(treestatus_th == 4)) %>%
  as.data.frame() 

tree_FUNDIV %>%
  filter(country == "ES") %>%
  filter(treestatus_th %in% 3:5) %>% # only dead trees
  left_join(spain_regions) %>%
  mutate(problem_province = ifelse(Provincia %in%c(15, 27, 36),
                                   "Province 15, 27 or 36",
                                   "Other provinces")) %>%
  ggplot(aes(treestatus_th)) +
  geom_bar() + facet_wrap(problem_province ~ .)


#### Reclassify harvest ----
# In problem-provinces set to harvest, if plot-level management recorded in the 3rd Spanish NFI (Corta3)
# (OR set to NA if in one of the 3 provinces using only one code for dead trees = harvest.status2)

tree_FUNDIV <- tree_FUNDIV %>%
  left_join(spain_regions) 

harvest_table <- tree_FUNDIV %>% 
  mutate(spain = country == "ES",
         problem_province = Provincia %in%c(15, 27, 36),
         management_es = problem_province & (Corta3 != "0")) %>%
  select(treestatus_th, spain, problem_province, management_es) %>%
  distinct() %>%
  arrange(spain, problem_province, management_es, treestatus_th) %>%
  mutate(harvest.status = case_when(
    !spain & treestatus_th %in% c(3, 5) ~ 1,
    !spain & treestatus_th %in% c(4) ~ 0,
    spain & !problem_province & treestatus_th %in% c(3, 4) ~ 1,
    spain & !problem_province & treestatus_th %in% c(5) ~ 0,
    spain & problem_province & management_es ~ 1,
    spain & problem_province & !management_es ~ 0,
  ))

dim(tree_FUNDIV)

tree_FUNDIV <- tree_FUNDIV %>%
  mutate(database.code = "FUN",
         tree.id = as.factor (gsub(" ", "", treecode, fixed = TRUE)),
         tmt.tree.id = paste (database.code, tree.id, sep = '.'),
         tmt.plot.id = paste (database.code, plotcode, sep = '.'),
         plot.id = paste (plotcode, sep = '.'),
         tmt.census.id = paste (database.code, plotcode, census.n, sep = '.'),
         tmt.obs.id = paste (tmt.tree.id, census.n,sep = '.'),
         spain = country == "ES",
         problem_province = Provincia %in%c(15, 27, 36),
         management_es = problem_province & (Corta3 != "0") ) %>%
  left_join(harvest_table)

dim(tree_FUNDIV)

         # harvest.status = case_when(
         #   treestatus_th %in% c(3, 5) ~ 1,
         #   treestatus_th %in% c(4) & !Provincia %in%c(15, 27, 36) ~ 0,
         #   treestatus_th %in% c(1, 2) ~ 0,
         #   treestatus_th == 4 & country == "ES" & Provincia %in%c(15, 27, 36) & Corta3 != "0" ~ 1,
         #   treestatus_th == 4 & country == "ES" & Provincia %in%c(15, 27, 36) & Corta3 == "0" ~ 0) )



# # attach province info to coordinates for checking in QGIS:
# plot_FUN <- read_tmt("plot", database.code = "FUN")
# 
# plot_FUN <- plot_FUN %>%
#   filter(country == "Spain") %>%
#   left_join(spain_regions %>% select(!country), by = c("plot.id" = "plotcode")) %>%
#   mutate(to_NA = Provincia %in%c(15, 27, 36))
# 
# write.csv(plot_FUN, file="./data/processed/check_lists/Spain_regions.csv")

# Netherlands -------------------------------------------------------------

list.files(path="./data/raw/TreeMort-database/Original_data/NFI-Europe/Netherlands/",
           pattern="treedata", full.names = TRUE)

# tree_NNL7 <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Netherlands//treedata2020.csv")
# tree_NNL6 <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Netherlands//treedata6.csv")
# tree_NNL <- rbind(tree_NNL7, tree_NNL6)

tree_NNL <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Netherlands/NL_NFI_DATA/tree_data_NL.csv")

table(tree_NNL$Cause.of.death..standing.fallen.harvested.)

tree_NNL <- tree_NNL %>%
  mutate(database.code = "NNL",
         census.n = as.numeric(gsub("NBI",replacement = "", x=censusid)),
         tmt.plot.id = paste (database.code, plotid, sep = '.'),
         tmt.tree.id = paste (database.code, treeid, sep = '.'),
         census.id = as.character (paste (plotid, census.n, sep = '.')),
         tmt.census.id = paste (database.code,census.id, sep = '.'),
         tmt.obs.id = paste (tmt.tree.id, census.n,sep = '.'),
         tree.status = ifelse (Tree.status == 'Alive', 0,1),
         harvest.status = case_when(
           Cause.of.death..standing.fallen.harvested.%in% 
             c("Dead, harvested", "Harvested", "Windthrown, harvested") ~ 1,
           Cause.of.death..standing.fallen.harvested.%in% 
             c("Dead lying", "Dead standing", "Decomposed", "Outside plot") ~ 0)
         )

# Sweden ------------------------------------------------------------------
# - no changes from the original mode-of-death column!

tree_NSW <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Sweden/Sweden_tree_data.csv",
                     sep=";")

table(tree_NSW$status)
table(is.na(tree_NSW$status))

# Follow TreeMort-db codes to create tmt.id's

database.code <- "NSW"
td <- tree_NSW

td$database.code <- database.code
td$tree.id <- td$TreeID
td$plot.id <- as.character(td$PlotID)

td <- td %>% filter (!is.na (ClusterID))
td$tmt.plot.id <- paste (database.code, td$plot.id, sep = '.')
td$TreeID <- as.numeric (as.factor (td$TreeID))
td$tmt.tree.id <- paste (td$tmt.plot.id, td$TreeID, sep = '.')
td$subplot.id <- td$SubplotID
td$census.n <- 1
td [which (td$inv_year > 2007 & td$inv_year < 2013 ),'census.n'] <- 2
td [which (td$inv_year > 2012 ),'census.n'] <- 3
td$tmt.obs.id <- paste (td$tmt.tree.id, td$census.n,sep = '.')
td$census.id <- as.character (paste (td$tmt.plot.id, td$census.n, sep = '.'))
td$tmt.census.id <- td$census.id

table(td$status)
table(is.na(td$status))

# Check NSW data in processed TMt tree table
with(tree_tmt %>% filter(database.code == "NSW"), table(mode.death))

# Combine

summary(td$tmt.obs.id %in% tree_tmt$tmt.obs.id) # not all above created id's in the ready data??

tmt_NSW <- tree_tmt %>%
  filter(database.code == "NSW") %>%
  left_join(td %>% select(plot.id, tree.id, census.n, status))

table(tree_NSW$status)

with(tmt_NSW, table(mode.death, status))
with(tmt_NSW, table(mode.death, is.na(status)))

tmt_NSW %>%
  filter(mode.death == 4) %>%
  select(plot.id, tree.id, census.n) %>%
  head()

tree_NSW %>%
  filter(PlotID == 831509312 &
           TreeID == "{DC556F48-1D2F-40C5-A8BF-DA958DFAB3C2}" )

# --> These are trees that are alive in first 1-2 census and do not have any 
# data for the last census??

# ### use same classification as in mode.death ----
# 
# tree_tmt <- tree_tmt %>%
#   mutate(harvest.mode.death = case_when(
#     substr(mode.death, 1, 1) == "2" ~ 1,
#     substr(mode.death, 1, 1) == "1" ~ 0,
#     mode.death == "3" ~ 1,
#     mode.death == "4" ~ as.numeric(NA)),
#     harvest.status = ifelse(database.code == "NSW", 
#                             harvest.mode.death,
#                             harvest.status))
# 
# tree_tmt %>%
#   filter(tree.status == 1) %>%
#   with(table(database.code, is.na(harvest.status)))
# 
# tree_tmt %>%
#   filter(tree.status == 0) %>%
#   with(table(database.code, is.na(harvest.status)))


# Czech Republic ----------------------------------------------------------

tree_tmt_NCZ <- read_tmt("treedata_biomass", datatype="Processed_data", database.code="NCZ")

tree_NCZ <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Czech Republic/tree_data.csv")

# all mode.death NAs???
colnames(tree_NCZ)
summary(tree_NCZ$mode.death)
table(tree_NCZ$tree.status)

with(tree_NCZ %>% filter(DBH > 10), table(tree.status))

# but values in the TMt data?? 
# --> these are coming from the quality control script,
# trees measured in first census, not in second --> set to dead
tree_tmt_NCZ %>%
  filter(database.code=="NCZ" & tree.status == 1) %>%
  with(table(mode.death))

# ### use same classification as in TMt data mode.death ----
# 
# tree_tmt <- tree_tmt %>%
#   mutate(harvest.mode.death = case_when(
#     substr(mode.death, 1, 1) == "2" ~ 1,
#     substr(mode.death, 1, 1) == "1" ~ 0,
#     mode.death == "3" ~ 1,
#     mode.death == "4" ~ as.numeric(NA)),
#     harvest.status = ifelse(database.code == "NCZ", 
#                             harvest.mode.death,
#                             harvest.status))
# 
# tree_tmt %>%
#   filter(tree.status == 1) %>%
#   with(table(database.code, is.na(harvest.status)))
# 
# tree_tmt %>%
#   filter(tree.status == 0) %>%
#   with(table(database.code, is.na(harvest.status)))


#  Germany ----------------------------------------------------------------

tree_NFG_org <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Germany/Germany_treedata.csv")

colnames(tree_NFG_org)
table(tree_NFG_org$Cause.of.death..standing.fallen.harvested.) 
summary(is.na(tree_NFG_org$Cause.of.death..standing.fallen.harvested.))

# explore duplicates
summary(duplicated(tree_NFG_org[, c("plotid", "treeid", "censusid")])) # 659 duplicated trees??
duplicates_NFG <- tree_NFG_org %>%
  group_by(plotid, treeid, censusid) %>%
  mutate(n_dupl = n()) %>%
  filter(n_dupl > 1)

duplicates_NFG <- duplicates_NFG %>%
  arrange(plotid, treeid, censusid) %>%
  group_by(plotid, treeid, censusid) %>%
  mutate(dbh_distinct = n_distinct(DBH),
         species_distinct = n_distinct(Species),
         status_distinct = n_distinct(Tree.status),
         dupl_id = row_number())

summary(duplicates_NFG) # same DBH, Species and tree status -> doesn't matter which duplicate kept

rm(duplicates_NFG)

# remove duplicated rows

tree_NFG <- tree_NFG_org %>%
  group_by(plotid, treeid, censusid) %>%
  mutate(dupl_id = row_number()) %>%
  filter(dupl_id == 1) %>%
  ungroup()

# process
tree_NFG <- tree_NFG %>%
  mutate(database.code = "NFG",
         census.n = as.numeric(gsub("BWI",replacement = "", x=censusid)),
         tmt.plot.id = paste (database.code, plotid, sep = '.'),
         tmt.tree.id = paste (database.code, treeid, sep = '.'),
         census.id = as.character (paste (plotid, census.n, sep = '.')),
         tmt.census.id = paste (database.code,census.id, sep = '.'),
         tmt.obs.id = paste (tmt.tree.id, census.n,sep = '.'),
         tree.status = ifelse (Tree.status == 'alive', 0,1),
         harvest.status = case_when(
           Cause.of.death..standing.fallen.harvested.%in% 
             c("harvested") ~ 1,
           Cause.of.death..standing.fallen.harvested.%in% "alive" ~ 0,
           Cause.of.death..standing.fallen.harvested.%in% "dead" ~ 0,
           Cause.of.death..standing.fallen.harvested.%in% "vanished" ~ 0)
  )


# Poland ---------------------------------------------------------------------

tree_NPO <- read.table("./data/raw/TreeMort-database/Original_data/NFI-Europe/Poland/TreeMort_PL_3/NFI12_TREE_DATA",
                       sep="\t", dec = ",", header=TRUE)

str(tree_NPO)
with(tree_NPO, table(Tree_status, Cause_of_death))

tree_NPO <- tree_NPO %>%
  mutate(database.code = "NPO",
         tree.id = treeid,
         plot.id = plotid,
         tmt.plot.id = paste (database.code, plotid, sep = '.'),
         tmt.tree.id = paste (database.code, plot.id,treeid, sep = '.'),
         census.n = censusid,
         census.id = as.character (paste (plotid, census.n, sep = '.')),
         tmt.census.id = paste (database.code,census.id, sep = '.'),
         tmt.obs.id = paste (tmt.tree.id, census.n,sep = '.'),
         harvest.status = case_when(
           Tree_status == "Alive" ~ 0,
           Tree_status == "Dead" & Cause_of_death == "Harvested" ~ 1,
           Tree_status == "Dead" & Cause_of_death != "Harvested" ~ 0,
           Tree_status == "Unknown" ~ 0 
         ) )

with(tree_NPO, table(harvest.status))
with(tree_NPO, table(harvest.status, census.n))



# Flanders, Belgium -------------------------------------------------------
# No suggested changes -- just use the ones in TMt data
# But there is some data weirdness with duplicated IDs - needs more attention??

#from TMt data load scripts:
pd1 <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Flanders/Flanders_NFI_plotdata_selected.csv")
td1 <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Flanders/Flanders_NFI_treedata_selected_AGB20210211.csv") %>% 
  filter (plotid %in% pd1$plotid)
td1_ingrowth <- td1 %>% filter (is.na (IDLink1_2))
td1 <- td1 %>% mutate (treeid = ifelse (is.na (IDLink1_2), paste (treeid, 'i', sep = ''), treeid))

tree_NFL <- td1

table(tree_NFL$Tree_status, tree_NFL$Cause_death)
table(tree_NFL$Cause_death_specifics)

summary(duplicated(tree_NFL[, c("plotid", "censusid", "treeid")]))

# # explore duplicates
# summary(duplicated(tree_NFL[, c("plotid", "treeid", "censusid")])) # 659 duplicated trees??
# 
# duplicates_NFL<- tree_NFL %>%
#   group_by(plotid, treeid, censusid) %>%
#   mutate(n_dupl = n()) %>%
#   filter(n_dupl > 1)
# 
# duplicates_NFL <- duplicates_NFL %>%
#   arrange(plotid, treeid, censusid) %>%
#   group_by(plotid, treeid, censusid) %>%
#   mutate(dbh_distinct = n_distinct(DBH),
#          species_distinct = n_distinct(Species),
#          status_distinct = n_distinct(Tree_status),
#          dupl_id = row_number())
# 
# summary(duplicates_NFL) # same DBH, Species and tree status 
# 
# rm(duplicates_NFL)

# # check other file
# tree_NFL2 <- read.csv("./data/raw/TreeMort-database/Original_data/NFI-Europe/Flanders/Flanders_NFI_treedata_selected.csv")
# 
# summary(duplicated(tree_NFL2[, c("plotid", "censusid", "FinalID")]))
# 
# duplicates_NFL2<- tree_NFL2 %>%
#   group_by(plotid, treeid, censusid) %>%
#   mutate(n_dupl = n()) %>%
#   filter(n_dupl > 1)
# 
# duplicates_NFL2 <- duplicates_NFL2 %>%
#   arrange(plotid, treeid, censusid) %>%
#   group_by(plotid, treeid, censusid) %>%
#   mutate(dbh_distinct = n_distinct(DBH),
#          species_distinct = n_distinct(Species),
#          status_distinct = n_distinct(Tree_status),
#          dupl_id = row_number())

# Process

tree_NFL <- tree_NFL %>%
  mutate(database.code = "NFL",
         tmt.plot.id = paste (database.code, plotid, sep = '.'),
         tmt.tree.id = paste (database.code, plotid, treeid, sep = '.'),
         census.id = as.character (paste (plotid, censusid, sep = '.')),
         census.n = censusid,
         tmt.census.id = paste (database.code,census.id, sep = '.'),      
         tmt.obs.id = paste (tmt.tree.id, census.n,sep = '.'),
         tree.status = ifelse (Tree_status == 'Alive', 0,1),
         tree.id = paste (plotid, treeid, sep = '.'),
         plot.id = plotid,
         harvest.status = case_when(
           Tree_status == "Dead" & Cause_death == "harvested/fallen" ~ 1,
           Tree_status == "Dead" & Cause_death == "standing" ~ 0,
           Tree_status == "Alive" ~ 0
         ))

table(tree_NFL$harvest.status)

# dealing with duplicates (from format tree data TMt function)
tree_NFL <- tree_NFL[rev (order (tree_NFL$DBH)),]
tree_NFL$dup <- duplicated (tree_NFL$tmt.obs.id)
tree_NFL <- tree_NFL [-which (tree_NFL$dup == TRUE),]

summary(duplicated(tree_NFL[, c("plotid", "censusid", "treeid")]))

# Check for duplicates ----------------------------------------------------

summary(duplicated(tree_org_NSI$tmt.obs.id))
summary(duplicated(tree_FUNDIV$tmt.obs.id)) 
summary(duplicated(tree_NNL$tmt.obs.id))  
summary(duplicated(tree_NFG$tmt.obs.id))
summary(duplicated(tree_NPO$tmt.obs.id))
summary(duplicated(tree_NFL$tmt.obs.id))

#### Tables with original codes ----

write.csv(tree_org_NSI %>%
  group_by(HISTORY, TREE_STATUS) %>%
  summarize(harvest.status = unique(harvest.status)),
  file=paste0(outdir, "/codings/harvest_codes_NSI.csv"),
  row.names = FALSE)

write.csv(tree_FUNDIV %>%
  mutate(problem_region_spain = Provincia%in%c(15, 27, 36),
         management_spain = Corta3 != "0") %>%
  group_by(problem_region_spain, management_spain, treestatus_th) %>%
  summarize(harvest.status = unique(harvest.status)),
  file=paste0(outdir, "/codings/harvest_codes_FUNDIV.csv"),
  row.names = FALSE)

write.csv(tree_NNL %>%
  group_by(Cause.of.death..standing.fallen.harvested.) %>%
  summarize(harvest.status = unique(harvest.status)),
  file=paste0(outdir, "/codings/harvest_codes_NNL.csv"),
  row.names = FALSE)

write.csv(tree_NFG %>%
  group_by(Cause.of.death..standing.fallen.harvested.) %>%
  summarize(harvest.status = unique(harvest.status)),
  file=paste0(outdir, "/codings/harvest_codes_NFG.csv"),
  row.names = FALSE)

write.csv(tree_NPO %>%
  group_by(Tree_status, Cause_of_death) %>%
  summarize(harvest.status = unique(harvest.status)),
  file=paste0(outdir, "/codings/harvest_codes_NPO.csv"),
  row.names = FALSE)

write.csv(tree_NFL %>%
  group_by(Tree_status, Cause_death) %>%
  summarize(harvest.status = unique(harvest.status)),
  file=paste0(outdir, "/codings/harvest_codes_NFL.csv"),
  row.names = FALSE)
  

# Write to files ----------------------------------------------------------

#### NSI ----
write.csv(tree_org_NSI %>% 
            select(database.code, tmt.tree.id, tmt.obs.id, HISTORY, TREE_STATUS, harvest.status),
          file=paste0(outdir, "tree_harvest_status_NSI.csv") ,
          row.names = FALSE)

#### FUNDIV ----
write.csv(tree_FUNDIV %>% 
            select(database.code, tmt.tree.id, tmt.obs.id, treestatus_th, harvest.status),
          file=paste0(outdir, "tree_harvest_status_FUN.csv") ,
          row.names = FALSE)

#### NNL  ----
write.csv(tree_NNL %>% 
            select(database.code, tmt.tree.id, tmt.obs.id, 
                   Cause.of.death..standing.fallen.harvested., harvest.status),
          file=paste0(outdir, "tree_harvest_status_NNL.csv") ,
          row.names = FALSE)

#### NFG ----
write.csv(tree_NFG %>% 
            select(database.code, tmt.tree.id, tmt.obs.id, 
                   Cause.of.death..standing.fallen.harvested., harvest.status),
          file=paste0(outdir, "tree_harvest_status_NFG.csv") ,
          row.names = FALSE)


#### NPO ----
write.csv(tree_NPO %>% 
            select(database.code, tmt.tree.id, tmt.obs.id, 
                   Tree_status, Cause_of_death, harvest.status),
          file=paste0(outdir, "tree_harvest_status_NPO.csv") ,
          row.names = FALSE)

#### NFL ----
# Because of the duplicates, I'm including extra columns here to NFL file to be able to identify 
# the correct trees to join with in processed data
write.csv(tree_NFL %>% 
            select(database.code, tmt.tree.id, tmt.obs.id, 
                   Tree_status, Cause_death, harvest.status),
          file=paste0(outdir, "tree_harvest_status_NFL.csv") ,
          row.names = FALSE)


