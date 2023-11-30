############################################################'
##
## Process VMI data to TMt form
##
############################################################'

rm(list=ls())

library(tidyverse)
library(sf)
source("./src/read_vmi.R")
source("./src/basic_stats_nplot.R")

# check species names from The Plant List?
check_sp_names <- FALSE

sp_codes <- read.csv("./data/raw/VMI/species_codes.csv")

database.code <- "FIN"

dbh_thold <- 100

out_dir <- "./data/raw/VMI/TMt_format/"

# data string for file names
fl_date <-format(Sys.Date(), "%d-%m-%y")

# Tmt format cols ---------------------------------------------------------
tree_cols <- c('tmt.obs.id','database.code','tmt.tree.id','tree.id','tmt.plot.id','plot.id',
               'tmt.census.id','census.id','census.date','census.n',
               'species.id','tmt.species.id','species.orig','genus.orig','family.orig','func.type',
               'subplot.id','tree.coord.x','tree.coord.y','azimuth','dist.centre',
               'd','pom','height','agb','ba','n.plot','g.plot','n.ha',
               'tree.status','mode.death','mode.death.other',
               'canopy.position','liana.infe','multistem')

census_cols <- c("database.code","tmt.census.id", "census.id", "census.date","census.n",     
                 "tmt.plot.id","plot.id","plot.area",
                 "management",'disturbance',"time.human.intervention", "stand.age",
                 "lianas","land.ownership",
                 "census.contact","census.contact.email")

plot_cols <-  c("database.code","tmt.plot.id","plot.id","plot.area","cluster",
                "latitude", "longitude","country","active","change.protocol","d.threshold", "plot.shape",
                "slope","aspect","elevation", "soil.depth","soil.age",
                "soil.type","plot.contact","plot.contact.email", "loc.accuracy")

# VMI 11 -------------------------------------------------------------------
vmi_n <- 11
fl_name <- "./data/raw/VMI/vmi11/vmi11P.dat"
vmi11 <- read.vmi11(fl_name)

# checks
lapply(vmi11, dim)
lapply(vmi11, is.data.frame)
vmi11$kuvio %>%
  ggplot(aes(I_koord, P_koord, col=factor(lohko_muoto))) +
  geom_point() + coord_fixed()


# check how many stands in vmi11
n_stands_vmi11 <- vmi11$kuvio %>%
  filter(mluok %in% 1:2) %>%
  group_by(lohko, ka_id) %>%
  summarise(n_stands = n_distinct(kuvio_id))

table(n_stands_vmi11$n_stands)
round(prop.table(table(n_stands_vmi11$n_stands))*100, 2)

### Census table ------------------------------------------------------------
# consider only kuvio 1 (centre point stand)

census_vmi11 <- vmi11$kuvio %>%
  # filter(mluok %in% c("1", "2")) %>%
  filter(FRA == 1) %>%         # FAO definition forest land
  filter(kuvio_id == 1) %>%
  group_by(lohko, ka_id) %>%
  mutate(database.code = database.code,
         census.n = vmi_n,
         plot.id = paste(lohko, ka_id, sep="_"),
         census.id = paste(plot.id, census.n, sep="."),
         tmt.census.id = paste(database.code, census.id, sep="."), 
         tmt.plot.id = paste(database.code, plot.id, sep="."),
         census.date = lubridate::decimal_date(as.Date(pvm, format = "%d%m%y")),
         plot.area = kuvio_osuus_relas,
         management = ifelse( (teht_hakkuu1 == 0 | is.na(teht_hakkuu1)) |
                                (teht_hakkuu2 == 0 | is.na(teht_hakkuu2)) |
                                (teht_hakkuu2 == 0 | is.na(teht_hakkuu2)),
                              NA, 2), # more detailed management in separate DF for VMI12
         disturbance = NA,
         hakaika1_num = numeric_hak_aika(hak_aika1),
         hakaika2_num = numeric_hak_aika(hak_aika2),
         hakaika3_num = numeric_hak_aika(hak_aika3),
         time.human.intervention = pmap_dbl(list(hakaika1_num, hakaika2_num, hakaika3_num), 
                                            function(x, y, z) {
                                              min_out <- min(x, y, z, na.rm=TRUE)
                                              min_out <- ifelse(min_out == Inf, NA, min_out)
                                              }),
         hakaika_NA = all(is.na(hak_aika1) & is.na(hak_aika2) & is.na(hak_aika3)),
         stand.age = ika1_rinnankork + ika1_lisays,
         lianas = NA,
         land.ownership = case_when(
           omistajaryhma %in% c(1:3, 6, 8, 9) ~ "Private",
           omistajaryhma %in% c(4:5, 7) ~ "Public",
           omistajaryhma %in% c(0, "A") ~ "NA"),
         census.contact = NA,
         census.contact.email =  NA,
         )

table(census_vmi11$kuvio_id) # stand IDs should be all 1

census_vmi11 %>% # check: only one row per plot per census
  group_by(tmt.plot.id, tmt.census.id) %>%
  tally() %>% 
  with(table(n))

### Plot table --------------------------------------------------------------

# Transform coordinates to lat/lon
plot_sf <- vmi11$kuvio %>%
  mutate(p_koord = P_koord,
         i_koord = I_koord) %>%
  st_as_sf(coords=c("i_koord", "p_koord"), crs="epsg:2393", remove=FALSE) %>%
  st_transform("epsg:4326") %>%
  mutate(longitude = unlist(purrr::map(geometry,1)),
         latitude = unlist(purrr::map(geometry,2))) %>%
  select(-geometry) %>%
  as.data.frame()

# plot(plot_sf$latitude, plot_sf$longitude)

plot_vmi11 <- plot_sf %>%
  # filter(mluok %in% c("1", "2")) %>%
  filter(FRA == 1) %>%         # FAO definition forest land
  filter(kuvio_id == 1) %>%
  mutate(database.code = database.code,
         plot.id = paste(lohko, ka_id, sep="_"),
         tmt.plot.id = paste(database.code, plot.id, sep="."),
         plot.area = NA, # calculate based on fixed radius AND the proportion of forest in this area?
         cluster = lohko,
         country = "Finland",
         active = TRUE,
         change.protocol = FALSE,
         d.threshold = 0,
         plot.shape = "angle count",
         slope = NA,
         aspect = NA,
         elevation = korkeus,
         soil.depth = NA,
         soil.age = NA,
         soil.type = NA,
         plot.contact = NA,
         plot.contact.email = NA,
         loc.accuracy = NA)

### Tree table --------------------------------------------------------------
# REMOVED TREES THAT ARE DEAD ALREADY IN VMI11!

tree_vmi11 <- vmi11$puu %>%
  left_join(sp_codes) %>%
  left_join(vmi11$kuvio %>% select(lohko, lohko_muoto, 
                                   lohkoX, lohkoY, ka_id, kuvio_id,
                                   pvm, mluok)) %>%
  left_join(census_vmi11 %>% select(lohko, lohko_muoto, ka_id,
                                    kuvio_osuus_relas)) %>%
  filter(kuvio_id == 1) %>%
  # filter(mluok %in% 1:2) %>%
  left_join(vmi11$kuvio %>% select(lohko, ka_id, kuvio_id, FRA)) %>%
  filter(FRA == 1) %>%         # FAO definition forest land
  filter(!puutyyppi %in% c("Z", "M", "J", "E", "P")) %>% # remove trees that do not belong to sample plot in vmi11
  filter(!puuluokka %in% c("A", "B", "C", "D", "E", "F", "G")) %>% # remove trees dead in vmi11
  mutate(database.code = database.code,
         census.n = vmi_n,
         plot.id = paste(lohko, ka_id, sep="_"),
         census.id = paste(plot.id, census.n, sep="."),
         tree.id = paste(plot.id, puu_id, sep="."),
         tmt.tree.id = paste(database.code, tree.id, sep="."),
         tmt.census.id = paste(database.code, census.id, sep="."), 
         tmt.plot.id = paste(database.code, plot.id, sep="."),
         tmt.obs.id = paste(tmt.tree.id, census.n, sep="."),
         census.date = lubridate::decimal_date(as.Date(pvm, format = "%d%m%y")), # from kuvio pvm
         species.id = puulaji,
         tmt.species.id = paste(database.code,species.id, sep = '.'),
         species.orig = species,
         genus.orig = word(species.orig, 1),
         family.orig = NA,
         func.type = NA,
         subplot.id = NA,
         tree.coord.x = NA,
         tree.coord.y = NA,
         azimuth = suunta, # 1/400 -- needs to be transformed to 1-360 if used!
         dist.centre = etaisyys,
         d = dbh_mm,
         pom = 1.3,
         height = pituus,
         agb = NA,
         ba = pi*(d/2)^2,
         n.plot = n_ha_vmi(d=d, 
                           lomu=lohko_muoto, 
                           forest_share = kuvio_osuus_relas/10),
         n.plot.fullarea = n_ha_vmi(d=d, lomu=lohko_muoto),
         g.plot = ba * n.plot, # calculate
         g.plot.fullarea = ba * n.plot.fullarea,
         n.ha = n.plot, # calculate
         tree.status = ifelse(puuluokka %in% c("A", "B", "C", "D", "E", "F", "G"),
                              1, 0),
         mode.death = case_when (
           puuluokka %in% c("C", "D") ~ "1", # natural death (unknown if standing or failing, i.e. lying dead)
           puuluokka %in% c("A") ~ "1s", # natural standing
           puuluokka %in% c("B") ~ "1f", # natural failing, (wind related)
           puuluokka %in% c("F", "G") ~ "1h", # natural loss followed by salvage logging
           puuluokka %in% c("E") ~ "2", # harvested
           TRUE ~ "NA" # tree is alive
         ),
         mode.death.other = NA, # could be derived from tuho-columns
         canopy.position = NA,
         liana.infe = NA,
         multistem = NA,
         harvest.status = ifelse(puuluokka %in% c("E"), 1, 0),
         harvest.type = kanto_haktapa) %>%
  filter(tree.status == 0) %>%
  filter(d > 0)

# VMI 12 ------------------------------------------------------------------
vmi_n <- 12
fl_name <- "./data/raw/VMI/vmi12/vmi12P.dat"
vmi12 <- read.vmi12(fl_name)

# checks
lapply(vmi12, dim)
lapply(vmi12, is.data.frame)
vmi12$kuvio %>%
  ggplot(aes(I_ykj, P_ykj, col=factor(lohko_muoto))) +
  geom_point() + coord_fixed()

### Census table ------------------------------------------------------------

# stand harvest info levels
harvest_levels <- c("Tending of seedling stands",
                    "Precommercial thinning",
                    "Other thinning",
                    "Thinning, mature stand",
                    "Overstorey cutting",
                    "Regeneration cut, clear cut",
                    "Regeneration cut, natural reg.",
                    "Shelterwood cut",
                    "Special cutting",
                    "Selective cut for unevenaged",
                    "Small area cut for unevenaged",
                    "Removing trees, unproductive peatland")

harvest_levels <- rev(harvest_levels)

# Go
census_vmi12 <- vmi12$kuvio %>%
  group_by(lohko, ka_id) %>%
  filter(kuvio_id == 1) %>%
  mutate(database.code = database.code,
         census.n = vmi_n,
         plot.id = paste(lohko, ka_id, sep="_"),
         census.id = paste(plot.id, census.n, sep="."),
         tmt.census.id = paste(database.code, census.id, sep="."), 
         tmt.plot.id = paste(database.code, plot.id, sep="."),
         census.date = lubridate::decimal_date(as.Date(pvm, format = "%d%m%y")),
         plot.area = NA, 
         management = ifelse( (teht_hakkuu1 == 0 | is.na(teht_hakkuu1)) |
                                (teht_hakkuu2 == 0 | is.na(teht_hakkuu2)) |
                                (teht_hakkuu2 == 0 | is.na(teht_hakkuu2)),
                              NA, 2), # more detailed management in separate DF for VMI12
         disturbance = NA,
         hakaika1_num = numeric_hak_aika(hak_aika1),
         hakaika2_num = numeric_hak_aika(hak_aika2),
         hakaika3_num = numeric_hak_aika(hak_aika3),
         time.human.intervention = pmap_dbl(list(hakaika1_num, hakaika2_num, hakaika3_num), 
                                            function(x, y, z) {
                                              min_out <- ifelse(all(is.na(c(x, y, z))),
                                                     NA,
                                                     min(x, y, z, na.rm=TRUE))
                                              return(min_out)
                                            }),
         stand.age = ika1_rinnankork + ika1_lisays,
         lianas = NA,
         land.ownership = case_when(
           omistajaryhma %in% c(1:3, 6, 8, 9) ~ "Private",
           omistajaryhma %in% c(4:5, 7) ~ "Public",
           omistajaryhma %in% c(0, "A") ~ "NA"),
         census.contact = NA,
         census.contact.email =  NA,
         harvest_5y = ifelse(hak_aika1%in%as.character(0:5), teht_hakkuu1, NA),
         stand_harvest_type = recode(harvest_5y,
                                     "1" = "Tending of seedling stands",
                                     "2" = "Tending of seedling stands",
                                     "3" = "Precommercial thinning",
                                     "C" = "Precommercial thinning",
                                     "4" = "Other thinning",
                                     "D" = "Other thinning",
                                     "5" = "Thinning, mature stand",
                                     "6" = "Overstorey cutting",
                                     "7" = "Regeneration cut, clear cut",
                                     "8" = "Regeneration cut, natural reg.",
                                     "9" = "Shelterwood cutting",
                                     "A" = "Special cutting",
                                     "E" = "Selective cutting, unevenaged",
                                     "P" = "Small area cutting, unevenaged",
                                     "S" = "Removal of trees, unproductive peatland"),
         stand_harvest_type = factor(stand_harvest_type, levels = harvest_levels))

### Tree table --------------------------------------------------------------

tree_vmi12 <- vmi12$puu %>%
  left_join(sp_codes) %>%
  left_join(vmi12$kuvio %>% select(lohko, lohko_muoto,
                                   lohkoX, lohkoY, ka_id, kuvio_id,
                                   pvm, mluok, kuvio_osuus_9)) %>%
  filter(!puutyyppi %in% c("U", "S", "T")) %>% # remove new trees in vmi12 added because of change in plot type!
  mutate(database.code = database.code,
         census.n = vmi_n,
         plot.id = paste(lohko, ka_id, sep="_"),
         census.id = paste(plot.id, census.n, sep="."),
         tree.id = paste(plot.id, puu_id_vanha, sep="."),
         tmt.tree.id = paste(database.code, tree.id, sep="."),
         tmt.census.id = paste(database.code, census.id, sep="."), 
         tmt.plot.id = paste(database.code, plot.id, sep="."),
         tmt.obs.id = paste(tmt.tree.id, census.n, sep="."),
         census.date = lubridate::decimal_date(as.Date(pvm, format = "%d%m%y")), # from kuvio pvm
         puutyyppi_12 = puutyyppi,
         species.id = puulaji,
         tmt.species.id = paste(database.code,species.id, sep = '.'),
         species.orig = species,
         genus.orig = word(species.orig, 1),
         family.orig = NA,
         func.type = NA,
         subplot.id = NA,
         tree.coord.x = NA,
         tree.coord.y = NA,
         azimuth = suunta, # 1/400 -- needs to be transformed to 1-360!
         dist.centre = etaisyys,
         d = dbh_mm,
         pom = 1.3,
         height = pituus,
         agb = NA,
         ba = pi*(d/2)^2,
         n.plot = n_ha_vmi(d=d, 
                           lomu=lohko_muoto, 
                           forest_share = kuvio_osuus_9/10), # calculated from the whole 9m plot, this not used at the moment for vmi12
         g.plot = ba * n.plot, # calculate
         n.ha = n.plot, # calculate
         tree.status = ifelse(puuluokka %in% c("A", "B", "C", "D", "E", "F", "G"),
                              1, 0),
         mode.death = case_when (
           puuluokka %in% c("C", "D") ~ "1", # natural death (unknown if standing or failing, i.e. lying dead)
           puuluokka %in% c("A") ~ "1s", # natural standing
           puuluokka %in% c("B") ~ "1f", # natural failing, (wind related)
           puuluokka %in% c("F", "G") ~ "1h", # natural loss followed by salvage logging
           puuluokka %in% c("E") ~ "2", # harvested
           TRUE ~ "NA" # tree is alive
         ),
         harvest.status = ifelse(puuluokka %in% c("E"), 1, 0),
         harvest.type = kanto_haktapa,
         mode.death.other = NA, # could be derived from tuho-columns
         canopy.position = NA,
         liana.infe = NA,
         multistem = NA) %>%
  filter(d > 0)

# Should be EMPTY TIBBLE (are there any trees without census information in vmi12?)
tree_vmi12 %>% 
  filter(!(tmt.census.id %in% census_vmi12$tmt.census.id) ) %>%
  select(tmt.tree.id, tmt.census.id, lohko, ka_id, kuvio_id)

### Plot table ---------------------------------------------------------------

# Transform coordinates to lat/lon
plot_sf12 <- vmi12$kuvio %>%
  mutate(p_koord = P_ykj,
         i_koord = I_ykj) %>%
  st_as_sf(coords=c("i_koord", "p_koord"), crs="epsg:2393", remove=FALSE) %>%
  st_transform("epsg:4326") %>%
  mutate(longitude = unlist(purrr::map(geometry,1)),
         latitude = unlist(purrr::map(geometry,2))) %>%
  select(-geometry) %>%
  as.data.frame()

plot(plot_sf12$longitude, plot_sf12$latitude)

plot_vmi12 <- plot_sf12 %>%
  filter(kuvio_id == 1) %>%
  mutate(database.code = database.code,
         plot.id = paste(lohko, ka_id, sep="_"),
         tmt.plot.id = paste(database.code, plot.id, sep="."),
         plot.area = NA, # calculate based on fixed radius AND the proportion of forest in this area?
         cluster = lohko,
         country = "Finland",
         active = TRUE,
         change.protocol = TRUE,
         d.threshold = 0,
         plot.shape = NA,
         slope = NA,
         aspect = NA,
         elevation = korkeus,
         soil.depth = NA,
         soil.age = NA,
         soil.type = NA,
         plot.contact = NA,
         plot.contact.email = NA,
         loc.accuracy = NA)



# Represented forest area -------------------------------------------------
# Calculation based on Tomppo et al. 2011 book, section 3.1, Eqs 3.1 - 3.3

## Read areas per municipality
# V1 = municipality code
# kunnat13: V4 = land area of municipality
# kunnat19: V5 = land area of municipality
kunnat13 <- read.csv("./data/raw/VMI/vmi11/kunnat13_lmuoto.csv", header=FALSE)
kunnat19 <- read.csv("./data/raw/VMI/vmi12/kunnat19.csv", header=FALSE)

head(kunnat19)
sum(kunnat19$V5)

sum(kunnat13$V4)
head(kunnat13)
table(kunnat13$V2, useNA = "always")
table(kunnat13$V3, useNA = "always")

vmi12_plots <- vmi12$kuvio %>%
  filter(kuvio_id == 1 & !(mluok %in% c("A", "B")))

vmi11_plots <- vmi11$kuvio %>%
  filter(kuvio_id == 1 & !(mluok %in% c("A", "B")))

summary(as.numeric(unique(vmi12_plots$kunta)) %in% kunnat19$V1)
kunnat_puuttuu12 <- setdiff(as.numeric(vmi12_plots$kunta), kunnat19$V1)
kunnat13 %>%
  filter(V1 %in% kunnat_puuttuu12)

kunnat13

summary(as.numeric(unique(vmi12_plots$kunta)) %in% kunnat13$V1)
summary(as.numeric(unique(vmi12_plots$kunta)) )

# forest area per sampling region

reparea_lomu <- vmi12_plots %>%
  group_by(lohko_muoto,kunta) %>%
  summarize(n_all_kunta = n(),
            n_forest_kunta = sum(mluok%in%1:2)) %>%
  mutate(kunta = as.numeric(kunta)) %>%
  left_join(kunnat13 %>%
              rename(kunta = V1,
                     kunta_nimi = V8,
                     kunta_ala = V4) %>%
              select(kunta, kunta_nimi, kunta_ala)) %>%
  group_by(lohko_muoto) %>%
  summarize(n_all_lomu = sum(n_all_kunta), # all plots within sampling area (=lomu =lohko muoto)
         n_forest_lomu = sum(n_forest_kunta), # forest plots within lomu
         area_lomu = sum(kunta_ala), # total land area of lomu (from municipality table)
         forest_share_lomu = n_forest_lomu/n_all_lomu, # share of forest plots
         forest_area_lomu = area_lomu * forest_share_lomu, # total forest are in lomu
         forest_area_rep = forest_area_lomu/n_forest_lomu) # represented forest area per forest plot

write.csv(reparea_lomu, 
          file=paste0("./data/raw/VMI/processed/represented_area_vmi12_", 
                      fl_date, ".csv") )

reparea_plots <- census_vmi12 %>%
  select(tmt.plot.id, tmt.census.id, lohko_muoto, lohko, ka_id, kuvio_id) %>%
  left_join(reparea_lomu)

write.csv(reparea_plots, 
          file=paste0("./data/raw/VMI/processed/represented_area_per_plot_vmi12_", 
                      fl_date, ".csv") )

# Combine & quality control ---------------------------------------------------------
# - combine vmi 11 & 12s
# - remove trees that are dead already in VMI11
# - check/rename species

#### Remove plots excluded from vmi12 (e.g. northern Lapland) ----

dim(tree_vmi11)
dim(plot_vmi11)
dim(census_vmi11)

tree_vmi11 <- tree_vmi11 %>%
  filter(tmt.plot.id %in% plot_vmi12$tmt.plot.id)

plot_vmi11 <- plot_vmi11 %>%
  filter(tmt.plot.id %in% plot_vmi12$tmt.plot.id) 

census_vmi11 <- census_vmi11 %>%
  filter(tmt.plot.id %in% plot_vmi12$tmt.plot.id)

#### Check that all vmi11 trees found in next census ----

summary(tree_vmi11$tmt.tree.id %in% tree_vmi12$tmt.tree.id)

#### n.ha = 0 even though tree alive?? ----
# only in vmi12 -- leaving this now, because n.ha info not used for the latest census (only information about harvest status!)
# puutyyppi/tree type (number of cases):
# A (111) - alive and not within new plot (over 9 meters from centre)
# C (2) - dead and not within new plot (over 9 meters from centre) --> tree.status should be 1
# J (4) - tree alive but land class changed (not included in the plot because of this)
# M (5) - land class changed and tree no longer present --> tree status should be 1
# V (50) - old sample tree, also sample tree in vmi12

tree_vmi12 %>%
  filter(n.ha == 0 & tree.status == 0) %>%
  with(table(puutyyppi))

idx_census <- tree_vmi12 %>%
  filter(n.ha == 0 & tree.status == 0) %>% pull(unique(tmt.census.id))

census_vmi12 %>%
  filter(tmt.census.id %in% idx_census)
  
tree_vmi12 %>%
  # filter(puutyyppi %in% c("A", "J", "M", "V", "P")) %>%
  filter(n.ha == 0 & tree.status == 0) %>%
  select(puutyyppi, puuluokka) %>%
  with(table(puutyyppi, puuluokka, useNA = "always"))
  # with(table(puutyyppi, is.na(puuluokka)))


# Combine 11 and 12 ----------------------------------------------------

vmi_harvest <- tree_vmi12 %>%   
  select(database.code, tmt.plot.id, tmt.census.id, census.n, tmt.tree.id, tmt.obs.id, 
         puuluokka, tree.status, harvest.status)

tree_vmi11_2 <- tree_vmi11[, tree_cols]
census_vmi11_2 <- census_vmi11[,census_cols]
plot_vmi11_2 <- plot_vmi11 %>%  filter(kuvio_id == 1) %>% select(any_of(plot_cols))

tree_vmi12_2 <- tree_vmi12[, tree_cols]
census_vmi12_2 <- census_vmi12[,census_cols]
plot_vmi12_2 <- plot_vmi12 %>%  filter(kuvio_id == 1) %>% select(any_of(plot_cols))

tree_vmi <- rbind(tree_vmi11_2, tree_vmi12_2)
census_vmi <- rbind(census_vmi11_2, census_vmi12_2)

# for plot_info:
# - take only from vmi12, since we're only including data measured in both censuses
# - take only plots included for census_info
plot_vmi <- rbind(plot_vmi12_2) %>% filter(tmt.plot.id %in% census_vmi$tmt.plot.id) 

if(any(duplicated(plot_vmi$tmt.plot.id))){
  stop("Error: Duplicates in plot_info!")
} else {
  message("No duplicates in plot_info")
}

# DBH threshold -----------------------------------------------------------
# Remove observations with DBH < dbh_thold (except if above thold in first measurement!)
dbh_thold

n_org <- nrow(tree_vmi)

tree_vmi2 <- tree_vmi %>%
  group_by(tmt.tree.id) %>%
  mutate(meas1 = min(census.n),
         in_1st_meas = d[census.n == meas1] >= dbh_thold,
         in_final = d >= dbh_thold | in_1st_meas)

# Example: below threshold in vmi11, above threshold in vmi12, only 12 included
tree_vmi2 %>%
  filter(tmt.tree.id == "FIN.0510006_10.4") %>%
  select(tmt.tree.id, census.n, d, meas1, in_1st_meas, in_final) %>%
  arrange(tmt.tree.id) 

# Example: above threshold in vmi11, below threshold in vmi12 (shrunk), both included
tree_vmi2 %>%
  filter(tmt.tree.id == "FIN.0512010_9.14") %>%
  select(tmt.tree.id, census.n, d, meas1, in_1st_meas, in_final) %>%
  arrange(tmt.tree.id)

tree_vmi <- tree_vmi2 %>%
  filter(in_final) %>%
  select(!any_of(c("meas1", "in_1st_meas", "in_final")))

n_new <- nrow(tree_vmi)

message(paste("DBH thredhold set (dbh >=",
              dbh_thold, 
              " mm). Total",
              n_org - n_new, "trees removed."))



#  Check n.ha -------------------------------------------------------------

summary(tree_vmi$n.ha) 

tree_vmi %>%
  left_join(census_vmi11 %>% select(tmt.census.id, plot.area)) %>% # plot area = kuvion osuus relaskoop. koealasta
  filter(n.ha > 1e3) %>%
  select(d, plot.area)

tree_vmi %>%
  filter(lo)

vmi11$kuvio %>%
  filter(lohko == "0512004" & ka_id == 11) %>%
  select(lohko_muoto, kuvio_osuus_relas)

vmi11$puu %>%
  filter(lohko == "0512004" & ka_id == 11) %>%
  select(lohko, ka_id, puuluokka, puutyyppi, dbh_mm)

n_ha_vmi(151, 0, 7)
n_ha_vmi(289, 0, 3)

# Cross-check tables ------------------------------------------------------

#### include census and plot ID only if trees in data ----
census_vmi <- census_vmi %>%
  filter(tmt.census.id %in% tree_vmi$tmt.census.id)

plot_vmi <- plot_vmi %>%
  filter(tmt.plot.id %in% tree_vmi$tmt.plot.id)

#### ID checks ----

# should be all TRUE (are all census/plot IDs of tree table included in the census/plot tables?)
summary(tree_vmi$tmt.census.id %in% census_vmi$tmt.census.id)
summary(tree_vmi$tmt.plot.id %in% plot_vmi$tmt.plot.id)

# should be all FALSE (do census/plot tables have census/plot ids not in tree table?)
summary(!(census_vmi$tmt.census.id %in% tree_vmi$tmt.census.id))
summary(!(plot_vmi$tmt.plot.id %in% tree_vmi$tmt.plot.id))

# all VMI12 plots measured/included in VMI11?
# - NO, these are cases where the plot in vmi11 does not have alive trees above 
#   DBH threshold, or doesn't meet the criteria in other ways (e.g. mluok)
# --> removing these from data
ids_check <- census_vmi %>%
  group_by(tmt.plot.id) %>%
  mutate(n_census = n()) %>%
  filter(n_census == 1) %>%
  pull(tmt.plot.id)

## Explore
# vmi11$kuvio %>%
#   # filter(lohko == "0508010" & ka_id == 4) %>%
#   filter(kuvio_id == 1) %>%
#   mutate(database.code = database.code,
#          census.n = vmi_n,
#          plot.id = paste(lohko, ka_id, sep="_"),
#          census.id = paste(plot.id, census.n, sep="."),
#          tmt.census.id = paste(database.code, census.id, sep="."), 
#          tmt.plot.id = paste(database.code, plot.id, sep=".") ) %>%
#   filter(tmt.plot.id %in% ids_check) %>%
#   # ggplot(aes(I_koord, P_koord)) + geom_point()
#   select(tmt.plot.id, lohko, ka_id, kuvio_id, mluok)
# 
# # exaple all trees dead or not found or smaller than dbh threshold
# vmi11$puu %>%
#   # filter(lohko == "0508010" & ka_id == 4) %>%
#   filter(lohko == "1530050" & ka_id == 2) %>%
#   filter(kuvio_id == 1) %>%
#   mutate(database.code = database.code,
#          census.n = vmi_n,
#          plot.id = paste(lohko, ka_id, sep="_"),
#          census.id = paste(plot.id, census.n, sep="."),
#          tmt.census.id = paste(database.code, census.id, sep="."), 
#          tmt.plot.id = paste(database.code, plot.id, sep=".") ) %>%
#   select(lohko, ka_id, kuvio_id, puu_id, dbh_mm, kuvio_id, puuluokka, puutyyppi)
# 
# tree_check <- tree_vmi %>%
#   filter(tmt.plot.id %in% ids_check) %>%
#   group_by(tmt.plot.id) %>%
#   mutate(n_measurements = n_distinct(census.n))
# 
# table(tree_check$n_measurements)
# n_distinct(tree_check$tmt.plot.id)


# remove if not included in vmi11

dim(tree_vmi); dim(census_vmi); dim(plot_vmi)

tree_vmi <- tree_vmi %>%
  filter(!(tmt.plot.id %in% ids_check))

census_vmi <- census_vmi %>%
  filter(!(tmt.plot.id %in% ids_check))

plot_vmi <- plot_vmi %>%
  filter(!(tmt.plot.id %in% ids_check))

dim(tree_vmi); dim(census_vmi); dim(plot_vmi)

# check that all plots have 2 census in the data
census_vmi %>%
  group_by(tmt.plot.id) %>%
  mutate(n_census = n()) %>%
  with(table(n_census))

# TMt Quality control ---------------------------------------------------------

# repeated dead?

tree_repeated_dead <- tree_vmi %>%
  group_by(tmt.tree.id) %>%
  mutate(n_dead = sum(tree.status == 1))

table(tree_repeated_dead$n_dead) # trees have status=dead max. one time, no problems here!

# Added 'return(tree)' to the end of function MissingD(), otherwise did not work!
source("./src/treemort-database-master/01_Format-data/functions/04_QualityControl.R") 

nrow_tree <- nrow(tree_vmi)

qc_fin <- QualityControl(tree_vmi,
                         database.code = "FIN",
                         plot_info = plot_vmi,
                         census_info = census_vmi)

tree_qc <- qc_fin[[1]]
census_qc <- qc_fin[[2]]
plot_qc <- qc_fin[[3]]

# Check that quality control does not add trees
if(nrow_tree < nrow(tree_qc)) {
  stop("CHECK!!! New trees added in the TMt quality control!!!")
} 
if(nrow_tree > nrow(tree_qc)) {
  stop("CHECK!!! Trees removed in the TMt quality control!!! 
       (This usually ok if relates to the min diameter threshold)")
} 

qc_fin[[4]] # repeated.dead actually are below.threshold.d??

table(tree_vmi$census.n)
table(tree_qc$census.n)

# indeed species sometimes changes between censuses...
tree_vmi %>%
  group_by(tmt.tree.id) %>%
  mutate(n_sp = n_distinct(species.orig)) %>%
  filter(n_sp > 1) %>%
  select(tmt.tree.id, species.orig, n_sp, census.n) %>%
  arrange(tmt.tree.id)


# TMt correct species names -----------------------------------------------
# Can't get the functions to work.
# source("./src/treemort-database-master/01_Format-data/functions/05_CorrectSpp.R") # which package is TPL func from??

if(check_sp_names){
  source("./src/treemort-database-master/01_Format-data/functions/02_Conver2TMtData/02_FormatTMtSppData.R")
  
  spp_fin <- FormatTMtSppData(tree = tree_qc,
                              database.code = "FIN")
  
  tpl_spp <- Taxonstand::TPL(spp_fin$species.orig)
  
  spp_fin <- cbind(spp_fin, tpl_spp)
  
  spp_fin <- spp_fin %>%
    mutate(species.cor = paste(New.Genus, New.Species),
           genus.cor = New.Genus,
           family.cor = ifelse(Family == "", NA, Family) ) %>%
    select(tmt.species.id, species.orig, species.cor, genus.cor, family.cor)
  
  tree_qc <- tree_qc %>%
    left_join(spp_fin)
  
  colnames(tree_qc)
} else { 
  # if not running the sp name check, just add specoes col with old data 
  # and match the genus + family from NSW species table (because these colnames needed later)
  spp_NSW <- read.csv("./data/processed/additional_tables/NSW_spp_table.csv")
  spp_NSW <- rbind(spp_NSW,
                   c("Juniperus communis", "Juniperus", "Cupressaceae")) %>%
    select(!species.cor) %>% distinct()
  
  tree_qc <- tree_qc %>%
    mutate(species.cor = species.orig,
           genus.cor = genus.orig) %>%
    left_join(spp_NSW)
}


# More checks and changes -------------------------------------------------

#### DBH = 0 for dead trees ----
tree_qc$d[tree_qc$tree.status == 1] <- 0


# Write files -------------------------------------------------------------

# TMt files
write.csv(tree_vmi,
          file= paste0(out_dir, "01_treedata_TMt_FIN_", fl_date, ".csv"),
          row.names=FALSE)

write.csv(census_vmi,
          file= paste0(out_dir, "03_census-info_TMt_FIN_", fl_date, ".csv"),
          row.names=FALSE)

write.csv(plot_vmi,
          file= paste0(out_dir, "04_plot-info_TMt_FIN_", fl_date,".csv"),
          row.names=FALSE)

# qc file for treedata

write.csv(tree_qc,
          file= paste0(out_dir, "01_qc-treedata_TMt_FIN_", fl_date, ".csv"),
          row.names=FALSE)

# harvest.status
write.csv(vmi_harvest,
          file= "./data/processed/harvest_status/tree_harvest_status_FIN.csv",
          row.names = FALSE)

# vmi12 census data with harvest info etc. all columns

write.csv(census_vmi12, 
          file=paste0("./data/raw/VMI/processed/vmi12_census_w_harvest_", 
                      fl_date, ".csv"),
          row.names=FALSE)

