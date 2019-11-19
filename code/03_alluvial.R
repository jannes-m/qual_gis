# Filename: 03_alluvial.R (2018-04-30)
#
# TO DO:  Visualize contingency table (data collection methods, GIS methods,
#         used GIS), i.e., create an alluvial diagram
#
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. REPORTED PORTIONS
# 4. CONTINGENCY ANALYSIS
# 5. VISUALIZATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("ggthemes")
library("tidyverse")
library("plotly")
library("ggraph")
library("igraph")
library("magrittr")
# attach data
abs_df = readRDS("images/00_abs_df.rds")
qual = readRDS("images/00_qual.rds")
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
gis_all = readRDS("images/00_gis_all.rds")
# load keys
(load("images/00_keys.rda"))

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# join wos and qual
relevant = left_join(wos, qual, by = c("id_citavi" = "fid_citavi"))
table(relevant$fidQualGIS_transfer)
colSums(is.na(relevant))
# build required tables
transfer = select(relevant, w = WOS, t = fidQualGIS_transfer)
soft = select(relevant, w = WOS, year = year, GIS = fidGIS)
trans_soft = left_join(soft, transfer, by = "w")
# renaming trans_soft levels in accordance with gis_key
filter(gis_key, idGIS %in% trans_soft$GIS)
# RAP GIS is not open-source -> CLARIFICATION NEEDED!!
table(trans_soft$GIS)  # RAP-GIS 3 times
trans_soft %<>% mutate(GIS = as.factor(GIS))
levels(trans_soft$GIS) =
  list("No GIS" = "1",
       "ArcGIS" = "4",
       "free GIS" =
         levels(trans_soft$GIS)[!levels(trans_soft$GIS) %in% c("1", "4")])


# applied GIS method
setdiff(trans_soft$t, agis_key$idQualGIS_transfer)  # 0, perfect
# join
trans_soft =
  inner_join(trans_soft, dplyr::select(agis_key, -Description),
             by = c("t" = "idQualGIS_transfer")) %>%
  dplyr::select(-t, t = QualGIS_transfer) %>%
  mutate(t = as.factor(t))
# set "Modelling spatial reasoning to NA (-> is not really a GIS method)
levels(trans_soft$t) =
  c("GIS extensions", "Hyperlinks", NA, NA, "Transfor-\nmations")
colSums(is.na(trans_soft))  # ok, there are NAs
# but deleting them is not really a good idea
# trans_soft = trans_soft[complete.cases(trans_soft), ]

# Data collection methods (qdata colums)
qdata = separate(relevant, fidQualData, into = paste0("qdata", 1:8), sep = ";")
qdata = dplyr::select(qdata, w = WOS, starts_with("qdata"))

# reshape
qdata = reshape2::melt(qdata, id.vars = "w")
qdata %<>%
  select(-variable) %>%
  rename(qdata = value) %>%
  mutate(qdata = as.numeric(qdata)) %>%
  # delete NAs (reasonable here)
  filter(!is.na(qdata))

# renaming qdata in accordance with qdata_key
setdiff(qdata$qdata, qdata_key$idQualData)  # 36 and 37 are missing from the
# key table
setdiff(qdata_key$idQualData, qdata$qdata)  # 0, perfect
# add 36 and 37
qdata_key = add_row(qdata_key,
                    idQualData = 36:37,
                    Qual_Data = c("Q-Survey", NA))
qdata = inner_join(qdata, qdata_key, by = c("qdata" = "idQualData")) %>%
  dplyr::select(-qdata, qdata = Qual_Data) %>%
  mutate(qdata = as.factor(qdata))
group_by(qdata, w) %>% filter(duplicated(qdata))  # no duplicates, perfect
# level condensing
levels(qdata$qdata) %<>%
  fct_collapse("Interview" = c("Interview", "Recording"),
               "Narration" = c("Story", "Narration", "Diary"),
               "Survey" = c("Survey", "Questionnaires", "Q-Survey"),
               "Observation" = c("Observation", "Field notes"))
group_by(qdata, w) %>% filter(duplicated(qdata))  # 140 duplicates (915 obs)
# level condesing has led to duplicated data collection methods per 
# WOS ID, just keep unique ones
qdata = group_by(qdata, w) %>%
  filter(!duplicated(qdata)) %>%
  ungroup  # 775

# level renaming
levels(qdata$qdata) %<>%
  fct_recode("Focus Group" = "Focus Group Discussion",
             "Mapping\nWorkshop" = "mapping Workshop",
             "Sketch" = "sketch Maps")

colSums(is.na(qdata))  # 37 has been converted into NA
filter(qdata, is.na(qdata))  # 1 NA observation
# I think it's ok to delete it here, not really because this study hasn't
# indicated a data collection method
# qdata = qdata[!is.na(qdata$qdata), ]

trans_soft_qd = full_join(trans_soft, qdata, by = "w")
# saveRDS(trans_soft_qd, file = "images/03_trans_soft_qd.rds")

#**********************************************************
# 3 PORTIONS-----------------------------------------------
#**********************************************************

# these portions are reported in the text

# used qualitative data collection methods (don't use portions, since more than
# one dc method might have been recorded per manuscript)
group_by(qdata, qdata) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

# applied GIS methods (transformations; portions)
group_by(trans_soft, t) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per))

# applied GIS software (portions)
group_by(trans_soft, GIS) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n)) %>%
  arrange(desc(per))

# reported CAQDAS software
setdiff(relevant$fidCAQDAS, caq_key$idCAQDAS)  # 0, perfet
caq = select(relevant, fidCAQDAS) %>%
  left_join(., select(caq_key, idCAQDAS, CAQDAS_Typ),
             by = c("fidCAQDAS" = "idCAQDAS")) %>%
  group_by(CAQDAS_Typ) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n) * 100) %>%
  arrange(desc(per))
filter(caq, CAQDAS_Typ != "NA") %>%
  summarise_at(list(sum), .vars = c("n", "per"))

# reported database usage
setdiff(relevant$fidGeodatabase, gdb_key$idGeodatabase)
# ok, just keep the first recorded geodatabase
gdb = select(relevant, fidGeodatabase) %>%
  mutate(fidGeodatabase = gsub(";.*", "", fidGeodatabase)) %>%
  mutate(fidGeodatabase = as.integer(fidGeodatabase))
setdiff(gdb$fidGeodatabase, gdb_key$idGeodatabase)
# anti_join(., gdb_key, by = c("fidGeodatabase" = "idGeodatabase"))
# so there are already NAs and there is an undefined class (8)
# I suppose empty entries can be recorded as NAs
filter(gdb, is.na(fidGeodatabase))
gdb %<>%
  mutate(fidGeodatabase = ifelse(is.na(fidGeodatabase), 1, fidGeodatabase))
gdb %<>%
  left_join(., gdb_key, by = c("fidGeodatabase" = "idGeodatabase"))
# add an unknown db (8)
gdb = mutate(gdb,
             Geodatabase = ifelse(fidGeodatabase == 8, "unknown", Geodatabase))

gdb = group_by(gdb, Geodatabase) %>%
  summarize(n = n()) %>%
  mutate(per = n / sum(n) * 100) %>%
  arrange(desc(per))

filter(gdb, Geodatabase != "NA") %>%
  summarize_at(list(sum), .vars = c("n", "per"))
# however, this also includes a category named "no Database"...

#**********************************************************
# 4 ALLUVIAL DIAGRAM---------------------------------------
#**********************************************************

# Method 1: alluvial=======================================
#**********************************************************
library("alluvial")

dim(qdata)
dim(trans_soft)
setdiff(qdata$w, trans_soft$w)
ind = setdiff(trans_soft$w, qdata$w)  # wos ids not occurring in qdata
filter(trans_soft, w %in% ind)
n_distinct(qdata$w)
# this means, we don't have a qdata observation for these WOS (I'd say)
# let's check, qdata was created from df relevant and col fidQualData
filter(relevant, WOS %in% ind) %>% select(fidQualData)  # NAs, good
# so, we have to join the NA qdata again
d = left_join(trans_soft, qdata, by = "w")
d = select(d, -w, -year)

d %<>% group_by(qdata, t, GIS) %>%
  summarise(n = n())
colSums(is.na(d))
filter(d, is.na(qdata) | is.na(t))
# change "No GIS" to NA
d[d$GIS == "No GIS", "GIS"] = NA
# rename NA to (Missing)
d = d %>% 
  ungroup %>%
  mutate_at(c("qdata", "t", "GIS"), forcats::fct_explicit_na)

alluvial(d[, c(1:3)], freq = d$n, 
         col = ifelse(d$GIS == "No GIS", "grey", "orange"),
         border = ifelse(d$GIS == "No GIS", "grey", "orange"),
         hide = d$n == 0,
         cex = 0.6)

# Method 2: ggalluvial=====================================
#**********************************************************
library("ggalluvial")

# change order of the levels (relevel)
# make sure that qdata also uses (Missing) instead of NA
qdata$qdata = fct_explicit_na(qdata$qdata)  
# just to make sure
setdiff(levels(qdata$qdata), levels(d$qdata))  # 0, perfect
setdiff(levels(d$qdata), levels(qdata$qdata))  # 0, perfect
# relevel
d$qdata = factor(d$qdata, levels = names(sort(table((qdata$qdata)))))

# levels(d$qdata) = c("NA", names(sort(table(qdata$qdata))))
# levels(d$GIS) = c("ArcGIS", "free GIS", "No GIS")  # this only changes the labels but not the flows...
d_2 = to_lodes_form(d, key = "qdata", axes = c(1:2))
g = ggplot(data = d_2,
       aes(x = qdata, stratum = stratum, alluvium = alluvium,
           y = n, label = stratum)) +
  geom_alluvium(aes(fill = GIS)) +
  geom_stratum(width = 0.35) +
  geom_text(stat = "stratum", cex = 2.5) +
  theme_void(base_size = 6.5)  # theme_minimal()
g
ggsave("figures/03_alluvial.tif", g, device = "tiff", width = 15, height = 15 / 1.688, 
       units = "cm", dpi = 600)

# find out why there are NAs in the data collection method axes!
# because there were NAs in the beginning!
# filter(relevant, fidQualData == "") %>% select(WOS, fidQualData)

# https://github.com/corybrunson/ggalluvial/issues/13
# hava a look at function parameter relevel.strata
