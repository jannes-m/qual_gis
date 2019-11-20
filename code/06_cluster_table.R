# Filename: cluster_table.R (2018-11-29)
#
# TO DO: cluster table: n, mean(authors), mean(citations), barcharts 
#        representing used GIS, geoprocessing method and data collection method
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. CREATE TABLE/FIGURE
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
# source your own helper functions
source("code/helper_funs.R")

# attach data
qual = readRDS("images/00_qual.rds")
wos = readRDS("images/00_wos.rds")
# times cited
tc = readRDS("images/00_tc.rds")
# k-mean cluster categories in a dataframe
clus = readRDS("images/04_classes_df.rds")
# data collection method
dc = readRDS("images/03_trans_soft_qd.rds")

# steps
# 1. add wos to qual 
# 2. add qual to classes via id_citavi 
# 3. join with 03_trans_soft_qd.rd -> create barcharts per class

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# sum(rownames(clus) == rownames(mat)) == nrow(mat)  # TRUE, perfect
setdiff(clus$id_citavi, qual$fid_citavi)
setdiff(clus$id_citavi, wos$id_citavi)
clus = inner_join(clus, dplyr::select(wos, year, no_authors, WOS, id_citavi),
                  by = "id_citavi")
# add times cited
setdiff(clus$WOS, tc$WOS)
setdiff(tc$WOS, clus$WOS) 
# 13 WOS missing, this is because there was no abstract for thirteen pubs,
# hence, we could not assign them a cluster categorie (which was based on the
# abstract words)
clus = inner_join(clus, dplyr::select(tc, -year), by = "WOS")

# join GIS & Co.
setdiff(clus$WOS, dc$w)
clus_molt = inner_join(clus, select(dc, -year), by = c("WOS" = "w"))

# compute percentages by groups
gis = compute_percentage(clus_molt, cluster, GIS)
gis = dplyr::rename(gis, feature = GIS) %>%
  mutate(cat = "GIS")
transf = compute_percentage(clus_molt, cluster, t)
# gp = geoprocessing
transf = dplyr::rename(transf, feature = t) %>%
  mutate(cat = "gp")
qdata = compute_percentage(clus_molt, cluster, qdata)
# dc = data collection
qdata = dplyr::rename(qdata, feature = "qdata") %>%
  mutate(cat = "dc")
# construct output table
out = rbind(gis, transf, qdata)

#**********************************************************
# 3 CREATE TABLE/FIGURE------------------------------------
#**********************************************************

# 3.1 Table version========================================
#**********************************************************
# attach further necessary packages
library("flextable")
library("officer")

# 3.1.1 Barplots###########################################
#**********************************************************
# create gis barplots
d = filter(out, cat == "GIS")
d[d$feature == "No GIS", "feature"] = NA
d = d %>% mutate(feature = forcats::fct_explicit_na(feature),
                 feature = forcats::fct_drop(feature))
# reorder 
d = arrange(d, cluster, feature)
save_barplot(d, value = "percent", bar_name = "feature", 
             dir_name = "figures/bars/bar_gis_")

# create geoprocessing barplots
d = filter(out, cat == "gp") %>%
  mutate(feature = gsub("-\\n", "", feature),
         feature = forcats::fct_explicit_na(feature))
# reorder factor levels
d$feature =
  factor(d$feature, rev(levels(d$feature)))
save_barplot(d, value = "percent", bar_name = "feature",
             dir_name = "figures/bars/bar_geopro_")

# create data collection barplots
# first, find out which are the most widely used dc methods
filter(out, cat == "dc") %>%
  group_by(cluster) %>% arrange(cluster, desc(percent)) %>% slice(1:4) 
# filter the two most important dc methods of each cluster
d = filter(out, cat == "dc" & feature %in% 
             c("Mapping\nWorkshop", "Survey", NA, "Interview")) %>%
  mutate(feature = gsub("\\n", " ", feature),
         feature = forcats::fct_explicit_na(feature))
# create data collection barplots
save_barplot(d, value = "percent", bar_name = "feature", 
             dir_name = "figures/bars/bar_dc_")

# 3.1.2 Create a flextable#################################
#**********************************************************

# I guess the year is not that important, so delete it
boxplot(clus$year ~ clus$cluster)
# summary table (which forms the basis of the subsequent flextable)
tab = group_by(clus, cluster) %>%
  summarize(n = n(),
            # median_year = median(year),
            mean_author = round(mean(no_authors), 2),
            mean_tc = mean(tc)) %>%
  arrange(desc(n))
# have a peak
tab
# ok, since we have changed the order of tab using desc(n), we have to change
# accordingly the cluster factor levels
tab$cluster = factor(tab$cluster, levels = tab$cluster)
clus$cluster = factor(clus$cluster, levels = tab$cluster)
# otherwise, we would add our barplots in the previous order of the cluster
# levels which would be very wrong...

# create the flextable
tab_2 = tab
# not sure how to tell a flextable to round, I have only figured out how to
# display only a certain number of digits. Therefore, round the numbers, convert
# them into a character and back into numerics
tab_2 = mutate_at(tab_2, .vars = c("mean_author", "mean_tc"),
                  .funs = function(x) as.numeric(as.character(round(x, 1))))
# add three columns which should hold the barplots
tab_2[, c("gis", "geopro", "dc")] = NA
ft = flextable(tab_2) 
# specify how the columns should be named in the output table
ft = set_header_labels(ft,
                       gis = "Used GIS (%)", 
                       geopro = "Applied geoprocessing (%)",
                       dc = "Data collection\nmethod (%)",
                       mean_author = "Mean # authors",
                       mean_tc = "Mean # citations")
# align
ft = align(ft, align = "center", part = "header")
ft = align(ft, align= "left", part = "header", j = c("gis", "geopro", "dc"))
ft = align(ft, align = "center")
# make sure that only one digit is shown in the case of numeric columns
ft = colformat_num(ft, digits = 1, j = c("mean_author", "mean_tc"))
# add barplots to the flextable
gis_src = paste0("figures/bars/bar_gis_", levels(clus$cluster), ".png")
ft = display(ft,
        i = 1:4,
        col_key = "gis", 
        pattern = "{{pic}}",
        formatters = 
          list(pic ~ as_image("gis", src = gis_src, width = 1.8,
                              height = 0.9)))
geopro_src = paste0("figures/bars/bar_geopro_", levels(clus$cluster), ".png")
ft = display(ft,
             i = 1:4,
             col_key = "geopro", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("geopro",
                                   src = geopro_src, width = 1.8,
                                   height = 0.9)))
dc_src = paste0("figures/bars/bar_dc_", levels(clus$cluster), ".png")
ft = display(ft,
             i = 1:4,
             col_key = "dc", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("dc",
                                   src = dc_src, width = 1.8,
                                   height = 0.9)))
# have a look at the output
ft

# create a docx file using libreoffice
# doc = read_docx(path = "~/Desktop/table.docx")
# doc = body_add_flextable(doc, value = ft)
# print(doc, target = "~/Desktop/table_out.docx")

# caption:
# Summarizing all reviewed papers by assigned cluster group. Please note that we have only considered the four most frequent data collection methods in column “Data collection method %”.