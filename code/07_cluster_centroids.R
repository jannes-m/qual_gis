# Filename: 07_cluster_centroids.R (2018-12-20)
#
# TO DO: Find the 20 publications closest to the cluster centroid
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. TOP20 STUDIES NEAR CLUSTER CENTROIDS
# 3. TOP20 TABLE-BARPLOT
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("vegan")

# source your own functions
source("code/helper_funs.R")

# attach data
abs_df = readRDS("images/00_abs_df.rds")
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
dc = readRDS("images/03_trans_soft_qd.rds")
ord = readRDS("images/04_ord.rds")
clus = readRDS("images/04_classes_df.rds")

#**********************************************************
# 2 TOP20 STUDIES NEAR CLUSTER CENTROIDS-------------------
#**********************************************************

# add id_citavi
abs_df = inner_join(abs_df, dplyr::select(wos, WOS, id_citavi),
                    by = "WOS")
# add times cited
abs_df = inner_join(abs_df, dplyr::select(tc, tc, WOS), by = "WOS")

# find cluster centroids (centers) in ordination space
x = data.frame(scores(ord, display = "sites", choices = 1:2),
               class = clus$cluster)

plot(x[, 1:2], col = x$class, pch = 16)
cen = group_by(x, class) %>%
  summarize_all(funs(mean))
points(cen[, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
# find the points closest to each center
dists = sp::spDists(as.matrix(dplyr::select(x, -class)),
                    as.matrix(dplyr::select(cen, -class)))
dists = as.data.frame(dists)

# using a spatial approach (centroids of convex hulls)
# x = st_as_sf(x, coords = c("DCA1", "DCA2"))
# x_1 = filter(x, class == 1)
# x_1 = st_union(x_1) %>% st_convex_hull
# plot(st_centroid(x_1), add = TRUE, col = "blue", pch = 16)
# plot(st_geometry(x))
# plot(st_centroid(x_1), add = TRUE, col = "blue", pch = 16)
# points(within(cen, rm(class)), col = "red", cex = 1, pch = 16)

# use the original rownames (inherited from abs_df, i.e., id_citavi)
rownames(dists) = rownames(x)
# find the 5 closest points to each centroid
out = lapply(seq_len(ncol(dists)), function(i) {
  # make sure to only select points belonging to the corresponding cluster
  tmp = dists[x$class == levels(x$class)[i], ]
  # now select the twenty closest points
  rownames(tmp[order(tmp[, i]), ])[1:20]
})
# check
x[out[[1]], ]  # ok
x[out[[2]], ]  # ok
i = 2
filter(x, class == levels(x$class)[i]) %>% dplyr::select(-class) %>% plot
points(cen[i, c("DCA1", "DCA2")], cex = 3, pch = 16,  col = "yellow")
points(dplyr::select(x[out[[i]], ], -class), pch = 16, col = "lightblue")

res = data.frame(x[unlist(out), "class"], as.numeric(unlist(out)),
                 stringsAsFactors = FALSE)
names(res) = c("class", "id_citavi")
# check
tmp = inner_join(res, 
                 dplyr::select(wos, title, year, no_authors, WOS, id_citavi),
                 by = "id_citavi")
tmp = inner_join(dplyr::select(abs_df, titel, id_citavi), tmp,
                 by = "id_citavi")
filter(tmp, titel != title) %>%
  select(titel, title) # everything's fine, perfect

clus = select(tmp, -titel) %>%
  rename(cluster = class)
# add times cited
setdiff(clus$WOS, tc$WOS)
clus = inner_join(clus, dplyr::select(tc, -year), by = "WOS")

# table output for Susann
clus = group_by(clus, cluster) %>%
  # top_n(15, tc) %>%
  arrange(cluster, desc(tc))
# levels(clus$cluster)


# library("xlsx")
# write.xlsx(x = as.data.frame(clus), row.names = FALSE, 
#            file = "core_pubs.xlsx")

#**********************************************************
# 3 CREATE TABLE-BARPLOT-----------------------------------
#**********************************************************

# more or less the exact same code as used in 06_cluster_table.R but here only
# applied to the 20 pubs closest to the cluster center

# 3.1 Data preparation=====================================
#**********************************************************

# join GIS & Co.
setdiff(clus$WOS, dc$w)
clus_molt = inner_join(clus, select(dc, -year), by = c("WOS" = "w"))

compute_percentage = function(df, ...) {
  group_var = quos(...)
  df %>%
    group_by(!!!group_var) %>%
    summarize(n = n()) %>%
    mutate(total = sum(n),
           percent = round(n / total * 100, 2)) %>%
    select(!!!group_var, percent) %>%
    ungroup %>%
    tidyr::complete(!!!group_var, fill = list(percent = 0))
}

gis = compute_percentage(clus_molt, cluster, GIS)
gis = dplyr::rename(gis, feature = GIS) %>%
  mutate(cat = "GIS")
gis[gis$feature == "No GIS", "feature"] = NA

# gp = geoprocessing
transf = compute_percentage(clus_molt, cluster, t)
transf = dplyr::rename(transf, feature = t) %>%
  mutate(cat = "gp")

# dc = data collection
qdata = compute_percentage(clus_molt, cluster, qdata)
qdata = dplyr::rename(qdata, feature = "qdata") %>%
  mutate(cat = "dc")

# construct output table
out = rbind(gis, transf, qdata)

# 3.2 Barplots=============================================
#**********************************************************

# create gis barplots
d = filter(out, cat == "GIS")
d = d %>% mutate(feature = forcats::fct_explicit_na(feature),
                 feature = forcats::fct_drop(feature))
# reorder 
d = arrange(d, cluster, feature)
save_barplot(d, value = "percent", bar_name = "feature", 
             dir_name = "figures/bars/bar_gis_core_")

# create geoprocessing barplots
d = filter(out, cat == "gp") %>%
  mutate(feature = gsub("-\\n", "", feature),
         feature = forcats::fct_explicit_na(feature))
save_barplot(d, value = "percent", bar_name = "feature",
             dir_name = "figures/bars/bar_geopro_core_")

# create data collection barplots
# first, find out which are the most widely used dc methods
filter(out, cat == "dc") %>%
  group_by(cluster) %>% arrange(cluster, desc(percent)) %>% slice(1:4) 
# filter the two most important dc methods of each cluster
d = filter(out, cat == "dc" & feature %in% 
             c("Mapping\nWorkshop", "Narration", NA, "Interview")) %>%
  mutate(feature = gsub("\\n", " ", feature),
         feature = forcats::fct_explicit_na(feature))

# create data collection barplots
save_barplot(d, value = "percent", bar_name = "feature", 
             dir_name = "figures/bars/bar_dc_core_")

# 3.3 Flextable============================================
#**********************************************************
# attach further necessary packages
library("flextable")
library("officer")

# I guess the year is not that important, so delete it
boxplot(clus$year ~ clus$cluster)
# summary table (which forms the basis of the subsequent flextable)
tab = group_by(clus, cluster) %>%
  summarize(n = n(),
            # median_year = median(year),
            mean_author = round(mean(no_authors), 2),
            mean_tc = mean(tc)) %>%
  # doesn't make any sense here, since we have considered 20 pubs per cluster
  arrange(desc(mean_tc))
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
gis_src = paste0("figures/bars/bar_gis_core_", levels(clus$cluster), ".png")
ft = display(ft,
             i = 1:4,
             col_key = "gis", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("gis", src = gis_src, width = 1.8,
                                   height = 0.9)))

geopro_src = paste0("figures/bars/bar_geopro_core_", levels(clus$cluster), ".png")
ft = display(ft,
             i = 1:4,
             col_key = "geopro", 
             pattern = "{{pic}}",
             formatters = 
               list(pic ~ as_image("geopro",
                                   src = geopro_src, width = 1.8,
                                   height = 0.9)))

dc_src = paste0("figures/bars/bar_dc_core_", levels(clus$cluster), ".png")
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

# create a docx file using Word (landscape mode)
# doc = read_docx(path = "~/Desktop/table_out.docx")
# doc = body_add_flextable(doc, value = ft)
# print(doc, target = "~/Desktop/table_out.docx")
# caption
# Summarizing the 20 papers that are closest to the cluster center by cluster group.