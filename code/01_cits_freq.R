# Filename: 01_cits_freq.R (2017-11-21)
#
# TO DO: - Compare visually number of publications/citations between qualitative
#          GIS and GIS research as a whole
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. VISUALIZATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("lattice")
library("grid")
library("gridExtra")
library("data.table")
library("tidyverse")

# attach data
tc = readRDS("images/00_tc.rds")
wos = readRDS("images/00_wos.rds")
qual = readRDS("images/00_qual.rds")
gis_all = readRDS("images/00_gis_all.rds")

#**********************************************************
# 2 VISUALIZATION------------------------------------------
#**********************************************************

# construct publication frequency table per year
# left_join to keep all wos records
pubs = left_join(wos, dplyr::select(qual, fid_citavi, Qual_Context),
                 by = c("id_citavi" = "fid_citavi"))
dim(pubs)  # 380
# check
pubs[duplicated(pubs$id_citavi), ]  # 0, perfect
pubs[duplicated(pubs$WOS), ]  # 0, perfect

# construct number of publication-per-year table (qualitative GIS)
pubs_agg = group_by(pubs, year) %>%
  summarize(pubs = sum(length(year)))

# aggregate citation-per-year table
tc_agg = group_by(tc, year) %>%
  summarize(tc = sum(tc))

# join number of publications and citations
pub_cits = inner_join(pubs_agg, tc_agg, by = "year") %>%
  # just keep publications which appeared prior to 2017
  filter(year < 2017) 
summarize_at(pub_cits, c("pubs", "tc"), sum)
# compute percentages
pub_cits = mutate(pub_cits,
                  pubs = pubs / sum(pubs) * 100,
                  tc = tc / sum(tc) * 100)
pub_cits = reshape2::melt(pub_cits, id.var = "year")
names(pub_cits) = c("year", "class", "qual_gis")
levels(pub_cits$class) = c("Publications", "Citations")

# GIS research as a whole
gis_all = filter(gis_all, PY < 2017)
# find out the total number of studies/citations 
summarize_at(gis_all, c("TC", "n"), sum)
# compute percentages
gis_all = mutate(gis_all,
                 TC = TC / sum(TC) * 100,
                 n = n / sum(n) * 100)
gis_all = reshape2::melt(gis_all, id.var = "PY")
names(gis_all) = c("year", "class", "gis_all")
levels(gis_all$class) = c("Citations", "Publications")
# reverse level order
gis_all$class = factor(gis_all$class, levels = c("Publications", "Citations"))

# merge qual_gis studies with all_gis studies
d = full_join(gis_all, pub_cits, by = c("year", "class"))

p_1 = xyplot(gis_all + qual_gis ~ year | class, data = d, type = "b",
       col = c("black", grey(0.5)),
       ylab = list("%", cex = 0.8),
       xlab = list("Year", cex = 0.8),
       aspect = 0.75,
       layout = c(2, 1),
       between = list(x = 0.5),
       scales = list(
         # y = list(relation = "free"),
         # x = list(relation = "free"),
         tck = c(1, 0),
         alternating = c(1, 1)),
       strip = strip.custom(bg = "white"),
       par.strip.text = list(cex = 0.8),
       #Create your own legend
       key = list(space = "right", columns = 1,
                  text = list(c("GIS", "qualitative\nGIS"), cex = 0.6),
                  lines = list(col = c("black", grey(0.5)), lwd = 2,
                               cex = 0.6))
)

# save figure
png(filename = "figures/01_freq_cits.tif", width = 18,
    height = 8, units = "cm", res = 600)
print(p_1)
grid.text("a)", x = unit(0.12, "npc"), y = unit(0.825, "npc"),
          gp = gpar(font = "bold", fontsize = 10))
grid.text("b)", x = unit(0.46, "npc"), y = unit(0.825, "npc"),
          gp = gpar(font = "bold", fontsize = 10))
dev.off()

