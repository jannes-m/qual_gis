# Filename: 05_cluster_mapping.R (2018-04-25)
#
# TO DO: map kmeans classes
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. STATIC CLUSTER MAP
# 3. INTERACTIVE CLUSTER MAP
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("sp")
library("spData")
library("sf")
library("lattice")
library("latticeExtra")
library("leaflet")

# attach data
qual = readRDS("images/00_qual.rds")
wos = readRDS("images/00_wos.rds")
ind = readRDS("images/04_ind.rds")
ord = readRDS("images/04_ord.rds")
clus = readRDS("images/04_classes_df.rds")
# load("images/07_p_1.rda")

#**********************************************************
# 2 STATIC CLUSTER MAP-------------------------------------
#**********************************************************

# sum(rownames(clus) == rownames(mat)) == nrow(mat)  # TRUE, perfect
setdiff(clus$id_citavi, qual$fid_citavi)
# 13 manuscripts without an abstract
setdiff(qual$fid_citavi, clus$id_citavi)
clus = inner_join(clus,
                  dplyr::select(qual, fid_citavi, lat = Latitude,
                                lon = Longtitude),
                  by = c("id_citavi" = "fid_citavi"))
head(clus)
clus[clus$lon > 180 | clus$lon < -180, ]
clus[clus$lon > 180 | clus$lon < -180, "lon"] = -12.31

# check lat/lon data
dim(clus[clus$lon == 0 & clus$lat == 0, ])  # 100 studies without coordinates
# remove them
clus = clus[!(clus$lon == 0 & clus$lat == 0), ]

plot(st_geometry(world), ylim = c(-30, 30), xlim = c(-140, 170))
clus$col = as.factor(clus$cluster)
# use the exact same palette as in 07_word_ordi_cluster.R
pal = RColorBrewer::brewer.pal("Set3", n =  6)[3:6]
levels(clus$col) = pal
points(clus$lon, clus$lat, pch = 16, col = as.character(clus$col))
legend(0, -45, legend = paste("cluster", 1:4), fill = pal, ncol = 2)
# check if there were really studies in Hawaii and Tahiti
ind = filter(clus, lon < -129 & lon > -160) %>% dplyr::pull(id_citavi)
filter(qual, fid_citavi %in% ind)
filter(wos, id_citavi %in% c(109, 448))
# ok, first article is located in Hawaii, second in Polynesia, perfect

x = clus[, c("lon", "lat", "cluster")]
coordinates(clus) =~ lon + lat

world = spTransform(as(world, "Spatial"), "+proj=wintri")
proj4string(clus) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
clus = spTransform(clus, "+proj=wintri")
clus@data$cluster = as.factor(clus@data$cluster)

# this draws a "bar" legend, but I would prefer points...
spplot(clus, "cluster", col.regions = pal, border = "black", cex = 0.75,
       colorkey = list(
         right = list( # see ?levelplot in package trellis, argument colorkey:
           fun = draw.colorkey,
           args = list(
             key = list(
               at = 0:4,
               col = pal, width = 1, height = 0.5,
               labels = list(
               at = seq(0.5, 3.5, 1),
               labels = levels(clus$cluster))
             )
           ))),
       sp.layout = list(
         # list("sp.points", clus, col = "black", pch = 21, cex = 0.7),
         list("sp.polygons", world, col = "lightgrey",
              first = TRUE)
       ))

# # in fact, spplot.points uses xyplot (see github sp)
# xyplot(coordinates(clus)[, 2] ~ coordinates(clus)[, 1], asp = "iso", col = pal,
#        pch = 20,
#        key = list(points = list(fill = pal, pch = 21,
#                                 border = "black", cex = 2),
#                   space = "right",
#                   text = list(paste("cluster", 1:4)))
#        )
# # sth. not right with the point coloring but not important here...

map = spplot(clus, "cluster", col.regions = pal, cex = 0.8,
             key.space = "right",
             key = list(
               points = list(fill = pal, pch = 21, border = "black",
                             cex = 1.5),
               text = list(paste(levels(clus$cluster), "cluster"),
                           cex = 0.8)
             ),
             sp.layout = list(
               # list("sp.points", clus, col = "black", pch = 21, cex = 1.2),
               list("sp.polygons", world, col = "lightgrey",
                    first = TRUE)
             )
)

# save your result
tiff(filename = "figures/05_map_cluster.tif", res = 600,
    width = 17, height = 8, units = "cm")
print(map)
dev.off()

#**********************************************************
# 3 INTERACTIVE CLUSTER MAP--------------------------------
#**********************************************************

# reproject again
proj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
clus =
  # first transfrom since gdal of sf doesn't like "+proj=wintri"
  spTransform(clus, CRSobj = proj) %>%
  st_as_sf %>%
  left_join(., wos, by = "id_citavi")
# rename levels properly
levels(clus$cluster) = paste(levels(clus$cluster), "cluster")

# save clus for Shiny-app
saveRDS(clus, file = "images/05_clus_sf_for_shiny.rds")

# popups
clus_content = paste(
  "<b>","Title",": ","</b>", clus$title,
  "</br>",
  "<b>Author(s): </b>",
  clus$author,"</br>",
  "<b>Year: </b>", clus$year, "</br>",
  "<b>Cluster: </b>", clus$cluster)

pal = RColorBrewer::brewer.pal("Set3", n = 6)[3:6]
# leaflet
m = leaflet(clus) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "Dark") %>%
  addCircleMarkers(st_coordinates(clus)[, 1], st_coordinates(clus)[, 2],
                   popup = as.character(clus_content), color = ~col,
                   radius =  4) %>%
  addLegend("bottomright",
            colors = pal,
            labels = levels(clus$cluster),
            title = "Legend",
            opacity = 1)

# save your output
# library("htmlwidgets")
# saveWidget(m, file = file.path(getwd(), "figures/05_leaflet_clusters.html"))
# mapview::mapshot(m, "figures/05_leaflet_clusters.html")
