# Filename: 02_ppc.R (2018-05-03)
#
# TO DO: ppc = productivity per country, i.e., qual_gis papers per country, per 
#              capita and compared to total GIS output
#
# Author(s): Jannes Muenchow, Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. CONSTRUCT OUTPUT TABLE
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("stringr")
library("forcats")
library("magrittr")
library("R2wd")
library("wpp2015")

# attach data
wos = readRDS("images/00_wos.rds")
qual = readRDS("images/00_qual.rds")
gis_all = readRDS("images/00_gis_all.rds")
# attach keys
(load("images/00_keys.rda"))
# attach population data
data(pop, package = "wpp2015")

# attach GIS studies per country (just use the first 195 observations (last two
# rows are just summary stats), and first two columns)
gpc = data.table::fread("data/gis_total/gis_per_country.txt", nrow = 195, 
                        select = 1:2)
# rename
names(gpc) = c("country", "n")
gpc = as.data.frame(gpc)

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

qual = left_join(wos, qual, by = c("id_citavi" = "fid_citavi"))
qual = select(qual, id_citavi, fidCountries)
# several countries have been identified per manuscript, one for each author,
# here, we just make use of the first author's country
# first find out the maximum number of countries of a paper
my_max = stringr::str_count(qual$fidCountries, ";") %>%
  max(na.rm = TRUE)
# split accordingly, and just keep the first country id
cou_key = str_split_fixed(qual$fidCountries, ";", my_max + 1)[, 1] %>%
  as.integer
qual$fidCountries = cou_key
qual = left_join(select(qual, fidCountries),
                 select(cous_key, idCountries, Country),
                 by = c("fidCountries" = "idCountries"))

# 2.1 Harmonize country names==============================
#**********************************************************

# harmonize country names of qual and pop
setdiff(qual$Country, pop$name)
grep(pop$name, pattern = "Korea", value = TRUE)
pop$name = gsub("Republic of Korea", "South Korea", pop$name)
grep(pop$name, pattern = "Tanzania", value = TRUE)
grep(pop$name, pattern = "Iran", value = TRUE)
grep(pop$name, pattern = "China", value = TRUE)
grep(pop$name, pattern = "Venezuela", value = TRUE)
grep(pop$name, pattern = "United", value = TRUE)
pop$name = gsub(" of America", "", pop$name)
pop$name = gsub(" \\(.*", "", pop$name)
pop$name = gsub(".*Republic of ", "", pop$name)
qual$Country %<>%
  fct_recode(Finland = "Finnland",
             Belgium = "Belguim",
             China = "Republic of China",
             "United States" = "USA")
setdiff(qual$Country, pop$name)  # 0, perfect
# aggregate, i.e. sum up the number of qualitative GIS papers per country
# before we had "China" and "Republic of China"
qual = group_by(qual, Country) %>%
  dplyr::summarize(n_qual_gis = n()) %>%
  arrange(desc(n_qual_gis))
# ok, publications from 38 countries though only 8 of them published more than
# 3 papers


# harmonize country names of qual and gcp
# more or less copied from ?chartr
simple_cap = function(x) {
  s = strsplit(x, " ")
  sapply(s, function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2),
          sep = "", collapse = " ")
  })
}

# make sure that countries start with a capital letter
gpc$country %<>%
  tolower %>%
  simple_cap
# find out about the differences
setdiff(qual$Country, gpc$country)
# harmonize the differences
grep("usa", gpc$country, value = TRUE)
gpc$country = gsub("Usa", "United States", gpc$country)
gpc$country = gsub(".*China", "China", gpc$country)
grep("England|Scotland|Wales|Ireland", gpc$country, value = TRUE)
gpc$country %<>%
  fct_collapse("United Kingdom" =
                 c("England", "Scotland", "Wales", "North Ireland"))
# aggregate since UK now consists of four rows
gpc = group_by(gpc, country) %>%
  dplyr::summarize(n = sum(n)) %>%
  dplyr::rename(n_gis = n)
setdiff(qual$Country, gpc$country)  # 0, perfect

#**********************************************************
# 3 CONSTRUCT OUTPUT TABLE---------------------------------
#**********************************************************

# join qual (qualitative GIS), pop and gcp (all GIS manuscripts per country)
tab = inner_join(qual, dplyr::select(pop, name, pop = "2015"),
                 by = c("Country" = "name"))
tab = inner_join(tab, gpc, by = c("Country" = "country"))

tab =
  mutate(tab,
         por = n_qual_gis / n_gis * 100,
         per_capita = n_qual_gis / (pop / 1000)) %>%
  arrange(desc(por))

# Word output
# just keep countries with > 3 qual. GIS publications
out = filter(tab, n_qual_gis > 3) %>%
  select(-pop, -n_gis) %>%
  mutate_at(.funs = list(~round(., 2)), .vars = c("por", "per_capita"))
names(out) = c("Country", "Total",
               "in relation to all GIS studies (%)", "per capita")
cap = paste(": Number of all qualitative GIS studies per country (Total),",
            "in relation to all GIS studies,",
            "and per one million inhabitants (per capita).",
            "Only countries with >3 publications were considered.")
# upload the table to the Word document
# wdGet()
# wdTable(out,
#         caption = cap, pointsize = 10, align = "l", row.names = FALSE,# attach population data
# 
#         caption.pos = "above")
