library(tidyverse)
library(skimr)
library(huxtable)

blue_green_2018<-read_csv("data/export/blue_green_2018.csv")

covariates <- blue_green_2018 %>%
  skim() %>%
  filter(skim_type == "numeric")  %>%
  select(skim_variable, numeric.mean, numeric.sd) %>%
  mutate(
    numeric.mean = round(numeric.mean, 1),
    numeric.sd = round(numeric.sd, 1),
    meansd = paste0(numeric.mean, " (", numeric.sd, ")")
  ) %>%
  select(-numeric.mean, -numeric.sd) %>%
  filter(!skim_variable %in% list("Hylak_id", "dWL", "LSA", "n_dep")) %>%
  mutate(Description = NA) %>%
  mutate(Description =
           ifelse(
             skim_variable == "precip",
             "mean monthly precipitation (mm)",
             ifelse(
               skim_variable == "air_temp",
               "mean annual air temperature (°C)",
               ifelse(
                 skim_variable == "area",
                 "lake surface area (km2)",
                 ifelse(
                   skim_variable == "elev",
                   "lake elevation (m)",
                   ifelse(
                     skim_variable == "pop_sum",
                     "total human population",
                     ifelse(
                       skim_variable == "maxdepth",
                       "max lake depth (m)",
                       ifelse(
                         skim_variable == "meandepth",
                         "mean lake depth (m)",
                         ifelse(
                           skim_variable == "ws_area",
                           "watershed area (km2)",
                           ifelse(
                             skim_variable == "no3_dep",
                             "total no3 deposition (2018)",
                             ifelse(
                               skim_variable == "nh3_dep",
                               "total nh3 deposition (2018)",
                               ifelse(
                                 skim_variable == "carb",
                                 "some geological descriptor",
                                 ifelse(
                                   skim_variable == "sil",
                                   "some geological descriptor",
                                   ifelse(
                                     skim_variable == "perc_ice",
                                     "% of watershed area classified as ice/snow land cover",
                                     ifelse(
                                       skim_variable == "perc_urban",
                                       "% of catchment area classified as developed, low+med+high-intensity land use",
                                       ifelse(
                                         skim_variable == "perc_shrub",
                                         "% of catchment area classified as shrub/scrub land cover ",
                                         ifelse(
                                           skim_variable == "perc_grass",
                                           "% of watershed area classified as grassland/herbaceous land cover ",
                                           ifelse(
                                             skim_variable == "perc_wetland",
                                             "% of watershed area classified as herbaceous+woody wetland land cover",
                                             ifelse(
                                               skim_variable == "perc_ag",
                                               "% of watershed area classified as crop + hay",
                                               ifelse(
                                                 skim_variable == "perc_forest",
                                                 "% of watershed area classified as deciduous, confider, and mixed forset",
                                                 ifelse(
                                                   skim_variable == "perc_barren",
                                                   "% of watershed area classified as barren land cover",
                                                   ifelse(
                                                     skim_variable == "slope",
                                                     "*Check on this* Mean watershed slope angle",
                                                     ifelse(
                                                       skim_variable == "WALA",
                                                       "Watershed area:lake area",
                                                       ifelse(
                                                         skim_variable == "cti",
                                                         "*Check on this* Composite topographic index",
                                                         " "
                                                       )
                                                     )
                                                   )
                                                 )
                                               )
                                             )
                                           )
                                         )
                                       )
                                     )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             )
           )) %>%
  mutate(
    skim_variable = recode(
      skim_variable,
      precip = "precip.",
      air_temp = "air temp.",
      area = "LA (km2)",
      elev = "elev. (m)",
      meandepth = "Mean depth (m)",
      maxdepth = "Max depth (m)",
      pop_sum = "Population",
      ws_area = "WSA (km2)",
      WALA = "WA:LA",
      no3_dep = "NO3 dep.",
      nh3_dep = "NH3 dep.",
      carb = "carb.",
      sil = "sil.",
      perc_ice = "% ice,",
      perc_urban = "% urban",
      perc_shrub = "% shrub",
      perc_grass = "% grassland",
      perc_wetland = "% wetland",
      perc_ag = "% agrilculture",
      perc_forest = "% forest",
      perc_barren = "% barren",
      cti = "CTI"
    )
  )

###EXPORT TABLE
summary_table_export <-
  covariates %>%
  rename("Variable" = skim_variable,
         "Mean (sd)" = meansd) %>%
  hux(add_colnames = TRUE) %>%
  # add_footnote() %>%
  set_bold(row = 1, col = everywhere, value = TRUE) %>%
  set_all_borders(TRUE) %>%
  set_all_padding(0) %>%
  set_outer_padding(0) %>%
  theme_article()


quick_docx(summary_table_export, file = 'figs/Table1.docx')