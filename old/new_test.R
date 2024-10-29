tinyforestR::initialise_tf(
    
)

library(pak)
library(tinyforestR)

pak::pkg_download("julianflowers/myScrapers")
library(myScrapers)

dftd <- tinyforestR::get_tf_data()
dftd$tidy_tf_sf


get_tf_data

library(needs)
library(tinyforestR)
library(sf)
needs(rvest, tidyverse, lubridate, tidyfast, data.table, 
      devtools, sf, vegan, mapview, ggthemes, ggmap, tictoc)
devtools::install_github("julianflowers/myScrapers", force = FALSE)
library(myScrapers)

url <- "https://tinyforest.earthwatch.org.uk/tiny-forest-sites"
links <- enframe(unique(get_page_links(url) %>% .[grepl("tiny-forest-sites/", 
                                                        .)]))

tf_df <- drop_na(mutate(links, url = paste0("https://tinyforest.earthwatch.org.uk", 
                                            value), tf_id = str_extract(str_remove(value, "catid=8"), 
                                                                        "\\d{1,}")))

create_tf_table <- function(df, i) {
    dplyr::slice(group_by(distinct(drop_na(mutate(dplyr::filter(unnest(enframe(html_text2(html_elements(read_html(tf_df$url[i]), 
                                                                                                        "div"))), "value"), str_detect(value, "^Planted") | 
                                                                    str_detect(value, "GPS") | str_detect(value, "Area") | 
                                                                    str_detect(value, "^Species") | str_detect(value, 
                                                                                                               "Featured")), tf_id = tf_df$tf_id[i], metric = case_when(str_detect(value, 
                                                                                                                                                                                   "^Planted\nBy") ~ "planted_by", str_detect(value, 
                                                                                                                                                                                                                              "^Species") ~ "trees", str_detect(value, "^GPS") ~ 
                                                                                                                                                                            "gps")))), metric), 1)
}
tf_table <- map_dfr(1:nrow(tf_df), function(x) create_tf_table(tf_df, 
                                                               x), .progress = TRUE)
tidy_tf_table <- dplyr::filter(mutate(dplyr::select(separate(mutate_at(separate(mutate(pivot_wider(dplyr::select(tf_table, 
                                                                                                                 -name), names_from = "metric", values_from = "value", 
                                                                                                   values_fill = "NA"), gps = str_remove(gps, "\\\n.*"), 
                                                                                       gps = str_remove(gps, "\\\r.*"), gps = str_remove(gps, 
                                                                                                                                         "GPS: ")), gps, c("lat", "lon"), ",\\s?"), c("lat", 
                                                                                                                                                                                      "lon"), parse_number), planted_by, c("pb", "planted_by", 
                                                                                                                                                                                                                           "plant_date", "area", "class_area", "feature"), ":\\\n"), 
                                                    -pb), area = parse_number(area), class_area = parse_number(class_area), 
                                      plant_date = dmy(str_remove(plant_date, "Planted Area")), 
                                      planted_by = str_remove(planted_by, "Planting.*"), planted_by = str_remove(planted_by, 
                                                                                                                 "Date*"), tf_age = lubridate::today() - plant_date, 
                                      trees = str_remove(trees, "Species Planted in the Forest:")), 
                               trees != "NA", !is.na(plant_date), !is.na(lon))
tidy_tf_table <- filter(arrange(mutate(tidy_tf_table, site = as.numeric(tf_id)), 
                                site), site != 1)
sf::sf_use_s2(FALSE)
tidy_tf_table_sf <- st_as_sf(tidy_tf_table, coords = c("lon", 
                                                       "lat"), crs = 4326)
uk <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Countries_December_2022_UK_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
uk <- st_union(uk)
uk_tf <- st_intersection(tidy_tf_table_sf, uk)
map = mapview(uk_tf)

bbox <- dftd$tidy_tf_sf[1,]

bbox <- bbox |>
    st_transform(crs = 27700) |>
    st_buffer(500) |>
    st_transform(crs = 4327)
sf_use_s2(FALSE)
key <- Sys.getenv('OSDATAHUB')
test <- os_ngd_api_call(bbox, key, 0)


"https://api.os.uk/features/ngd/ofa/v1?key=QQfpQgnuiTQLA3fErTbffq8G4VOGdP6b"

test <- os_ngd_api_call_water(bbox, key, 0)

tinyforestR::get_nbn_buffer(dftd$tidy_tf$lon[1],dftd$tidy_tf$lat[1], n= 40000, tf_id=1)


tinyforestR::tree_images

get_dw_landcover()



initialise_tf()
