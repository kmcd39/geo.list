## code to prepare `geo.list` dataset goes here

# geo.list stores different geography layers as a list of sf objects.
# Lists can easily be parsed by indexing from shiny input or other strings.
library(dplyr)
library(sf)
library(lwgeom)
rm(list = ls())

requireNamespace("divDat")
requireNamespace("xwalks")
# selectables$region_type

# czs --------------------------------------------------------------------------
(cz <- divDat::czs_simplified)

# counties ---------------------------------------------------------------------
(county <- divDat::counties_simplified)

# make minimal
county <- county %>%
  select(county = geoid,
         county_name = name)

# cbsas ------------------------------------------------------------------------
(cbsa <- divDat::cbsas)

# make minimal
cbsa <- cbsa %>% select(1,2,geometry)

# double simplify this one (cld be better soln: redownloading from api to mirror czs or smthn)
cbsa <- rmapshaper::ms_simplify(cbsa)


# build list -------------------------------------------------------------------
geo.list <- list("cz" = cz,
                 "county" = county,
                 "cbsa" = cbsa)


# rmapsimplify (further) -------------------------------------------------------
geo.list <-
  purrr::imap(geo.list,
              ~rmapshaper::ms_simplify(
                st_buffer(st_make_valid(.), 0)) )


# add states & larger areas ----------------------------------------------------

# cz_blox <- divDat::czs_simplified

states <- tigris::states()
colnames(states) <- tolower(colnames(states))

geo.list$state <- states %>%
  select(state = statefp,
         state_name = name)

geo.list$division <-
  states %>%
  group_by(division) %>%
  summarise(., do_union = T) %>%
  left_join(
    distinct(xwalks::state2div[, c("division", "division_name")])) %>%
  select("division", "division_name", geometry)

geo.list$division["division_name"] %>% plot()

'geo.list$region <- # too many options! I take this one out
  states %>%
  group_by(region) %>%
  summarise(., do_union = T) %>%
  left_join(
    distinct(xwalks::state2div[, c("region", "region_name")]))'

geo.list$national <-
  states %>%
  st_union() %>%
  st_sf(us = 1,
        us_name = "National",
        geometry = .)

# re-project -------------------------------------------------------------------
geo.list <-
  purrr::imap( geo.list,
               ~st_transform(., 4326 ) )

# remove part of alaska that stretches into eastern hemisphere -----------------
# also run st_make_valid
# i.e., see:
geo.list[['cz']]['cz'] %>%
  #st_buffer(0) %>%
  st_crop(
    c(xmin = -180, xmax = 40,
      ymin = -20, ymax = 80)
  ) %>%
  plot()





geo.list <-
  geo.list %>%
  map( ~st_crop(st_make_valid(.),
                c(xmin = -180, xmax = 40,
                  ymin = -20, ymax = 80)
  ))

geo.list$cbsa["cbsa"] %>% plot()


# check colnames ------------------------------------------------------------
geo.list %>% purrr::map(colnames)

# region.id/name form -------------------------------------------------------
geo.list <-
  imap(geo.list,
       ~rename(., region.id = 1, region.name = 2))

# everything is characters...
geo.list <-
  imap(geo.list,
       ~mutate(., region.id = as.character(region.id)))


# final spatial cleans ---------------------------------------------------------

# to check
geo.list %>%
  map( divM::count.geo )

tmp <- geo.list %>%
  map( ~filter(.,
               st_geometry_type(.$geometry) ==
                 "GEOMETRYCOLLECTION")
  )

tmp <- tmp[ map_lgl(tmp, ~nrow(.) > 0) ]
tmp %>%
  map( ~st_collection_extract(., "POLYGON")) %>%
  map( plot )



geo.list <-
  geo.list %>%
  map(
    ~mutate(
      ., geometry =
        st_collection_extract(.$geometry, "POLYGON"),
      .$geometry)
  )



# peek & write ------------------------------------------------------------------------
geo.list

# as RDS (for early development)
#saveRDS(geo.list,
#        file = "R/data/geo.list.RDS")


# geo.list <- readRDS("data/geo.list.RDS")
usethis::use_data(geo.list
                  ,overwrite = TRUE)


