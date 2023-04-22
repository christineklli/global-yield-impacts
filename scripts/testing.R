a <- list(data.frame(x=rnorm(10, 0, 1), crop="maize", model=1, m=1),
          data.frame(x=rnorm(10,0,1), crop="soy", model=1, m=2),
          data.frame(x=rnorm(10,1,2),crop="maize", model=1, m=3),
          data.frame(x=rnorm(10,1,2),crop="soy", model=1, m=4))


b <- a %>% rbindlist()
c <-  split(b, b$crop)
d <- lapply(1:2, function(i){
  split(c[[i]], 
        interaction(c[[i]]$model, c[[i]]$m)
  )      })

# testing population scaling from Olen and Lehsten et al. 2022

# reprojected by JC
pop_10km_file <- here("data", "Food security data", "RCP8.5_10000m.tif")

all <- raster::stack(pop_10km_file) 
# need only 2021-2040
pop_2010 <- raster::subset(all, 1)

# unprojected
pop_og_file <- here("data", "Food security data", "CMPI6wp_grid_pop_count2010_SSP5_RCP8_5.tif")

og_2010 <- raster::raster(pop_og_file)

# aggregate both to country level
worldmap <- cleangeo::clgeo_Clean(rworldmap::countriesCoarse)

country_pop_2010 <- exactextractr::exact_extract(
  pop_2010,
  worldmap,
  fun="sum"
) %>% as_tibble() %>% 
  cbind(
    iso_a3=worldmap$ISO_A3,
    name=worldmap$NAME
  )

country_og_2010 <- exactextractr::exact_extract(
  og_2010,
  worldmap,
  fun='sum'
) %>% as_tibble() %>% 
  cbind(
    iso_a3=worldmap$ISO_A3,
    name=worldmap$NAME
  )

country_pop_2010 %>% 
  rename(value_projected = value) %>% 
  left_join(country_og_2010, by=c("iso_a3", "name")) %>% 
  rename(value_unprojected = value) %>% 
  relocate(iso_a3, name)


# find out if IIASA years are already averages