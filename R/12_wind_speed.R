library("terra")
library("sf")

# Load windspeed
wspeed <- rast("./data_crude/11_dataset-sis-biodiversity-era5-global/wind-speed_monthly-mean_era5_1979-2018_v1.0.nc")
wspeed_mean <- app(wspeed,mean)

# Load struct shape to use as molde.
struct <- vect("./data_crude/05_InventarioEstructuras/estructuras_final_unido_.shp")

# Reproject and crop wind speed.
wspeed_reproj <- project(wspeed_mean, y  = crs(struct), method = "near")
wspeed_reproj <- crop(wspeed_reproj, struct)

# List coastal refetence grids.
c_list <- list.files("./data/06_DunasCost250116_malla_ref_50m/",
                     pattern = "\\.tif$",
                     full.names = TRUE,
                     recursive = TRUE)

df_list <- list()
counter = 0
for (region in c_list){
  
    #region <- c_list[1]
    print(region)

    region_ <- rast(region)

    region_ <- project(region_, y  = crs(struct), method = "near")
    
    #ws_ <- crop(wspeed_reproj, region_)
    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    
    points_ <- as.points(region_)
    extracted <- extract(wspeed_reproj, points_)
    region_points$windspeed <- extracted$mean

    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)

saveRDS(full_df, "./data_features/12_avg_windspeed.rds")
