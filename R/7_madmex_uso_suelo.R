library("terra")
library("sf")

# Load madmex LULC
madmx <- rast("./data_crude/16_madmex/madmex_landsat_2017_31.tif")

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

    region_ <- project(region_, y  = crs(madmx), method = "near")
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    
    madmx_ <- crop(madmx, region_)
    
    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.points(region_)
    
    # LU classes
    region_points$aurban <- 0
    region_points$agrassland <- 0
    region_points$aagriculture <- 0

    for (i in 59802:nrow(region_points)){
      r_point <- region_points[i,]
      r_buffer <- buffer(r_point, width = 2500)
      madmx_masked <- crop(madmx_, r_buffer)
      counts <- as.data.frame(terra::freq(madmx_masked))
      # Urban
      uc <- counts$count[counts$value == 29]
      if (length(uc)>0){
        region_points$aurban[i] <- uc
      }
      
      # Grassland
      ug <- counts$count[counts$value == 27]
      if (length(ug)>0){
        region_points$agrassland[i] <- ug
      }
        
      # Agriculture
      ua <- counts$count[counts$value == 28]
      if (length(ua)>0){
      region_points$agrassland[i] <- ua
      }
}

    df_list[[counter]] <- region_points
}

nrow(region_points)
i

View(as.data.frame(region_points))

full_df <- dplyr::bind_rows(df_list)

saveRDS(full_df, "./data_features/7_madmex_landuse.rds")
