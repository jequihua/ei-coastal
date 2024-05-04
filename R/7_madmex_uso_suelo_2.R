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
  
    region <- c_list[1]
    print(region)

    region_ <- rast(region)

    region_ <- project(region_, y  = crs(madmx), method = "near")
    
    madmx_ <- crop(madmx, region_)
    
    region_id <- strsplit(region, split = "/")[[1]][4]
    
    #region_points <- as.points(region_)
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    
    madmx_points <- as.data.frame(madmx_, xy = TRUE)
    
    names(madmx_points)[3] <- "layer"
    
    madmx_points$layer <- as.factor(madmx_points$layer)

    modelkknn <- kknn(layer~x+y, madmx_points, region_points, distance = 2, k=1000,
                      kernel = "optimal")
    
    region_points$manglares <- modelkknn$prob[,2]

    df_list[[counter]] <- region_points
}

nrow(region_points)
i

View(as.data.frame(region_points))

full_df <- dplyr::bind_rows(df_list)

saveRDS(full_df, "./data_features/7_madmex_landuse.rds")
