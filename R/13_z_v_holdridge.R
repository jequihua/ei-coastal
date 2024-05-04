library("terra")
library("sf")
library("kknn")

# Load madmex LULC
manglares <- vect("./data_crude/02_cm-conabio/cm-conabio.shp")
zvh <- rast("./data_crude/07_zvh_mx3gw/zvh_mx3gw.tif")
zvh <- project(zvh, y  = crs(manglares), method = "near")

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

    region_ <- project(region_, y  = crs(manglares), method = "near")
    
    zvh_ <- crop(zvh, region_)

    region_id <- strsplit(region, split = "/")[[1]][4]

    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    
    zvh_points <- as.data.frame(zvh_, xy = TRUE)

    names(zvh_points)[3] <- "layer"
    
    zvh_points$layer <- as.factor(zvh_points$layer)

    modelkknn <- kknn(layer~x+y, zvh_points, region_points, distance = 2, k=1,
                        kernel = "rectangular")

    region_points$zvh <- modelkknn$fitted.values

    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)
table(full_df$zvh)
saveRDS(full_df, "./data_features/13_zvh.rds")
