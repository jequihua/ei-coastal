library("terra")
library("sf")
library("kknn")

# Load madmex LULC
manglares <- vect("./data_crude/02_cm-conabio/cm-conabio.shp")
madmx <- rast("./data_crude/16_madmex/cdv_usuev250sVII_cnal_s.tif")
madmx <- project(madmx, y  = crs(manglares), method = "near")

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
    
    madmx_ <- crop(madmx, region_)
    madmx_ <- ifel(!(madmx_ %in% c(6,1,3)), NA, madmx_)

    region_id <- strsplit(region, split = "/")[[1]][4]
    
    #region_points <- as.points(region_)
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    
    madmx_points <- as.data.frame(madmx_, xy = TRUE)

    names(madmx_points)[3] <- "layer"
    
    madmx_points$layer <- as.factor(madmx_points$layer)

    # Grassland 6
    region_points$grassland <- 9999
    grs <- madmx_points[madmx_points$layer==6,]
    if (nrow(grs)>0){
      modelkknn <- kknn(layer~x+y, grs, region_points, distance = 2, k=1,
                        kernel = "rectangular")
      distances <- modelkknn$D
      
      region_points$grassland <- distances
    }
    
    # Agriculture 1
    region_points$agriculture <- 9999
    ars <- madmx_points[madmx_points$layer==1,]
    if (nrow(ars)>0){
      modelkknn <- kknn(layer~x+y, ars, region_points, distance = 2, k=1,
                        kernel = "rectangular")
      distances <- modelkknn$D
      
      region_points$agriculture <- distances
    }
    
    # Urban 3
    region_points$urban <- 9999
    urs <- madmx_points[madmx_points$layer==3,]
    if (nrow(urs)>0){
      modelkknn <- kknn(layer~x+y, urs, region_points, distance = 2, k=1,
                        kernel = "rectangular")
      distances <- modelkknn$D
      
      region_points$urban <- distances
    }

    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)

full_df$grassland[full_df$grassland == 9999]<-NA
max_a <- max(full_df$grassland, na.rm = TRUE)
full_df$grassland[is.na(full_df$grassland)]<-max_a*1.5
hist(full_df$grassland)

full_df$agriculture[full_df$agriculture == 9999]<-NA
max_a <- max(full_df$agriculture, na.rm = TRUE)
full_df$agriculture[is.na(full_df$agriculture)]<-max_a*1.5
hist(full_df$agriculture)

full_df$urban[full_df$urban == 9999]<-NA
max_a <- max(full_df$urban, na.rm = TRUE)
full_df$urban[is.na(full_df$urban)]<-max_a*1.5
hist(full_df$urban)

saveRDS(full_df, "./data_features/7_inegi_landuse.rds")
