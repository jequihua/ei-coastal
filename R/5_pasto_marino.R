library("terra")
library("sf")
library("kknn")
library("readxl")
library("stringr")
library("data.table")

# Load sea grass shapefile.
grass <- vect("./data_crude/03_seagrasses-pol-simar/seagrasses-pol-simar.shp")

# List coastal refetence grids.
c_list <- list.files("./data/06_DunasCost250116_malla_ref_50m/",
                     pattern = "\\.tif$",
                     full.names = TRUE,
                     recursive = TRUE)

df_list <- list()
counter = 0
for (region in c_list){
  region <- c_list[10]
    print(region)

    region_ <- rast(region)
    region_ <- project(region_, y  = crs(grass), method = "near")
    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    region_points$grass <- 999
    
    grass_rast <- rasterize(grass, region_)

    if (sum(!is.nan(values(grass_rast)))>0){
      grass_points <- as.data.frame(grass_rast, xy = TRUE)
      grass_points$part<-1
      
      if (nrow(grass_points)==1){
        grass_points <- grass_points[c(1,1),]
      }
      
      modelkknn <- kknn(part~x+y, grass_points, region_points, distance = 2, k=1,
                        kernel = "rectangular")
      distances <- modelkknn$D
      hist(distances)
      region_points$grass <- distances

    }


    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)
full_df$grass[full_df$grass == 99999] <- NA
max_dist <- max(full_df$grass, na.rm = TRUE)
max_dist
full_df$grass[is.na(full_df$grass)] <- 1.5*max_dist
hist(full_df$grass)
saveRDS(full_df, "./data_features/5_seagrass_distance.rds")
head(full_df)
