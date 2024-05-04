library("terra")
library("sf")
library("kknn")
library("readxl")
library("stringr")
library("data.table")

# Load corales shapefile.
corales <- vect("./data_crude/08_coral-global/coral-global.shp")

# List coastal refetence grids.
c_list <- list.files("./data/06_DunasCost250116_malla_ref_50m/",
                     pattern = "\\.tif$",
                     full.names = TRUE,
                     recursive = TRUE)

df_list <- list()
counter = 0
for (region in c_list){
  #region <- c_list[10]
    print(region)

    region_ <- rast(region)
    region_ <- project(region_, y  = crs(corales), method = "near")

    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    region_points$corals <- 999
    
    corales_rast <- rasterize(corales, region_)

    if (sum(!is.nan(values(corales_rast)))>0){
      coral_points <- as.data.frame(corales_rast, xy = TRUE)
      coral_points$part<-1
      
      if (nrow(coral_points)==1){
        coral_points <- coral_points[c(1,1),]
      }
      
      modelkknn <- kknn(part~x+y, coral_points, region_points, distance = 2, k=1,
                        kernel = "rectangular")
      distances <- modelkknn$D
      region_points$corals <- distances

    }


    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)
max_dist <- max(full_df$corals)
full_df$corals[full_df$corals == 999] <- 1.5*max_dist
hist(full_df$corals)
saveRDS(full_df, "./data_features/4_coral_distance.rds")
