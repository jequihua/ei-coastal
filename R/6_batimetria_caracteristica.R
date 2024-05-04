library("terra")
library("sf")
library("kknn")

# Load batimetria
bat <- rast("./data_crude/13_Batimetría/01_GEBCO2020_SIMAR.tif")

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
    
    region_ <- project(region_, y  = crs(bat), method = "near")
    bat_<- crop(bat, region_)
    
    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)
    bat_points <- as.data.frame(bat_, xy = TRUE)
    names(bat_points)[3]<-"bat"
    batkknn <- kknn(bat~x+y, bat_points, region_points, distance = 2, k=7,
                      kernel = "optimal")
    
    predictions <- batkknn$fitted.values
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    region_points$batimetria <- predictions
    
    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)

saveRDS(full_df, "./data_features/6_batimetria_charact.rds")
