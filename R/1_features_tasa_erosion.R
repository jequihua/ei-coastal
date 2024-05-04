library("terra")
library("sf")
library("kknn")

# Load tasa de erosion
tasa_ero <- read.csv("./data_crude/04_Erosion_acresion/Tasas_erosionMEX_Actualizado2018.txt",
                     sep = ",", header = TRUE)

names(tasa_ero)[2] <- "x"
names(tasa_ero)[3] <- "y"

tasa_ero_spat <- st_as_sf(tasa_ero, coords = c("x", "y"))

st_crs(tasa_ero_spat) <- 4326 

# List coastal refetence grids.
c_list <- list.files("./data/06_DunasCost250116_malla_ref_50m/",
                     pattern = "\\.tif$",
                     full.names = TRUE,
                     recursive = TRUE)

df_list <- list()
counter = 0
for (region in c_list){
  
    print(region)

    region_ <- rast(region)
    
    region_ <- project(region_, y  = crs(tasa_ero_spat), method = "near")

    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    erokknn <- kknn(Tasa~x+y, tasa_ero, region_points, distance = 2, k=3,
                      kernel = "optimal")
    
    predictions <- erokknn$fitted.values
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    region_points$erosion <- predictions
    
    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)

saveRDS(full_df, "./data_features/1_tasa_erosion.rds")
