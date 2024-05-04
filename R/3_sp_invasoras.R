library("terra")
library("sf")
library("kknn")
library("readxl")
library("stringr")
library("data.table")

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

# Load sp invasoras from snib.
#sp_inv <- fread("./data_crude/15_plantas_snib/plantas.csv")
sp_inv_s <- read_excel("./data_crude/15_plantas_snib/especies_invasoras.xlsx", col_names = FALSE)
sp_inv_s$sp <- ""
for (i in 1:nrow(sp_inv_s)){
  sp_inv_s$sp[i] <- word(sp_inv_s$...1[i], 1,2, sep=" ")
}

#sp_inv <- sp_inv[sp_inv$especievalida %in% sp_inv_s$sp,]

#write.csv(sp_inv, "./data_crude/15_plantas_snib/plantas_invasoras.csv")

sp_inv <- read.csv("./data_crude/15_plantas_snib/plantas_invasoras.csv",
                     sep = ",", header = TRUE)

names(sp_inv)
names(sp_inv)[12] <- "x"
names(sp_inv)[13] <- "y"

sp_inv_spat <- st_as_sf(sp_inv, coords = c("x", "y"))
st_crs(sp_inv_spat) <- 4326 

unique_inv <- unique(sp_inv$especievalida)

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
    
    region_ <- project(region_, y  = crs(sp_inv_spat), method = "near")

    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)
    
    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    
    for (sp in unique_inv){
      sp_inv_f <- sp_inv_spat[sp_inv_spat$especievalida==sp,]
      sp_inv_coords <- as.data.frame(st_coordinates(sp_inv_f))
      names(sp_inv_coords)<-c("x", "y")
      sp_inv_coords$part<-1

      modelkknn <- kknn(part~x+y, sp_inv_coords, region_points, distance = 2, k=1,
                   kernel = "rectangular")
      
      distances <- modelkknn$D

      region_points[, sp] <- distances

    }

    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)

for (i in 6:17){
  full_df[,i] <- (1 - normalize(full_df[,i]))
}

full_df$sp_inv_potential <- rowSums(full_df[,6:17])
hist(full_df$sp_inv_potential)

saveRDS(full_df, "./data_features/3_sp_inv_potential.rds")
