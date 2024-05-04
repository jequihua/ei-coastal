library("terra")
library("sf")
library("kknn")
library("readxl")
library("stringr")
library("data.table")

#setwd("C:/Users/dev/work/ei-coastal/")

# Load manglares shapefile.
costas <- vect("./data_crude/12_Tipo_de_costa/TipoCosta.SHP")

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
    region_ <- project(region_, y  = crs(costas), method = "near")
    
    region_id <- strsplit(region, split = "/")[[1]][4]
    
    region_points <- as.data.frame(region_, xy = TRUE)

    counter = counter+1
    region_points$pixid <- 1:nrow(region_points)
    region_points$regionid <- region_id
    region_points$tipo_costa <- ""

    costas_rast <- rasterize(costas, region_, field="TipoCosta")
    costas_table <- as.data.frame(costas_rast, xy = TRUE)
    costas_table$TipoCosta <- as.factor(costas_table$TipoCosta)
    modelkknn <- kknn(TipoCosta~x+y, costas_table, region_points, distance = 2, k=1,
                      kernel = "rectangular")
    
    costa_prediction <- modelkknn$fitted.values

    region_points$tipo_costa <- costa_prediction

    df_list[[counter]] <- region_points
}

full_df <- dplyr::bind_rows(df_list)

saveRDS(full_df, "./data_features/11_tipo_costa.rds")
