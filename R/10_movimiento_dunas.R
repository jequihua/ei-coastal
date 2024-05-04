library("terra")
library("sf")

#WRITEPATH = "./data/8_ref_grid_50m/"

# Load inland ecosystems deltavp.
deltavp <- rast("./data/5_deltavp250m/1delt_vp_250m.tif")

# Load dune raster based on INEGI Series VII.
dunesinegi <- rast("./data/6_inegiSVII_dunes/cdv_usuev250sVII_dunas.tif")

# Load dune polygons from ??? (source?).
dunesother <- st_read("./data/7_dunes250116/DunasCosteras250116.shp")
dunesother_reproj <- st_transform(dunesother, crs(dunesinegi))
names(dunesother_reproj)
# Load coastal regions shapefile.
coastalr <- st_read("./data/3_misc_cesia/RegionesCosteras40km.shp")
coastalr <- st_set_crs(coastalr, 4326)
coastalr_reproj <- st_transform(coastalr, crs(dunesinegi))
head(coastalr_reproj)
# Cut dunes with coastal regions polygons.

manglares <- vect("./data_crude/02_cm-conabio/cm-conabio.shp")
crs(manglares)

df_list <- list()
counter = 0
for (i in 1:nrow(coastalr_reproj)){
  print(i)
  coastr_poly <- buffer(vect(coastalr_reproj[i,]), width=10000) #10km
  dinegi_cropped <- extend(crop(dunesinegi, coastr_poly), coastr_poly)
  dinegi_disagg <- disagg(dinegi_cropped, fact=5)
  
  coastr_rast <- rasterize(coastr_poly, dinegi_disagg)

  dother <- vect(dunesother_reproj)
  dother_rast <- rasterize(dother, coastr_rast, field="NESTB_EDO")
  dother_rast[is.na(coastr_rast)] <- NA
  
  dother_rast_ <- project(dother_rast, y  = crs(manglares), method = "near")
  
  region_points <- as.data.frame(dother_rast_, xy = TRUE)
  
  counter = counter+1
  region_points$pixid <- 1:nrow(region_points)
  region_points$regionid <- coastr_poly$myid[1]
  
  df_list[[counter]] <- region_points

}

full_df <- dplyr::bind_rows(df_list)
full_df <- full_df[order(full_df$regionid,decreasing=FALSE),]
saveRDS(full_df, "./data_features/10_movimiento_dunas.rds")

