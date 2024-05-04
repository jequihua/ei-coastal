library("terra")
library("sf")

WRITEPATH = "./data/8_ref_grid_50m/"

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

# Cut dunes with coastal regions polygons.
for (i in 1:nrow(coastalr_reproj)){
  i=1
  coastr_poly <- buffer(vect(coastalr_reproj[i,]), width=10000) #10km
  dinegi_cropped <- extend(crop(dunesinegi, coastr_poly), coastr_poly)
  dinegi_disagg <- disagg(dinegi_cropped, fact=5)
  
  coastr_rast <- rasterize(coastr_poly, dinegi_disagg)

  dother <- vect(dunesother_reproj)
  head(dother)
  dother_rast <- rasterize(dother, coastr_rast, field="CONSERV_ED")
  dother_rast[is.na(coastr_rast)] <- NA
  plot(dother_rast)
  dir.create(file.path(WRITEPATH, paste0("region_",coastr_poly$myid[1])),
             showWarnings = FALSE)
  
  writeRaster(dother_rast, filename = file.path(WRITEPATH,
                                                paste0("region_",
                                                       coastr_poly$myid[1],
                                                       "/ref_grid.tif")),
                                                overwrite=TRUE,
                                                gdal=c("COMPRESS=LZW", "TFW=YES","of=COG"))
  
}

