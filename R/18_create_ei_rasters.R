library("terra")
library("data.table")
#library("raster")

dat <- fread("./data_training_tables/cei_final_train_v1ask.csv")
dat <- dat[,1:3]
dat$idx <- 1:nrow(dat)

regiones <- unique(dat$regionid.x)
head(dat)

eipred <- fread("./ei_predictions/1_puerto/cei_final_ie_expected_port_5_equal.csv", data.table = FALSE)
min(as.numeric(unlist(eipred)))
max(as.numeric(unlist(eipred)))
eipred <- (as.numeric(unlist(eipred))-1.5)/(5.5-1.5)
hist(eipred)
# Load corales shapefile.
corales <- vect("./data_crude/08_coral-global/coral-global.shp")

# List coastal refetence grids.
c_list <- list.files("./data/06_DunasCost250116_malla_ref_50m/",
                     pattern = "\\.tif$",
                     full.names = TRUE,
                     recursive = TRUE)

for (i in 1:length(regiones)){

    region_ <- rast(c_list[i])
    
    region <- regiones[i]
    print(region)
    
    region_dat <- dat[dat$regionid.x==region,]

    ei_df <- data.frame(x=region_dat$x, y=region_dat$y, z=eipred[region_dat$idx])
    #ei_df <- vect(ei_df, geom=c("x", "y"), crs=crs(corales), keepgeom=FALSE)
    #ei_df <- terra::project(x = ei_df, y = region_)
    
    hist(ei_df$z)

    #ei_rast <- rasterize(ei_df, region_, field = "z")

    ei_rast <- rast(ei_df, type="xyz", digits = 5)
    crs(ei_rast) <- crs(corales)
    output <- paste0("./ei_predictions/1_puerto/eicoastal_",region,".tif")
    writeRaster(ei_rast, output, overwrite=TRUE)
}

?writeRaster

