library("dplyr")

rdss <- list.files("./data_features/", pattern = "\\.rds$", full.names = TRUE)
rdss

### 1 tasa de erosion
rdss[1]
dat <- readRDS(rdss[1])

### 10 movimiento dunas
rdss[2]
dat_aux <- readRDS(rdss[2])
dat <- inner_join(dat, dat_aux, by = c("x", "y"))
dat$pixid.y = NULL
dat$regionid.y = NULL

### 11 tipo de costa
rdss[3]
dat_aux <- readRDS(rdss[3])
dat <- data.frame(dat, tipo_costa=dat_aux$tipo_costa)

### 12 average windspeed
rdss[4]
dat_aux <- readRDS(rdss[4])
dat <- data.frame(dat, windspeed=dat_aux$windspeed)

### 13 zonas de vida de holdridge
rdss[5]
dat_aux <- readRDS(rdss[5])
dat <- data.frame(dat, zvh=dat_aux$zvh)

### 2 infraestructura
rdss[6]
dat_aux <- readRDS(rdss[6])
dat <- data.frame(dat, escollera=dat_aux$Escollera,
                       espigon=dat_aux$Espigón,
                       muro=dat_aux$Muro,
                       rompeolas=dat_aux$Rompeolas,
                       puerto=dat_aux$Puerto)

### 3 especies invasoras
rdss[7]
dat_aux <- readRDS(rdss[7])
dat <- data.frame(dat, sp_inv_pot=dat_aux$sp_inv_potential)

### 4 distancia corales
rdss[8]
dat_aux <- readRDS(rdss[8])
dat <- data.frame(dat, d_corales=dat_aux$corals)

### 5 distancia pastos marinos
rdss[9]
dat_aux <- readRDS(rdss[9])
dat <- data.frame(dat, d_pastosmarinos=dat_aux$grass)

### 6 batimetria caracteristica
rdss[10]
dat_aux <- readRDS(rdss[10])
dat <- data.frame(dat, bati_char=dat_aux$batrimetria)

### 7 inegi landuse
rdss[11]
dat_aux <- readRDS(rdss[11])
dat <- data.frame(dat, d_grassland=dat_aux$grassland,
                       d_agriculture=dat_aux$agriculture,
                       d_urban=dat_aux$urban)

### 8 proporcion manglares
rdss[12]
dat_aux <- readRDS(rdss[12])
dat <- data.frame(dat, p_manglares=dat_aux$manglares)

## 9 condicion dunas
rdss[13]
dat_aux <- readRDS(rdss[13])
dat <- inner_join(dat, dat_aux, by = c("x", "y"))
dat$pixid = NULL
dat$regionid = NULL
head(dat)
hist(dat$sp_inv_pot)

idx <- 1:nrow(dat)
dat$pixid.x <- idx
#idx_sample <- sample(idx, nrow(dat))
#dat <- dat[idx_sample,]
saveRDS(dat, "./data_training_tables/train_dat_2c.rds")
