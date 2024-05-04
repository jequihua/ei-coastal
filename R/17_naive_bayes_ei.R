library("bnlearn")
library("dplyr")

dat <- read.table()

source("./R/misc_functions.R")

dat <- readRDS("./data_training_tables/cei_final_train_v1.rds")
head(dat)

table(dat$tipo_costa)

write.csv(dat, "./data_training_tables/cei_final_train_v1.csv", row.names = FALSE)

# Variables.
names(dat)

# Categorical variables.
#dat$ei_qnint[dat$ei_qnint==0]<-NA
cat_vars <- c("NESTB_EDO", "tipo_costa", "zvh","ei_qnint")

# Continous variables.
con_vars <- c("erosion", "windspeed", "escollera", "espigon", "muro", "rompeolas",
              "puerto", "sp_inv_pot", "d_corales", "d_pastosmarinos", "bati_char",
              "d_grassland", "d_agriculture", "d_urban", "p_manglares")

# Coerce categorical variables to factor.
dat = factorCols(dat, cat_vars)

# Discretize continuous variables.
dat = discretizeCols(dat, con_vars)

write.csv(dat, "./data_training_tables/cei_final_train_v1d.csv", row.names = FALSE)

# Drop unlabeled (no condition assessment) pixels.
vars <- 4:22
dat_clean <- dat[!is.na(dat$ei_qnint), vars]
#dat_clean <- dat[dat$ei_qnint != 0, vars]
names(dat_clean)
# train naive bayes
bn <- naive.bayes(dat_clean, training = "ei_qnint", explanatory = names(dat_clean)[1:18])
bn_fitted <- bn.fit(bn, dat_clean, method = "mle", replace.unidentifiable = TRUE) 

### Predict on all data
unique_regions <- unique(dat$regionid.x)

set.seed(42)

region_dat <- dat[dat$regionid.x==unique_regions[1], vars]
region_dat$ei_qnint<-1
region_dat$ei_qnint <- factor(region_dat$ei_qnint, levels = c(1,2,3,4,5))

predicted <- predict(bn_fitted, data = region_dat, prob = TRUE)
table(predicted)
# prediction
prediction <- predict(prior,
                     response = "ei_qnint",
                     newdata = region_dat[,4:21],
                     type = "distribution")

prediction


head(region_dat[,1:19])
