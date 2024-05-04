library("bnlearn")
library("dplyr")
library("ggplot2")

source("./R/misc_functions.R")

dat <- readRDS("./data_training_tables/train_dat_2c.rds")

# Variables.
names(dat)

# Categorical variables.
cat_vars <- c("NESTB_EDO", "tipo_costa", "zvh", "CONSERV_ED")

# Continous variables.
con_vars <- c("erosion", "windspeed", "escollera", "espigon", "muro", "rompeolas",
              "puerto", "sp_inv_pot", "d_corales", "d_pastosmarinos", "bati_char",
              "d_grassland", "d_agriculture", "d_urban", "p_manglares")

# Coerce categorical variables to factor.
dat = factorCols(dat, cat_vars)

# Discretize continuous variables.
dat = discretizeCols(dat, con_vars)

# Only best conserved dunes.
dat_b <- dat[dat$CONSERV_ED==1, 6:23]

# Learn BN structure.
bn = hc(dat_b)

# Fit bn.
bn_fitted = bn.fit(bn, dat_b, method = "bayes")

# Loglike scores
loglike_scores <- as.data.frame(logLik(bn_fitted, dat[,6:23], by.sample = TRUE))
names(loglike_scores)<-c("loglik")
hist(loglike_scores$loglik)
dat$loglik <- loglike_scores$loglik

### for histogram
hist(dat$loglik)
histlik <- -1*(loglike_scores$loglik)
hist(histlik)
dat$likelihood <- histlik
p<-ggplot(dat, aes(x=likelihood)) + 
  geom_histogram(color="black", fill="white")+
  theme(text = element_text(size=20))
p

histlik <- (histlik - min(histlik)) / (max(histlik)-min(histlik))
hist(histlik)
hist(as.numeric(dat$CONSERV_ED))
table(dat$CONSERV_ED)
dat$CONSERV_ED_num <- as.numeric(dat$CONSERV_ED)
ggplot(dat, aes(CONSERV_ED_num, fill = CONSERV_ED)) + 
  geom_histogram() +
  scale_fill_manual(values = c("1" = "#009933",
                               "2" = "#66cc33",
                               "3" = "#fff999",
                               "4" = "#ff9966",
                               "5" = "#cc0000"))
###

#saveRDS(dat, "./data_training_tables/train_dat_2c_loglik.rds")

datlik <- readRDS("./data_training_tables/train_dat_2c_loglik.rds")
head(datlik)
# Discretizations of log likelihood

# equal interval
eq_int <- bnlearn::discretize(loglike_scores,breaks=5,method="interval")
dat_eq_int <- dat
dat_eq_int$ei_eqint <- 0

# Set levels.
dat_eq_int$ei_eqint[dat_eq_int$CONSERV_ED==5 & eq_int =="[-115.149,-92.5824]"] <- 1 # Very low EI
dat_eq_int$ei_eqint[dat_eq_int$CONSERV_ED==4 & eq_int =="(-92.5824,-70.0163]"] <- 2 # Low EI
dat_eq_int$ei_eqint[dat_eq_int$CONSERV_ED==3 & eq_int =="(-70.0163,-47.4501]"] <- 3 # Medium EI
dat_eq_int$ei_eqint[dat_eq_int$CONSERV_ED==2 & eq_int =="(-47.4501,-24.8839]"] <- 4 # High EI
dat_eq_int$ei_eqint[dat_eq_int$CONSERV_ED==1 & eq_int =="(-24.8839,-2.31777]"] <- 5 # Very high EI

saveRDS(dat_eq_int[dat_eq_int$ei_eqint!=0,], "./data_training_tables/train_dat_2c_eieqint.rds")


# quartile interval
qn_int <- bnlearn::discretize(data.frame(loglik=datlik$loglik),breaks=5,method="quantile")
dat_qn_int <- datlik
hist(as.numeric(unlist(qn_int)))
dat_qn_int$ei_qnint <- 0

table(qn_int$loglik, dat_qn_int$CONSERV_ED)

# Set levels.
dat_qn_int$ei_qnint[dat_qn_int$CONSERV_ED==5 & qn_int =="[-115.149,-42.8388]"] <- 1 # Very low EI
dat_qn_int$ei_qnint[dat_qn_int$CONSERV_ED==4 & qn_int =="(-42.8388,-12.7669]"] <- 2 # Low EI
dat_qn_int$ei_qnint[dat_qn_int$CONSERV_ED==3 & qn_int =="(-12.7669,-8.36479]"] <- 3 # Medium EI
dat_qn_int$ei_qnint[dat_qn_int$CONSERV_ED==2 & qn_int =="(-8.36479,-4.67103]"] <- 4 # High EI
dat_qn_int$ei_qnint[dat_qn_int$CONSERV_ED==1 & qn_int =="(-4.67103,-2.31777]"] <- 5 # Very high EI

saveRDS(dat_qn_int[dat_qn_int$ei_qnint!=0,], "./data_training_tables/train_dat_2c_eiqnint.rds")

dat_su <- group_by(dat_qn_int[dat_qn_int$ei_qnint!=0,], CONSERV_ED)
summarise(dat_su, meanll = mean(loglik))

# Final training data without discretization.
head(dat)
table(dat$regionid.x)
final_train <- dat[,c(1:2,5:23)]
final_train$ei_qnint <- dat_qn_int$ei_qnint
head(final_train)
saveRDS(final_train, "./data_training_tables/cei_final_train_v1.rds")
write.csv(final_train, "./data_training_tables/cei_final_train_v1.csv", row.names = FALSE)

# Continous variables.
con_vars <- c("erosion", "windspeed", "escollera", "espigon", "muro", "rompeolas",
              "puerto", "sp_inv_pot", "d_corales", "d_pastosmarinos", "bati_char",
              "d_grassland", "d_agriculture", "d_urban", "p_manglares")

# Discretize continuous variables.
datd = discretizeCols(final_train, con_vars)

write.csv(datd, "./data_training_tables/cei_final_train_v1d.csv", row.names = FALSE)

############## with * for netica

datd$ei_qnint <- as.character(datd$ei_qnint)

datd$ei_qnint[datd$ei_qnint=="0"]<-"*"
table(datd$ei_qnint)
write.csv(datd, "./data_training_tables/cei_final_train_v1d.csv", row.names = FALSE)


final_train$ei_qnint <- as.character(final_train$ei_qnint)

final_train$ei_qnint[final_train$ei_qnint=="0"]<-"*"
table(final_train$ei_qnint)
write.csv(final_train, "./data_training_tables/cei_final_train_v1ask.csv", row.names = FALSE)
head(final_train)

library(data.table)
dat <- fread("./data_training_tables/cei_final_train_v1ask.csv")

hist(as.numeric(dat$ei_qnint))

