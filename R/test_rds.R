
rdss <- list.files("./data_features/", pattern = "\\.rds$", full.names = TRUE)

for (i in 1:length(rdss)){
  dat <- readRDS(rdss[i])
  print(rdss[i])
  print(nrow(dat))
  print(head(dat))
}
