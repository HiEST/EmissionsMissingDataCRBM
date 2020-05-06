library(dplyr)
ihsexp <- read.table("~/IHSTestData.txt", header = T)
ihs <- read.table("~/data/IHS/IHSData.csv", header = T)


ihsfilt <- ihs %>%
    filter(imo %in% ihsexp$LRIMOShipNO)  %>%
    select(imo, n_installed_me, design_speed)

ihs_final <- ihsexp %>% inner_join(ihsfilt, by = c("LRIMOShipNO" = "imo"))
ihs_final$designSpeed <- ihs_final$design_speed
ihs_final$design_speed <- NULL


write.table(ihs_final,
    "~/workspace/EmissionsMissingDataCRBM/EmissionModeling/IHSTestData.csv",
    sep = "\t", row.names = F)
