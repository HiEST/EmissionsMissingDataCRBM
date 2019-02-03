load('KmeansData.data')

t <- table(ships$cluster, ships$navstatus)
round(t/rowSums(t), 2)
