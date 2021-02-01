library(data.table)

gm = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_timestamps.csv", header = T, sep = ',')
gm$hoursdiff = (gm$seconds + 50400 - gm$TimeSecs1970)/3600
newgm = gm[which(gm$hoursdiff>=-12 & gm$hoursdiff<12),]
dim(newgm)  # 1986 x 23 (good)
write.csv(newgm, file="GRIDMET_PGE_DAY_match.csv")