library(data.table)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(fuzzyjoin)

gm = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_timestamps.csv", header = T, sep = ',')
gm$hoursdiff = (gm$seconds + 50400 - gm$TimeSecs1970)/3600
newgm = gm[which(gm$hoursdiff>=-12 & gm$hoursdiff<12),]
dim(newgm)  # 1986 x 23 (good)
#write.csv(newgm, file="GRIDMET_PGE_DAY_match.csv")
contact = fread("C:/Users/walle/Downloads/PGE_contact.csv", header = T, sep = ',')

# This file didn't actually exist (User memory limit had actually been exceeded in GEE):
#pct = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_ALL_pcts.csv", header = T, sep = ',')
# This first "pct" has fm100 and fm1000
pct = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_fm_pcts.csv", header = T, sep = ',')
# Append all of these
# rmax and rmin:
hums = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_rmaxrmin_pcts.csv", header = T, sep = ',')
#tmmx and tmmn
temps = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_temps_pcts.csv", header = T, sep = ',')
ercbi = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_ercbi_pcts.csv", header = T, sep = ',')
vsvpd = fread("C:/Users/walle/Downloads/PGE_GRIDMET_20142018_vsvpd_pcts.csv", header = T, sep = ',')

pctall = pct
pctall$rmax = hums$rmax
pctall$rmin = hums$rmin
pctall$tmax = temps$tmmx
pctall$tmin = temps$tmmn
pctall$erc = ercbi$erc
pctall$bi = ercbi$bi
pctall$vs = vsvpd$vs
pctall$vpd = vsvpd$vpd

pctall$pctile <- str_split_fixed(pctall$'system:index', "_", 2)[,1]

# Definitely not what I wanted (huge output):
#newtest <- difference_left_join(newgm,pct2, by='fm100', max_dist=10)
# maybe could have tried difference_semi_join, but I still don't think it had a "closest" option

newgm2 <- newgm
newgm2$siteid <- newgm2[[21]]
pctall$siteid = pctall[[5]]

# THIS CAUSED PROBLEMS - suspect it's not recognizing properly as number
#newpct2 <- pct2[order(pct2$siteid,pct2$pctile)]

#news1 = newgm2[which(newgm2$siteid==1,)]
#news2 = newpct2[which(newpct2$siteid==1,)]

library("MALDIquant")

#newtest3 <- match.closest(news1$fm100, news3$fm100, tolerance=Inf, nomatch = NA_integer_)
nt <- function(x,y){
  return(match.closest(x,y, tolerance=Inf, nomatch = NA_integer_))
}
#nt(news1$fm100, news3$fm100) # yep, this worked

#newlist2 <- dlply(newgm2, .(siteid))  # make sure plyr enabled
# This didn't work - working through list of newlist2, but doesn't know how to only search same site ID's in pct3
#newresult <- lapply(newlist2, nt(fm100,pct3$fm100))

# AD HOC SOLUTION: multiply siteid by 1000
newgm2$site1000 = newgm2$siteid*1000
newgm2$site1000f1 = newgm2$site1000+newgm2$fm100
newgm2$site1000f2 = newgm2$site1000+newgm2$fm1000
newgm2$site1000h1 = newgm2$site1000+newgm2$rmax
newgm2$site1000h2 = newgm2$site1000+newgm2$rmin
newgm2$site1000t1 = newgm2$site1000+newgm2$tmmx
newgm2$site1000t2 = newgm2$site1000+newgm2$tmmn
newgm2$site1000e1 = newgm2$site1000+newgm2$erc
newgm2$site1000e2 = newgm2$site1000+newgm2$bi
newgm2$site1000v1 = newgm2$site1000+newgm2$vs
newgm2$site1000v2 = newgm2$site1000+newgm2$vpd

newpct2 <- pctall[order(as.numeric(pctall$siteid))]
newpct2$site1000 = newpct2$siteid*1000
newpct2$site1000f1 = newpct2$site1000+newpct2$fm100
newpct2$site1000f2 = newpct2$site1000+newpct2$fm1000
newpct2$site1000h1 = newpct2$site1000+newpct2$rmax
newpct2$site1000h2 = newpct2$site1000+newpct2$rmin
newpct2$site1000t1 = newpct2$site1000+newpct2$tmax
newpct2$site1000t2 = newpct2$site1000+newpct2$tmin
newpct2$site1000e1 = newpct2$site1000+newpct2$erc
newpct2$site1000e2 = newpct2$site1000+newpct2$bi
newpct2$site1000v1 = newpct2$site1000+newpct2$vs
newpct2$site1000v2 = newpct2$site1000+newpct2$vpd

# RUN MATCH FUNCTION ON THE ABOVE TWO (BELOW DIDN'T WORK: EITHER HAS A DECREASE OR AN NA)
f1_matches <- nt(newgm2$site1000f1, newpct2$site1000f1)
f2_matches <- nt(newgm2$site1000f2, newpct2$site1000f2)
h1_matches <- nt(newgm2$site1000h1, newpct2$site1000h1)
h2_matches <- nt(newgm2$site1000h2, newpct2$site1000h2)
t1_matches <- nt(newgm2$site1000t1, newpct2$site1000t1)
t2_matches <- nt(newgm2$site1000t2, newpct2$site1000t2)
e1_matches <- nt(newgm2$site1000e1, newpct2$site1000e1)
e2_matches <- nt(newgm2$site1000e2, newpct2$site1000e2)
v1_matches <- nt(newgm2$site1000v1, newpct2$site1000v1)
v2_matches <- nt(newgm2$site1000v2, newpct2$site1000v2)

newgm2$matchf1 = f1_matches
newgm2$matchf2 = f2_matches
newgm2$matchh1 = h1_matches
newgm2$matchh2 = h2_matches
newgm2$matcht1 = t1_matches
newgm2$matcht2 = t2_matches
newgm2$matche1 = e1_matches
newgm2$matche2 = e2_matches
newgm2$matchv1 = v1_matches
newgm2$matchv2 = v2_matches

newgm2$match_f1b = newgm2$matchf1-(newgm2$siteid-1)*101
newgm2$match_f2b = newgm2$matchf2-(newgm2$siteid-1)*101
newgm2$match_h1b = newgm2$matchh1-(newgm2$siteid-1)*101
newgm2$match_h2b = newgm2$matchh2-(newgm2$siteid-1)*101
newgm2$match_t1b = newgm2$matcht1-(newgm2$siteid-1)*101
newgm2$match_t2b = newgm2$matcht2-(newgm2$siteid-1)*101
newgm2$match_e1b = newgm2$matche1-(newgm2$siteid-1)*101
newgm2$match_e2b = newgm2$matche2-(newgm2$siteid-1)*101
newgm2$match_v1b = newgm2$matchv1-(newgm2$siteid-1)*101
newgm2$match_v2b = newgm2$matchv2-(newgm2$siteid-1)*101


// JUST PRINTING OUT SOME RESULTS HERE
mean(newgm2$match_f1b)
mean(newgm2$match_f2b)
mean(newgm2$match_h1b)
mean(newgm2$match_h2b)
mean(newgm2$match_t1b)
mean(newgm2$match_t2b)
mean(newgm2$match_e1b)
mean(newgm2$match_e2b)
mean(newgm2$match_v1b)
mean(newgm2$match_v2b)

median(newgm2$match_f1b)
median(newgm2$match_f2b)
median(newgm2$match_h1b)
median(newgm2$match_h2b)
median(newgm2$match_t1b)
median(newgm2$match_t2b)
median(newgm2$match_e1b)
median(newgm2$match_e2b)
median(newgm2$match_v1b)
median(newgm2$match_v2b)

newgm2$object = contact$Object
newgm3 = newgm2[which(newgm2$object=='Vegetation'),]

mean(newgm3$match_f1b)
mean(newgm3$match_f2b)
mean(newgm3$match_h1b)
mean(newgm3$match_h2b)
mean(newgm3$match_t1b)
mean(newgm3$match_t2b)
mean(newgm3$match_e1b)
mean(newgm3$match_e2b)
mean(newgm3$match_v1b)
mean(newgm3$match_v2b)

median(newgm3$match_f1b)
median(newgm3$match_f2b)
median(newgm3$match_h1b)
median(newgm3$match_h2b)
median(newgm3$match_t1b)
median(newgm3$match_t2b)
median(newgm3$match_e1b)
median(newgm3$match_e2b)
median(newgm3$match_v1b)
median(newgm3$match_v2b)


# NEED TO DELETE A SERIOUS AMOUNT OF COLUMNS, OR GRAB SELECT COLUMNS BEFORE WRITING TO FILE(!):
#write.csv(newgm2, file="GRIDMET_PGE_DAY_match_add_pcts.csv")

