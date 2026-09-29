censusKey<-readRDS("../censusKey.rds")
key <-censusKey
year <- 2021
year <-2024
GEOG="STATE";GSUBSET="01"

agep=T
gasp=F
grpip=F
jwap=T
jwdp=T
jwmnp=F
sex=T
fer=F
hhl=F
sch=F
schl=F

geog="region"
geog="division"
geog="state"
geog="st"

gsubset="2"
gsubset="All"

#bad
geog<-"state";year<-2021
#good
geog<-"st";year<-2024


remove(year,agep,gasp,jwap,jwdp,jwmnp,sex,fer,hhl,sch,schl,geog,gsubset)
