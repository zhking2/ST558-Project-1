censusKey<-readRDS("../censusKey.rds")
key <-censusKey

year <- 2021
year <-2024

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

gsubset="02"
gsubset="All"

geog<-"state";year<-2021

woah <-apiHarmer(key=censusKey,year=2021,
                 agep+T,gasp=F,grpip=F,jwap=T,jwdp=T,jwmnp=F,
                 sex=T,fer=F,hhl=F,sch=F,schl=F,geog="state",gsubset="02")


remove(year,agep,gasp,jwap,jwdp,jwmnp,sex,fer,hhl,sch,schl,geog,gsubset)
