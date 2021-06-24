library(tidyverse)
library(sf)
library(prism)
library(raster)

#Note: the raster package will screw up a few tidyverse functions
#So if you want to use tidy select, you need to call dplyr::select


mydf_sf<-st_as_sf(mydf2,
                  coords=c("Longitude","Latitude"),
                  remove=F,
                  crs=4326)



library(prism)
options(prism.path="~/prism")

#download 30-year normals of annual statistics
get_prism_normals(type="ppt", resolution="800m", keepZip = F, annual = T)
get_prism_normals(type="tmean", resolution="800m", keepZip = F, annual = T)
get_prism_normals(type="tmax", resolution="800m", keepZip = F, annual = T)
get_prism_normals(type="tmin", resolution="800m", keepZip = F, annual = T)
#download 30-year normals of monthly statistics, ppt only
get_prism_normals(type="ppt", resolution="800m", keepZip = F, mon=1:12)

ls_prism_data(name=T)
# ppt_RS<-prism_stack(ls_prism_data()[13,1]) #Numeric indices are generally a bad idea

#Turn them into rasters with projection info
ppt_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_annual_bil"))
proj4string(ppt_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

tmax_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_tmax_30yr_normal_800mM2_annual_bil"))
proj4string(tmax_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

tmean_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_tmean_30yr_normal_800mM2_annual_bil"))
proj4string(tmean_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

tmin_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_tmin_30yr_normal_800mM2_annual_bil"))
proj4string(tmin_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")



ppt.m01_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_01_bil"))
proj4string(ppt.m01_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m02_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_02_bil"))
proj4string(ppt.m02_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m03_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_03_bil"))
proj4string(ppt.m03_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m04_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_04_bil"))
proj4string(ppt.m04_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m05_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_05_bil"))
proj4string(ppt.m05_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m06_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_06_bil"))
proj4string(ppt.m06_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m07_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_07_bil"))
proj4string(ppt.m07_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m08_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_08_bil"))
proj4string(ppt.m08_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m09_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_09_bil"))
proj4string(ppt.m09_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m10_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_10_bil"))
proj4string(ppt.m10_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m11_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_11_bil"))
proj4string(ppt.m11_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
ppt.m12_RS<-prism_stack(ls_prism_data() %>% filter(files=="PRISM_ppt_30yr_normal_800mM2_12_bil"))
proj4string(ppt.m12_RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

mydf_prism<-data.frame(globalid=mydf_sf$globalid) %>%
  cbind(
    extract(tmean_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(tmean = 1),
    extract(tmax_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(tmax = 1),
    extract(tmin_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(tmin = 1),
    extract(ppt_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt = 1),
    
    extract(ppt.m01_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m01 = 1),
    extract(ppt.m02_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m02 = 1),
    extract(ppt.m03_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m03 = 1),
    extract(ppt.m04_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m04 = 1),
    extract(ppt.m05_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m05 = 1),
    extract(ppt.m06_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m06 = 1),
    extract(ppt.m07_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m07 = 1),
    extract(ppt.m08_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m08 = 1),
    extract(ppt.m09_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m09 = 1),
    extract(ppt.m10_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m10 = 1),
    extract(ppt.m11_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m11 = 1),
    extract(ppt.m12_RS, mydf_sf, fun=mean, na.rm=T, sp=F) %>% as.data.frame() %>% rename(ppt.m12 = 1)
    )

write.csv(mydf_prism, "Input/prism_041320.csv")
