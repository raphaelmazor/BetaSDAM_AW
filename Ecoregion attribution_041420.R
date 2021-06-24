library(tidyverse)
library(sf)

mydf_sf<-st_as_sf(mydf,
                  coords=c("Longitude","Latitude"),
                  remove=F,
                  crs=4326) 

ecoreg_sf<-st_read("us_eco_l3/us_eco_l3.shp")

ecoreg_sf2<-ecoreg_sf %>%
  select(US_L3CODE, NA_L2CODE, L3_KEY,L2_KEY) %>%
  mutate(EcoIII=as.factor(US_L3CODE),
         EcoII=as.factor(NA_L2CODE))

ecoreg_sf2<-st_transform(ecoreg_sf2, crs=4326)

mydf_sf_eco<-st_join(mydf_sf, ecoreg_sf2)

mydf_eco_export <- mydf_sf_eco %>%
  as.data.frame() %>%
  select(globalid, EcoIII, EcoII, L3_KEY, L2_KEY)

write.csv(mydf_eco_export, "Input/Ecoregion_Metrics_041420.csv", row.names = F)

ggplot()+
  # geom_sf(data=ecoreg_sf, aes(fill=as.factor(US_L3CODE)))+
  geom_sf(data=ecoreg_sf, aes(fill=L2_KEY))+
  # geom_sf(data=mydf_sf_eco, aes(color=EcoIII))+
  scale_fill_viridis_d()+
  theme(legend.position = "none")
