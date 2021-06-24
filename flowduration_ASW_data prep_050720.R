library(tidyverse)
library(ggrepel)
library(sf)
library(randomForest)
#Import core flow-duration data
# new_gids<-setdiff(read.csv("Input/NewData/flowduration_34merge_06092020.csv", stringsAsFactors = FALSE)$globalid,
#         read.csv("Input/Flowduration_34M_121719.csv", stringsAsFactors = FALSE)$globalid)
# mydf %>%
#   filter(globalid %in% new_gids) %>%
#   write.table(file="clipboard", sep="\t", row.names=F)
#Rename variables, and create a few new ones
mydf<-#read.csv("Input/Flowduration_34M_121719.csv", stringsAsFactors = FALSE) %>%
  read.csv("Input/NewData/flowduration_34merge_06092020.csv", stringsAsFactors = FALSE) %>%
  #renaming variables, want to keep original variable order and maintain syntax
  rename(sitecode_entry = sitecode,
         source=MERGE_SRC,
         collection_date = collection,
         disturbed_note = disturbed_,
         bnkwidth0 = bankfullwi,
         bnkwidth15 = bankfull_1,
         bnkwidth30 = bankfull_2,
         reachlengthcalc1 = reachlengt,
         reachlengthcalc2 = reachlen_1,
         reachlength = reachlen_2,
         valleyslope = valleyslop,
         landuse = s_landuse,
         landuse_notes = landuse_no,
         ripariancorr_evidence = riparian,
         erosion_evidence = erosion,
         floodplain_evidence = floodplain,
         evidence_notes = ancillaryn,
         pctsurfaceflow = hi_reachle,
         pctsubsurfaceflow = hi_reach_1,
         numberpools = pools_obse,
         springs_detected = hi_seepssp,
         waterinchannel_score = hi_channel,
         waterinchannel_notes = hi_notes,
         hydric_detected = hydricsoil,
         hydric_locations = locations,
         hydric_notes = hydricnote,
         numberwoodyjams = woodyjams1,
         woodysource = trees_shru,
         woody_notes = woodyjamsn,
         rootedplants_score = hi_streamb,
         rootedplants_notes = streambedn,
         seddep_score = hi_debriss,
         seddep_notes = debrisnote,
         soilmoist_1 = locationon,
         soilmoist_2 = locationtw,
         soilmoist_3 = locationth,
         soiltexture_1 = location_1,
         soiltexture_2 = location_2,
         soiltexture_3 = location_3,
         sinuosity_score = si_sinuosi,
         sinuosity_method_notes = sinuositym,
         sinuosity_conditions_notes = sinuosityc,
         floodplaindim_score = gi_dimensi,
         floodplaindim_method_notes = dimensionm,
         riffpoolseq_score = gi_sequenc,
         riffpoolseq_notes = seqnotes,
         substratesorting_score = gi_substra,
         substratesorting_notes = ginotes,
         fishabund_score = abundances,
         bmiabund_score = abundanc_1,
         algabund_score = abundancea,
         mosquitofish = mosquitofi,
         abund_notes = abundancen,
         iofb_detected = observedfu,
         snake_detected = observedsn,
         snakeabund = obsnakesab,
         turtle_detected = observedtu,
         turtleabund = obturtlesa,
         amphib_detected = observedam,
         ampibabund = obampiabun,
         frogvoc_detected = observedvo,
         abund_notes2 = observedab,
         algcover_live = streambedl,
         algcover_dead = streambedd,
         algcover_upstreamsource = streambe_1,
         algcover_notes = streambeda,
         dens_UU = u_upstream,
         dens_UL = u_left,
         dens_UR = u_right,
         dens_UD = u_downstre,
         dens_MU = m_upstream,
         dens_ML = m_left,
         dens_MR = m_right,
         dens_MD = m_downstre,
         dens_LU = l_upstream,
         dens_LL = l_left,
         dens_LR = l_right,
         dens_LD = l_downstre,
         moss_cover = bryophytem,
         liverwort_cover = bryophytel,
         vegdiff_score = vegetation,
         vegdiff_notes = vegenotes,
         regionalindicator = regionalin,
         additional_notes = additional,
         creation_date = CreationDa,
         edit_date = EditDate) %>%
  left_join(read.csv("Input/prism_041320.csv", stringsAsFactors=F)) %>%
  left_join(read.csv("Input/Ecoregion_Metrics_041420.csv", stringsAsFactors=F) %>%
              mutate(EcoII=as.factor(EcoII),
                     EcoIII=as.factor(EcoIII))) %>%
  mutate(pctsubsurfaceflow = case_when(is.na(pctsubsurfaceflow)~pctsurfaceflow, T~pctsubsurfaceflow))
# add new variables
#elevatr package not working

#Manual data correction
#Missing date
# mydf$collection_date[mydf$globalid=="{7677F32A-5B00-4979-85BF-12EDFDF58F6C}"]<-"8/10/2019"


#Bad coordinates--Will be fixed in original
mydf$latitude[mydf$globalid=="{B95DFF1F-6304-4DA0-AB1D-141BF0B946B4}"]<-33.4576800
mydf$longitude[mydf$globalid=="{B95DFF1F-6304-4DA0-AB1D-141BF0B946B4}"]<- -116.9707900
mydf$latitude[mydf$globalid=="{C87CAE26-0F0E-406B-92FE-5F945C710045}"]<-33.4788792
mydf$longitude[mydf$globalid=="{C87CAE26-0F0E-406B-92FE-5F945C710045}"]<- -117.1429828
mydf$latitude[mydf$globalid=="{207E5E65-55CC-4D29-AF31-955D243AD442}"]<-33.6626000
mydf$longitude[mydf$globalid=="{207E5E65-55CC-4D29-AF31-955D243AD442}"]<- -117.3027330
mydf$latitude[mydf$globalid=="{1AE9A7B8-E78C-412B-AFCC-31C933942E12}"]<-34.3148561
mydf$longitude[mydf$globalid=="{1AE9A7B8-E78C-412B-AFCC-31C933942E12}"]<- -112.0658067
mydf$latitude[mydf$globalid=="{D0C6954C-3204-4200-B456-87BB05B3B95A}"]<-33.9733214
mydf$longitude[mydf$globalid=="{D0C6954C-3204-4200-B456-87BB05B3B95A}"]<- -112.0993853
mydf$latitude[mydf$globalid=="{2B860B35-9215-4995-A95A-312A8A4D6162}"]<-36.0918470
mydf$longitude[mydf$globalid=="{2B860B35-9215-4995-A95A-312A8A4D6162}"]<- -115.0219500
mydf$latitude[mydf$globalid=="{098ED077-83E0-4E84-9779-A3D2835436F8}"]<-36.2325060
mydf$longitude[mydf$globalid=="{098ED077-83E0-4E84-9779-A3D2835436F8}"]<- -115.0763390
mydf$latitude[mydf$globalid=="{AA7C0D16-05B2-4644-B18A-477AD8A0C2DD}"]<- 35.3849330
mydf$longitude[mydf$globalid=="{AA7C0D16-05B2-4644-B18A-477AD8A0C2DD}"]<- -113.6595240
mydf$latitude[mydf$globalid=="{535F78A0-E097-465B-9DC2-4C7CDCFA9925}"]<- 36.6525858
mydf$longitude[mydf$globalid=="{535F78A0-E097-465B-9DC2-4C7CDCFA9925}"]<- -121.7938869
#Other errors
mydf$waterinchannel_score[mydf$globalid=="{C87CAE26-0F0E-406B-92FE-5F945C710045}"]<-4 #Entry missing from Survey123, but on data sheet.
mydf$waterinchannel_score[mydf$globalid=="{A485A5B9-2436-4C06-8139-91E11900229D}"]<-0 #field notes
# mydf$fishabund_score[mydf$globalid=="{A80CD7EC-27C9-4EB3-AC7E-2DC537105A18}"]<-0 #field notes
# mydf$vegdiff_score[mydf$globalid=="{E4649AE8-81F2-491B-8775-F01ACC07A2E3}"]<-2 #field notes and site photos
# mydf$vegdiff_score[mydf$globalid=="{FD4E312A-9AFE-4DFC-844D-3308FD8A5C5D}"]<-3 #field notes and site photos

mydf$fishabund_score[mydf$globalid=="{A80CD7EC-27C9-4EB3-AC7E-2DC537105A18}"]<-0 #field notes
mydf$fishabund_score2[mydf$globalid=="{A80CD7EC-27C9-4EB3-AC7E-2DC537105A18}"]<-0 #field notes MIGHT BE A LITTLE HIGHER


mydf$floodplaindim_score[mydf$globalid=="{195DB4CB-28BD-4BB1-A672-F77B608BCCF0}"]<-3 #site photos CHECK WITH JOHN
mydf$seddep_score[mydf$globalid=="{88638AA0-E038-4AE0-95F5-7852CE06519C}"]<-1.5 #site photos
mydf$substratesorting_score[mydf$globalid=="{6C4C6810-7B60-46D4-B81A-96534F8475EA}"]<-3 #site photos
mydf$vegdiff_score[mydf$globalid=="{FD4E312A-9AFE-4DFC-844D-3308FD8A5C5D}"]<-2 #site photos
mydf$substratesorting_score[mydf$globalid=="{FADB518F-8CC5-4F87-820A-C238D1B723D5}"]<-0.5#site photos CHECK WITH JOHN
mydf$vegdiff_score[mydf$globalid=="{E4649AE8-81F2-491B-8775-F01ACC07A2E3}"]<-0 #site photos
mydf$floodplaindim_score[mydf$globalid=="{E7723A40-D609-4386-9731-CD2DC21FD69A}"]<-3 #site photos CHECK WITH JOHN
mydf$floodplaindim_score[mydf$globalid=="{643F5396-780D-4B62-804D-25C02EB17B2D}"]<-2 #site photos CHECK WITH JOHN

mydf$riffpoolseq_score[mydf$globalid=="{7677F32A-5B00-4979-85BF-12EDFDF58F6C}"]<-3#site photos and sketch
mydf$rootedplants_score[mydf$globalid=="{CB632178-6F30-4354-A607-5D56B2127A42}"]<-1#site photos
mydf$seddep_score[mydf$globalid=="{CB632178-6F30-4354-A607-5D56B2127A42}"]<-0#site photos
mydf$rootedplants_score[mydf$globalid=="{645520CB-1975-4757-BA6E-0094C71DB7B8}"]<-3#site photos
mydf$numberwoodyjams[mydf$globalid=="{645520CB-1975-4757-BA6E-0094C71DB7B8}"]<-0#site photos
mydf$seddep_score[mydf$globalid=="{645520CB-1975-4757-BA6E-0094C71DB7B8}"]<-1#site photos

mydf$valleyslope[mydf$globalid=="{17CD3DCE-3CE9-4C74-A866-7AE152ACE663}"]<-15# Not sure how to backfill. Very steep in photos CHECK WITH JOHN
mydf$vegdiff_score[mydf$globalid=="{17CD3DCE-3CE9-4C74-A866-7AE152ACE663}"]<-2# Not much opportunity for riparian growth CHECK WITH JOHN
mydf$substratesorting_score[mydf$globalid=="{17CD3DCE-3CE9-4C74-A866-7AE152ACE663}"]<-3# Not much opportunity for riparian growth CHECK WITH JOHN



mydf<-mydf %>%
  mutate(
    wet = waterinchannel_score > 3, #Logical: Is the stream flowing?  Cutoff based on visual examination of the data
    springs_score = case_when(springs_detected == "present"~ 1.5,
                              springs_detected == "notdetected"~ 0,
                              T ~ 0),
    hydric_score = case_when(hydric_detected %in% c("present", "3")~3,
                             hydric_detected %in% c("notdetected","", "0")~0,
                             T ~ 0),
    iofb_score = case_when(iofb_detected == "present"~ 1.5,
                           iofb_detected == "notdetected"~ 0,
                           T ~ 0),
    fishabund_score2 = case_when(mosquitofish=="yes"~0,T~fishabund_score),
    moss_cover_score = case_when(moss_cover=="notdetected"~0,
                                 moss_cover=="<2%"~1,
                                 moss_cover=="2-10%"~2,
                                 moss_cover==">10%"~3,
                                 T~ 0), #Treat missing data as non-detect
    liverwort_cover_score = case_when(liverwort_cover=="notdetected"~0,
                                      liverwort_cover=="<2%"~1,
                                      liverwort_cover=="2-10%"~2,
                                      liverwort_cover==">10%"~3,
                                      T~ 0), #Treat missing data as non-detect
    snake_score = case_when(snake_detected=="present"~1,T~0),
    amphib_score = case_when(amphib_detected=="present"~1,T~0),
    turt_score = case_when(turtle_detected=="present"~1,T~0),
    frogvoc_score = case_when(frogvoc_detected=="present"~1,T~0),
    vert_score = pmax(snake_score,amphib_score, turt_score),
    vertvoc_score = pmax(snake_score,amphib_score, turt_score,frogvoc_score),
    vert_sumscore = (snake_score+amphib_score+turt_score),
    vertvoc_sumscore = (snake_score+amphib_score+ turt_score+frogvoc_score),
    ripariancorr_score = case_when(ripariancorr_evidence=="yes"~1,T~0),
    erosion_score = case_when(erosion_evidence=="yes"~1,T~0),
    floodplain_score = case_when(floodplain_evidence=="yes"~1,T~0),
    alglive_cover_score = case_when(algcover_live=="notdetected"~0,
                                    algcover_live=="<2%"~1,
                                    algcover_live=="2-10%"~2,
                                    algcover_live=="10-40%"~3,
                                    algcover_live==">40%"~4,
                                    T~ 0), #Treat missing data as non-detect
    algdead_cover_score = case_when(algcover_dead=="notdetected"~0,
                                    algcover_dead=="<2%"~1,
                                    algcover_dead=="2-10%"~2,
                                    algcover_dead=="10-40%"~3,
                                    algcover_dead==">40%"~4,
                                    T~ 0), #Treat missing data as non-detect
    algdead_noupstream_cover_score = case_when(algcover_upstreamsource == "yes"~0,
                                               algcover_dead=="notdetected"~0,
                                               algcover_dead=="<2%"~1,
                                               algcover_dead=="2-10%"~2,
                                               algcover_dead=="10-40%"~3,
                                               algcover_dead==">40%"~4,
                                               T~ 0), #Treat missing data as non-detect
    alglivedead_cover_score=case_when(alglive_cover_score>0~alglive_cover_score,
                                      T~algdead_cover_score)
  )

#Load crosswalk
##Crosswalk includes metadata about region, stratum, determination (E vs I vs P),
##visit type (validation vs baseline, etc.), and visit number.
# xwalk<-read.csv("Input/Xwalk_Original_042920.csv", stringsAsFactors = F) %>%
xwalk<-#read.csv("Input/Xwalk_Original_052120.csv", stringsAsFactors = F) %>%
  read.csv("Input/Xwalk_Original_062720.csv", stringsAsFactors = F) %>%
  select(-collection_date) %>%
  # rename(globalid=ï..globalid) %>%
  mutate(SourceType=case_when(!(Determination_Hyd%in% c("E","I","P")) & !(Determination_Exp%in% c("E","I","P"))~"TechTeam BPJ or case study",
                              (!(Determination_Hyd%in% c("E","I","P")) & (Determination_Exp%in% c("E","I","P")))~"Local expertise",
                              ((Determination_Hyd%in% c("E","I","P")) & !(Determination_Exp%in% c("E","I","P")))~"Hydro data",
                              ((Determination_Hyd%in% c("E","I","P")) & (Determination_Exp%in% c("E","I","P")))~"Both local and hydro",
                              T~"XXXXX"),
         SourceType2=case_when(!(Determination_Hyd%in% c("E","I","P")) & !(Determination_Exp%in% c("E","I","P"))~"bpj",
                               (!(Determination_Hyd%in% c("E","I","P")) & (Determination_Exp%in% c("E","I","P")))~"L",
                               ((Determination_Hyd%in% c("E","I","P")) & !(Determination_Exp%in% c("E","I","P")))~"H",
                               ((Determination_Hyd%in% c("E","I","P")) & (Determination_Exp%in% c("E","I","P")))~"LH",
                               T~"XXXXX"),
         SourceType3=case_when(SourceType2=="bpj"~"L",T~SourceType2))

setdiff(xwalk$globalid, mydf$globalid)
mydf%>%
  filter(globalid %in% setdiff(mydf$globalid,xwalk$globalid ) )%>%
  select(sitecode_entry)

xwalk[duplicated(xwalk$globalid),]


#
# xwalk %>% filter (OriginSource == "Bryant Dickens and Patti Spindler")
# xwalk$OriginSource %>% unique()
#
# xwalk<-xwalk %>%
#   mutate(OriginSource2 = case_when(Determination_Final=="Unknown"~"Local expertise",
#                                    OriginSource=="Bryant Dickens and Patti Spindler"~"Wildlife cam",
#                                    OriginSource=="USGS"~"USGS",
#                                    OriginSource %in% c("Lindsey Reynolds")~"Logger and expertise"
#                                    T~"xxx"))


#Join to xwalk and drop junk data
mydf2<-left_join(mydf, xwalk) %>%
  filter(SiteType!="Reject" & Visit_No>0) %>%
  mutate(Region.f=factor(Region, levels=c("ASW","WM")),
         Stratum_ASW = case_when(Region=="ASW"~Stratum,
                                 State=="California"~"CA",
                                 State=="Nevada"~"NV",
                                 State=="Arizona"~"AZ",
                                 State=="New Mexico"~"NM-TX",
                                 State %in% c("Colorado","Utah","Wyoming","Montana")~"CO-WY-UT-MT",
                                 T~"XXXXX") %>% as.factor()
  ) %>%
filter(Region=="ASW")

# mydf2 %>%
#   group_by(Stratum, Determination_Final, SiteType, Visit_No) %>%
#   tally() %>%
#   pivot_wider(id_cols=c(Stratum, SiteType, Visit_No), names_from=Determination_Final,
#               values_from=n, values_fill=list(n=0)) %>%
#   write.table(file="clipboard", sep="\t", row.names=F)

mydf2 %>%
  filter(Determination_Final!="Unknown" & Visit_No==1) %>%
  select(SITECODE) %>%
  unique()

# #check regional attributions for errors
#
# study_area_sf<-st_read("M:/Data/RaphaelMazor/Flow Duration Indicators/Feb2019/ASWandWMStudyAreas.shp")
study_area_sf<-st_read("Input/studyarea_shp/ASWandWMStudyAreas.shp")
st_crs(study_area_sf)
study_area_sf<-st_transform(study_area_sf, crs=4326) %>%
  mutate(Region=case_when(Region=="USACE Arid West Region"~"ASW",
                          Region=="USACE Western Mountains, Valleys, and Coast Region"~"WM",
                          T~"xxx"))
study_area_pnw_sf<-st_read("Input/studyarea_shp/USACE_Lower48.shp") %>%
  st_transform(crs=4326) %>% 
  filter(Region=="USACE Arid West Region")
ggplot()+geom_sf(data=study_area_pnw_sf)

mydf_sf<-st_as_sf(mydf2,
                  coords=c("Longitude","Latitude"),
                  remove=F,
                  crs=4326) 

lower48<-#st_read("Input/studyarea_shp/USACE_Lower48.shp") %>%
  st_read("Input/studyarea_shp/statesp020_dissolve.shp") %>%
  st_transform(crs=4326)


area.map<-ggplot()+
  geom_sf(data=study_area_sf, show.legend = F, fill="gray50")+
  geom_sf(data=study_area_sf %>% filter(Region=="ASW"), show.legend = F, fill="gray80")+
  # geom_sf(data=study_area_sf %>% filter(Region=="ASW"), aes(fill=Stratum))+
  scale_fill_brewer(palette="Accent")+
  theme(axis.text=element_blank(), panel.background = element_blank(),axis.ticks = element_blank())

mybb<-st_bbox(study_area_sf)


mydf_sf<-mydf_sf %>%
  mutate(SourceType3 = case_when(SourceType2 %in% c("bpj","L")~"Local expertise or study",
                                 SourceType2 =="H"~c("Hydro data"),
                                 SourceType2=="LH"~"Both",
                                 T~"x"))

mydf_sf2<-mydf_sf %>%
  filter(Determination_Final!="Unknown") %>%
  mutate(DetFinal = case_when(Determination_Final=="E"~"Ephemeral", 
                              Determination_Final=="I"~"Intermittent",T~"Perennial"))

dev_map<-ggplot()+
  geom_sf(data=lower48, fill="white")+
  geom_sf(data=study_area_pnw_sf, fill="gray", color="white")+
  geom_sf(data=study_area_sf %>% filter(Region=="ASW"), aes(fill=Stratum), color="white")+
  geom_sf(data=lower48, fill=NA)+
  scale_fill_brewer(palette="Pastel1", guide=F)+
  theme(axis.text=element_blank(), panel.background = element_blank(),axis.ticks = element_blank())+
  geom_sf(data=mydf_sf2) + 
  facet_wrap(~DetFinal, nrow = 1)+
  coord_sf(xlim=c(mybb[1], mybb[3]),
           ylim=c(mybb[2], mybb[4]))

ggsave(dev_map, filename = "Figures/DevMap2.jpg", dpi=300, height=3, width=6)

area.map +
  geom_sf(data=mydf_sf %>% filter(Determination_Final!="Unknown" & Region=="ASW"),
          aes(color=SourceType))+
  facet_wrap(~Determination_Final)+
  theme_minimal()+
  scale_color_brewer(name="Source", palette="Set1")+
  theme(axis.text=element_blank(),
        legend.position = "bottom")


wm_area.map<-ggplot()+
  geom_sf(data=study_area_sf, show.legend = F, fill="gray50")+
  geom_sf(data=study_area_sf %>% filter(Region!="ASW"), show.legend = F,
          aes(fill=Stratum))+scale_fill_brewer(palette="Pastel1")


#
#
mydf2 %>%
  select(globalid, SITECODE, sitecode_entry, sitename, SiteID, latitude, longitude, 
         collection_date, Region, Stratum, Determination_Final, SourceType2, DeterminationStatus) %>%
  arrange(Region,Stratum,Determination_Final, SITECODE, collection_date)

# mydf2 %>%
#   group_by(Region,Stratum,Determination_Final) %>%
#   # filter(Region=="ASW") %>%
#   tally() %>%
#   pivot_wider(names_from = Determination_Final, values_from = n) %>%
#   write.table(file="clipboard", sep="\t")


###Add miscellaneous metrics


mydf2_shade<-mydf2 %>%
  pivot_longer(cols=c(dens_UU, dens_UR, dens_UD, dens_UL,
                      dens_MU, dens_MR, dens_MD, dens_ML,
                      dens_LU, dens_LR, dens_LD, dens_LL),
               values_drop_na = T
              ) %>%
  group_by(SITECODE, Visit_No) %>%
  summarise(PctShading = 100*mean(value/17)) %>%
  ungroup()



mydf2_bank<-mydf2 %>%
  pivot_longer(cols=c(bnkwidth0, bnkwidth15, bnkwidth30),
               values_drop_na = T
  ) %>%
  group_by(SITECODE, Visit_No) %>%
  summarise(BankWidthMean = mean(value)) %>%
  ungroup()

mydf2<-mydf2 %>%
  left_join(mydf2_shade) %>% left_join(mydf2_bank)
  # mutate(PctShading = (dens_UU + dens_UR + dens_UD + dens_UL+
  #                        dens_MU + dens_MR + dens_MD + dens_ML+
  #                        dens_LU + dens_LR + dens_LD + dens_LL)/(17*12*.01),
  #        BankWidthMean = (bnkwidth0 + bnkwidth15 + bnkwidth30)/3
         # 
  # )

mydf2$PctShading[mydf2$globalid=="{C9500F1D-2A9C-4A67-9BD5-FC33B7B90BF9}"]<-0 #field notes
mydf2$PctShading[mydf2$globalid=="{F5A4BC1D-0264-47A9-BF2A-E838FFC093A0}"]<-0 #field notes
mydf2$PctShading[mydf2$globalid=="{A80CD7EC-27C9-4EB3-AC7E-2DC537105A18}"]<-0 #field notes
mydf2$PctShading[mydf2$globalid=="{C62A141A-7EC7-49D6-AF97-002CE99C6DBA}"]<-0 #field notes
mydf2$PctShading[mydf2$globalid=="{7852F5B8-CC32-4CF1-8CD5-7571BCE472F8}"]<-0 #field notes
mydf2$PctShading[mydf2$globalid=="{7192F109-8617-4231-871C-DAD86552E24B}"]<-0 #site photos


#calculate soil moisture metric, scaled 0-1-2
mydf2_soil<-mydf2 %>%
  select(globalid, waterinchannel_score, soilmoist_1, soilmoist_2, soilmoist_3) %>%
  mutate(soilmoist1_score = case_when(waterinchannel_score>0~2,
                                      soilmoist_1=="dry"~0,
                                      soilmoist_1=="partiallydry"~1,
                                      soilmoist_1=="saturated"~2,
                                      T~ -99),
         soilmoist2_score = case_when(waterinchannel_score>0~2,
                                      soilmoist_2=="dry"~0,
                                      soilmoist_2=="partiallydry"~1,
                                      soilmoist_2=="saturated"~2,
                                      T~ -99),
         soilmoist3_score = case_when(waterinchannel_score>0~2,
                                      soilmoist_3=="dry"~0,
                                      soilmoist_3=="partiallydry"~1,
                                      soilmoist_3=="saturated"~2,
                                      T~ -99),
         SoilMoist_MeanScore = (soilmoist1_score+soilmoist2_score+soilmoist3_score)/3,
         SoilMoist_MaxScore = pmax(soilmoist1_score,soilmoist2_score,soilmoist3_score)
  )
#Manual updates due to missing data, where possible
mydf2_soil$SoilMoist_MaxScore[mydf2_soil$globalid=="{C62A141A-7EC7-49D6-AF97-002CE99C6DBA}"]<-0 #Concrete channel
mydf2_soil$SoilMoist_MeanScore[mydf2_soil$globalid=="{C62A141A-7EC7-49D6-AF97-002CE99C6DBA}"]<-0 #Concrete channel
mydf2_soil$SoilMoist_MaxScore[mydf2_soil$globalid=="{E7723A40-D609-4386-9731-CD2DC21FD69A}"]<-0 #Reasonable inference (bone dry) ASK GROUP
mydf2_soil$SoilMoist_MeanScore[mydf2_soil$globalid=="{E7723A40-D609-4386-9731-CD2DC21FD69A}"]<-0 #Reasonable inference (bone dry)



mydf2<-left_join(mydf2, mydf2_soil %>% select(globalid, SoilMoist_MeanScore, SoilMoist_MaxScore) %>% unique())




####NM tool application
nm_score_vars<-c("waterinchannel_score","fishabund_score","bmiabund_score","algabund_score","vegdiff_score","rootedplants_score",
                 "sinuosity_score","floodplaindim_score","riffpoolseq_score",
                 "substratesorting_score","hydric_score","seddep_score",
                 "springs_score","iofb_score")
#Review data gaps
mydf2y<-mydf2 %>%
  mutate(nm_score_final = (waterinchannel_score+
           fishabund_score+
           bmiabund_score+
           algabund_score+
           vegdiff_score+
           rootedplants_score+
           sinuosity_score+
           floodplaindim_score+
           riffpoolseq_score+
           substratesorting_score+
           hydric_score+
           seddep_score+
           springs_score+
           iofb_score),
         nm_class1 = case_when(nm_score_final<9~"E",
                               nm_score_final<12~"It",
                               nm_score_final<=19~"I",
                               nm_score_final<=22~"Pt",
                               nm_score_final>22~"P",
                               T~"error"),
         nm_class2 = case_when(nm_class1=="It"~"I",
                               nm_class1=="Pt"~"P",
                               T~nm_class1),
         nm_class_final = case_when(nm_class2=="E" & (fishabund_score+bmiabund_score)>0 ~ "I",
                                    T~nm_class2)
  )

mydf2y %>%
  filter(is.na(nm_score_final)) %>%
  filter(Determination_Final %in% c("E","I","P")) %>%
  filter(!SITECODE %in% c("NVAW1193","NVAW1190")) %>%
  select(SITECODE, all_of(nm_score_vars))



####PNW tool application
mydf_ai<-# read.csv("Input/aquaticinvertebrates_34M_manual update_040720.csv", stringsAsFactors = F) %>%
  read.csv("Input/aquaticinvertebrates_34M_manual update_061120.csv", stringsAsFactors = F) %>%
  rename(#globalid = Parent_GID, #"globalid" will link invert data to core data
    # AiGlobal_ID = Global_ID, #AiGlobal_ID is the unique identifier for bug data
    Taxon_original= Taxon,
    Taxon=Taxon2,
    Abundance = Updated.Abundance) %>%
  left_join(read.csv("Input/OrganismLookUp_SAFIT.csv", stringsAsFactors = F), by=c("Taxon"="FinalID"))


mydf_ai<-mydf_ai %>%
  filter(!is.na(Phylum)) %>% #You lose a few terrestrials and all "no bug" samples 
  mutate(mayfly = Order=="Ephemeroptera",
         perennialindicator = Family %in% c("Pleuroceridae","Ancylidae","Hydrobiidae",
                                            "Margaritiferidae", "Unionidae",
                                            "Rhyacophilidae","Philopotamidae","Hydropsychidae","Glossosomatidae",
                                            "Pteronarcyidae","Perlidae",
                                            "Elmidae","Psephenidae","Elmidae",
                                            "Corydalidae",
                                            "Gomphidae","Cordulegastridae","Calopterygidae"),
         perennialindicator2 = case_when(Genus %in% c("Neohermes","Protochauliodes")~F,T~perennialindicator),
         deadmaterial = Ai_select %in% c("deadmaterial",  #Logical vector to indicate if observation is from dead material only
                                         "perennialindicator,deadmaterial",
                                         "deadmaterial,perennialindicator"),
         EPT= Order %in% c("Ephemeroptera","Plecoptera","Trichoptera"),
         GOLD= Class %in% c("Gastropoda", "Oligochaeta") | Order %in% c("Diptera"),
         OCH = Order %in% c("Odonata", "Coleoptera", "Hemiptera"),
         Noninsect = !Class %in% "Insecta") 




mydf_ai_metrics<-mydf_ai %>%
  filter(globalid!="") %>%
  #Aggregate FinalIDs split over multiple rows
  select(globalid, Taxon,Abundance,deadmaterial, Class,Order, Family,
         perennialindicator, perennialindicator2, mayfly, EPT, GOLD, OCH, Noninsect) %>%
  group_by(globalid, Taxon,deadmaterial, Class,Order, Family,
           perennialindicator, perennialindicator2, mayfly, EPT, GOLD, OCH, Noninsect) %>%
  summarise(Abundance=sum(Abundance)) %>%
  ungroup() %>%
  #Calculate metrics
  group_by(globalid) %>%
  summarise(TotalAbundance = sum(Abundance),
            Richness = length(unique(Taxon)),
            mayfly_abundance = sum(mayfly*Abundance),
            mayfly_gt6 = mayfly_abundance>=6,
            perennial_abundance=sum(perennialindicator*Abundance),
            perennial_taxa=sum(perennialindicator),
            perennial_live_abundance = sum(perennialindicator*Abundance*!deadmaterial),
            perennial_abundance2=sum(perennialindicator2*Abundance),
            perennial_taxa2=sum(perennialindicator2),
            perennial_live_abundance2 = sum(perennialindicator2*Abundance*!deadmaterial),
            EPT_abundance = sum(EPT*Abundance),
            EPT_taxa = sum(EPT),
            EPT_relabd = case_when(TotalAbundance==0~1,T~(EPT_abundance / TotalAbundance)),
            EPT_reltaxa = case_when(Richness==0~1,T~(EPT_taxa / Richness)),
            GOLD_abundance = sum(GOLD*Abundance),
            GOLD_taxa = sum(GOLD),
            OCH_abundance = sum(OCH*Abundance),
            OCH_taxa = sum(OCH),
            Noninsect_abundance = sum(Noninsect * Abundance),
            Noninsect_taxa=sum(Noninsect),
            Noninsect_relabund = case_when(TotalAbundance==0~1,T~(Noninsect_abundance / TotalAbundance)),
            Noninsect_reltaxa = case_when(Richness==0~1,T~(sum(Noninsect) / Richness)),
            GOLD_relabd = case_when(TotalAbundance==0~1,T~(GOLD_abundance / TotalAbundance)),
            GOLD_reltaxa = case_when(Richness==0~1,T~(GOLD_taxa / Richness)),
            OCH_relabd = case_when(TotalAbundance==0~1,T~(OCH_abundance / TotalAbundance)),
            OCH_reltaxa = case_when(Richness==0~1,T~(OCH_taxa / Richness)),
            GOLDOCH_relabd = case_when(TotalAbundance==0~1,T~((OCH_abundance + GOLD_abundance) / TotalAbundance)),
            GOLDOCH_reltaxa = case_when(Richness==0~1,T~((OCH_taxa + GOLD_taxa) / Richness))
  ) %>%
  pivot_longer(cols=c(TotalAbundance, Richness, mayfly_abundance, 
                 mayfly_gt6, perennial_abundance, perennial_taxa, perennial_live_abundance, 
                 perennial_abundance2, perennial_taxa2, perennial_live_abundance2, 
                 EPT_abundance, EPT_taxa,EPT_relabd,EPT_reltaxa, GOLD_abundance, GOLD_taxa, OCH_abundance, 
                 OCH_taxa, Noninsect_abundance, Noninsect_taxa, Noninsect_relabund, 
                 Noninsect_reltaxa, GOLD_relabd, GOLD_reltaxa,OCH_relabd,OCH_reltaxa,GOLDOCH_relabd,GOLDOCH_reltaxa),
               names_to="Metric",
               values_to = "MetricResult") 
bmi_metrics<-mydf_ai_metrics$Metric %>% unique()

mydf_ai_metrics2<-mydf_ai_metrics %>%
  right_join(    crossing(globalid = mydf$globalid,
             Metric =    bmi_metrics ) ) %>%
  replace_na(list(MetricResult=0)) %>%
  pivot_wider(id_cols=globalid, names_from = "Metric",values_from = "MetricResult") %>%
  mutate(mayfly_gt6 = mayfly_gt6==1)

#Add those new data frames (i.e., those with invert metrics) to core data
mydf.x<-mydf2y %>%
  left_join(mydf_ai_metrics2)



#Import and up HYDROVEG data
mydf_hydroveg <- #read.csv("Input/hydroveg_34M_121619.csv", stringsAsFactors = FALSE) %>%
  read.csv("Input/NewData/hydrovegdata_34merge_06092020.csv", stringsAsFactors = FALSE) %>%
  rename(globalid = ParentGID, #globalid links back to core data
         vegGlobal_ID = GlobalID) %>% #vegGlobal_ID is unique identifier of veg data
  mutate(hydrophyte = (IndStatus == "hydrophyte"),#creates variable T/F for hydrophytes
         UnusualDis = (UnusualDis == "yes")) #creates variable T/F for unusual indicator distribution

#Overwrite errors
#Should be labeled as a hydrophyte
mydf_hydroveg$hydrophyte[mydf_hydroveg$Species %in%
                           c("Willow","Willow spp unk",
                             "Alnus spp","Alnus spp?","Alnus spp unknown?",
                             "Aquatic plant 1","Aquatic plant 2",
                             "Watercress",
                             "Narrowleaf cottonwood?","Narrow leaf cottonwood","Populus angustifolia",
                             "Narrowleaf willow?","Salix Gooddingii")]<-T
#Should not be labeled as a hydrophyte
mydf_hydroveg$hydrophyte[mydf_hydroveg$Species %in%
                           c("Bee balm / wild bergamot",
                             "Freemont cottonwood","Fremont cottonwood")]<-F
mydf_hydroveg$hydrophyte[mydf_hydroveg$vegGlobal_ID=="{292551D8-FAE9-48A9-9364-03223F8E7B6D}"]<-F #This was a rose
mydf_hydroveg$Species[mydf_hydroveg$vegGlobal_ID=="{2C1EEF48-FD75-4C79-94BB-3F37DFF258D1}"]<-"Equisetum"

#No species were named for this site, but in the photos, at least one is Scirpus
mydf_hydroveg %>%  filter(globalid=="{D9B33486-3745-4513-9101-5475A775DD9B}")
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{D9B33486-3745-4513-9101-5475A775DD9B}"][1]<-"Scirpus"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{D9B33486-3745-4513-9101-5475A775DD9B}"][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{D9B33486-3745-4513-9101-5475A775DD9B}"][1]<-T

#No species were named for this site, but there are willows and alders
mydf_hydroveg %>%  filter(globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}")
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}"][1]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}"][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}"][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}"][2]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}"][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{166BF7D4-9B48-4C85-841A-ADF5DA2FC029}"][2]<-T

#No species were named for this site, but there are willows and alders
mydf_hydroveg %>%  filter(globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}")
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}"][1]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}"][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}"][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}"][2]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}"][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{F238494B-0251-4BFF-BE45-9C58A388CBF1}"][2]<-T


#No species were named for this site, but there are willows,alders and sedges
mydf_hydroveg %>%  filter(globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}")
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}"][1]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}"][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}"][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}"][2]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}}"][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}}"][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}"][3]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}}"][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}}"][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}"][5]<-"Platanus racemosa"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}}"][5]<-"nonhydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{89A5B7C0-9616-4364-95A6-DE801FC8E4BD}}"][5]<-F

#No species were named for this site, but there are alders and sedges
mydf_hydroveg %>%  filter(globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}")
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}"][1]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}"][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}"][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}"][2]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}"][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{52403A9A-54BC-45F5-B23D-D29658219B59}"][2]<-T


#No species were named for this site, but there is darmera, sedges
mydf_hydroveg %>%  filter(globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}")
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][1]<-"Darmera peltata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][2]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][3]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid=="{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"][3]<-T


#No species were named for this site, but there is alder and horsetail
update.site<-"{FAB5EE74-1DBB-4AE1-97F6-6AE088766AAD}"
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Equisetum hyemale"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T


#No species were named for this site, but there are several hydrophytes
update.site<-"{F1B3DEF9-16D2-4397-8939-8A43FF1C0F28}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Persicaria"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Nasturtium officinale"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Atriplex prostrata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][5]<-"Polypogon monspeliensis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][5]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][5]<-T



#No species were named for this site, but there are typha, horsetail, willow
update.site<-"{0FCA084D-D648-439B-A395-545D6CEB66E5}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Typha"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Fraxinus velutina"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"nonhydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-F
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Equisetum hyemale"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T



#No species were named for this site, but there are sedges
update.site<-"{06A7512C-EC2C-4C45-BDCA-E19DBEB59AB0}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T

#No species were named for this site, but there are sedges
update.site<-"{C4B26978-8955-4CB6-8D75-80412FBE9D97}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T


#No species were named for this site, but there are sedges
update.site<-"{F092D24A-EB77-461A-BD52-FD733D22004F}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T



#No species were named for this site, but there are arundo
update.site<-"{17CD3DCE-3CE9-4C74-A866-7AE152ACE663}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Arundo donax"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T


#No species were named for this site, but there are several macrophytes
update.site<-"{5B3B4CA7-B962-4FC3-A59D-86BEB3EBD155}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Salix laevigata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T


#No species were named for this site, but there are several macrophytes
update.site<-"{FADB518F-8CC5-4F87-820A-C238D1B723D5}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Nymphaea"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Typha"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Unknown pondweed"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T




#No species were named for this site, but there are willows
update.site<-"{AF9CBBEB-DA8B-4613-982A-8C87B5C89F96}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Artemesia douglasiana"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Stachys albens"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T


#No species were named for this site, but there are hydrophytes
update.site<-"{CAC9CA19-997C-411D-8982-3F6F5B5A5D9B}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Salix laevigata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Menyanthes trifoliata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Datisca glomerata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T


#No species were named for this site, but there are hydrophytes
update.site<-"{2FEF0481-C3A9-47DC-90C1-EF2A19E2212B}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Cornus"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Platanthera"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T


#No species were named for this site, but there are hydrophytes
update.site<-"{93875B9D-4BE9-4E08-835D-295202FFAD39}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Cyperaceae"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][4]<-"Petasites frigidus"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][4]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][4]<-T



#No species were named for this site, but there are hydrophytes
update.site<-"{B562BACC-AA9B-418E-BB0D-5E11A8A2B534}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Cyperus eragrostis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T



#No species were named for this site, but there are hydrophytes
update.site<-"{16894BF5-B3D9-4D40-BA70-037CD685382B}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Equisetum hyemale" #I WANT CONFRIMATION OF THIS!
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T



#No species were named for this site, but there are hydrophytes
update.site<-"{90190FDB-89E1-4C3A-A5AD-08513D7CE265}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Ludwigia"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Polypogon monspeliensis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Schoenoplectus"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T



#No species were named for this site, but there are hydrophytes
update.site<-"{643F5396-780D-4B62-804D-25C02EB17B2D}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Typha"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T


#No species were named for this site, but there are hydrophytes
update.site<-"{0E999336-86B6-4F9B-B719-FB68A2B1237C}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Baccharis salicina" #GET VERIFICATION
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T


#No species were named for this site, but there are hydrophytes
update.site<-"{6162F393-6BBD-4A6A-A3E3-F1D022C10485}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Typha"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T

#No species were named for this site, but there are hydrophytes
update.site<-"{B6224C76-FD28-40E6-AA68-78F21457286B}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][3]<-"Salix"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][3]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][3]<-T

#No species were named for this site, but there are hydrophytes
update.site<-"{12CFE0F7-37CC-442B-9980-A3D7A92E2935}" 
mydf %>% filter(globalid == update.site) %>% select(sitename)
mydf_hydroveg %>%  filter(globalid==update.site)
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][1]<-"Salix lasiolepis"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][1]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][1]<-T
mydf_hydroveg$Species[mydf_hydroveg$globalid==update.site][2]<-"Salix laevigata"
mydf_hydroveg$IndStatus[mydf_hydroveg$globalid==update.site][2]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$globalid==update.site][2]<-T





#This site has chilopsis identified as a willow
update.vegid<-"{B76EC82E-9688-4309-832F-A308F58312A7}" 
mydf_hydroveg$Species[mydf_hydroveg$vegGlobal_ID==update.vegid]<-"Chilopsis"
mydf_hydroveg$IndStatus[mydf_hydroveg$vegGlobal_ID==update.vegid]<-"nonhydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$vegGlobal_ID==update.vegid]<-F
mydf_hydroveg %>%  filter(globalid=="{6C4C6810-7B60-46D4-B81A-96534F8475EA}")


#This site has narrow-leaf cottonwood identified as a populus unknown
update.vegid<-"{B618D026-8AC5-475A-802F-1980575BED08}" 
mydf_hydroveg$Species[mydf_hydroveg$vegGlobal_ID==update.vegid]<-"Populus angustifolium"
mydf_hydroveg$IndStatus[mydf_hydroveg$vegGlobal_ID==update.vegid]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$vegGlobal_ID==update.vegid]<-T
update.vegid<-"{7AF42B3F-CC5E-4250-B338-E4EFB3353130}" 
mydf_hydroveg$Species[mydf_hydroveg$vegGlobal_ID==update.vegid]<-"Alnus rhombifolia"
mydf_hydroveg$IndStatus[mydf_hydroveg$vegGlobal_ID==update.vegid]<-"hydrophyte"
mydf_hydroveg$hydrophyte[mydf_hydroveg$vegGlobal_ID==update.vegid]<-T



#Other manual overwrites
mydf_hydroveg$Species[mydf_hydroveg$vegGlobal_ID=="{CDADCE29-AB4A-4AA1-B487-D2D108F4E17F}"]<-"Juncus"
mydf_hydroveg$hydrophyte[mydf_hydroveg$vegGlobal_ID=="{CDADCE29-AB4A-4AA1-B487-D2D108F4E17F}"]<-TRUE


#No species were named for this site, but there are  willow
update.site<-"{535F78A0-E097-465B-9DC2-4C7CDCFA9925}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
AZAW0035_veg.df<-data.frame(vegGlobal_ID="{535F78A0-E097-465B-9DC2-4C7CDCFA9925}_1",
                             Note="Row inserted by RDM",
                             Species="Salix goodingii",
                             IndStatus="hydrophyte",
                             UnusualDis=F,
                             HVNote="In flower in several photos",
                             globalid="{535F78A0-E097-465B-9DC2-4C7CDCFA9925}",
                             hydrophyte=T)



#No plants were entered for this site, even though Typha is present
NVAW1193_veg.df<-data.frame(vegGlobal_ID="{FEDD663B-6EEB-44C0-B4B7-EEDA6CA8D8D7}_1",
                        Note="Row inserted by RDM",
                        Species="Typha",
                        IndStatus="hydrophyte",
                        UnusualDis=F,
                        HVNote="Large clump visible in photos",
                        globalid="{FEDD663B-6EEB-44C0-B4B7-EEDA6CA8D8D7}",
                        hydrophyte=T)



#No species were named for this site
update.site<-"{C62A141A-7EC7-49D6-AF97-002CE99C6DBA}"
mydf %>% filter(globalid == update.site)
mydf_hydroveg %>%  filter(globalid==update.site)
AZAW0050_veg.df<-data.frame(vegGlobal_ID=c(paste0(update.site,"_1"),paste0(update.site,"_2"),paste0(update.site,"_3")),
                            Note=c("Row inserted by RDM"),
                            Species=c("Ambrosia","Tamarix","Bermuda grass","Mimulus guttatus","Baccharais salicina","Polypogon monspeliensis"),
                            IndStatus=c("nonhydrophyte","nonhydrophyte","nonhydrophyte","hydrophyte","hydrophyte","hydrophyte"),
                            UnusualDis=c(F,F,F,F,F,F),
                            # HVNote="In flower in several photos",
                            globalid=c(update.site),
                            hydrophyte=c(F,F,F,T,T,T))


mydf_hydroveg<-mydf_hydroveg %>%
  bind_rows(list(AZAW0035_veg.df, NVAW1193_veg.df))


#If you want to use an alternative list of hydrophytes (e.g., cottonwoods),
mydf_hydroveg$hydrophyte2<-case_when(mydf_hydroveg$Species %in%
                                       c("Cottonwood" ,"Fremont cottonwood","Freemont cottonwood","Populus fremontii","Populus spp","Populus spp unkn")~T,  #Add names of "alternative" hydrophytes here, and default to original status otherwise
                                     T~mydf_hydroveg$hydrophyte)


#Calculate metrics based on hydroveg data
mydf_hydroveg_summary<-mydf_hydroveg %>%
  group_by(globalid) %>%
  summarise(
    hydrophytes_present_any = sum(any(hydrophyte)), #Number of hydrophyte species recorded
    hydrophytes_present_any_noflag = sum(any(hydrophyte[!UnusualDis])), #Number of unflagged hydrophyte species recorded
    hydrophytes_present = sum(hydrophyte), #Number of hydrophyte species recorded
    hydrophytes_present_noflag = sum(hydrophyte[!UnusualDis]), #Number of unflagged hydrophyte species recorded
    hydrophytes2_present = sum(any(hydrophyte2)), #Number of alt hydrophyte species recorded
    hydrophytes2_present_noflag = sum(any(hydrophyte2[!UnusualDis])) #Number of unflagged alt hydrophyte species recorded
  ) %>%
  ungroup() %>%
  right_join(mydf %>% select(globalid))%>%
  replace_na(list(hydrophytes_present=F,hydrophytes_present_noflag=F, #Replace NAs with F
                  hydrophytes2_present=F,hydrophytes2_present_noflag=F)) #Replace NAs with F

#Add hydroveg metrics to core + bug metric data
mydf.x2<- mydf.x %>%
  left_join(mydf_hydroveg_summary)


#Create PNW variables based on bug and hydroveg metrics, slope
mydf.x2 <- mydf.x2 %>%
  mutate(ai_present = bmiabund_score>0,
         peren_present = perennial_abundance>0,
         slope_gt10.5 = valleyslope>=10.5,
         slope_gt16 = valleyslope>=16,
         # hydrophytes_present_any = hydrophytes_present>0
         NULL)
#Create a vector of PNW metric names
pnw_vars<-c("ai_present","mayfly_gt6","hydrophytes_present_any","peren_present","slope_gt10.5","slope_gt16")

mydf.x2 <- mydf.x2 %>%
  mutate(
    #Make PNW determination, as per Nadeau 2015
    pnw_class_final = case_when(ai_present & mayfly_gt6 & peren_present ~ "P",
                                ai_present & mayfly_gt6 & !peren_present & slope_gt16 ~ "P",
                                ai_present & mayfly_gt6 & !peren_present & !slope_gt16 ~ "I",
                                ai_present & !mayfly_gt6 ~ "I",
                                !ai_present & !hydrophytes_present_any ~ "E",
                                !ai_present & hydrophytes_present_any & slope_gt10.5 ~ "E",
                                !ai_present & hydrophytes_present_any & !slope_gt10.5 ~ "I",
                                T~"error"),
    #Make PNW determination, but ignore flagged hydrophytes
    pnw.noflag_class_final = case_when(ai_present & mayfly_gt6 & peren_present ~ "P",
                                       ai_present & mayfly_gt6 & !peren_present & slope_gt16 ~ "P",
                                       ai_present & mayfly_gt6 & !peren_present & !slope_gt16 ~ "I",
                                       ai_present & !mayfly_gt6 ~ "I",
                                       !ai_present & hydrophytes_present_noflag==0 ~ "E",
                                       !ai_present & hydrophytes_present_noflag>0 & slope_gt10.5 ~ "E",
                                       !ai_present & hydrophytes_present_noflag>0 & !slope_gt10.5 ~ "I",
                                       T~"error")
  )

mydf2<-mydf.x2

####Add streamcat


nlcd2016<-read.csv("Input/NLCD_2011_streamcat.csv", stringsAsFactors = F) %>%
  select(globalid, 
         WsAreaSqKm, urb, urbRip, forr, forrRip, ag, agRip,
         wetlands=wet,wetlandsRip=wetRip,
         urbWs, urbRipWs, forrWs, forrWsRip, agWs, agRipWs,
         wetlandsWs=wetWS, wetlandsRipWS=wetRipWS) %>%
  na.omit()
lithocat<-read.csv("Input/GeoStreamCat120_all.csv", stringsAsFactors = F) %>%
  select(-Visit_No, -Region, -Latitude, -Longitude, -COMID, -NHDPlus_Region) %>%
  rename(wetlands=wet,wetlandsRip=wetRip, wetlandsWs=wetWS, wetlandsRipWS=wetRipWS) %>%
  unique()
  
mydf2<-mydf.x2 %>%
  # left_join(nlcd2016) #%>%
  left_join(lithocat)
setdiff(mydf.x2$SITECODE,lithocat$SITECODE)

head(mydf2)

mydf2%>%
  filter(Region=="ASW" &
           Determination_Final %in% c("E","I","P")) %>%
  filter(SourceType2=="bpj" & Determination_Final=="E")

#Overwrite status of two Hassayampa sites?
# mydf2$Determination_Final[mydf2$globalid=="{18F1B7F2-30B8-4442-8996-A6C3CCD5376B}"]<-"Unknown"
# mydf2$Determination_Final[mydf2$globalid=="{DC37829E-DEA9-485A-873D-D2BD7C2665C5}"]<-"Unknown"
mydf2 %>%
  filter(SourceType3=="LH" & DeterminationStatus!="Preferred")%>%
  select(sitecode_entry,
         SiteName, SiteID, SITECODE, Determination_Final, DeterminationStatus,HydroDataSource,OriginSource, OriginID,
         Determination_Hyd, Determination_Exp)%>%
  head()

mydf2$DeterminationStatus[which(mydf2$SourceType3=="LH")] <-"Preferred" #Is this right?




junk<-

library(skimr)
skim_without_charts(junk %>%
       select(SITECODE, collection_date, globalid,
              all_of(allpredz)))

dist_mat <- mydf2 %>% 
  filter(Visit_No>0) %>% 
    select(all_of(allpredz)) %>%
  as.matrix()

colnames(dist_mat)[colSums(is.na(dist_mat)) > 0]
mydf2 %>%
  filter(is.na(vegdiff_score) & Visit_No>0) %>%
  filter(!SITECODE %in% c("NVAW1193")) %>%
  select(SITECODE) %>%
  left_join( mydf2 %>% select(SITECODE, source, sitecode_entry, SiteID, collection_date, globalid))

mydf2 %>%
  filter(sitecode_entry=="USGS10321950") %>%
  select(vegdiff_score)

#Other corrections


# #These sites are missing values
# junk_preds<-setdiff(allpredz, c(GISPreds, StreamCatPreds))
# mydf2%>% 
#   # filter(!SITECODE %in% c("NVAW1193") & Determination_Final %in% c("I","E","P")) %>%
#   select(SITECODE, globalid, collection_date, sitecode_entry,source,
#          all_of(junk_preds )) %>%
#   pivot_longer(cols=all_of(c( junk_preds))) %>%
#   group_by(SITECODE, globalid, collection_date, sitecode_entry, source) %>%
#   summarise(missing=sum(is.na(value))) %>%
#   arrange(-missing) %>%
#   filter(missing>=5)
# 
# #Look here to see what's missing
# mydf2%>% 
#   select(SITECODE, globalid, collection_date, sitecode_entry,source,SiteName,latitude, longitude,
#          all_of(junk_preds )) %>%
#   filter(SITECODE %in% c("NVAW1190")) %>%
#   pivot_longer(cols=all_of(c( junk_preds))) %>%
#   filter(is.na(value)) %>% as.data.frame()
# 
# mydf2%>% 
#   select(SITECODE, globalid, collection_date, sitecode_entry,source,
#          all_of(junk_preds )) %>%
#   filter(SITECODE == c("NVAW1193")) %>%
#   pivot_longer(cols=all_of(c( junk_preds))) %>%
#   group_by(SITECODE, globalid, collection_date, sitecode_entry, source) %>%
#   summarise(missing=sum(is.na(value))) %>%
#   arrange(-missing) %>%
#   filter(missing==1)

  

mydf2 %>% 
  filter(!SITECODE %in% c("NVAW1193","NVAW1190") & 
           Determination_Final %in% c("E","I","P")) %>%
  filter(is.na(nm_score_final)) %>%
  select(SITECODE, SiteID, sitecode_entry, sitename, source)



write.csv(mydf2, "Input/mydf2.csv", row.names=F)
