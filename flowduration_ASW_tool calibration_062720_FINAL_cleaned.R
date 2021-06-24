# save.image("asw_fdam.Rdata")#
# load("asw_fdam.Rdata")
library(tidyverse)
library(ggrepel)
library(sf)
library(randomForest)
library(tidymodels)
library(rattle)
library(pdp)

# source("flowduration_ASW_data prep_042720.R")
mydf2<-read.csv("Input/mydf2.csv", stringsAsFactors = F) %>% 
  mutate(HydroDataFreq = case_when(HydroDataSource %in% 
                                     c("USGS stream gauge", "Hobo Logger","WildlifeCamera","Hobo logger","Temperature logger")~"Continuous",
                                   HydroDataSource %in% c("Stream Tracker")~"Discrete")) %>%
  filter(SiteID!="RRC.PCU") %>% #This "perennial" stream was dry. Cannot determine true class
  filter(!SITECODE %in% c( "NVAW1193", "NVAW1190")) #Too many missing values

mydf2 %>%
  filter(SITECODE %in% c("CAAW0419","CAAW0392")) %>%
  select(SITECODE, globalid, Visit_No)

#Quick review of data
mydf2 %>%
  filter( Region=="ASW" & Determination_Final %in% c("I","E","P")) %>%
  group_by(Visit_No, Stratum, Determination_Final) %>%
  tally() %>%
  pivot_wider(id_cols=c(Stratum, Visit_No), names_from = Determination_Final, values_from = n) %>%
  arrange(Stratum, Visit_No) %>%
  write.table(file="clipboard", sep="\t", row.names=F)



# mydf_sf<-st_as_sf(mydf2,
#                   coords=c("Longitude","Latitude"),
#                   remove=F,
#                   crs=4326) %>%
#   filter(Determination_Final %in% c("I","E","P"))
# 
# area.map +
# geom_sf(data=mydf_sf %>% filter(Region=="ASW")) +
# facet_wrap(~Determination_Final)
# 
# area.map + 
#   geom_sf(data=mydf_sf %>% filter(Region=="ASW" & Visit_No==2), show.legend = F) +
#   theme(legend.position="none")



########
#CREATE A NEW TOOL
#First, evaluate indicator importance in a random forest model!
########

#Let's make bunch of useful character vectors
# mydf2$Stratum_ASW<-case_when(mydf2$wet~"WET", T~"DRY")
asw_strata<-unique(mydf2$Stratum_ASW)

#potential predictors
BioPreds<-c(
  #NM varz
  "fishabund_score2","bmiabund_score","algabund_score","vegdiff_score",
  "rootedplants_score","iofb_score",
  #BMI varz
  "mayfly_abundance","perennial_abundance","perennial_taxa","perennial_live_abundance",
  #Vertebrates
  "snake_score","turt_score","vert_score",
  # "vertvoc_score","vertvoc_sumscore","frogvoc_score",
  "vert_sumscore",
  #Hydrophytes
  # "hydrophytes_present",
  "hydrophytes_present_noflag",
  #Novel bio
  "alglive_cover_score","algdead_cover_score","algdead_noupstream_cover_score",
  "alglivedead_cover_score",
  "moss_cover_score","liverwort_cover_score","PctShading",
  "ripariancorr_score", #This is from the PNW ancillary data
  #A few novel BMI metrics
  "TotalAbundance", "Richness",  
  "EPT_abundance", "EPT_taxa", "EPT_relabd","EPT_reltaxa",
  "GOLD_abundance", "GOLD_taxa", "OCH_abundance",   "OCH_taxa",
  "GOLD_relabd", "GOLD_reltaxa", "OCH_relabd","OCH_reltaxa", "GOLDOCH_relabd","GOLDOCH_reltaxa",  
  "Noninsect_abundance", "Noninsect_taxa", "Noninsect_relabund",   "Noninsect_reltaxa"
  
)

HydroPreds<-c(
  #NM varz
  "waterinchannel_score","hydric_score","springs_score",
  #Others and novel
  "pctsurfaceflow","pctsubsurfaceflow","numberpools","numberwoodyjams",
  "SoilMoist_MeanScore","SoilMoist_MaxScore"
)

GeomorphPreds<-c(
  #NM varz
  "sinuosity_score","floodplaindim_score","riffpoolseq_score",
  "substratesorting_score","seddep_score",
  #Other varz
  "BankWidthMean","valleyslope","erosion_score","floodplain_score"
)

GISPreds<-c("EcoII","EcoIII",
            "tmean","tmax","tmin",
            "ppt","ppt.m01","ppt.m02","ppt.m03","ppt.m04","ppt.m05","ppt.m06","ppt.m07","ppt.m08","ppt.m09","ppt.m10","ppt.m11","ppt.m12")



StreamCatPreds<-c(#"SITECODE",  "CatPctFullRp100", "WsPctFullRp100",
  "ElevCat", "ElevWs", "AgKffactCat", "KffactCat", 
  "AgKffactWs", "KffactWs", "Al2O3Cat", "CaOCat", "Fe2O3Cat", "K2OCat", 
  "MgOCat", "Na2OCat", "P2O5Cat", "SCat", "SiO2Cat", "Al2O3Ws", 
  "CaOWs", "Fe2O3Ws", "K2OWs", "MgOWs", "Na2OWs", "P2O5Ws", "SWs", 
  "SiO2Ws", "NCat", "NWs", "HydrlCondCat", "HydrlCondWs", "CompStrgthCat", 
  "CompStrgthWs", "MAST_2008", "MAST_2009", "MAST_2013", "MAST_2014", 
  "MSST_2008", "MSST_2009", "MSST_2013", "MSST_2014", "MWST_2008", 
  "MWST_2009", "MWST_2013", "MWST_2014", "Precip8110Cat", "Tmax8110Cat", 
  "Tmean8110Cat", "Tmin8110Cat", "Precip8110Ws", "Tmax8110Ws", 
  "Tmean8110Ws", "Tmin8110Ws", "ClayCat", "SandCat", "ClayWs", 
  "SandWs", "OmCat", "PermCat", "RckDepCat", "WtDepCat", "OmWs", 
  "PermWs", "RckDepWs", "WtDepWs", "PctCarbResidCat", "PctNonCarbResidCat", 
  "PctAlkIntruVolCat", "PctSilicicCat", "PctExtruVolCat", "PctColluvSedCat", 
  "PctGlacTilClayCat", "PctGlacTilLoamCat", "PctGlacTilCrsCat", 
  "PctGlacLakeCrsCat", "PctGlacLakeFineCat", "PctHydricCat", "PctEolCrsCat", 
  "PctEolFineCat", "PctSalLakeCat", "PctAlluvCoastCat", "PctCoastCrsCat", 
  "PctWaterCat", "PctCarbResidWs", "PctNonCarbResidWs", "PctAlkIntruVolWs", 
  "PctSilicicWs", "PctExtruVolWs", "PctColluvSedWs", "PctGlacTilClayWs", 
  "PctGlacTilLoamWs", "PctGlacTilCrsWs", "PctGlacLakeCrsWs", "PctGlacLakeFineWs", 
  "PctHydricWs", "PctEolCrsWs", "PctEolFineWs", "PctSalLakeWs", 
  "PctAlluvCoastWs", "PctCoastCrsWs", "PctWaterWs", "WetIndexCat", 
  "WetIndexWs", #"CatAreaSqKmRp100", "WsAreaSqKmRp100",
  "urb", "urbRip", "forr", "forrRip", "ag", "agRip", 
  "wetlands", "wetlandsRip", "urbWs", "urbRipWs", "forrWs", "forrWsRip", 
  "agWs", "agRipWs", "wetlandsWs", "wetlandsRipWS")

WaterPreds<-c("waterinchannel_score", "springs_score", "pctsurfaceflow","pctsubsurfaceflow","numberpools","SoilMoist_MeanScore", "SoilMoist_MaxScore")

HydroPreds_nowater<-HydroPreds %>%  setdiff(WaterPreds)


RejectPreds<-#rejected based on BPJ, etc.
  c("SoilMoist_MeanScore", #We will use MaxScore instead
    # "PctShading", #Really important, but do we trust it?
    "erosion_score","floodplain_score","ripariancorr_score",#PNW ancillaries not intended for this type of use
    "CatAreaSqKmRp100", "WsAreaSqKmRp100",#Streamcat metadata
    #Invariant preds:
    c("PctCoastCrsCat", "PctCoastCrsWs", "PctGlacLakeCrsCat", 
      "PctGlacLakeCrsWs", "PctGlacLakeFineCat", "PctGlacLakeFineWs", 
      "PctGlacTilClayCat", "PctGlacTilClayWs", "PctGlacTilLoamCat", 
      "PctGlacTilLoamWs", "PctHydricCat", "PctHydricWs", "PctSalLakeCat", 
      "PctSalLakeWs", "PctWaterCat",
      #Constrained StreamCat that cannot be calculated for new sites
      "MAST_2008","MAST_2009","MAST_2013","MAST_2014",
      "MSST_2008","MSST_2009","MSST_2013","MSST_2014",
      "MWST_2008","MWST_2009","MWST_2013","MWST_2014",
      #We thought these metrics were misleading in unrelativized form
      "GOLD_abundance", "GOLD_taxa", "OCH_abundance",   "OCH_taxa",
      #These became invariant with slightly different data set. They are nearly invariant so I'm still ruling out
      "PctColluvSedCat", "PctColluvSedWs", "PctExtruVolCat", "PctGlacTilCrsCat", 
      "PctGlacTilCrsWs", "PctWaterWs",
      #Not really sure how to transform this
      "numberpools"
    ),
    NULL)

#How many preds to I have overall?

BioPreds<-setdiff(BioPreds,RejectPreds)
GeomorphPreds<-setdiff(GeomorphPreds, RejectPreds)
HydroPreds<-setdiff(HydroPreds, RejectPreds)
HydroPreds_nowater<-setdiff(HydroPreds_nowater, RejectPreds)
GISPreds<-setdiff(GISPreds, RejectPreds)
StreamCatPreds<-setdiff(StreamCatPreds, RejectPreds) %>%
  c(GISPreds)


allpredz<-c(BioPreds,HydroPreds,GeomorphPreds, StreamCatPreds) %>% setdiff(RejectPreds)



invariantPreds<-mydf2 %>% 
  select(globalid, all_of(allpredz)) %>%
  # na.omit() %>%
  pivot_longer(cols=all_of(allpredz)) %>%
  group_by(name) %>%
  summarise(sd=sd(value, na.rm=T)) %>%
  filter(sd==0) %>%
  select(name) 
length(invariantPreds$name)
setdiff(invariantPreds$name, RejectPreds)
#Are they all represented in the data?
setdiff(c(BioPreds,HydroPreds, GeomorphPreds, GISPreds, StreamCatPreds), names(mydf2))
glimpse(mydf2 %>% select(allpredz))
# skimr::skim(mydf2 %>% select(allpredz))
#Finally, I need to make the y a factor for random forest
mydf2$DetFinal.f<-factor(mydf2$Determination_Final, levels=c("E","I","P")) 
mydf2_revisits<-mydf2 %>%   filter(Visit_No>1)
mydf2<-mydf2 %>%   filter(Visit_No==1 &
                            !is.na(DetFinal.f))
######
#Ordination to provide general overivew

library(vegan)
ordplot.df<-mydf2 %>%
  mutate(WetEphemeral = DetFinal.f=="E" & wet) %>%
  filter(!is.na(DetFinal.f) &           Region=="ASW")  



dist_mat<-ordplot.df %>%
  select(all_of(c(BioPreds,HydroPreds, GeomorphPreds))) %>%
  # skim_without_charts()
  # na.omit() %>%
  as.matrix() %>%
  vegdist(method="gower")

preds_NMDS<-metaMDS(dist_mat, k=2, autotransform = F, wascores=T)
preds_NMDS.df<-bind_cols(ordplot.df, 
                         preds_NMDS$points %>% as.data.frame())

preds_NMDS_varscores<-data.frame(Predictor=c(BioPreds,HydroPreds, GeomorphPreds, GISPreds, StreamCatPreds)) 

preds_NMDS_varscores$Rho1<-sapply(preds_NMDS_varscores$Predictor, function(pred){
  x=preds_NMDS.df %>% select(all_of(pred))
  y=preds_NMDS.df %>% select(MDS1)
  cor(x,y, method="spearman", use="pairwise.complete")
}) 

preds_NMDS_varscores$Rho2<-sapply(preds_NMDS_varscores$Predictor, function(pred){
  x=preds_NMDS.df %>% select(all_of(pred))
  y=preds_NMDS.df %>% select(MDS2)
  cor(x,y, method="spearman", use="pairwise.complete")
}) 


preds_NMDS_varscores$Selected<-  pmax(abs(preds_NMDS_varscores$Rho1), abs(preds_NMDS_varscores$Rho2) )> sqrt(.5)
preds_NMDS_varscores2<-preds_NMDS_varscores %>%
  mutate(Group=case_when(Predictor %in% BioPreds ~ "Biological",
                         Predictor %in% GeomorphPreds ~ "Geomorphological",
                         Predictor %in% WaterPreds ~ "Hydrological",
                         Predictor %in% HydroPreds_nowater ~ "Hydrological",
                         Predictor %in% GISPreds ~ "GIS",
                         Predictor %in% StreamCatPreds ~ "StreamCat",
                         T~"error"),
         Group2=case_when(Group %in% c("GIS","StreamCat")~"Geospatial",
                                 T~Group))

preds_NMDS_varscores2 %>%
  group_by(Group2) %>%
  top_n(-Rho2, n=2)

sel_mets<-c("TotalAbundance", "hydrophytes_present_noflag", "algdead_cover_score","vert_score",
            "hydric_score","pctsurfaceflow","springs_score","valleyslope",
            "riffpoolseq_score","substratesorting_score","BankWidthMean",
            "Precip8110Ws","WetIndexWs")
sel_mets %in% preds_NMDS_varscores2$Predictor
# overall
flow_hull_all<-preds_NMDS.df %>%
  group_by(DetFinal.f) %>%
  slice(chull(MDS1, MDS2))

ord_overall_plot<-ggplot(data=preds_NMDS.df,
                         aes(x=MDS1, y=MDS2))+
  geom_point(aes(color=DetFinal.f, shape=wet))+
  geom_polygon(data=flow_hull_all, aes(fill=DetFinal.f))+
  scale_color_brewer(palette="Set1", name="", label=c("Eph","Int", "Per"))+
  scale_shape_discrete(name="", labels=c("Dry","Flowing"))+
  scale_fill_manual(values=c("#e41a1c50","#377eb850","#4daf4a50"), name="", label=c("Eph","Int", "Per"))+
  theme_bw()
ord_overall_plot
ggsave(ord_overall_plot, filename="Figures/ord_overall_plot.jpg", dpi=300, height=4, width=6)



ordplot2<- ggplot(data=preds_NMDS.df, aes(x=MDS1, y=MDS2))+
  geom_segment(data=preds_NMDS_varscores2 %>%filter(Selected), 
               aes(x=Rho1, y=Rho2, color=Group), xend=0, yend=0, size=1)+
  theme_bw()+
  coord_cartesian(xlim=c(-1,.1), ylim=c(-1,1))+
  scale_color_brewer(palette="Set2", name="")+
  xlab("Rho with MDS1")+ylab("Rho with MDS2")+
  theme(legend.position = "bottom")

sel_var_labels<-preds_NMDS_varscores2 %>% filter(Predictor %in% sel_mets) %>%
  mutate(Lab=LETTERS[1:length(Rho1)])


ordplot2_alt<-ggplot(data=preds_NMDS_varscores2)+
  
  geom_segment(data=preds_NMDS_varscores2, 
               aes(x=Rho1, y=Rho2), xend=0, yend=0)+
  geom_segment(data=preds_NMDS_varscores2 %>% filter(Rho1^2>.5 | Rho2^2>0.5),  aes(x=Rho1, y=Rho2), xend=0, yend=0, color="#1f78b4")+
  theme_bw()+
  geom_hline(yintercept=0, linetype="dotted", color="gray")+geom_vline(xintercept=0, linetype="dotted", color="gray")+
  facet_wrap(~Group2)+
  # coord_cartesian(xlim=c(-1,.1), ylim=c(-1,1))+
  # scale_color_brewer(palette="Set2", name="")+
  xlab("Rho with MDS1")+ylab("Rho with MDS2")+
  theme(panel.grid=element_blank())+
  scale_x_continuous(breaks=seq(-1,1, by=.5))+
  scale_y_continuous(breaks=seq(-1,1, by=.5))+
  geom_point(data=sel_var_labels, aes(x=Rho1, y=Rho2), shape=16)+
  geom_text(data=sel_var_labels, aes(x=Rho1*1.14, y=Rho2*1.14, label=Lab), size=2)+
  coord_cartesian(xlim=c(-1,.5), ylim=c(-.6,.6))


library(ggpubr)
ordplot1<- ord_overall_plot+  theme(legend.box="vertical", legend.position = "bottom")+
  guides(color=guide_legend(order=1),fill=guide_legend(order=1),
         shape=guide_legend(order=2))
two_ordplots<-ggarrange(ordplot1 ,
                        # guides(col=guide_legend(nrow=2, byrow = T)),
                        ordplot2_alt, labels=c("A","B"), nrow=1)
two_ordplots
ggsave(two_ordplots, filename="Figures/two_ordplots.jpg", dpi=300, height=4, width=6.5)


# by stratum
flow_hull_strat<-preds_NMDS.df %>%
  group_by(Stratum, DetFinal.f) %>%
  slice(chull(MDS1, MDS2))

ord_stratum_plot<-ggplot(data=preds_NMDS.df,
                         aes(x=MDS1, y=MDS2))+
  geom_point(aes(color=DetFinal.f, shape=wet))+
  geom_polygon(data=flow_hull_strat, aes(fill=DetFinal.f))+
  scale_color_brewer(palette="Set1")+
  scale_shape_discrete(name="Was water present?")+
  scale_fill_manual(values=c("#e41a1c50","#377eb850","#4daf4a50"))+
  facet_wrap(~Stratum)+
  theme_bw()
ord_stratum_plot
ggsave(ord_stratum_plot, filename="Figures/ord_stratum_plot.jpg", dpi=300, height=4, width=8)

#######
###Predictor summaries
#Predictor screening

predictor_summary<-data.frame(Predictor = c(BioPreds, GeomorphPreds, HydroPreds, GISPreds, StreamCatPreds)) %>%
  mutate(PredGroup = case_when(Predictor %in% BioPreds ~ "Biological",
                               Predictor %in% GeomorphPreds ~ "Geomorphological",
                               Predictor %in% WaterPreds ~ "Hydrological-Direct",
                               Predictor %in% HydroPreds_nowater ~ "Hydrological-Indirect",
                               Predictor %in% GISPreds ~ "GIS",
                               Predictor %in% StreamCatPreds ~ "StreamCat",
                               T~"error"),
         DirectWater = case_when(Predictor %in% WaterPreds ~ T, T~F),
         MetricType = case_when(Predictor %in% c("fishabund_score2", "bmiabund_score", "algabund_score", "vegdiff_score", 
                                                 "rootedplants_score", "vert_sumscore", "vertvoc_sumscore",      
                                                 "alglive_cover_score", "algdead_cover_score", "algdead_noupstream_cover_score", 
                                                 "alglivedead_cover_score",
                                                 "moss_cover_score", "liverwort_cover_score", 
                                                 "sinuosity_score", "floodplaindim_score", "riffpoolseq_score", 
                                                 "substratesorting_score", "seddep_score","alglivedead_cover_score",
                                                 "hydrophytes_present", "hydrophytes_present_noflag",
                                                 "waterinchannel_score",  "SoilMoist_MeanScore", "SoilMoist_MaxScore") ~ "Ordinal",
                                Predictor %in% c("mayfly_abundance", "perennial_abundance", 
                                                 "perennial_taxa", "perennial_live_abundance",
                                                 "PctShading", "TotalAbundance", "Richness", "EPT_abundance", "EPT_taxa", "GOLD_abundance", 
                                                 "GOLD_taxa", "OCH_abundance", "OCH_taxa", "Noninsect_abundance", 
                                                 "Noninsect_taxa", "Noninsect_relabund", "Noninsect_reltaxa",
                                                 "BankWidthMean", "valleyslope",
                                                 "pctsurfaceflow",
                                                 "tmean", "tmax", "tmin", "ppt", "ppt.m01", "ppt.m02", 
                                                 "ppt.m03", "ppt.m04", "ppt.m05", "ppt.m06", "ppt.m07", "ppt.m08", 
                                                 "ppt.m09", "ppt.m10", "ppt.m11", "ppt.m12",
                                                 "WsAreaSqKm", "urb", "urbRip", "forr", "forrRip", "ag", "agRip", 
                                                 "wetlands", "wetlandsRip", "urbWs", "urbRipWs", "forrWs", "forrWsRip", 
                                                 "agWs", "agRipWs", "wetlandsWs", "wetlandsRipWS", StreamCatPreds,
                                                 "EPT_abundance", "EPT_taxa", "EPT_relabd","EPT_reltaxa",
                                                 "GOLD_abundance", "GOLD_taxa", "OCH_abundance",   "OCH_taxa",
                                                 "GOLD_relabd", "GOLD_reltaxa", "OCH_relabd","OCH_reltaxa", "GOLDOCH_relabd","GOLDOCH_reltaxa",  
                                                 "Noninsect_abundance", "Noninsect_taxa", "Noninsect_relabund",   "Noninsect_reltaxa"
                                ) ~ "Continuous",
                                Predictor %in% c("hydrophytes_present_any", "hydrophytes_present_any_noflag",
                                                 "iofb_score", "snake_score", 
                                                 "turt_score", "frogvoc_score", "vert_score", "vertvoc_score",
                                                 "floodplain_score","erosion_score","ripariancorr_score",
                                                 "hydric_score","springs_score","pctsubsurfaceflow", "numberpools", 
                                                 "numberwoodyjams",
                                                 "EcoII") ~ "Binary",
                                Predictor %in% c("EcoIII")~"Categorical",
                                T~"errorXXXXXXXXXXXXXX")
  ) %>%
  unique()


predictor_summary$PctZero<-sapply(predictor_summary$Predictor, function(x){
  xdf<-mydf2 %>%
    select(met=all_of(x)) %>%
    na.omit()
  sum(xdf$met==0)/length(xdf$met)
})

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

predictor_summary$PctDom<-sapply(predictor_summary$Predictor, function(x){
  xdf<-mydf2 %>%
    select(met=all_of(x)) %>%
    na.omit()
  mode.i<-getmode(xdf$met)
  sum(xdf$met==mode.i)/length(xdf$met)
})

sum(predictor_summary$PctDom<0.75)

ggplot(data=predictor_summary,aes(y=PctDom, x=PctZero))+
  geom_point(aes(color = PctZero>.67 | PctDom>0.67)) + 
  geom_text_repel(data=predictor_summary %>% filter(PctDom!=PctZero & PctDom>0.67), aes(label=Predictor))

predictor_summary$Range<-sapply(predictor_summary$Predictor, function(x){
  xdf<-mydf2 %>%
    select(met=all_of(x)) %>%
    na.omit()
  max(xdf$met) - min(xdf$met)
})

predictor_summary$PvIvE_F<-sapply(predictor_summary$Predictor, function(x){
  xdf<-mydf2 %>%
    select(met=all_of(x), DetFinal.f) %>%
    na.omit()
  myaov<-aov(met~DetFinal.f, data=xdf) %>%
    summary()
  mystat<-myaov[[1]][1,4]
  ifelse(is.na(mystat),0,mystat)
})

predictor_summary$EvNE_t_abs<-sapply(predictor_summary$Predictor, function(x){
  xdf<-mydf2 %>%
    select(met=all_of(x), DetFinal.f) %>%
    na.omit() %>%
    mutate(EvNE=case_when(DetFinal.f =="E"~"E",T~"NE"))
  myttest<-t.test(met~EvNE, data=xdf) 
  mystat<-myttest$statistic %>% abs()
  ifelse(is.na(mystat),0,mystat)
})

predictor_summary$PvNP_t_abs<-sapply(predictor_summary$Predictor, function(x){
  xdf<-mydf2 %>%
    select(met=all_of(x), DetFinal.f) %>%
    na.omit() %>%
    mutate(EvNE=case_when(DetFinal.f =="P"~"P",T~"NP"))
  myttest<-t.test(met~EvNE, data=xdf) 
  mystat<-myttest$statistic %>% abs()
  ifelse(is.na(mystat),0,mystat)
})
# 

predictor_summary$PvIwet_t_abs<-sapply(predictor_summary$Predictor, function(x){
  # print(x)
  xdf<-mydf2 %>%
    filter(wet) %>%
    select(met=all_of(x), DetFinal.f) %>%
    na.omit() %>%
    mutate(EvNE=case_when(DetFinal.f =="P"~"P",T~"NP"))
  myttest<-try(t.test(met~EvNE, data=xdf))
  if(class(myttest)=="try-error")
    mystat<-0
  else
    mystat<-myttest$statistic %>% abs()
  ifelse(is.na(mystat),0,mystat)
})

predictor_summary$EvIdry_t_abs<-sapply(predictor_summary$Predictor, function(x){
  # print(x)
  xdf<-mydf2 %>%
    filter(!wet) %>%
    select(met=all_of(x), DetFinal.f) %>%
    na.omit() %>%
    mutate(EvNE=case_when(DetFinal.f =="E"~"E",T~"NE"))
  myttest<-try(t.test(met~EvNE, data=xdf))
  if(class(myttest)=="try-error")
    mystat<-0
  else
    mystat<-myttest$statistic %>% abs()
  ifelse(is.na(mystat),0,mystat)
})

ggplot(data=predictor_summary, aes(x=PvNP_t_abs, y=EvNE_t_abs))+
  geom_point(aes(color=PredGroup))+
  geom_point(data=predictor_summary %>% filter(Predictor=="hydrophytes_present_noflag"), aes(color=PredGroup), size=5)+
  geom_abline(slope=1, linetype="dotted")+
  scale_color_brewer(palette="Set1")

ggplot(data=predictor_summary, aes(x=PvIwet_t_abs, y=EvIdry_t_abs))+
  geom_point(aes(color=PredGroup))+
  geom_point(data=predictor_summary %>% filter(Predictor=="hydrophytes_present_noflag"), aes(color=PredGroup), size=5)+
  geom_abline(slope=1, linetype="dotted")+
  geom_label_repel(data=predictor_summary %>% filter(PvIwet_t_abs>2 &EvIdry_t_abs>2), aes(label=Predictor)) +
  scale_color_brewer(palette="Set1")

ggplot(data=mydf2, aes(x=DetFinal.f , y=hydrophytes_present_noflag))+
  geom_boxplot(outlier.colour = NA)+ geom_point(position=position_jitter(height=0, width=.2))



# # # # ####USE GGFORCE TO MAKE A PDF OF ALL PREDICTOR BOXPLOTS
# library(ggforce)
# all_box_df<-mydf2 %>%
#   select(DetFinal.f, all_of(allpredz), -EcoII, -EcoIII) %>%
#   # pivot_longer(cols=2:154, names_to = "Predictor", values_to = "PredResult") %>%
#   pivot_longer(cols=c(-DetFinal.f), names_to = "Predictor", values_to = "PredResult") %>%
#   left_join(predictor_summary) %>%
#   arrange(PredGroup, Predictor) %>%
#   mutate(Predictor=factor(Predictor, levels=unique(Predictor)))
# #It works! but very slowly
# # library(ggforce)
# all_box_plot<-ggplot(data=all_box_df, aes(x=DetFinal.f, y=PredResult))+
#   geom_boxplot(aes(color=PredGroup))
# pdf("Figures/boxplots/preds_box2.pdf")
# for(i in 1:13){
#   print(all_box_plot +
#           facet_wrap_paginate(~Predictor, nrow=4, ncol=3, page=i, scales="free_y"))
#   }
# dev.off()

mydf2 %>%
  filter(hydrophytes_present_noflag>1 & 
           DetFinal.f=="E") %>%
  select(SITECODE, sitecode_entry, collection_date, hydrophytes_present_noflag)
# 

#Global RF model
rf_dat<-mydf2 %>%
  filter(Region=="ASW") %>%
  select(DetFinal.f, all_of(allpredz)) %>%
  na.omit()

set.seed(12)
rf_global<-randomForest(y=rf_dat$DetFinal.f,
                        x=rf_dat[,allpredz],
                        ntree=1000, importance=T, proximity=T)

rf_global_importance<-  rf_global$importance %>%
  as.data.frame() %>%
  mutate(Predictor=row.names(rf_global$importance))
varImpPlot(rf_global)


predictor_summary<-inner_join(predictor_summary, 
                              rf_global_importance %>% select(Predictor, rf_MDA=MeanDecreaseAccuracy, rf_MDG=MeanDecreaseGini)) 

predictor_summary$rf_MDA_rank<-rank(predictor_summary$rf_MDA, ties.method = "average")

library(corrr)
predictor_redundancy<-correlate(mydf2 %>% select(setdiff(allpredz, RejectPreds)),
                                method="spearman", diagonal=1,
                                use="pairwise.complete.obs") %>%
  reshape2::melt()
write.csv(predictor_redundancy, file="Output/predictor_redundancy.csv", row.names = F)

####
predictor_summary %>% filter(PctDom<.95) %>% nrow() #145
predictor_summary %>% filter(PvIvE_F > 2) %>% nrow() #86
predictor_summary %>% filter(EvNE_t_abs > 2) %>% nrow() #87
predictor_summary %>% filter(EvIdry_t_abs > 2) %>% nrow() #36
predictor_summary %>% filter(PvIwet_t_abs > 2) %>% nrow() #18
predictor_summary %>% filter(rf_MDA_rank>=(nrow(predictor_summary)*.75)) %>% nrow() #39

predictor_summary<-predictor_summary %>%
  mutate(Screened = 
           case_when(PctDom<.95 & #Must have sufficient variability of values
                       #And also meet at least one responsiveness criterion
                       (PvIvE_F > 2 |
                          EvNE_t_abs > 2 |
                          PvNP_t_abs > 2|
                          EvIdry_t_abs >2|
                          PvIwet_t_abs >2|
                          rf_MDA_rank>=(nrow(predictor_summary)*.75)
                       ) ~"Pass",
                     T~"Fail"))

predictor_summary %>% filter(Screened=="Pass") %>% nrow()

ggplot(data=predictor_summary, aes(x=PvIvE_F, y=EvIdry_t_abs))+
  geom_point(aes(color=PredGroup ))+
  geom_point(data=predictor_summary %>% filter(Predictor=="hydrophytes_present_noflag"), aes(color=PredGroup ), size=5)+
  # geom_label_repel(data=predictor_summary %>% filter(EvIdry_t_abs>2), aes(label=Predictor))+
  theme_bw()


plot_dat_responsiveness <-predictor_summary %>%
  pivot_longer(cols=c( PvIvE_F,EvNE_t_abs,PvNP_t_abs,  #rf_MDG,
                       EvIdry_t_abs, PvIwet_t_abs)) %>%
  arrange(PredGroup,value) %>%
  mutate(Predictor=factor(Predictor, levels=Predictor %>% unique()),
         name=factor(name, levels=c("PvIvE_F","EvNE_t_abs","PvNP_t_abs","PvIwet_t_abs","EvIdry_t_abs"))) 

levels(plot_dat_responsiveness$name)<-c("PvIvE","EvNE","PvNP","PvIwet","EvIdry")

individual_pred_summary_plot<-ggplot(data=plot_dat_responsiveness %>% 
                                       filter(Screened =="Pass"),
                                     aes(x=Predictor, y=value))+
  geom_point(aes(color=PredGroup))+
  facet_wrap(~name, scales="free_x", nrow=1)+
  scale_color_brewer(palette="Set2", name="Metric type")+
  xlab("")+ylab("")+
  geom_hline(yintercept=2, linetype="dashed")+
  coord_flip()+
  theme_bw()+
  # guides(color = guide_legend(nrow=2))+
  scale_size_manual(values=c(0.75,2))+
  theme(#legend.position = "bottom",
        axis.text.y = element_text(size=7),
        panel.grid=element_blank(),
        legend.text = element_text(size=12))
individual_pred_summary_plot

ggsave(individual_pred_summary_plot, filename="Figures/individual_pred_summary_plot.jpg", dpi=300,
       height=7, width=10)

plot_dat_responsiveness %>%
  arrange(name,-value) %>%
  # filter(name!="RFE_optimal")%>%
  group_by(name) %>%
  select(-Screened) %>%
  top_n(value, n=5) %>%
  write.table(file="clipboard", sep="\t", row.names=F)

predictor_summary %>%
  filter(Predictor=="pctsurfaceflow")

#########

#Alternate screened variables

screened_varz_all<-predictor_summary$Predictor[which(predictor_summary$Screened=="Pass")]
# screened_varz_noSC<-setdiff(screened_varz_all, StreamCatPreds)
screened_varz_noSC<-setdiff(screened_varz_all, c(GISPreds, StreamCatPreds)) #Treaing GIS like StreamCat
screened_varz_noH2O<-setdiff(screened_varz_all, WaterPreds)
screened_varz_noSC_noH2O<-intersect(screened_varz_noSC, screened_varz_noH2O)
intersect(screened_varz_all, StreamCatPreds)

#########
#Guide to all models
# mod_summary is for the 48 submodels (each gets a unique rf object)
mod_summary<-crossing(#Regions = c("ASW","ASW_WM"),
  Regions = c("ASW"),                    
  Strata=c("All", as.character(asw_strata)),
  IncludeWaterPreds=c(T,F),
  IncludeStreamCatPreds=c(T,F)
  # IncludeStreamCatPreds=c(F)
) %>%
  mutate(Index=row_number(),
         Stratification=case_when(Strata=="All"~"Unstratified",
                                  T~"Stratified"),
         getPredz = case_when(IncludeWaterPreds & IncludeStreamCatPreds~"screened_varz_all",
                              IncludeWaterPreds & !IncludeStreamCatPreds~"screened_varz_noSC",
                              !IncludeWaterPreds & IncludeStreamCatPreds~"screened_varz_noH2O",
                              !IncludeWaterPreds & !IncludeStreamCatPreds~"screened_varz_noSC_noH2O",
                              T~"xxxx"))

mod_summary$ModName<-sapply(1:nrow(mod_summary), function(i){
  reg.i<-mod_summary$Regions[i]
  # reg.j<-case_when(reg.i=="ASW"~"ASW",T~"West")
  reg.j<-case_when(reg.i=="ASW"~"A",T~"W")
  strat.i<-mod_summary$Strata[i]
  # strat.j<-case_when(strat.i=="NM-TX"~"NM",
  #                    strat.i=="CO-WY-UT-MT"~"NCR",
  #                    T~strat.i)
  strat.j<-case_when(strat.i=="All"~"A",
                     strat.i=="AZ"~"Z",
                     strat.i=="CA"~"C",
                     strat.i=="CO-WY-UT-MT"~"M",
                     strat.i=="NM-TX"~"N",
                     strat.i=="NV"~"V",T~"X")
  wat.i<-mod_summary$IncludeWaterPreds[i]
  wat.j<-case_when(wat.i~"W",T~"X")
  cat.i<-mod_summary$IncludeStreamCatPreds[i]
  cat.j<-case_when(cat.i~"S",T~"T")
  paste(reg.j, strat.j, wat.j, cat.j, sep="")
})

mod_summary<-mod_summary %>%
  mutate(ModName2 = case_when(ModName %in% c("PNW_NoFlag","NM")~ModName,
                              Strata=="All" & IncludeWaterPreds & IncludeStreamCatPreds ~ "Unstrat H2O SC",
                              Strata=="All" & IncludeWaterPreds & !IncludeStreamCatPreds ~ "Unstrat H2O",
                              Strata=="All" & !IncludeWaterPreds & IncludeStreamCatPreds ~ "Unstrat SC",
                              Strata=="All" & !IncludeWaterPreds & !IncludeStreamCatPreds ~ "Unstrat",
                              Strata!="All" & IncludeWaterPreds & IncludeStreamCatPreds ~ "Strat H2O SC",
                              Strata!="All" & IncludeWaterPreds & !IncludeStreamCatPreds ~ "Strat H2O",
                              Strata!="All" & !IncludeWaterPreds & IncludeStreamCatPreds ~ "Strat SC",
                              Strata!="All" & !IncludeWaterPreds & !IncludeStreamCatPreds ~ "Strat",
                              T~"XXX" ))

mod_dats<-lapply(1:nrow(mod_summary), function(i){
  reg.i<-mod_summary$Regions[i]
  strat.i<-mod_summary$Strata[i] 
  if(strat.i=="All")
    asw_strata
  else
    strat.i
  predz.i<-mod_summary$getPredz[i]%>% get()
  print(predz.i)
  xdf<-  mydf2 %>%
    filter(Region==reg.i) %>%
    select(globalid, Stratum_ASW, DetFinal.f, wet, all_of(predz.i)) %>%
    na.omit() %>%
    mutate(split_strat=paste(Stratum_ASW, DetFinal.f))
  xdf
})
# 
# mod_dats_split[[1]] %>%
#   testing() %>%
#   group_by(split_strat, DetFinal.f, Stratum_ASW) %>%
#   tally()
###
#Single-indicator data

mydf2$SingleInd_PNW<-case_when((mydf2$fishabund_score2 + mydf2$amphib_score + mydf2$snake_score)>0~1,T~0)
# mydf2$SingleInd_PNW<-case_when((mydf2$fishabund_score2 )>0~1,T~0) #Trying fish alone. I'm keeping the name to facilitate coding
mydf2$SingleInd_NM<-case_when((mydf2$fishabund_score2 + mydf2$bmiabund_score)>0~1,T~0)

mydf2$SingleInd_fish<-case_when((mydf2$fishabund_score2)>0~1,T~0)
mydf2$SingleInd_bmi<-case_when((mydf2$bmiabund_score)>0~1,T~0)
mydf2$SingleInd_alg<-case_when((mydf2$algabund_score)>0~1,T~0)
mydf2$SingleInd_iofb<-case_when((mydf2$iofb_score)>0~1,T~0)
mydf2$SingleInd_hydric<-case_when((mydf2$hydric_score)>0~1,T~0)
mydf2$SingleInd_amphib<-case_when((mydf2$amphib_score)>0~1,T~0)
mydf2$SingleInd_snake<-case_when((mydf2$snake_score)>0~1,T~0)
mydf2$SingleInd_verts<-case_when((mydf2$vert_score)>0~1,T~0)



singind.plot.df<-mydf2 %>% 
  select(wet, DetFinal.f, fishabund_score2, amphib_score, snake_score, vert_score, bmiabund_score, hydrophytes_present) %>%
  pivot_longer(cols =c(-wet, -DetFinal.f))

ggplot(data=singind.plot.df, aes(x=DetFinal.f, y=value))+
  geom_boxplot(aes(fill=wet))+
  facet_wrap(~name, scales="free")

# mydf2 %>%
#   filter(DetFinal.f=="E"  & bmiabund_score>0) %>%
#   select(SITECODE,sitename, SiteID, sitecode_entry, wet, waterinchannel_score, bmiabund_score,TotalAbundance, source, globalid) %>%
#   arrange(waterinchannel_score) %>% 
#   write.table(file="clipboard", sep="\t", row.names = F)
# 
# mydf2 %>%
#   filter(DetFinal.f=="E" & !wet & bmiabund_score>0) %>%
#   select(SITECODE, wet, bmiabund_score,  waterinchannel_notes, source, sitecode_entry, sitename) %>%
#   arrange()
# 
# mydf_ai %>%  filter(globalid=="{4CC21818-81BA-42D2-B873-FA822B1F1CD7}")

#######

set.seed(11111)
mod_dats_split<-lapply(mod_dats, function(x){  initial_split(x, prop=4/5, strata=split_strat) })

#This didn't really go anywhere....
my_rfs_total<-lapply(1:nrow(mod_summary), function(i){
  mydat<-mod_dats[[i]] %>%
    select(-globalid, -wet, -split_strat, -Stratum_ASW )
  
  set.seed(20+i)
  randomForest(DetFinal.f~., data=mydat,
               ntree=1500, importance=T, proximity=T)
})
#This didn't really go anywhere....
my_rfs_training<-lapply(1:nrow(mod_summary), function(i){
  mydat<-mod_dats_split[[i]] %>%
    training() %>%
    select(-globalid, -wet, -split_strat, -Stratum_ASW )
  set.seed(20+i)
  randomForest(DetFinal.f~., data=mydat,
               ntree=1500, importance=T, proximity=T)
})


#Dump negative importance, and then select the top x most important
#Again, this didn't really end up being used
my_rfs_important_predictors<-lapply(1:nrow(mod_summary), function(i){
  myrf<-my_rfs_total[[i]]
  myrf_imp<-myrf$importance %>%
    as.data.frame() %>%
    mutate(Predictor=row.names(myrf$importance)) %>%
    filter(MeanDecreaseAccuracy >0) %>%
    top_n(n=10, wt=MeanDecreaseAccuracy)
  myrf_imp$Predictor
})


library(caret)
my_ctrl <- rfeControl(functions = rfFuncs, method = "cv",verbose = FALSE, returnResamp = "all")

maxsize_x<-21
my_rfes_total<-lapply(1:nrow(mod_summary), function(i){
  mydat<-mod_dats[[i]] %>%
    select(-globalid, -wet,-split_strat, -Stratum_ASW )
  predz.i<-mod_summary$getPredz[i]%>% get() %>% length()
  set.seed(200+i)
  rfe(DetFinal.f~., 
      data=mydat,
      # sizes=c(1:maxsize_x),
      sizes=c(3:20, seq(from=25, to=predz.i, length.out = 5), predz.i) %>% unique() %>% round(),
      rfeControl = my_ctrl,
      metric="Kappa", #could do kappa
      maximize = T )
})

myrfe<-my_rfes_total[[1]]
plot(myrfe)

tolerance<- 1
maxsize_x<-20
pickSizeTolerance(my_rfes_total[[1]]$results, metric="Kappa", tol=tolerance, maximize = T)
my_rfes_optimal_preds<-lapply(1:nrow(mod_summary), function(i){
  myrfe<-my_rfes_total[[i]]
  mysize<-pickSizeTolerance(myrfe$results[1:20,], metric="Kappa", tol=tolerance, maximize = T)
  pickVars(myrfe$variables, size=min(mysize, maxsize_x))
})
mod_summary$n_optpredz<-sapply(my_rfes_optimal_preds, function(x) length(x))
mod_summary %>% filter(n_optpredz==1)

#PNW-like models
#This doesn't go anywhere
pnw_vars<-c("TotalAbundance", "mayfly_abundance", "perennial_taxa","hydrophytes_present_noflag","valleyslope")
pnw_dat<-mydf2 %>%
  filter(Region=="ASW") %>%
  select(globalid, Stratum_ASW, DetFinal.f, wet, all_of(pnw_vars)) %>%
  na.omit() %>%
  mutate(split_strat=paste(Stratum_ASW, DetFinal.f)) %>% 
  initial_split(strata=split_strat, prop=4/5)
pnw_dat_training<-pnw_dat %>% training()
pnw_dat_testing<-pnw_dat %>% testing()
set.seed(15)
pnw_rf_training<-randomForest(DetFinal.f~TotalAbundance+ mayfly_abundance+perennial_taxa+hydrophytes_present_noflag+valleyslope,
                              data=pnw_dat_training,
                              ntree=1500, importance=T, proximity=T)
varImpPlot(pnw_rf_training)
vip(pnw_rf_training, geom="point") + theme_bw()

my_pnw2.rf_prediction<-pnw_dat_training %>%
  select(globalid) %>%
  mutate(Prediction=predict(pnw_rf_training) %>% as.character(),
         ModName="PNW2.rf", ModName2="PNW2.rf",ModType="PNW2.rf",Set="Training") %>%
  bind_rows(
    pnw_dat_testing %>%
      select(globalid) %>%
      mutate(Prediction=predict(pnw_rf_training, newdata=pnw_dat_testing)%>% as.character(),
             ModName="PNW2.rf", ModName2="PNW2.rf",ModType="PNW2.rf", Set="Testing"))
my_pnw2.rf_prediction_revisits<-mydf2_revisits %>%
  select(globalid) %>%
  mutate(Prediction=predict(pnw_rf_training, newdata=mydf2_revisits)%>% as.character(),
         ModName="PNW2.rf", ModName2="PNW2.rf",ModType="PNW2.rf",Set="Revisit")


library(rpart)
library(rpartScore)
pnw_rpart_training<-rpart(DetFinal.f~TotalAbundance+ mayfly_abundance+perennial_taxa+hydrophytes_present_noflag+valleyslope,
                          data=pnw_dat_training, control = rpart.control())
fancyRpartPlot(pnw_rpart_training, type=0)
ggsave(fancyRpartPlot(pnw_rpart_training, type=1, sub=""),filename="Figures/Rpart_dendrogram.jpg", dpi=300, height=5,width=6)

mydf2 %>%
  filter(DetFinal.f=="I") %>%
  group_by(wet) %>%
  summarise(x=sum(TotalAbundance>0)/length(TotalAbundance))

summary(pnw_rpart_training)
my_pnw2.rp_prediction<-pnw_dat_training %>%
  select(globalid) %>%
  mutate(Prediction=predict(pnw_rpart_training, type="vector"),
         Prediction=case_when(Prediction==1~"E",Prediction==2~"I", Prediction==3~"P", T~"XXXX"),
         ModName="PNW2.rp", ModName2="PNW2.rp",ModType="PNW2.rp",Set="Training") %>%
  bind_rows(
    pnw_dat_testing %>%
      select(globalid) %>%
      mutate(Prediction=predict(pnw_rpart_training, type="vector", newdata=pnw_dat_testing),
             Prediction=case_when(Prediction==1~"E",Prediction==2~"I", Prediction==3~"P", T~"XXXX"),
             ModName="PNW2.rp", ModName2="PNW2.rp",ModType="PNW2.rp",Set="Testing"))
my_pnw2.rp_prediction_revisits<-mydf2_revisits %>%
  select(globalid) %>%
  mutate(Prediction=predict(pnw_rpart_training, type="vector", newdata=mydf2_revisits),
         Prediction=case_when(Prediction==1~"E",Prediction==2~"I", Prediction==3~"P", T~"XXXX"),
         ModName="PNW2.rp", ModName2="PNW2.rp",ModType="PNW2.rp",Set="Revisit")



# 
# 
# my_pnw_prediction_revisits<-mydf2_revisits %>%
#   select(globalid, Prediction=pnw.noflag_class_final) %>%
#   mutate(ModName="PNW",
#          ModName2="PNW",
#          ModType="PNW",
#          Set="Training")
# my_nm_prediction_revisits<-mydf2_revisits %>%
#   select(globalid, Prediction=nm_class_final) %>%
#   mutate(ModName="NM",
#          ModName2="NM",
#          ModType="NM",
#          Set="Training")

# pnw_rpartscore_training<-rpartScore(DetFinal.f~TotalAbundance+ mayfly_abundance+perennial_taxa+hydrophytes_present_noflag+valleyslope,
# data=pnw_dat_training)
# fancyRpartPlot(pnw_rpart_training)


#Random Forest models
my_rfs_optimized_training<-lapply(1:nrow(mod_summary), function(i){
  optvarz<-my_rfes_optimal_preds[[i]]
  mydat<-mod_dats_split[[i]] %>%
    training() %>%
    # select(-globalid, -wet)
    select(DetFinal.f, all_of(optvarz))
  set.seed(20+i)
  randomForest(DetFinal.f~., data=mydat,
               ntree=1500, importance=T, proximity=T)
})

final_rf_unrevised<-my_rfs_optimized_training[[1]]
save(final_rf_unrevised, file="Output/final_rf_unrevised_FINAL.Rdata")
save(mod_dats_split, file="Output/my_dats_split_FINAL.Rdata")

mod_pred_summary<-crossing(mod_summary, Predictor=c(screened_varz_all)) %>%
  mutate(REMOVE=case_when(!IncludeWaterPreds & Predictor %in% WaterPreds ~ "REMOVE",
                          !IncludeStreamCatPreds & Predictor %in% StreamCatPreds~ "REMOVE", T~"KEEP")) %>%
  filter(REMOVE!="REMOVE") %>% 
  select(-REMOVE)
mod_pred_summary$SelectedVar<-sapply(1:nrow(mod_pred_summary), function(i){
  mypred=mod_pred_summary$Predictor[i]
  modIndex.i=mod_pred_summary$Index[i]
  mod.i=my_rfs_optimized_training[[modIndex.i]]
  preds.i=mod.i %>% predictors()
  mypred %in% preds.i
})

mod_pred_summary_selection<-mod_pred_summary %>%
  inner_join(predictor_summary %>% select(Predictor, PredGroup)) %>%
  # filter(SelectedVar) %>%
  group_by(ModName2, PredGroup, Predictor) %>%
  summarise(SelectedVar2=sum(SelectedVar)>0) %>%
  inner_join(
    mod_pred_summary %>%
      group_by(Predictor) %>%
      summarise(TimesSelected=sum(SelectedVar))
  ) %>%
  filter(TimesSelected>0) %>%
  mutate(TimesEligible = case_when(PredGroup %in% c("Biological", "Geomorphological","Hydrological-Indirect")~24,
                                   PredGroup %in% c("Hydrological-Direct","GIS","StreamCat")~12),
         PercentSelected=TimesSelected/TimesEligible,
         ModNamePretty=str_replace_all(ModName2," H2O","\nH20") %>%
           str_replace_all(" SC","\nGIS"),
         PredGroupColor = case_when(PredGroup=="Biological"~"#66a61e",
                                    PredGroup=="Geomorphological"~"#a6761d",
                                    PredGroup %in% c("Hydrological-Direct","Hydrological-Indirect")~"#377eb8",
                                    PredGroup %in% c("StreamCat","GIS")~"#7570b3",
                                    T~"Chartreuse"),
         PredName2=paste0(Predictor, " (",TimesSelected,"/",TimesEligible,")")
                                 )
mod_pred_summary_selection_order<-mod_pred_summary_selection %>%
  ungroup() %>%
  select(PredGroup, Predictor, TimesSelected, TimesEligible, PercentSelected, PredGroupColor, PredName2) %>%
  unique() %>%
  arrange(PercentSelected)

mod_pred_summary_selection$PredName2<-factor(mod_pred_summary_selection$PredName2, levels=mod_pred_summary_selection_order$PredName2)

# mod_pred_summary_selection$PredGroup %>% unique()
mod_pred_summary_selection_plot<-ggplot(data=mod_pred_summary_selection,
       aes(x=ModNamePretty, y=PredName2))+
  geom_tile(aes(fill=SelectedVar2), color="white")+
  scale_fill_manual(values=c("gray25","gray80"), na.value="pink", name="",labels=c("Not selected", "Selected"))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "bottom"
        )+
  scale_x_discrete(name="")+
  scale_y_discrete(name="")
ggsave(mod_pred_summary_selection_plot, filename="Figures/mod_pred_summary_selection_plot.jpg", dpi=300, height=6, width=6)

  # rename("Strat_H20"=`Strat H2O`)
write.table(mod_pred_summary_selection, file="clipboard", sep="\t", row.names=F)
  



set.seed(21)
my_rfs_optimized_prediction<-lapply(1:nrow(mod_summary), function(i){
  modname.i<-mod_summary$ModName[i]
  modname2.i<-mod_summary$ModName2[i]
  myRF<-my_rfs_optimized_training[[i]]
  mydat_train<-mod_dats_split[[i]] %>%
    training() %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="RandomForest",
           Set="Training",
           Prediction=predict(myRF))
  mydat_test<-mod_dats_split[[i]] %>%
    testing() %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="RandomForest",
           Set="Testing")
  mydat_test$Prediction<-predict(myRF, newdata=mydat_test)
  bind_rows(mydat_train, mydat_test)%>%
    select(globalid, ModName, ModName2, ModType, Set, Prediction)
})

my_rfs_optimized_prediction_revisits<-lapply(1:nrow(mod_summary), function(i){
  modname.i<-mod_summary$ModName[i]
  modname2.i<-mod_summary$ModName2[i]
  myRF<-my_rfs_optimized_training[[i]]
  mydat_test<-mydf2_revisits %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="RandomForest",
           Set="Revisit")
  mydat_test$Prediction<-predict(myRF, newdata=mydat_test) 
  mydat_test %>%
    select(globalid, ModName, ModName2, ModType, Set, Prediction)
})



varImpPlot(my_rfs_optimized_training[[1]])
partialPlot(my_rfs_optimized_training[[1]], pred.data=mod_dats_split[[1]] %>% training(),
            x.var="EPT_taxa")
library(pdp)
library(vip)
vip(my_rfs_optimized_training[[3]], geom="point") + 
  theme_bw()+
  theme(axis.text.y = element_text(size=10))+
  ggtitle("Yes H2O")


vip(my_rfs_optimized_training[[1]], geom="point") + 
  theme_bw()+
  theme(axis.text.y = element_text(size=10))

FinalRF_VIP_FINAL_plotdat<-my_rfs_optimized_training[[1]]$importance %>%
  as.data.frame() %>%
  mutate(Variable=row.names(my_rfs_optimized_training[[1]]$importance)) %>%
  arrange(MeanDecreaseAccuracy)
FinalRF_VIP_FINAL_plotdat$Variable<-factor(FinalRF_VIP_FINAL_plotdat$Variable, levels=FinalRF_VIP_FINAL_plotdat$Variable)
FinalRF_VIP_FINAL<-
  ggplot(data=FinalRF_VIP_FINAL_plotdat, aes(x=Variable, y=MeanDecreaseAccuracy))+
  geom_point()+
  coord_flip()+
  xlab("")+ylab("Mean decrease in accuracy") +
  theme_bw()

vip()
ggsave(vip(my_rfs_optimized_training[[1]], geom="point") + 
         theme_bw()+
         theme(axis.text.y = element_text(size=10))
       ,       filename="Figures/FinalRF_VIP_FINAL.jpg", height=5, width=4)


# i<-1
# optvarz<-my_rfes_optimal_preds[[i]]
# mydat<-mod_dats_split[[i]] %>%
#   training() %>%
#   # select(-globalid, -wet)
#   select(DetFinal.f, all_of(optvarz))
# set.seed(20+i)
# test_rf<-randomForest(DetFinal.f~., data=mydat,
#              ntree=1500, importance=T, proximity=T)
# partial(test_rf, pred.var = c("hydrophytes_present_noflag","tmean"), plot = TRUE,
#         # pred.grid=mod_dats_split[[1]] %>% training(),
#         plot.engine = "ggplot2")
# rm(i, optvarz, mydat, test_rf)

# tree.i<-my_rfs_optimized_training[[1]] %>% getTree(1)


my_rfs_optimized_training[[4]] %>%
  getTree(k=3, labelVar=T)
# mds_plot_1<-mod_dats_split[[1]] %>% training() %>% select(DetFinal.f)
# MDSplot(my_rfs_optimized_training[[1]], mds_plot_1$DetFinal.f)
whichmod<-1
mds_plot_1<-cmdscale(1-my_rfs_optimized_training[[whichmod]]$proximity) %>%
  as.data.frame() %>%
  mutate(DetFinal.f=(mod_dats_split[[whichmod]] %>% training() )$DetFinal.f ) 
mds_plot_1_varz<-mod_dats_split[[whichmod]] %>% training() %>%
  bind_cols(mds_plot_1) %>%
  select(c(my_rfs_optimized_training[[whichmod]] %>% predictors(),V1, V2)) %>%
  pivot_longer(cols=my_rfs_optimized_training[[whichmod]] %>% predictors(), names_to = "Metric", values_to = "MetricResult") %>%
  group_by(Metric) %>%
  summarise(Rho1=cor(V1, MetricResult, method="spearman"),
            Rho2=cor(V2, MetricResult, method="spearman"))
ggplot(data=mds_plot_1, aes(x=V1, y=V2))+
  geom_segment(data=mds_plot_1_varz, aes(x=0, y=0, xend=Rho1, yend=Rho2), color="midnightblue")+
  geom_point(aes(color=DetFinal.f))+
  geom_label(data=mds_plot_1_varz, aes(x=Rho1, y=Rho2, label=Metric))+
  theme_bw()

# rattle::printRandomForests(my_rfs_optimized_training[[1]])
# rattle::ggVarImp(my_rfs_optimized_training[[1]])
# library(vip)
# vip(my_rfs_optimized_training[[1]])
# partial(my_rfs_optimized_training[[1]], 
#         pred.var="hydrophytes_present_noflag", plot=T, rug=T, plot.engine = "ggplot2",
#         pred.grid = mod_dats_split[[1]] %>% training())


varImpPlot((my_rfs_optimized_training[[1]]))
# rattle::fancyRpartPlot((my_rparts_optimized_training[[2]]))
# rattle::fancyRpartPlot((my_rpartScore_optimized_training[[2]]))
#rpart models
library(rpart)
my_rparts_optimized_training<-lapply(1:nrow(mod_summary), function(i){
  optvarz<-my_rfes_optimal_preds[[i]]
  mypreds<-mod_summary$getPredz[[i]] %>% get()
  mydat<-mod_dats_split[[i]] %>%
    training() %>%
    # select(-globalid, -wet)
    # select(DetFinal.f, all_of(optvarz))
    select(DetFinal.f, all_of(mypreds))
  set.seed(20+i)
  rpart(DetFinal.f~., data=mydat, control=rpart.control())
})


rattle::fancyRpartPlot(my_rparts_optimized_training[[1]])
vip(my_rfs_optimized_training[[1]], geom="point") + theme_bw()
rattle::fancyRpartPlot(my_rparts_optimized_training[[2]])
rattle::fancyRpartPlot(my_rparts_optimized_training[[3]])
vip(my_rfs_optimized_training[[3]], geom="point") + theme_bw()
rattle::fancyRpartPlot(my_rparts_optimized_training[[4]])
dev.off()

my_rparts_optimized_prediction<-lapply(1:nrow(mod_summary), function(i){
  modname.i<-mod_summary$ModName[i]
  modname2.i<-mod_summary$ModName2[i]
  myRF<-my_rparts_optimized_training[[i]]
  mydat_train<-mod_dats_split[[i]] %>%
    training() %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="Rpart",
           Set="Training",
           Prediction=predict(myRF,type="vector"))%>%
    mutate(Prediction=case_when(Prediction==1~"E",Prediction==2~"I", Prediction==3~"P", T~"XXXX"))
  mydat_test<-mod_dats_split[[i]] %>%
    testing() %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="Rpart",
           Set="Testing")
  mydat_test$Prediction<-predict(myRF, newdata=mydat_test, type="vector")
  mydat_test$Prediction<-case_when(mydat_test$Prediction==1~"E",mydat_test$Prediction==2~"I", mydat_test$Prediction==3~"P", T~"XXXX")
  bind_rows(mydat_train, mydat_test)%>%
    select(globalid, ModName, ModName2, ModType, Set, Prediction)
})

my_rparts_optimized_prediction_revisits<-lapply(1:nrow(mod_summary), function(i){
  modname.i<-mod_summary$ModName[i]
  modname2.i<-mod_summary$ModName2[i]
  myRF<-my_rparts_optimized_training[[i]]
  mydat_test<-mydf2_revisits %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="Rpart",
           Set="Revisit")
  mydat_test$Prediction<-predict(myRF, newdata=mydat_test, type="vector")
  mydat_test$Prediction<-case_when(mydat_test$Prediction==1~"E",mydat_test$Prediction==2~"I", mydat_test$Prediction==3~"P", T~"XXXX")
  mydat_test%>%
    select(globalid, ModName, ModName2, ModType, Set, Prediction)
})

library(rpartScore)

my_rpartScore_optimized_training<-lapply(1:nrow(mod_summary), function(i){
  optvarz<-my_rfes_optimal_preds[[i]]
  mypreds<-mod_summary$getPredz[[i]] %>% get()
  mydat<-mod_dats_split[[i]] %>%
    training() %>%
    # select(-globalid, -wet)
    # select(DetFinal.f, all_of(optvarz)) %>%
    select(DetFinal.f, all_of(mypreds)) %>%
    mutate(DetFinal.f=as.numeric(DetFinal.f))
  set.seed(20+i)
  rpartScore(DetFinal.f~., data=mydat)
})

my_rpartScore_optimized_prediction<-lapply(1:nrow(mod_summary), function(i){
  modname.i<-mod_summary$ModName[i]
  modname2.i<-mod_summary$ModName2[i]
  myRF<-my_rpartScore_optimized_training[[i]]
  mydat_train<-mod_dats_split[[i]] %>%
    training() %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="RpartScore",
           Set="Training",
           Prediction=predict(myRF,type="vector"))%>%
    mutate(Prediction=case_when(Prediction==1~"E",Prediction==2~"I", Prediction==3~"P", T~"XXXX"))
  mydat_test<-mod_dats_split[[i]] %>%
    testing() %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="RpartScore",
           Set="Testing")
  mydat_test$Prediction<-predict(myRF, newdata=mydat_test, type="vector")
  mydat_test$Prediction<-case_when(mydat_test$Prediction==1~"E",mydat_test$Prediction==2~"I", mydat_test$Prediction==3~"P", T~"XXXX")
  bind_rows(mydat_train, mydat_test) %>%
    select(globalid, ModName, ModName2, ModType, Set, Prediction)
})

my_rpartScore_optimized_prediction_revisits<-lapply(1:nrow(mod_summary), function(i){
  modname.i<-mod_summary$ModName[i]
  modname2.i<-mod_summary$ModName2[i]
  myRF<-my_rpartScore_optimized_training[[i]]
  mydat_test<-mydf2_revisits %>%
    mutate(ModName=modname.i,
           ModName2=modname2.i,
           ModType="RpartScore",
           Set="Revisit")
  mydat_test$Prediction<-predict(myRF, newdata=mydat_test, type="vector")
  mydat_test$Prediction<-case_when(mydat_test$Prediction==1~"E",mydat_test$Prediction==2~"I", mydat_test$Prediction==3~"P", T~"XXXX")
  mydat_test %>%
    select(globalid, ModName, ModName2, ModType, Set, Prediction)
})


rattle::fancyRpartPlot(my_rpartScore_optimized_training[[1]])
rattle::fancyRpartPlot(my_rpartScore_optimized_training[[2]])
rattle::fancyRpartPlot(my_rpartScore_optimized_training[[3]])
rattle::fancyRpartPlot(my_rpartScore_optimized_training[[4]])


#Assemble predictions

my_pnw_prediction<-mydf2 %>%
  select(globalid,  Prediction=pnw.noflag_class_final) %>%
  mutate(ModName="PNW",
         ModName2="PNW",
         ModType="PNW",
         Set="Training")
my_nm_prediction<-mydf2 %>%
  select(globalid,  Prediction=nm_class_final) %>%
  mutate(ModName="NM",
         ModName2="NM",
         ModType="NM",
         Set="Training")
my_pnw_prediction_revisits<-mydf2_revisits %>%
  select(globalid, Prediction=pnw.noflag_class_final) %>%
  mutate(ModName="PNW",
         ModName2="PNW",
         ModType="PNW",
         Set="Revisit")
my_nm_prediction_revisits<-mydf2_revisits %>%
  select(globalid, Prediction=nm_class_final) %>%
  mutate(ModName="NM",
         ModName2="NM",
         ModType="NM",
         Set="Revisit")


all_predictions<-mydf2 %>%
  select(globalid, SITECODE, DetFinal.f, Region, Stratum, wet, collection_date, Visit_No ) %>%
  left_join(
    bind_rows(my_rfs_optimized_prediction,
              my_rparts_optimized_prediction,
              my_rpartScore_optimized_prediction,
              my_pnw_prediction,
              my_nm_prediction,
              my_pnw2.rf_prediction,
              my_pnw2.rp_prediction)) %>%
  mutate(CorrectPrediction=Prediction==DetFinal.f) %>%
  filter(Region=="ASW")



all_predictions_revisits<-mydf2_revisits %>%
  select(globalid, SITECODE, DetFinal.f, Region, Stratum, wet, collection_date, Visit_No  ) %>%
  left_join(
    bind_rows(my_rfs_optimized_prediction_revisits,
              my_rparts_optimized_prediction_revisits,
              my_rpartScore_optimized_prediction_revisits,
              my_pnw_prediction_revisits,
              my_nm_prediction_revisits,
              my_pnw2.rf_prediction_revisits,
              my_pnw2.rp_prediction_revisits
    )) %>%
  mutate(CorrectPrediction=Prediction==DetFinal.f) %>%
  filter(Region=="ASW")

all_predictions_revisits2<-left_join(all_predictions_revisits,
                                     all_predictions %>% 
                                       select(SITECODE,ModName, ModName2,ModType, PreviousPrediction=Prediction)) %>%
  mutate(Consistent= Prediction==PreviousPrediction)

# all_predictions_revisits$Consistent <- all_predictions_revisits$DetFinal.f == all_predictions_revisits$Prediction

# #Count up unique preds
# mod_summary_predictors<-mod_summary_modtype %>%
#   select(ModType, ModName2) %>%
#   filter(ModType!="PNW" & ModType!="NM") %>%
#   unique()
# 
# mod_summary_predictors.list<-lapply(1:nrow(mod_summary_predictors), function(i){
#   modtype.i<-mod_summary_predictors$ModType[i]
#   modname.i<-mod_summary_predictors$ModName2[i]
#   j<-mod_summary$Index[which(mod_summary$ModName2==modname.i)]
#   if(modtype.i=="RandomForest")  {  mypredz<-my_rfs_optimized_training[[j]] %>%predictors()      }
#   if(modtype.i=="Rpart")  {  mypredz<-my_rparts_optimized_training[[j]] %>%predictors()      }
#   if(modtype.i=="RpartScore")  {  mypredz<-my_rpartScore_optimized_training[[j]] %>%predictors()      }
# })
# 
# mod_summary_predictors$n_streamcat<-sapply(mod_summary_predictors.list, function(x)
#   sum(x %in% StreamCatPreds)
#   )



my_rfes_optimal_preds<-lapply(1:nrow(mod_summary), function(i){
  myrfe<-my_rfes_total[[i]]
  mysize<-pickSizeTolerance(myrfe$results, metric="Kappa", tol=tolerance, maximize = T)
  pickVars(myrfe$variables, size=min(mysize, maxsize_x))
})


mod_summary_modtype<-mod_summary %>%
  crossing(
    ModType=c("RandomForest","Rpart","RpartScore")) %>%
  bind_rows(
    # data.frame(Strata=c("All","All"),
    #            Index=c(-1,-2),
    #            ModName=c("PNW","NM"),
    #            ModName2=c("PNW","NM"),
    #            ModType=c("PNW","NM"))
    data.frame(Strata=c("All","All","All","All"),
               Index=c(-1,-2,-3,-4),
               ModName=c("PNW","NM","PNW2.rf", "PNW2.rp"),
               ModName2=c("PNW","NM","PNW2.rf", "PNW2.rp"),
               ModType=c("PNW","NM","PNW2.rf", "PNW2.rp"))
  ) %>% 
  select(ModName, Index, ModName2, ModType, CalStrata=Strata) %>%
  crossing(EvalStratum=asw_strata) %>%
  filter(CalStrata=="All" | is.na(CalStrata) |
           CalStrata==EvalStratum) %>%
  crossing(Comparison=c("PvIvE",
                        "PvNP",
                        "EvNE","EvNE_SIpnw","EvNE_SInm",
                        "PvIwet",
                        "EvIdry",
                        "PnotE",
                        "EnotP"),
           Set=c("Training","Testing"))



mod_summary_modtype$n_tests<-sapply(1:nrow(mod_summary_modtype), function(i){
  modname.i=mod_summary_modtype$ModName[i]
  modtype.i=mod_summary_modtype$ModType[i]
  comp.i=mod_summary_modtype$Comparison[i]
  set.i=mod_summary_modtype$Set[i]
  evalstrat.i=mod_summary_modtype$EvalStratum[i]
  xdf<-all_predictions %>%
    filter(Region=="ASW") %>%
    filter(Set==set.i) %>%
    filter(Stratum==evalstrat.i) %>%
    filter(ModName==modname.i) %>%
    filter(ModType==modtype.i)
  # print(head(xdf))
  if(comp.i == "PvIvE")
  {
    xdf<-xdf %>% filter(DetFinal.f %in% c("E","I","P"))
    nrow(xdf)
  }
  else
    if(comp.i == "EvNE")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P"))
      nrow(xdf)
    }
  else
    if(comp.i == "EvNE_SIpnw")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf2 %>% select(globalid, SingleInd_PNW)) %>% na.omit()
      nrow(xdf)
    }
  else
    if(comp.i == "EvNE_SInm")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf2 %>% select(globalid, SingleInd_NM)) %>% na.omit()
      nrow(xdf)
    }
  else
    if(comp.i == "PvNP")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P"))
      nrow(xdf)
    }
  else
    if(comp.i == "EvIdry")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I") & !wet)
      nrow(xdf)
    }
  else
    if(comp.i == "PvIwet")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("P","I") & wet)
      nrow(xdf)
    }
  else
    if(comp.i == "PnotE")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("P"))
      nrow(xdf)
    }
  else
    if(comp.i == "EnotP")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E"))
      nrow(xdf)
    }
  else
    -99
})

mod_summary_modtype %>%  filter(n_tests==0)
mod_dats[[4]] %>% filter(split_strat=="CA P")

mod_summary_modtype$n_correct<-sapply(1:nrow(mod_summary_modtype), function(i){
  modname.i=mod_summary_modtype$ModName[i]
  modtype.i=mod_summary_modtype$ModType[i]
  comp.i=mod_summary_modtype$Comparison[i]
  set.i=mod_summary_modtype$Set[i]
  evalstrat.i=mod_summary_modtype$EvalStratum[i]
  xdf<-all_predictions %>%
    filter(Region=="ASW") %>%
    filter(Set==set.i) %>%
    filter(Stratum==evalstrat.i) %>%
    filter(ModName==modname.i) %>%
    filter(ModType==modtype.i)
  if(comp.i == "PvIvE")
  {
    xdf<-xdf %>% filter(DetFinal.f %in% c("E","I","P"))
    sum(xdf$DetFinal.f==xdf$Prediction)
  }
  else
    if(comp.i == "EvNE")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"~"E",T~"NE"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "EvNE_SIpnw")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf2 %>% select(globalid, SingleInd_PNW)) %>%
        na.omit() %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"& SingleInd_PNW==0~"E" ,T~"NE"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "EvNE_SInm")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf2 %>% select(globalid, SingleInd_NM)) %>%
        na.omit() %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"  & SingleInd_NM==0~"E",T~"NE"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "PvNP")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P"))%>%
        mutate(DetFinal.f=case_when(DetFinal.f=="P"~"P",T~"NP"),
               Prediction=case_when(Prediction=="P"~"P",T~"NP"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "EvIdry")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I") & !wet)
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "PvIwet")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("P","I") & wet)
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "PnotE")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("P"))
      sum(xdf$Prediction %in% c("P","I"))
    }
  else
    if(comp.i == "EnotP")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E"))
      sum(xdf$Prediction %in% c("E","I"))
    }
  else
    -99
})

mod_summary_modtype$PctCorrect<-mod_summary_modtype$n_correct/mod_summary_modtype$n_tests



mod_summary_modtype %>%
  filter(ModName2=="Strat H2O SC" & Set=="Testing" & Comparison=="PnotE")



mod_summary_modtype_allstrata<-mod_summary_modtype %>%
  group_by(ModName2, ModType, Comparison, Set) %>%
  summarise(n_tests=sum(n_tests),
            n_correct=sum(n_correct)) %>%
  ungroup() %>%
  mutate(PctCorrect=n_correct/n_tests) 


mod_summary_modtype_eachstratum<-mod_summary_modtype %>%
  group_by(ModName2, ModType, Comparison, Set, EvalStratum) %>%
  summarise(n_tests=sum(n_tests),
            n_correct=sum(n_correct)) %>%
  ungroup() %>%
  mutate(PctCorrect=n_correct/n_tests) %>%
  na.omit()



#How do modeling methods compare in terms of accuracy and precision?
mod_type_comp_plot.df<-mod_summary_modtype_allstrata %>%
  na.omit() %>%
  mutate(Metric=paste0("Accuracy (",Comparison,")")) %>%
  rename(MetricResult=PctCorrect,
         MetricCount=n_tests) %>%
  select(-Comparison, -n_correct) %>%
  bind_rows(
    all_predictions_revisits2 %>%
      group_by(ModName2, ModType) %>%
      summarise(MetricCount=sum(!is.na(Consistent)),
                MetricResult = sum(Consistent,na.rm=T)/sum(!is.na(Consistent))
                # PctCorrectRevisits=sum(CorrectPrediction, na.rm=T)/sum(!is.na(CorrectPrediction))
                )%>%
      ungroup() %>%
      mutate(Metric="Consistency", Set="Training"))  %>%
  mutate(Stratified = ModName2 %in% c("Strat","Strat H2O", "Strat SC", "Strat H2O SC","PNW", "NM", "PNW2.rf","PNW2.rp"),
         Unstratified = ModName2 %in% c("Unstrat","Unstrat H2O", "Unstrat SC", "Unstrat H2O SC", "PNW","NM", "PNW2.rf","PNW2.rp"),
         Metric2 = case_when(Metric=="Consistency"~"Repeatability",T~Metric))

write.csv(mod_type_comp_plot.df, file="Output/mod_type_comp_plot.df.csv", row.names = F)

effects_of_stratification.df<-mod_type_comp_plot.df %>%
  filter(!ModType %in% c("PNW","NM","PNW2.rf","PNW2.rp")) %>%
  mutate(Preds = case_when(ModName2 %in% c("Strat","Unstrat")~"Core",
                           ModName2 %in% c("Strat H2O","Unstrat H2O")~"H2O",
                           ModName2 %in% c("Strat SC","Unstrat SC")~"SC",
                           ModName2 %in% c("Strat H2O SC","Unstrat H2O SC")~"H2O SC"),
         VAR = case_when(Stratified~"Stratified",T~"Unstratified")) %>%
  select(-ModName2, -MetricCount,-Stratified, -Unstratified) %>%
  pivot_wider(names_from=VAR, values_from = MetricResult) %>%
  mutate(Diff=Stratified-Unstratified)

effects_of_stratification_plot<-ggplot(data=effects_of_stratification.df, aes(x=ModType, y=Diff))+
  geom_point(aes(color=Preds, group=Set, shape=Set, size=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .25, jitter.height = 0))+
  scale_shape_manual(values=c(17,16))+
  geom_hline(yintercept=0)+
  geom_hline(yintercept=c(-.1,.1), linetype="dotted")+
  scale_size_manual(values=c(0.5,1))+
  facet_wrap(~Metric, scales="free")+
  scale_y_continuous(name="Performance (Stratified - Unstratified)", limits=c(-.2, .2))+
  # xlab("")+
  theme_bw(base_size=8)+
  ggtitle("Effect of stratification on model performance")+
  coord_flip()
ggsave(effects_of_stratification_plot, filename="figures/effects_of_stratification_plot.jpg", dpi=300, height=6, width=10)


ggplot(data=effects_of_stratification.df %>%
         filter(Metric=="Accuracy (PvIvE)" & 
                  ModType %in% c("Rpart","RandomForest") &
                  Preds %in% c("Core","H2O")), 
       aes(x=ModType, y=Diff))+
  geom_point(aes(color=Preds, group=Set, shape=Set, size=Set), 
             position=position_jitterdodge(dodge.width = .5, jitter.width = 0, jitter.height = 0))+
  scale_shape_manual(values=c(17,16))+
  geom_hline(yintercept=0)+
  # geom_hline(yintercept=c(-.1,.1), linetype="dotted")+
  scale_size_manual(values=c(2,4))+
  # facet_wrap(~Metric, scales="free")+
  scale_y_continuous(name="PvIvE Accuracy (Stratified - Unstratified)", limits=c(-.4, .4))+
  xlab("")+
  scale_color_brewer(palette="Paired", name="Predictors")+
  # scale_color_brewer(palette="Set1", name="Model", labels=c("Core", "Core + H2O", "Core + H2O + StreamCat", "Core + StreamCat"))+
  theme_bw()+
  theme(axis.text = element_text(size=10))+
  # ggtitle("Impact of stratification")+
  coord_flip()

performance_across_strata_by_modeltype<-ggplot(data=mod_type_comp_plot.df, aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .025, jitter.height = 0))+
  scale_color_manual(values=c(brewer_pal(palette="YlGn")(6)[3:6],
                              brewer_pal(palette="OrRd")(6)[3:6],
                              brewer_pal(palette="BuPu")(6)[3:6]
                              # viridis_pal(option="A", end=.5, begin=0)(4),
                              # viridis_pal(option="B", end=1, begin=.5)(4)
                              # "#fc8d59","#e34a33","#b30000"
  ))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  # scale_size_manual(values=c(0.5,1.5))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  # xlab("")+
  theme_bw()+
  ggtitle("Performance aggregated across strata")+
  coord_flip()
ggsave(performance_across_strata_by_modeltype, filename="Figures/performance_across_strata_by_modeltype.jpg",
       dpi=300, height=5, width=8)

performance_across_strata_by_modeltype2<-
  ggplot(data=mod_type_comp_plot.df %>%
           filter(!ModName2 %in% c("RpartScore","PNW2.rp","PNW2.rf") &
                    !Metric %in% c("Accuracy (EvNE_SInm)", "Accuracy (EvNE_SIpnw)") &
                    ModType!="RpartScore") %>%
           mutate(Metric=case_when(Metric=="Accuracy (EvNE)"~"Accuracy (EvALI)",
                                   Metric=="Accuracy (EvNE_SIpnw)"~"Accuracy (EvALI_SI)",
                                   T~Metric)),
         aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  scale_color_manual(values=c(#brewer_pal(palette="YlGn")(6)[5:6],
    "black","gray",
    brewer_pal(palette="YlOrBr")(6)[3:6],
    brewer_pal(palette="YlGnBu")(6)[3:6]),
  name="Method or model",
  labels=c("NM","PNW",
           "Strat","Strat H2O","Strat H2O GIS","Strat GIS",
           "Unstrat","Unstrat H2O","Unstrat H2O GIS", "Unstrat GIS"))+
  scale_shape_manual(values=c(17,16))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw()+
  guides(color = guide_legend(order=1),
         shape = guide_legend(order=2))+
    coord_flip()
performance_across_strata_by_modeltype2
ggsave(performance_across_strata_by_modeltype2, filename="Figures/performance_across_strata_by_modeltype2.jpg",
       dpi=300, height=5, width=8)
#Let's not do the NM or PNW methods yet, and save those for a separate comparison with the "final final" method.
mod_type_comp_plot.df2<-mod_type_comp_plot.df %>%
  filter(!ModName2 %in% c("RpartScore","PNW2.rp","PNW2.rf", "NM","PNW") &
           !Metric %in% c("Accuracy (EvNE_SInm)", "Accuracy (EvNE_SIpnw)") &
           !ModType %in% c("RpartScore","NM","PNW")) %>%
  mutate(Metric=case_when(Metric=="Accuracy (EvNE)"~"Accuracy (EvALI)",
                          Metric=="Accuracy (EvNE_SIpnw)"~"Accuracy (EvALI_SI)",
                          T~Metric),
         ModName2=str_replace_all(ModName2,"SC","GIS"))

performance_across_strata_by_modeltype3<-
  ggplot(data=mod_type_comp_plot.df2,
         aes(x=ModName2, y=MetricResult))+
  geom_point(aes(fill=ModType, group=Set, shape=Set, color=ModType),
             position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  geom_point(data=mod_type_comp_plot.df2 %>%
               filter(ModName2=="Unstrat" & ModType=="RandomForest"),  aes(fill=ModType, group=Set, shape=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  
  scale_fill_brewer(palette="Paired", name="Model type", labels=c("Random forest","Single tree"))+
  scale_color_brewer(palette="Paired", name="Model type",labels=c("Random forest","Single tree"), guide=F)+
  scale_shape_manual(values=c(24,22))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw()+
  guides(fill = guide_legend(order=1,
                             override.aes=list(shape=21)),
         shape = guide_legend(order=2))+
  coord_flip() + 
  theme(legend.title = element_text(size=10),
        legend.text=element_text(size=10),
        legend.spacing.y = unit(0, "cm"),
        legend.position = c(1, 0), legend.justification = c(1, 0))
ggsave(performance_across_strata_by_modeltype3,
       filename="Figures/performance_across_strata_by_modeltype3.jpg",
       dpi=300, height=6, width=6)

mod_type_comp_plot.df2 %>%
  

ggplot(data=mod_type_comp_plot.df %>% 
                                           filter(#Metric=="Accuracy (PvIvE)" &
                                                    !ModType %in% c("PNW2.rp","PNW2.rf", "RpartScore") &
                                                    ModName2 %in% c("NM","PNW","Unstrat","Unstrat H2O","Strat","Strat H2O")),
                                         aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set), size=3,
             position=position_jitterdodge(dodge.width = .75, jitter.width = .025, jitter.height = 0))+
  scale_color_manual(values=c("black","black",
                              brewer_pal(palette="Paired")(4)
                              # viridis_pal(option="A", end=.5, begin=0)(4),
                              # viridis_pal(option="B", end=1, begin=.5)(4)
                              # "#fc8d59","#e34a33","#b30000"
                              ),
                     name="Model type"
                     )+
  scale_shape_manual(values=c(17,16))+
  ylab("% correct")+xlab("")+
  # scale_size_continuous(range=c(0.5,2.5))+
  # scale_size_manual(values=c(0.5,1.5))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="% correct (PvIvE)", limits=c(0,1), breaks=c(0,.5,1))+
  # xlab("")+
  theme_bw()+
  ggtitle("Performance across the ASW region")+
  theme(axis.text = element_text(size=10))+
  coord_flip()


ggsave(performance_across_strata_by_modeltype, filename="Figures/performance_across_strata_by_modeltype.jpg",
       dpi=300, height=5, width=8)


JustUnstratified_plot<-ggplot(data=mod_type_comp_plot.df[which(mod_type_comp_plot.df$Unstratified),], aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set, size=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .025, jitter.height = 0))+
  scale_color_manual(name="Model type", values=c("black","gray30",
                              brewer_pal(palette="Set1")(6),
                              # viridis_pal(option="A", end=.5, begin=0)(4),
                              # viridis_pal(option="B", end=1, begin=.5)(4)
                              # "#fc8d59","#e34a33","#b30000",
                              
                              labels=c("NM method", "PNW method", "Core", "Core + H2O", "Core + H2O + StreamCat", "Core + StreamCat")
  ))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(1,2))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw()+
  # ggtitle("Unstratified models")+
  coord_flip()
JustUnstratified_plot
ggsave(JustUnstratified_plot, filename="Figures/JustUnstratified_plot.jpg", dpi=300, height=6, width=10)

JustUnstratified_plot2<-ggplot(data=mod_type_comp_plot.df %>%
    filter(Unstratified & ModName2 %in% c("PNW","NM","Unstrat","Unstrat H2O", "PNW2.rf","PNW2.rp") & ModType !="RpartScore" &
             !Metric %in% c("Accuracy (EvNE_SIpnw)","Accuracy (EvNE_SInm)")),
  # [which(mod_type_comp_plot.df$Unstratified),], 
  aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set, size=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .1, jitter.height = 0))+
  scale_color_manual(name="Model type", values=c("gray30","gray30","gray30","gray30",
                                                 brewer_pal(palette="Set1")(2)
                                                 # viridis_pal(option="A", end=.5, begin=0)(4),
                                                 # viridis_pal(option="B", end=1, begin=.5)(4)
                                                 # "#fc8d59","#e34a33","#b30000",

                                             
  ))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(1,2))+
  facet_wrap(~Metric, scales="free", nrow=2)+
  scale_y_continuous(name="Performance")+
  xlab("")+
  theme_bw()+
  # ggtitle("Unstratified models")+
  coord_flip()
JustUnstratified_plot2
ggsave(JustUnstratified_plot2, filename="Figures/JustUnstratified_plot2.jpg", dpi=300, height=6, width=10)

mod_type_comp_plot.df<-mod_type_comp_plot.df %>%
  mutate(ModTypeName=  case_when(ModName2==ModType~ModName2,
                                 T~paste(ModType, ModName2)) )


JustUnstratified_plot2a<-ggplot(data=mod_type_comp_plot.df %>%
                                 filter(Unstratified & ModName2 %in% c("PNW","NM","Unstrat","Unstrat H2O") & ModType !="RpartScore" &
                                          !Metric %in% c("Accuracy (EvNE_SIpnw)","Accuracy (EvNE_SInm)")),
                               # [which(mod_type_comp_plot.df$Unstratified),], 
                               aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set, size=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .1, jitter.height = 0))+
  scale_color_manual(name="Model type", values=c("black","black",
                                                 brewer_pal(palette="Paired")(2)
                                                 # viridis_pal(option="A", end=.5, begin=0)(4),
                                                 # viridis_pal(option="B", end=1, begin=.5)(4)
                                                 # "#fc8d59","#e34a33","#b30000",
                                                 
                                                 
  ))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(1.5,3))+
  facet_wrap(~Metric2, scales="free", nrow=2)+
  scale_y_continuous(name="Performance")+
  xlab("")+
  theme_bw()+
  # ggtitle("Unstratified models")+
  coord_flip()
JustUnstratified_plot2a
ggsave(JustUnstratified_plot2a, filename="Figures/JustUnstratified_plot2a.jpg", dpi=300, height=6, width=10)

mod_type_comp_plot.df<-mod_type_comp_plot.df %>%
  mutate(ModTypeName=  case_when(ModName2==ModType~ModName2,
                                 T~paste(ModType, ModName2)) )


JustUnstratified_plot3<-ggplot(data=mod_type_comp_plot.df %>%
                                 filter(Unstratified & 
                                          Metric %in% c("Accuracy (EvNE)","Accuracy (EvNE_SIpnw)","Accuracy (EvNE_SInm)") &
                                          ModName2 %in% c("PNW","NM","Unstrat") & 
                                          !ModType  %in% c("RpartScore","Rpart")),
                               # [which(mod_type_comp_plot.df$Unstratified),], 
                               aes(x=ModTypeName, y=MetricResult))+
  geom_point(aes(color=Metric, group=Set, shape=Set, size=Set), alpha=.75,
             position=position_jitterdodge(dodge.width = .75, jitter.width = .0025, jitter.height = 0))+
  # scale_color_manual(name="Model type", values=c("black","gray30",
  #                                                brewer_pal(palette="Set1")(4),
  #                                                # viridis_pal(option="A", end=.5, begin=0)(4),
  #                                                # viridis_pal(option="B", end=1, begin=.5)(4)
  #                                                # "#fc8d59","#e34a33","#b30000",
  #                                                
  #                                                labels=c("NM method", "PNW method", "Core", "Core + H2O")
  # ))+
  # scale_color_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2", name="Single\nIndicator", labels=c("None","Fish + BMI", "Fish"))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(1,2))+
  # facet_wrap(~ModName2)+
  # scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  scale_y_continuous(name="Performance")+
  ylab("Accuracy")+xlab("")+
  theme_bw()+
  # ggtitle("Unstratified models")+
  coord_flip()
JustUnstratified_plot3 
ggsave(JustUnstratified_plot3, filename="Figures/JustUnstratified_plot3.jpg", dpi=300, height=6, width=10)



JustUnstratified_plot3a<-ggplot(data=mod_type_comp_plot.df %>%
                                 filter(Unstratified & 
                                          Metric %in% c("Accuracy (EvNE)","Accuracy (EvNE_SIpnw)","Accuracy (EvNE_SInm)") &
                                          ModName2 %in% c("PNW","NM","Unstrat") & 
                                          !ModType  %in% c("RpartScore")),
                               # [which(mod_type_comp_plot.df$Unstratified),], 
                               aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=Metric, group=Set, shape=Set, size=Set), 
             position=position_jitterdodge(dodge.width = .75, jitter.width = 0.05, jitter.height = 0))+
  # scale_color_manual(name="Model type", values=c("black","gray30",
  #                                                brewer_pal(palette="Set1")(4),
  #                                                # viridis_pal(option="A", end=.5, begin=0)(4),
  #                                                # viridis_pal(option="B", end=1, begin=.5)(4)
  #                                                # "#fc8d59","#e34a33","#b30000",
  #                                                
  #                                                labels=c("NM method", "PNW method", "Core", "Core + H2O")
  # ))+
  # scale_color_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2", name="Single\nIndicator", labels=c("None","NM (Fish + BMI)", "PNW (Fish + verts)"))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(1.5,3))+
  # facet_wrap(~ModName2)+
  # scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  scale_y_continuous(name="% correct (at least intermittent)")+
  ylab("Accuracy")+xlab("")+
  theme_bw()+
  # ggtitle("Unstratified models")+
  coord_flip()
JustUnstratified_plot3a 
ggsave(JustUnstratified_plot3a, filename="Figures/JustUnstratified_plot3a.jpg", dpi=300, height=6, width=10)

###Rank Performance Metrics

mod_type_comp_plot.df %>%
  filter(Unstratified & !ModName2 %in% c("Unstrat H2O SC", "Unstrat SC", "Unstrat H2O") & ModType!="RpartScore") %>%
  group_by(Metric, Set) %>%
  mutate(Rank = rank(MetricResult, ties.method = "average")) %>%
  
  select(ModName2, ModType, Set, Metric, Rank) %>%
  pivot_wider(names_from=Metric, values_from=Rank) %>%
  filter(Set=="Training") %>%
  write.table(file="clipboard", sep="\t", row.names = F)
  
  
  
mod_type_comp_plot.df %>%
  filter(Unstratified & !ModName2 %in% c("Unstrat H2O SC", "Unstrat SC", "Unstrat H2O") & ModType!="RpartScore") %>%
  group_by(Metric, Set) %>%
  filter(!Metric %in% c("Accuracy (EvNE_SInm)", "Accuracy (EvNE_SIpnw")) %>%
  mutate(Rank = rank(MetricResult, ties.method = "average"),
         Rank2 = case_when(Metric %in% c("Accuracy (PvIvE)", "Accuracy (EvNE)")~2*Rank, T~Rank)) %>%
  group_by(ModName2, ModType, Set) %>%
  summarise(SumRank=sum(Rank),
            SumRank2=sum(Rank2)) %>%
  filter(Set=="Training") 
####
  
  


mod_summary

varImpPlot(my_rfs_optimized_training[[1]])
rattle::fancyRpartPlot(my_rparts_optimized_training[[1]])
rattle::fancyRpartPlot(my_rpartScore_optimized_training[[4]])


JustStratified_plot<-ggplot(data=mod_type_comp_plot.df[which(mod_type_comp_plot.df$Stratified),], aes(x=ModType, y=MetricResult))+
  geom_point(aes(color=ModName2, group=Set, shape=Set, size=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .25, jitter.height = 0))+
  scale_color_manual(values=c("black","gray30",
                              brewer_pal(palette="Set1")(6)
                              # viridis_pal(option="A", end=.5, begin=0)(4),
                              # viridis_pal(option="B", end=1, begin=.5)(4)
                              # "#fc8d59","#e34a33","#b30000"
  ))+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(0.5,1))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  # xlab("")+
  theme_bw(base_size=8)+
  ggtitle("Performance  of stratified models")+
  coord_flip()
JustStratified_plot
ggsave(JustStratified_plot, filename="Figures/JustStratified_plot.jpg", dpi=300, height=6, width=10)


mod_type_comp_plot.df %>%
  filter(Metric=="Accuracy (PnotE)" & MetricResult<0.7)
all_predictions %>%
  filter(ModName2== "Strat H2O SC" & DetFinal.f=="P" & ModType=="Rpart" & Set=="Testing")

write.csv(mod_summary_modtype_eachstratum, file="Output/mod_summary_modtype_eachstratum.csv", row.names=F)


#Just the selected model
sel.modtype="RandomForest"

selmod_type_comp_plot.df<-mod_summary_modtype_eachstratum %>%
  bind_rows(mod_summary_modtype_allstrata %>%
              mutate(EvalStratum="All")
              ) %>%
  filter(ModType %in% c(sel.modtype, "PNW","NM") ) %>%
  na.omit() %>%
  mutate(Metric=paste0("Accuracy (",Comparison,")")) %>%
  rename(MetricResult=PctCorrect,
         MetricCount=n_tests) %>%
  select(-Comparison, -n_correct) %>%
  bind_rows(
    all_predictions_revisits2 %>%
      filter(ModType %in% c(sel.modtype)) %>%
      group_by(ModName2, ModType) %>%
      summarise(MetricCount=sum(!is.na(Consistent)),
                MetricResult = sum(Consistent,na.rm=T)/sum(!is.na(Consistent))
                # PctCorrectRevisits=sum(CorrectPrediction, na.rm=T)/sum(!is.na(CorrectPrediction))
      )%>%
      ungroup() %>%
      mutate(Metric="Consistency", Set="Training", EvalStratum="All") 
  )%>%
  mutate(Stratified = ModName2 %in% c("Strat","Strat H2O", "Strat SC", "Strat H2O SC","PNW", "NM"),
         Unstratified = ModName2 %in% c("Unstrat","Unstrat H2O", "Unstrat SC", "Unstrat H2O SC", "PNW","NM"))




JustUnstratified_byStrata_plot<-ggplot(data=selmod_type_comp_plot.df[which(selmod_type_comp_plot.df$Unstratified),], 
                                       aes(x=ModName2, y=MetricResult))+
  # geom_point(aes(color=EvalStratum, group=Set, shape=Set),
  #            position=position_jitterdodge(dodge.width = .75, jitter.width = .35, jitter.height = 0))+
  # geom_point(data=selmod_type_comp_plot.df %>% filter(EvalStratum=="All"),
  #            aes(color=EvalStratum, group=Set, shape=Set),
  #            position=position_jitterdodge(dodge.width = .75, jitter.width = .35, jitter.height = 0, seed=1))+
  geom_point(aes(color=EvalStratum, group=Set, shape=Set, size=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .35, jitter.height = 0, seed=1))+
    scale_color_manual(name="Evaluated\nStratum",
                     values=c("gray45",  brewer_pal(palette="Set1")(5) )
                              # viridis_pal(option="B", end=.75, begin=.2)(5)  )
                     )+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(0.5,1))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw(base_size = 8)+
  ggtitle(paste0("Performance by stratum\nUnstratified ",
                 sel.modtype," models"))+
  coord_flip()
ggsave(JustUnstratified_byStrata_plot, filename="Figures/JustUnstratified_byStrata_plot.jpg", dpi=300, height=6, width=10)


sel.modtype2<-c("RandomForest","Rpart")
selmod_type_comp_plot.df2<-mod_summary_modtype_eachstratum %>%
  bind_rows(mod_summary_modtype_allstrata %>%
              mutate(EvalStratum="All")
  ) %>%
  filter(ModType %in% c(sel.modtype2, "PNW","NM") ) %>%
  na.omit() %>%
  mutate(Metric=paste0("Accuracy (",Comparison,")")) %>%
  rename(MetricResult=PctCorrect,
         MetricCount=n_tests) %>%
  select(-Comparison, -n_correct) %>%
  bind_rows(
    all_predictions_revisits2 %>%
      filter(ModType %in% c(sel.modtype2)) %>%
      group_by(ModName2, ModType) %>%
      summarise(MetricCount=sum(!is.na(Consistent)),
                MetricResult = sum(Consistent,na.rm=T)/sum(!is.na(Consistent))
                # PctCorrectRevisits=sum(CorrectPrediction, na.rm=T)/sum(!is.na(CorrectPrediction))
      )%>%
      ungroup() %>%
      mutate(Metric="Consistency", Set="Training", EvalStratum="All") 
  )%>%
  mutate(Stratified = ModName2 %in% c("Strat","Strat H2O", "Strat SC", "Strat H2O SC","PNW", "NM"),
         Unstratified = ModName2 %in% c("Unstrat","Unstrat H2O", "Unstrat SC", "Unstrat H2O SC", "PNW","NM"),
         ModType2 = case_when(ModType %in% c("PNW","NM") ~ ModType,
                              T~paste(ModType,ModName2))) %>%
  filter(Unstratified) %>%
  filter(Metric %in% c("Accuracy (PvIvE)","Accuracy (EvNE)") &
           ModName2 %in% c("NM","PNW","Unstrat","Unstrat H2O"))

selmod_type_comp_plot.df2_summary<-selmod_type_comp_plot.df2 %>%
  group_by(ModType2,Metric, Set) %>%
  summarise(MinResult=min(MetricResult),
            MaxResult=max(MetricResult))


JustUnstratified_byStrata_plot2<-
  ggplot(data=selmod_type_comp_plot.df2, aes(x=ModType2, y=MetricResult))+
  # geom_segment(data=selmod_type_comp_plot.df2_summary,
  #              aes(x=ModType2, xend=ModType2, y=MinResult, yend=MaxResult, group=Set),
  #              position=position_dodge(width = .75))+
  geom_point(aes(color=EvalStratum, group=Set, shape=Set, size=Set),
             position=position_dodge(width = .75))+
  scale_color_manual(name="Evaluated\nStratum",
                     values=c("gray45",  brewer_pal(palette="Set1")(5) )
                     # viridis_pal(option="B", end=.75, begin=.2)(5)  )
  )+
  scale_shape_manual(values=c(17,16))+
  # scale_size_continuous(range=c(0.5,2.5))+
  scale_size_manual(values=c(1.5,3))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw()+
  ggtitle(paste0("Consistency across strata"))+
  coord_flip()
JustUnstratified_byStrata_plot2
ggsave(JustUnstratified_byStrata_plot2, filename="Figures/JustUnstratified_byStrata_plot2.jpg", dpi=300, height=6, width=10)





head(all_predictions_revisits2)

all_predictions_revisits2
  
mod_summary_modtype%>% filter(n_tests==0) %>% as.data.frame()

mod_summary_plot_dat<-mod_summary_modtype %>%
  bind_rows(mod_summary_modtype_allstrata %>%
              mutate(EvalStratum="All"))

mod_summary_plot_dat2<-mod_summary_plot_dat %>%
  mutate(Measure=paste("Accuracy", Comparison),
         Performance=PctCorrect)  ####FINISH



ggplot(data=mod_summary_plot_dat %>% filter(Set=="Training"), aes(x=Comparison, y=PctCorrect))+
  geom_point(data=mod_summary_plot_dat %>% filter(Set=="Training" & EvalStratum=="All"), aes(color=EvalStratum), size=2)+
  geom_point(aes(color=EvalStratum))+
  facet_wrap(ModType~ModName2)+
  scale_color_brewer(palette="Set1")+
  coord_flip()

ggplot(data=mod_summary_plot_dat , aes(x=EvalStratum, y=PctCorrect))+
  # geom_point(data=mod_summary_plot_dat %>% filter(Set=="Training" & EvalStratum=="All"), 
  #            aes(color=Comparison), size=2)+
  geom_point(aes(color=Comparison, size=Set, alpha=Set, group=Set), shape=16, 
             position=position_dodge(width=0.5))+
  facet_wrap(ModType~ModName2)+
  scale_color_brewer(palette="Dark2")+ 
  scale_alpha_manual(values=c(0.25,1))+scale_size_manual(values=c(0.5,1))+
  coord_flip()

ggplot(data=mod_summary_plot_dat %>%
         filter(EvalStratum=="All" )         )+
  geom_point(aes(x=ModName2, y=PctCorrect, color=ModType, size=Set, alpha=Set, group=Set), shape=16, 
             position=position_jitterdodge(dodge.width=0.5, jitter.width = 0.25, jitter.height = 0))+
  facet_wrap(~Comparison)+
  scale_color_brewer(palette="Set1")+ 
  scale_alpha_manual(values=c(.85,1))+scale_size_manual(values=c(1,2))+
  coord_flip()

####
#SO WHAT'S THE BEST MODEL?
#Unstratified, with streamcat, rpartscore?
mod_summary_modtype %>%
  filter(ModType=="RpartScore" & ModName2=="Unstrat SC")

rattle::fancyRpartPlot( my_rpartScore_optimized_training[[2]] )
rattle::fancyRpartPlot( my_rparts_optimized_training[[1]] )

mod_summary %>% filter(Stratification == "Unstratified")

# save.image("asw_tool_calibration_image_063020.Rdata")
# load("asw_tool_calibration_image_063020.Rdata")
save.image("asw_tool_calibration_image_040621.Rdata")


#####
study_area_sf<-st_read("Input/studyarea_shp/ASWandWMStudyAreas.shp")

study_area_sf<-st_transform(study_area_sf, crs=4326) %>%
  mutate(Region=case_when(Region=="USACE Arid West Region"~"ASW",
                          Region=="USACE Western Mountans, Valleys, and Coast Region"~"WM",
                          T~"xxx"))


mydf_sf<-st_as_sf(mydf2,
                  coords=c("Longitude","Latitude"),
                  remove=F,
                  crs=4326)

all_predictions_combo_sf<-all_predictions_combo %>%
  left_join(mod_summary %>% mutate(ModName2 = case_when(ModName %in% c("PNW_NoFlag","NM")~ModName,
                                                        Strata=="All" & IncludeWaterPreds & IncludeStreamCatPreds ~ "Unstrat H2O SC",
                                                        Strata=="All" & IncludeWaterPreds & !IncludeStreamCatPreds ~ "Unstrat H2O",
                                                        Strata=="All" & !IncludeWaterPreds & IncludeStreamCatPreds ~ "Unstrat SC",
                                                        Strata=="All" & !IncludeWaterPreds & !IncludeStreamCatPreds ~ "Unstrat",
                                                        Strata!="All" & IncludeWaterPreds & IncludeStreamCatPreds ~ "Strat H2O SC",
                                                        Strata!="All" & IncludeWaterPreds & !IncludeStreamCatPreds ~ "Strat H2O",
                                                        Strata!="All" & !IncludeWaterPreds & IncludeStreamCatPreds ~ "Strat SC",
                                                        Strata!="All" & !IncludeWaterPreds & !IncludeStreamCatPreds ~ "Strat",
                                                        T~ModName)) %>%
              select(ModName, ModStrata=Strata, ModName2)) %>%
  left_join(mydf2 %>% select(globalid, SITECODE, Latitude, Longitude)) %>%
  st_as_sf( coords=c("Longitude","Latitude"),
            remove=F,
            crs=4326)

area.map + 
  geom_sf(data=all_predictions_combo_sf, aes(color=Predicted))+
  scale_color_brewer(palette="Set1")+
  # scale_size_manual(values=c(2,1), name="Correct")+
  facet_wrap(~ModName2)


area.map<-ggplot()+
  geom_sf(data=study_area_sf, show.legend = F, fill="gray50")+
  geom_sf(data=study_area_sf %>% filter(Region=="ASW"), show.legend = F, fill="gray80")

wm_area.map<-ggplot()+
  geom_sf(data=study_area_sf, show.legend = F, fill="gray50")+
  geom_sf(data=study_area_sf %>% filter(Region!="ASW"), show.legend = F,
          aes(fill=Stratum))+scale_fill_brewer(palette="Pastel1")



write.csv(predictor_summary,"Output/predictor_summary.csv",row.names = F)
write.csv(all_predictions, "Output/all_predictions.csv",row.names = F)
write.csv(all_predictions_revisits2, "Output/all_predictions_revisits2.csv",row.names = F)


###
#Single Indicator investigation
