# save.image("asw_fdam.Rdata")#
# load("asw_fdam.Rdata")
library(tidyverse)
library(ggrepel)
library(sf)
library(randomForest)
library(tidymodels)
library(rattle)
library(pdp)
library(vip)
library(lubridate)

mydf_original<-read.csv("Input/mydf2.csv", stringsAsFactors = F) %>% 
  left_join(xwalk<-read.csv("Input/Xwalk_Original_062720.csv", stringsAsFactors = F) %>%
              select(SiteID, HydroDataSource) %>% unique()
  ) %>%
  mutate(HydroDataFreq = case_when(HydroDataSource %in% 
                                     c("USGS stream gauge", "Hobo Logger","WildlifeCamera","Hobo logger","Temperature logger")~"Continuous",
                                   HydroDataSource %in% c("Stream Tracker")~"Discrete"),
         DetFinal.f=factor(Determination_Final, levels=c("E","I","P")))
# mydf_original<-mydf_original[-19,]         #This row got duplicated due to hdyro data source stuff. Oops! #FIXED. Do not run

mydf<-mydf_original %>%
  filter(Region=="ASW") %>%
  filter(Determination_Final %in% c("E","I","P")) %>%
  filter(SiteID!="RRC.PCU") %>% #This "perennial" stream was dry. Cannot determine true class
  filter(!SITECODE %in% c( "NVAW1193", "NVAW1190")) #Too many missing values
mydf_revisits <- mydf %>%
  filter(Visit_No > 1)

mydf %>%
  filter(Visit_No==2) %>%
  mutate(Date=mdy_hm(collection_date)) %>%
  select(Date) %>%
  summary()

mydf_original %>%
  filter(SITECODE %in% mydf_revisits$SITECODE) %>%
  select(SITECODE, collection_date) %>%
  mutate(Date=mdy_hm(collection_date)) %>%
  arrange(SITECODE, Date) %>%
  group_by(SITECODE) %>%
  summarise(Diff = max(Date) - min(Date)) %>%
  arrange(Diff)

mydf %>% 
  select(SITECODE, Determination_Final, DeterminationStatus, SourceType) %>%
  unique() %>%
  group_by(Determination_Final, DeterminationStatus, SourceType) %>%
  tally() %>%
  pivot_wider(names_from = Determination_Final, values_from=n, values_fill=list(n=0))

####
#Review of final dataset
study_area_sf<-st_read("Input/studyarea_shp/ASWandWMStudyAreas.shp")
st_crs(study_area_sf)
study_area_sf<-st_transform(study_area_sf, crs=4326) %>%
  mutate(Region=case_when(Region=="USACE Arid West Region"~"ASW",
                          Region=="USACE Western Mountains, Valleys, and Coast Region"~"WM",
                          T~"xxx"))


mydf_sf<-st_as_sf(mydf %>% filter(Visit_No==1),
                  coords=c("Longitude","Latitude"),
                  remove=F,
                  crs=4326)  %>%
  mutate(PanelLabe=DetFinal.f) 
levels(mydf_sf$PanelLabe)  <- c("Ephemeral","Intermittent","Perennial")

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))


area.map<-ggplot()+
  geom_sf(data=usa, fill="gray80", color="white")+
  # geom_sf(data=study_area_sf, show.legend = F, fill="gray80")+
  # geom_sf(data=study_area_sf %>% filter(Region=="ASW"), show.legend = F, fill="gray80")
  geom_sf(data=study_area_sf %>% filter(Region=="ASW"), fill="#ccebc5", color="gray80")+
  scale_fill_brewer(palette="Accent")+
  theme(axis.text=element_blank(), panel.background = element_blank(),axis.ticks = element_blank())

dev_plot<-area.map+
  geom_sf(data=mydf_sf, size=1) +
  facet_wrap(~PanelLabe)+
  coord_sf(#crs=(3310),
    xlim=st_bbox(study_area_sf)[c(1,3)],
    ylim=st_bbox(study_area_sf)[c(2,4)]
    )

ggsave(dev_plot, filename="Figures/DevMap.jpg", dpi=300, height=3, width=6)




area.map+
  geom_sf(data=mydf_sf, size=2, aes(color=PanelLabe, shape=PanelLabe)) +
  scale_shape_manual(values=c(15,16,18), name="")+
  scale_color_viridis_d(name="", direction=-1)+
  # facet_wrap(~PanelLabe)+
  coord_sf(#crs=(3310),
    xlim=st_bbox(study_area_sf)[c(1,3)],
    ylim=st_bbox(study_area_sf)[c(2,4)]
  )+
  theme(legend.position = "bottom")

###



predictor_summary<-read.csv("Output/predictor_summary.csv", stringsAsFactors = F)
predictor_redundancy<-read.csv("Output/predictor_redundancy.csv", stringsAsFactors = F)
# load("Output/final_rf_unrevised.Rdata")
load("Output/final_rf_unrevised_FINAL.Rdata")
load("Output/my_dats_split.Rdata")


predictor_summary %>% 
  filter(Screened=="Pass") %>%
  nrow()

final_rf_unrevised_preds<-final_rf_unrevised %>% caret::predictors()
# 
# best_rf_predz<-c("Richness", "PctShading", "TotalAbundance", "bmiabund_score", 
#                  "hydrophytes_present_noflag", "algabund_score", "EPT_reltaxa", 
#                  "alglive_cover_score", "EPT_taxa")
best_rf_predz<-final_rf_unrevised %>% caret::predictors()
setdiff(final_rf_unrevised_preds, best_rf_predz)

best_rf_predz2<-c(
  #Riparian veg
  "PctShading",
  "hydrophytes_present_noflag",
  #other veg
 
  #BMI
  "Richness",
  "TotalAbundance", "bmiabund_score", 
  # "EPT_reltaxa","EPT_taxa",
  #Algae
  "algabund_score", "alglivedead_cover_score",
  #verts
  NULL
  )

setdiff(final_rf_unrevised_preds, best_rf_predz2)


ggplot(data=mydf, aes(x=DetFinal.f, y=alglivedead_cover_score))+
  geom_boxplot(aes(fill=wet))
ggplot(data=mydf, aes(x=DetFinal.f, y=alglive_cover_score))+
  geom_boxplot(aes(fill=wet))


predictor_summary %>% filter(Predictor=="alglivedead_cover_score")
#I am reverse engineering this RF

fru_train<-mod_dats_split[[1]] %>% training() %>% select(DetFinal.f, all_of(best_rf_predz))
fru_test<-mod_dats_split[[1]] %>% testing()
set.seed(21)
fru2<-randomForest(DetFinal.f~.,
                   data=fru_train, ntree=1500,
                   importance=T, proximity=T)

# vip(final_rf_unrevised, geom="point")

best_predictor_summary <-predictor_summary %>%
  filter(Predictor %in% best_rf_predz) 

ggplot(data=best_predictor_summary, aes(x=PvIwet_t_abs, y=EvIdry_t_abs))+
  geom_point() + 
  geom_label(aes(label=Predictor))



fru_imp<-final_rf_unrevised$importance %>%
  as.data.frame() %>%
  mutate(Predictor=row.names(final_rf_unrevised$importance)) %>%
  left_join(predictor_summary ) %>%
  arrange(MeanDecreaseAccuracy )

fru_imp$Predictor<-factor(fru_imp$Predictor, levels=fru_imp$Predictor)

ggplot(fru_imp, aes(x=Predictor, y=MeanDecreaseAccuracy))+
  geom_point(aes(color=MetricType))+
  coord_flip()+
  xlab("")+ylab("Importance (mean decrease accuracy)")+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  theme(legend.position = "bottom")
  
vip(final_rf_unrevised)


predictor_summary %>%
  filter(PredGroup!="StreamCat") %>%
  arrange(-PvIwet_t_abs) %>% 
  head(20)

# junk<-mydf2 %>% filter(Region=="ASW")
# ggplot(data=junk, aes(x=DetFinal.f, y=perennial_taxa))+
#   geom_boxplot(aes(fill=wet))+
#   scale_fill_brewer(palette="Set1", name="Water present?", labels=c("Dry","Wet"))+
#   theme_classic()

pred_red_fru<-predictor_redundancy %>%
  filter(term %in% best_rf_predz & variable %in% best_rf_predz & term!=variable) %>%
  mutate(rhosq = value^2)

pred_red_fru %>%
  group_by(rowname) %>%
  top_n(1,rhosq)
  

splom_df<-mydf %>%
  select(globalid, DetFinal.f, all_of(best_rf_predz2))

library(GGally)



my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    # geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

# ggpairs(data=splom_df,
#         columns = (best_rf_predz2),
#         lower = list(continuous = my_fn))
# 
# ggpairs(data=splom_df,
#         columns = (best_rf_predz2[4:10]),
#         lower = list(continuous = my_fn))

pred_red_fru %>%
  filter(term=="hydrophytes_present_noflag" & variable=="PctShading")

ggplot(data=mydf, aes(x=hydrophytes_present_noflag, y=PctShading))+
  geom_point(aes(color=DetFinal.f))+
  scale_color_brewer(palette="Set1", name="")+
  geom_smooth(method=lm)+
  theme_bw()+
  xlab("# hydrophyte species")+
  ylab("% shading")+
  theme(legend.position = "bottom")+
  annotate("text", x = 7.5, y = 100,
           label = "paste(italic(Rho) ^ 2, \" = .16\")", parse = TRUE)
splom_df2 <-mydf %>%
  select(DetFinal.f, Num_hydrophytes=hydrophytes_present_noflag, PctShading, wet) %>%
  pivot_longer(cols=c(-DetFinal.f, -wet), names_to="varz", values_to="valz")
ggplot(data= splom_df2, aes(x=DetFinal.f, y=valz))+
  geom_boxplot(aes(fill=wet))+
  facet_wrap(~varz, scales="free")+
  scale_fill_brewer(palette="Paired", name="Water present?", labels=c("No","Yes"))+
  theme_bw()+xlab("")+ylab("")


hydro_pdp<-fru2 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "hydrophytes_present_noflag") %>%
  autoplot(smooth = F, ylab = expression(f(hydrophytes_present_noflag))) +
  theme_light() 
totabund_pdp<-fru2 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "TotalAbundance") %>%
  autoplot(smooth = F, ylab = expression(f(TotalAbundance))) +
  theme_light() 

library(ggpubr)
partial_dependence_plots<-ggarrange(hydro_pdp,
          totabund_pdp, 
          labels=c("A","B"), nrow=1)
ggsave(partial_dependence_plots, filename="Figures/partial_dependence_plots.jpg", height=4, width=6, dpi=300)

partial_dependence_summary<-
  data.frame(Indicator="hydrophytes_present_noflag",
             Result=  partial(fru2, pred.var = "hydrophytes_present_noflag")$hydrophytes_present_noflag,
             yhat=partial(fru2, pred.var = "hydrophytes_present_noflag")$yhat) %>%
  bind_rows(data.frame(Indicator="TotalAbundance",
                       Result=  partial(fru2, pred.var = "TotalAbundance")$TotalAbundance,
                       yhat=partial(fru2, pred.var = "TotalAbundance")$yhat) )

ggplot(data=partial_dependence_summary, aes(x=Result, y=yhat))+
  geom_path()+
  facet_wrap(~Indicator, scales="free")+
  xlab("Indicator measurement")+
  ylab("f(Indicator measurement)")



partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="hydrophytes_present_noflag")
#Make a presence-absence version and a 3 or more species version
mydf$hydrophytes_pa<-ifelse(mydf$hydrophytes_present_noflag>0,1,0)
mydf$hydrophytes_3pa<-ifelse(mydf$hydrophytes_present_noflag>=3,1,ifelse(mydf$hydrophytes_present_noflag>=1,0.5,0))
mydf_revisits$hydrophytes_pa<-ifelse(mydf_revisits$hydrophytes_present_noflag>0,1,0)
mydf_revisits$hydrophytes_3pa<-ifelse(mydf_revisits$hydrophytes_present_noflag>=3,1,ifelse(mydf_revisits$hydrophytes_present_noflag>=1,0.5,0))


partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="PctShading")

partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="Richness")
fru2 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Richness") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Richness))) +
  theme_light() +
  geom_point()+
  scale_x_continuous(breaks=c(0:5,10,15,20))
#Possibly reducible to pa, or 5 or more taxa
#PA is redundant with abundance measures
mydf$BMItaxa_5pa<-ifelse(mydf$Richness>=5,1,ifelse(mydf$Richness>=1,0.5,0))
mydf_revisits$BMItaxa_5pa<-ifelse(mydf_revisits$Richness>=5,1,ifelse(mydf_revisits$Richness>=1,0.5,0))

partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="TotalAbundance")
fru2 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "TotalAbundance") %>%
  autoplot(smooth = TRUE, ylab = expression(f(TotalAbundance))) +
  theme_light() +
  geom_point()+
  scale_x_continuous(breaks=c(0,1,3,seq(33,40,4)))
#Make into pa, and >30
mydf$BMI_pa<-ifelse(mydf$TotalAbundance>=1,1,0)
mydf$BMI_20<-ifelse(mydf$TotalAbundance>=20,1,ifelse(mydf$TotalAbundance>=1,0.5,0))
mydf_revisits$BMI_pa<-ifelse(mydf_revisits$TotalAbundance>=1,1,0)
mydf_revisits$BMI_20<-ifelse(mydf_revisits$TotalAbundance>=20,1,ifelse(mydf_revisits$TotalAbundance>=1,0.5,0))



partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="bmiabund_score")
#Drop
partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="EPT_taxa")
fru2 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "EPT_taxa") %>%
  autoplot(smooth = TRUE, ylab = expression(f(EPT_taxa))) +
  theme_light() +
  geom_point()
#Can reduce to pa, but relationship looks pretty smooth and continuous
#Suggest retaining unless pa is really good
mydf$EPT_pa<-ifelse(mydf$EPT_taxa>=1,1,0)
mydf_revisits$EPT_pa<-ifelse(mydf_revisits$EPT_taxa>=1,1,0)

partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="EPT_reltaxa")
#redundant, drpo?

# fru2 %>%  # the %>% operator is read as "and then"
#   partial(pred.var = "mayfly_abundance") %>%
#   autoplot(smooth = TRUE, ylab = expression(f(mayfly_abundance))) +
#   theme_light() +
#   geom_point()+
#   scale_x_continuous(breaks=c(0:6,10,15,20,25))
#Consider pa and gt6 for consistency with pnw
# mydf$mayfly_pa_gt6<-ifelse(mydf$mayfly_abundance>=6,1,ifelse(mydf$mayfly_abundance>=1,0.5,0))

partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="algabund_score")
partialPlot(final_rf_unrevised, pred.data=mydf,
            x.var="alglive_cover_score")
#Presence of any live  algae!
mydf$livealg_pa<-ifelse(mydf$alglive_cover_score>=1,1,0)
mydf_revisits$livealg_pa<-ifelse(mydf_revisits$alglive_cover_score>=1,1,0)
#Try adding dead just in case. I didn't really analyze this the first time....
mydf$livedeadalg_pa<-ifelse(mydf$alglivedead_cover_score>=1,1,0)
mydf_revisits$livedeadalg_pa<-ifelse(mydf_revisits$alglivedead_cover_score>=1,1,0)

fru2 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "alglive_cover_score") %>%
  autoplot(smooth = TRUE, ylab = expression(f(algabund_score))) +
  theme_light() +
  geom_point()


new_varz<-
  c("PctShading", #keep
    # "hydrophytes_present_noflag",
    "hydrophytes_pa","hydrophytes_3pa", #one of these?
    # "Richness",
    "BMItaxa_5pa", #Yes or no, are there at least 5 taxa?
    # "TotalAbundance", Requires full-count
    "BMI_pa","BMI_20", #alternatives
    # "bmiabund_score", #reject because it's not an easily standardized indicator
    # "EPT_reltaxa", #requires full count of a sample
    # "EPT_taxa", #requires family level IDs
    "EPT_pa", #Only requires order level IDs
    # "algabund_score",  #Not easily standardized
    "alglive_cover_score", #ok to retain at first go-through
    "livedeadalg_pa" #easy alternative
    )
library(skimr)
mydf %>%
  select(DetFinal.f, all_of(new_varz)) %>%
  skim()
set.seed(70220)
mydata_split<-mydf%>%
  filter(Region=="ASW" & DetFinal.f %in% c("E","I","P") & Visit_No==1) %>%
  mutate(split_strat=paste(Stratum, DetFinal.f)) %>%
  initial_split(prop=4/5, strata=split_strat)

mydata_train<-mydata_split %>% training() %>% select(globalid,collection_date,Visit_No, SITECODE, wet, Region, Stratum_ASW, DetFinal.f, all_of(new_varz))
mydata_test<-mydata_split %>% testing() %>% select(globalid, collection_date,Visit_No,SITECODE, wet, Region, Stratum_ASW, DetFinal.f, all_of(new_varz))
skimr::skim(mydata_train)

final_rf_unrevised
varImpPlot(final_rf_unrevised)
final_rf_unrevised$importance


set.seed(21)
frf_1<- #
  randomForest(DetFinal.f~PctShading + hydrophytes_pa + hydrophytes_3pa +BMItaxa_5pa +BMI_pa+ BMI_20+ EPT_pa+ alglive_cover_score + livedeadalg_pa,
               data=mydata_train, ntree=1500,
               importance=T, proximity=T)
varImpPlot(frf_1)
frf_1 %>%  # the %>% operator is read as "and then"
  partial(pred.var = "BMI_20") %>%
  autoplot(smooth = TRUE, ylab = expression(f(hydrophytes_3pa))) +
  theme_light() +
  geom_point()

set.seed(21)
frf_2<- #
  randomForest(
    # This one should be the same as unsimplified
    # DetFinal.f~Richness+PctShading+TotalAbundance+bmiabund_score+hydrophytes_present_noflag+algabund_score+EPT_reltaxa+alglive_cover_score+EPT_taxa,
    DetFinal.f~hydrophytes_3pa + EPT_pa+ BMI_20+ livedeadalg_pa,
    
               data=mydata_train, ntree=1500,
               importance=T, proximity=T)
varImpPlot(frf_2)

predict(frf_2, type="prob")

my_frf2_prediction<-mydata_train %>%
  select(globalid, SITECODE, collection_date,DetFinal.f, Visit_No,Region, Stratum=Stratum_ASW, wet) %>%
  mutate(Prediction=predict(frf_2) %>% as.character(),
         ModName="FRF2", ModName2="FRF2",ModType="FRF2",Set="Training",
         CorrectPrediction=Prediction==DetFinal.f) %>%
  bind_rows(
    mydata_test %>%
      select(globalid, SITECODE, collection_date, DetFinal.f, Visit_No,Region, Stratum=Stratum_ASW, wet) %>%
      mutate(Prediction=predict(frf_2, newdata=mydata_test)%>% as.character(),
             ModName="FRF2", ModName2="FRF2",ModType="FRF2", Set="Testing",CorrectPrediction=Prediction==DetFinal.f))
my_frf2_prediction_revisits<-mydf_revisits %>%
  select(globalid, SITECODE, DetFinal.f, collection_date,Region,Visit_No, Stratum=Stratum_ASW, wet) %>%
  mutate(Prediction=predict(frf_2, newdata=mydf_revisits)%>% as.character(),
         ModName="FRF2", ModName2="FRF2",ModType="FRF2",Set="Revisit",CorrectPrediction=Prediction==DetFinal.f) %>%
  left_join(my_frf2_prediction %>% 
              select(SITECODE,ModName, ModName2,ModType, PreviousPrediction=Prediction)) %>%
  mutate(Consistent= Prediction==PreviousPrediction)

my_frf2_prediction_revisits%>%select(SITECODE, DetFinal.f, PreviousPrediction,Prediction,Consistent)

all_predictions<-read.csv("Output/all_predictions,csv", stringsAsFactors = F)
all_predictions_revisits<-read.csv("Output/all_predictions_revisits2,csv", stringsAsFactors = F)

final_predictions <- all_predictions %>%
  filter(ModType %in% c("RandomForest","PNW","NM") &
           ModName2 %in% c("Unstrat","PNW","NM")) %>%
  bind_rows(my_frf2_prediction)
skim(final_predictions)

final_predictions_revisits <- all_predictions_revisits %>%
  filter(ModType %in% c("RandomForest","PNW","NM") &
           ModName2 %in% c("Unstrat","PNW","NM")) %>%
  bind_rows(my_frf2_prediction_revisits)
skim(final_predictions_revisits)


####
final_mod_summary<-crossing(data.frame(ModName=c("FRF2","AAXT","PNW","NM"),
                            ModType=c("FRF2","RandomForest","PNW","NM")),
                            Set=c("Training","Testing"),
                            EvalStratum=mydf$Stratum_ASW %>% unique(),
                            Comparison=  c("PvIvE",
                                           "PvNP",
                                           "EvNE",#"EvNE_SIpnw","EvNE_SInm", "EvNE_SIfish",
                                           "EvNE_SIasw",
                                           "PvIwet",
                                           "EvIdry",
                                           "PnotE",
                                           "EnotP")
                            ) 


mydf$SingleInd_PNW<-case_when((mydf$fishabund_score2 + mydf$amphib_score + mydf$snake_score)>0~1,T~0)
mydf$SingleInd_NM<-case_when((mydf$fishabund_score2 + mydf$bmiabund_score)>0~1,T~0)
mydf_revisits$SingleInd_PNW<-case_when((mydf_revisits$fishabund_score2 + mydf_revisits$amphib_score + mydf_revisits$snake_score)>0~1,T~0)
mydf_revisits$SingleInd_NM<-case_when((mydf_revisits$fishabund_score2 + mydf_revisits$bmiabund_score)>0~1,T~0)
mydf$SingleInd_fish<-case_when((mydf$fishabund_score2)>0~1,T~0)
mydf_revisits$SingleInd_fish<-case_when((mydf_revisits$fishabund_score2)>0~1,T~0)

mydf$alglive_cover_score[which(mydf$algcover_live=="10 to 40%")]<-3
mydf$SingleInd_ASW<-case_when(mydf$fishabund_score2>0~1,
                              mydf$alglive_cover_score>2~1,
                              mydf$algdead_cover_score>2~1,
                              T~0)
mydf_revisits$SingleInd_ASW<-case_when(mydf_revisits$fishabund_score2>0~1,
                                       mydf_revisits$alglive_cover_score>2~1,
                                       mydf_revisits$algdead_cover_score>2~1,
                              T~0)
class_df<-mydf2 %>% 
  mutate(SDAM_AW_classification=
           case_when((hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==0)~"E",
                     (hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==1 & SingleInd_ASW==0)~"NMI",
                     (hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==1 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==0 & SingleInd_ASW==0)~"NMI",
                     (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==0 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==1 & SingleInd_ASW==0)~"NMI",
                     (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==1 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==1 )~"ALI",
                     
                     (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 0 & SingleInd_ASW==0)~"NMI",
                     (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 0 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 1 & SingleInd_ASW==0)~"NMI",
                     (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 1 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==1 )~"ALI",
                     (hydrophytes_3pa==.5 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW ==0)~"NMI",
                     (hydrophytes_3pa==.5 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW ==1)~"ALI",
                     (hydrophytes_3pa==.5 & BMI_20==0 & livedeadalg_pa==1 & SingleInd_ASW ==0)~"ALI",
                     (hydrophytes_3pa==.5 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==0)~"I",
                     (hydrophytes_3pa==.5 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==1)~"ALI",
                     (hydrophytes_3pa==.5 & BMI_20==.5 & EPT_pa==1 )~"I",
                     (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa==0)~"I",
                     (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa==1)~"ALI",
                     (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==1 & livedeadalg_pa==0)~"ALI",
                     (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==1 & livedeadalg_pa==1)~"I",
                     (hydrophytes_3pa==1 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==0)~"NMI",
                     (hydrophytes_3pa==1 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==1)~"ALI",
                     (hydrophytes_3pa==1 & BMI_20==0 & livedeadalg_pa==1)~"ALI",
                     (hydrophytes_3pa==1 & BMI_20==.5 & EPT_pa == 0)~"ALI",
                     (hydrophytes_3pa==1 & BMI_20==.5 & EPT_pa == 1)~"P",
                     (hydrophytes_3pa==1 & BMI_20==1 & EPT_pa == 0)~"ALI",
                     (hydrophytes_3pa==1 & BMI_20==1 & EPT_pa == 1)~"P",
                     T~"Error")) 
class_df%>%
  mutate(pnw_class_final=case_when(pnw_class_final=="error"~"E", T~pnw_class_final)) %>%
  # filter(pnw_class_final=="error") %>% select(hydrophytes_present_any,hydrophytes_3pa, TotalAbundance, mayfly_abundance, valleyslope)
  # filter(SDAM_AW_classification=="P" & DetFinal.f=="I" & !wet)
  # filter(SDAM_AW_classification=="Error") %>%  select(hydrophytes_3pa, BMI_20, EPT_pa, livedeadalg_pa, SingleInd_ASW) %>% unique()
  group_by(SDAM_AW_classification,  pnw_class_final ) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from=c(SDAM_AW_classification), values_from=n, values_fill=0) %>% write.table(file="clipboard", sep="\t", row.names=F)

class_df %>%
  group_by(SDAM_AW_classification, hydrophytes_3pa, BMI_20, EPT_pa, livedeadalg_pa, SingleInd_ASW) %>%
  tally()

class_df %>%
  # transmute(Class=paste0(SDAM_AW_classification,
  #                       "_Hyd.",hydrophytes_3pa,
  #                       "_BMI.",BMI_20,
  #                       "_EPT.",EPT_pa,
  #                       "_Alg.",livedeadalg_pa,
  #                       "_SI.",SingleInd_ASW)) %>%
  group_by(SDAM_AW_classification) %>%
  tally()

mydf2 %>% 
  filter(DetFinal.f=="E" & TotalAbundance>0)



final_mod_summary$n_tests<-sapply(1:nrow(final_mod_summary), function(i){
  modname.i=final_mod_summary$ModName[i]
  modtype.i=final_mod_summary$ModType[i]
  comp.i=final_mod_summary$Comparison[i]
  set.i=final_mod_summary$Set[i]
  evalstrat.i=final_mod_summary$EvalStratum[i]
  xdf<-final_predictions %>%
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
        left_join(mydf %>% select(globalid, SingleInd_PNW)) %>% na.omit()
      nrow(xdf)
    }
  else
    if(comp.i == "EvNE_SInm")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf %>% select(globalid, SingleInd_NM)) %>% na.omit()
      nrow(xdf)
    }
  else
    if(comp.i == "EvNE_SIasw")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf %>% select(globalid, SingleInd_ASW)) %>% na.omit()
      nrow(xdf)
    }
  else
    if(comp.i == "EvNE_SIfish")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf %>% select(globalid, SingleInd_fish)) %>% na.omit()
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


final_mod_summary$n_correct<-sapply(1:nrow(final_mod_summary), function(i){
  modname.i=final_mod_summary$ModName[i]
  modtype.i=final_mod_summary$ModType[i]
  comp.i=final_mod_summary$Comparison[i]
  set.i=final_mod_summary$Set[i]
  evalstrat.i=final_mod_summary$EvalStratum[i]
  xdf<-final_predictions %>%
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
        left_join(mydf %>% select(globalid, SingleInd_PNW)) %>%
        na.omit() %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"& SingleInd_PNW==0~"E" ,T~"NE"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "EvNE_SInm")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf %>% select(globalid, SingleInd_NM)) %>%
        na.omit() %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"  & SingleInd_NM==0~"E",T~"NE"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "EvNE_SIasw")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf %>% select(globalid, SingleInd_ASW)) %>%
        na.omit() %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"  & SingleInd_ASW==0~"E",T~"NE"))
      sum(xdf$DetFinal.f==xdf$Prediction)
    }
  else
    if(comp.i == "EvNE_SIfish")
    {
      xdf<-xdf %>% filter(DetFinal.f %in% c("E","I", "P")) %>%
        left_join(mydf %>% select(globalid, SingleInd_fish)) %>%
        na.omit() %>%
        mutate(DetFinal.f=case_when(DetFinal.f=="E"~"E",T~"NE"),
               Prediction=case_when(Prediction=="E"  & SingleInd_fish==0~"E",T~"NE"))
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

final_mod_summary$PctCorrect<-final_mod_summary$n_correct/final_mod_summary$n_tests



final_mod_summary_modtype_allstrata<-final_mod_summary %>%
  group_by(ModName, ModType, Comparison, Set) %>%
  summarise(n_tests=sum(n_tests, na.rm=T),
            n_correct=sum(n_correct,na.rm=T)) %>%
  ungroup() %>%
  mutate(PctCorrect=n_correct/n_tests) 

final_mod_summary_modtype_allstrata %>%
  filter(ModName=="FRF2") %>%
  write.table(file="clipboard", sep="\t", row.names=F)

final_mod_summary_modtype_eachstratum<-final_mod_summary %>%
  group_by(ModName, ModType, Comparison, Set, EvalStratum) %>%
  summarise(n_tests=sum(n_tests, na.rm=T),
            n_correct=sum(n_correct, na.rm=T)) %>%
  ungroup() %>%
  mutate(PctCorrect=n_correct/n_tests) %>%
  na.omit()


final_mod_type_comp_plot.df<-final_mod_summary_modtype_allstrata %>%
  na.omit() %>%
  mutate(Metric=paste0("Accuracy (",Comparison,")")) %>%
  rename(MetricResult=PctCorrect,
         MetricCount=n_tests) %>%
  select(-Comparison, -n_correct) %>%
  bind_rows(
    final_predictions_revisits %>%
      group_by(ModName, ModType) %>%
      summarise(MetricCount=sum(!is.na(Consistent)),
                MetricResult = sum(Consistent,na.rm=T)/sum(!is.na(Consistent))
                # PctCorrectRevisits=sum(CorrectPrediction, na.rm=T)/sum(!is.na(CorrectPrediction))
      )%>%
      ungroup() %>%
      mutate(Metric="Repeatability", Set="Training")) 

final_mod_type_comp_plot.df$ModType2<-factor(final_mod_type_comp_plot.df$ModType, levels=c("FRF2","RandomForest","PNW","NM"))
levels(final_mod_type_comp_plot.df$ModType2)<-c("Simplified","Unsimplified","PNW","NM")
ggplot(data=final_mod_type_comp_plot.df, aes(x=ModType2, y=MetricResult))+
  geom_point(aes(group=Set, shape=Set, color=Set),
             position=position_jitterdodge(dodge.width = .75, jitter.width = .025, jitter.height = 0))+
  # scale_color_manual(values=c(brewer_pal(palette="YlGn")(6)[3:6],
  #                             brewer_pal(palette="OrRd")(6)[3:6],
  #                             brewer_pal(palette="BuPu")(6)[3:6]
  #                             # viridis_pal(option="A", end=.5, begin=0)(4),
  #                             # viridis_pal(option="B", end=1, begin=.5)(4)
  #                             # "#fc8d59","#e34a33","#b30000"
  # ))+
  scale_shape_manual(values=c(17,16))+
  scale_color_manual(values=c("gray50","black"))+
  # scale_size_continuous(range=c(0.5,2.5))+
  # scale_size_manual(values=c(0.5,1.5))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw()+
  ggtitle("Performance aggregated across strata")+
  coord_flip()

all_mod_type_comp_plot.df<-read_csv("Output/mod_type_comp_plot.df.csv") %>% 
  filter(!ModName2 %in% c("PNW2.rf","PNW2.rp")) %>%
  filter(!ModType %in% c("RpartScore")) %>%
  mutate(ModName2=str_replace(ModName2, "SC","GIS")) 
Final_All_Mod_Comparison_df<- final_mod_type_comp_plot.df %>%
  filter(ModName=="FRF2") %>%
  transmute(ModName2="SDAM AW", 
            ModType="SDAM AW",
            Set=Set,MetricCount=MetricCount,MetricResult=MetricResult, Metric=Metric,
            Stratified=F, Unstratified=T, Metric2=Metric) %>%
  bind_rows(all_mod_type_comp_plot.df %>% select(-ModTypeName)) %>%
  mutate(Metric2=str_replace(Metric2, "EvNE","EvALI"),
         Metric2=str_replace(Metric2,"Consistency","Repeatability"),
         # Metric2=str_replace(Metric2,"EvALI_SIasw","EvALI_SI"),
         Metric=Metric2) %>%
  filter(!Metric %in% c("Accuracy (EvALI_SIpnw)","Accuracy (EvALI_SInm)","Accuracy (EvALI_SIasw)")) %>%
  mutate(Indicators = case_when(ModName2 %in% c( "Strat H2O", "Unstrat H2O")~"H2O",
                               ModName2 %in% c( "Strat H2O GIS", "Unstrat H2O GIS")~"H2O + GIS",
                               ModName2 %in% c( "Strat GIS", "Unstrat GIS")~"GIS",
                               T~"Other"))

head(all_mod_type_comp_plot.df)
head(Final_All_Mod_Comparison_df)

mypal1<-c("#f79c79","#ea4f88","#4b2991","#a6cee3")

Final_All_Mod_Comparison__unstratonly_plot<-
ggplot(data=Final_All_Mod_Comparison_df %>% filter(Unstratified), aes(x=ModType, y=MetricResult))+
  geom_point(aes(group=Set, shape=Set, fill=Indicators, color=Indicators), position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  geom_point(data=Final_All_Mod_Comparison_df %>%
               filter(ModName2 == "Unstrat" & ModType=="RandomForest"),size=2,
             aes(group=Set, shape=Set, fill=Indicators), position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  scale_fill_manual(values=mypal1)+
  scale_color_manual(values=mypal1, guide=F)+
  # scale_color_manual(values=c(brewer_pal(palette="YlGn")(6)[3:6],
  #                             brewer_pal(palette="OrRd")(6)[3:6],
  #                             brewer_pal(palette="BuPu")(6)[3:6]
  #                             # viridis_pal(option="A", end=.5, begin=0)(4),
  #                             # viridis_pal(option="B", end=1, begin=.5)(4)
  #                             # "#fc8d59","#e34a33","#b30000"
  # ))+
  scale_shape_manual(values=c(24,22))+
  # scale_color_manual(values=c("gray50","black"))+
  # scale_size_continuous(range=c(0.5,2.5))+
  # scale_size_manual(values=c(0.5,1.5))+
  facet_wrap(~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  scale_x_discrete(name="", labels=rev(c("SDAM AW", "Single Tree", "Random Forest", "PNW", "NM")))+
  theme_bw()+
  # ggtitle("Performance aggregated across strata")+
  coord_flip()+
  guides(fill = guide_legend(order=1,
                             override.aes=list(shape=21, size=2)),
         shape = guide_legend(order=2,
                              override.aes=list(fill="black")))

ggsave(Final_All_Mod_Comparison__unstratonly_plot, filename="Figures/Final_All_Mod_Comparison__unstratonly_plot.jpg", dpi=300,width=6.5, height=6)
Final_All_Mod_Comparison_df %>%
  select(-Metric2, -MetricCount) %>%
  pivot_wider(names_from=Metric, values_from=MetricResult) %>%
  write.csv(file="Output/Final_All_Mod_Comparison_df.csv", row.names=F)

final_mod_summary_modtype_eachstratum$ModType2<-factor(final_mod_summary_modtype_eachstratum$ModType, levels=c("FRF2","RandomForest","PNW","NM"))
levels(final_mod_summary_modtype_eachstratum$ModType2)<-c("Simplified","Unsimplified","PNW","NM")
ggplot(data=final_mod_summary_modtype_eachstratum, aes(x=ModType2, y=PctCorrect))+
  # geom_line(aes(group=paste(Set,ModType2)), color="gray",position=position_dodge(width = .75), size=1)+
  
geom_point(aes(group=Set, shape=Set, color=EvalStratum, size=Set), 
             position=position_dodge(width = .75))+
  
  scale_shape_manual(values=c(17,16))+
  facet_wrap(~Comparison)+
  scale_color_brewer(palette="Set1")+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  # ylab("")+
  scale_size_manual(values=c(1.5,3))+
  # scale_size_binned(n.breaks=3, breaks=c(1,5,10), range=c(0.5,3), trans="sqrt")+
  theme_bw()+
  ggtitle("Performance within each stratum")+
  coord_flip()




all_mod_type_comp_plot_eachstratum.df<-read_csv("Output/mod_summary_modtype_eachstratum.csv") %>%
  filter(!ModName2 %in% c("PNW2.rf","PNW2.rp")) %>%
  filter(!ModType %in% c("RpartScore")) %>%
  mutate(ModName2=str_replace(ModName2, "SC","GIS"),
         ModType2=ModType)
Final_All_Mod_Comparison_eachstratum_df<- final_mod_summary_modtype_eachstratum %>%
  filter(ModName=="FRF2") %>%
  transmute(ModName2="SDAM AW", 
            ModType="SDAM AW",
            Set=Set,n_tests=n_tests, Comparison=Comparison,n_correct=n_correct, PctCorrect=PctCorrect,EvalStratum=EvalStratum, ModType2=ModType) %>%
  bind_rows(all_mod_type_comp_plot_eachstratum.df) %>%
  mutate(Metric2=str_replace(Comparison, "EvNE","EvALI"),
         Metric2=str_replace(Metric2,"Consistency","Repeatability"),
         Metric2=paste0("Accuracy (",Metric2,")"),
         # Metric2=str_replace(Metric2,"EvALI_SIasw","EvALI_SI"),
         Metric=Metric2) %>%
  filter(!Metric %in% c("Accuracy (EvALI_SIpnw)","Accuracy (EvALI_SInm)","Accuracy (EvALI_SIasw)")) %>%
  mutate(Indicators = case_when(ModName2 %in% c( "Strat H2O", "Unstrat H2O")~"H2O",
                                ModName2 %in% c( "Strat H2O GIS", "Unstrat H2O GIS")~"H2O + GIS",
                                ModName2 %in% c( "Strat GIS", "Unstrat GIS")~"GIS",
                                T~"Other"),
         Stratified = ModName2 %in% c("Strat","Strat H2O","Strat GIS", "Strat H2O GIS"),
         Unstratified = !Stratified,
         ModType=case_when(ModType == "RandomForest" & Stratified~paste0("Random Forest (strat)"),
                           ModType == "RandomForest" & !Stratified~paste0("Random Forest (unstrat)"),
                           ModType == "Rpart" & Stratified~paste0("Single Tree (strat)"),
                           ModType == "Rpart" & !Stratified~paste0("Single Tree (unstrat)"),
                           T~ModType
                           ))
Final_All_Mod_Comparison_eachstratum_df$ModType<-
  factor(Final_All_Mod_Comparison_eachstratum_df$ModType, levels=rev(c("SDAM AW","Single Tree (strat)","Single Tree (unstrat)",
                                                                       "Random Forest (strat)", "Random Forest (unstrat)",
                                                                       "PNW","NM")))


Final_All_Mod_Comparison_eachstratum_df %>%
  select(ModName2, ModType, Indicators, Stratified, Set, EvalStratum, Comparison, n_tests, n_correct) %>%
  group_by(ModName2, ModType, Indicators, Stratified, EvalStratum,Comparison) %>%
  summarise(n_tests=sum(n_tests, na.rm=T), n_correct=sum(n_correct, na.rm=T)) %>%
  ungroup() %>%
  mutate(PctCorrect=n_correct/n_tests) %>%
  select(-n_correct) %>%
  # filter(ModName2=="SDAM AW" & Set=="Training" & Comparison=="EnotP" &EvalStratum=="AZ")
  pivot_wider(names_from=EvalStratum, values_from=c(n_tests, PctCorrect)) %>%
  write.csv(file="Output/Final_All_Mod_Comparison_eachstratum_df.csv", row.names=F)


Final_All_Mod_Comparison__unstratonly_bystrata_plot<-
  ggplot(data=Final_All_Mod_Comparison_eachstratum_df , aes(x=ModType, y=PctCorrect))+
  geom_point(aes(group=Set, shape=Set, fill=Indicators, color=Indicators), position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  geom_point(data=Final_All_Mod_Comparison_eachstratum_df %>%
               filter(ModName2 == "Unstrat" & ModType=="RandomForest"),size=2,
             aes(group=Set, shape=Set, fill=Indicators), position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  scale_fill_manual(values=mypal1)+
  scale_color_manual(values=mypal1, guide=F)+
  scale_shape_manual(values=c(24,22))+
  facet_grid(EvalStratum~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  xlab("")+
  theme_bw()+
  coord_flip()+
  guides(fill = guide_legend(order=1,
                             override.aes=list(shape=21, size=2)),
         shape = guide_legend(order=2,
                              override.aes=list(fill="black")))
Final_All_Mod_Comparison__unstratonly_bystrata_plot
ggsave(Final_All_Mod_Comparison__unstratonly_bystrata_plot + 
         theme(legend.position = "bottom"),
       filename="Figures/Final_All_Mod_Comparison__unstratonly_bystrata_plot.jpg",
       dpi=300, height=7,width=10)

Final_All_Mod_Comparison__stratified_bystrata_plot<-
  ggplot(data=Final_All_Mod_Comparison_eachstratum_df %>% filter(Unstratified | ModType %in% c("SDSAM AW","PNW","NM")), aes(x=ModType, y=PctCorrect))+
  geom_point(aes(group=Set, shape=Set, fill=Indicators, color=Indicators), position=position_jitterdodge(dodge.width = .75, jitter.width = 0, jitter.height = 0))+
  scale_fill_manual(values=mypal1)+
  scale_color_manual(values=mypal1, guide=F)+
  scale_shape_manual(values=c(24,22))+
  facet_grid(EvalStratum~Metric)+
  scale_y_continuous(name="Performance", limits=c(0,1), breaks=c(0,.5,1))+
  scale_x_discrete(name="", labels=rev(c("SDAM AW", "Single Tree", "Random Forest", "PNW", "NM")))+
  theme_bw()+
  coord_flip()+
  guides(fill = guide_legend(order=1,
                             override.aes=list(shape=21, size=2)),
         shape = guide_legend(order=2,
                              override.aes=list(fill="black")))
Final_All_Mod_Comparison__stratified_bystrata_plot
ggsave(Final_All_Mod_Comparison__stratified_bystrata_plot + 
         theme(legend.position = "bottom"),
       filename="Figures/Final_All_Mod_Comparison__stratified_bystrata_plot",
       dpi=300, height=6,width=9)

#####

rev_sites<-mydf_revisits$SITECODE
dyna_varz<-c(#"collection_date","Visit_No", "wet",
             new_varz)
revisit_review<-  mydf %>%
  filter(SITECODE %in% rev_sites) %>%
  select(SITECODE, Stratum_ASW, DetFinal.f, all_of(dyna_varz)) %>%
  pivot_longer(cols = dyna_varz, names_to="var", values_to="val1") %>%
  left_join(
    mydf_revisits %>%
      select(SITECODE, Stratum_ASW, DetFinal.f, all_of(dyna_varz)) %>%
      pivot_longer(cols = dyna_varz, names_to="var", values_to="val2") 
  ) %>%
  pivot_longer(cols=c(val1, val2), names_to="timepoint", values_to="value")

ggplot(revisit_review, aes(x=SITECODE, y=value))+
  geom_point(aes(fill=timepoint), shape=21, alpha=.5)+
  facet_wrap(~var, scales="free")+
  coord_flip()

revisit_review<-  mydf %>%
  filter(SITECODE %in% rev_sites) %>%
  select(SITECODE, Stratum_ASW, DetFinal.f, all_of(dyna_varz)) %>%
  pivot_longer(cols = dyna_varz, names_to="var", values_to="val1") %>%
  left_join(
    mydf_revisits %>%
      select(SITECODE, Stratum_ASW, DetFinal.f, all_of(dyna_varz)) %>%
      pivot_longer(cols = dyna_varz, names_to="var", values_to="val2")
  )

ggplot(revisit_review, aes(x=SITECODE, y=val1))+
  geom_segment(aes(xend=SITECODE, yend=val2))+
  geom_point( shape=21, fill="white")+
  geom_point(aes(y=val2), shape=21, fill="black")+
  facet_wrap(~var, scales="free")+
  coord_flip()


save(frf_2, file="Output/FinalRF.Rdata")
# save.image("asw_tool_finalization.Rdata")
# load("asw_tool_finalization.Rdata")


####
#Get probabilities of each class

my_frf2_prediction2<-mydata_train %>%
  select(globalid, SITECODE, collection_date,DetFinal.f, Visit_No,Region, Stratum=Stratum_ASW, wet) %>%
  mutate(Prediction=predict(frf_2) %>% as.character(),
         ModName="FRF2", ModName2="FRF2",ModType="FRF2",Set="Training",
         CorrectPrediction=Prediction==DetFinal.f) %>%
  bind_cols(
    predict(frf_2, type="prob") %>%      as.data.frame() 
  ) %>%
  bind_rows(
    mydata_test %>%
      select(globalid, SITECODE, collection_date, DetFinal.f, Visit_No,Region, Stratum=Stratum_ASW, wet) %>%
      mutate(Prediction=predict(frf_2, newdata=mydata_test)%>% as.character(),
             ModName="FRF2", ModName2="FRF2",ModType="FRF2", Set="Testing",CorrectPrediction=Prediction==DetFinal.f) %>%
    bind_cols(
      predict(frf_2, type="prob", newdata=mydata_test) %>%      as.data.frame() 
    )) %>%
  left_join(
    mydf %>%      transmute(globalid=globalid, FishSI=fishabund_score2>0, AlgSI=alglivedead_cover_score>1, Latitude=Latitude, Longitude=Longitude) %>% unique()
  )

my_frf2_prediction_revisits2<-mydf_revisits %>%
  select(globalid, SITECODE, DetFinal.f, collection_date,Region,Visit_No, Stratum=Stratum_ASW, wet) %>%
  mutate(Prediction=predict(frf_2, newdata=mydf_revisits)%>% as.character(),
         ModName="FRF2", ModName2="FRF2",ModType="FRF2",Set="Revisit",CorrectPrediction=Prediction==DetFinal.f) %>%
  left_join(my_frf2_prediction %>% 
              select(SITECODE,ModName, ModName2,ModType, PreviousPrediction=Prediction)) %>%
  mutate(Consistent= Prediction==PreviousPrediction) %>%
  bind_cols(
    predict(frf_2, type="prob", newdata=mydf_revisits) %>%      as.data.frame() 
  )  %>%
  left_join(
    mydf %>%
      transmute(globalid=globalid, FishSI=fishabund_score2>0, AlgSI=alglivedead_cover_score>1, Latitude=Latitude, Longitude=Longitude)
  )

my_frf2_alt<-
  mydata_train %>%
  select(globalid, SITECODE, collection_date,DetFinal.f, Visit_No,Region, Stratum=Stratum_ASW, wet) %>%
  mutate(Prediction=predict(frf_2) %>% as.character(),
         ModName="FRF2", ModName2="FRF2",ModType="FRF2",Set="Training",
         CorrectPrediction=Prediction==DetFinal.f) %>%
  bind_cols(
    predict(frf_2, type="prob") %>%      as.data.frame() 
  ) %>%
  bind_rows(
    mydata_test %>%
      select(globalid, SITECODE, collection_date, DetFinal.f, Visit_No,Region, Stratum=Stratum_ASW, wet) %>%
      mutate(Prediction=predict(frf_2, newdata=mydata_test)%>% as.character(),
             ModName="FRF2", ModName2="FRF2",ModType="FRF2", Set="Testing",CorrectPrediction=Prediction==DetFinal.f) %>%
      bind_cols(
        predict(frf_2, type="prob", newdata=mydata_test) %>%      as.data.frame() 
      )) %>%
  left_join(
    mydf %>%      transmute(globalid=globalid, FishSI=fishabund_score2>0, AlgSI=alglivedead_cover_score>1, Latitude=Latitude, Longitude=Longitude) %>% unique()
  )







head(my_frf2_prediction2)


plot_probs_dat<-my_frf2_prediction2 %>%
  arrange(-P, E) %>%
  mutate(SITECODE.f=factor(SITECODE, levels=SITECODE)) %>%
  pivot_longer(cols=c("E","I","P"), names_to="PredictedClass", values_to="PredictedProb") 

# mydf %>%
#   filter(globalid=="{D03B5D74-2808-4E12-8915-956250367862}") %>%
#   skimr::skim()
  # filter(SITECODE=="CAAW0419")
# 
ggplot(data=plot_probs_dat, aes(x=SITECODE.f, y=PredictedProb))+
  geom_bar(aes(fill=PredictedClass), position=position_stack(), stat="identity")+
  # facet_wrap(~DetFinal.f, scales="free")+
  coord_flip()+
  geom_hline(yintercept=c(0.25,0.5,0.75), linetype="dashed", color="black")+
  scale_fill_brewer(palette="RdYlBu")+
  theme(axis.text=element_text(size=6))

mincut<-.667
my_frf2_prediction2$pALI<-my_frf2_prediction2$I + my_frf2_prediction2$P
my_frf2_prediction2$TentClass<-
  case_when(my_frf2_prediction2$P>mincut~"P",
            my_frf2_prediction2$I>mincut~"I",
            my_frf2_prediction2$E>mincut~"E",
            my_frf2_prediction2$pALI>mincut~"ALI",
            T~"CBD")
my_frf2_prediction2$TentClass_majority<-
  case_when(my_frf2_prediction2$P>.5~"P",
            my_frf2_prediction2$I>.5~"I",
            my_frf2_prediction2$E>.5~"E",
            my_frf2_prediction2$pALI>.5~"ALI",
            T~"CBD")

my_frf2_prediction2$TentClass_SI<-
  case_when(my_frf2_prediction2$TentClass %in% c("E","CBD") &my_frf2_prediction2$FishSI ~"ALI",
            my_frf2_prediction2$TentClass %in% c("E","CBD") &my_frf2_prediction2$AlgSI ~"ALI",
            T~my_frf2_prediction2$TentClass)


preds_map_sf<-my_frf2_prediction2 %>%
  pivot_longer(cols=c(TentClass,TentClass_SI), names_to="Type", values_to="Determination") %>%
  st_as_sf( coords=c("Longitude","Latitude"),
            remove=F,
            crs=4326) %>%
  mutate(Type=case_when(Type=="TentClass"~"Core Indicators",T~"Core + Single Indicators"),
         Determination=case_when(Determination=="CBD"~"NMI",T~Determination),
         WOTUS_Correct=case_when(Determination %in% c("P","I","ALI") & DetFinal.f %in% c("P","I")~"Correct",
                                 Determination %in% c("E") & DetFinal.f %in% c("E")~"Correct",
                                 Determination == "NMI"~"No determination",
                                 T~"Incorrect"))
preds_map_sf$Determination<-factor(preds_map_sf$Determination, levels=c("P","I","E","ALI","NMI"))

ggplot()+
  geom_sf(data=study_area_sf, show.legend = F, fill="white")+
  geom_sf(data=study_area_sf %>% filter(Region=="ASW"), show.legend = F, color=NA, fill="gray80", alpha=.5)+
  # geom_sf(data=study_area_sf %>% filter(Region=="ASW"), aes(fill=Stratum))+
  # scale_fill_brewer(palette="Accent")+
  geom_sf(data=preds_map_sf, aes(color=WOTUS_Correct))+
  # facet_grid(Determination~Type)
  facet_grid(Type~Determination)+
  scale_color_brewer(palette="Set1", name="WOTUS\ndetermination")+
  theme(axis.text=element_blank(), panel.background = element_blank(),axis.ticks = element_blank(),
        legend.position = "bottom")

my_frf2_prediction2 %>%
  group_by(TentClass) %>%
  tally()

########
#######

cutpoint_summary<-my_frf2_prediction2 %>%
  select(ModName2, ModType) %>%
  unique() %>%
  crossing(mincut=seq(.3,1,.025),
           TentClass=c("P","I","E","ALI","CBD"))# %>%
  # mutate(wotusClass=case_when(TentClass %in% c("P","I","ALI")~"WOTUS",
                              # TentClass=="E"~"NonWOTUS",
                              # T~"CBD"))

cutpoint_summary$pct_classifications <-sapply(1:nrow(cutpoint_summary), function(i){
  # set.i<-cutpoint_summary$Set[i]
  modname.i<-cutpoint_summary$ModName2[i]
  modtype.i<-cutpoint_summary$ModType[i]
  mc.i<-cutpoint_summary$mincut[i]
  tent.i<-cutpoint_summary$TentClass[i]
  xdf<-my_frf2_prediction2 %>%
    filter(ModName2==modname.i & ModType==modtype.i ) %>%
    mutate(TentClass= case_when(P>=mc.i~"P",
                                I>=mc.i~"I",
                                E>=mc.i~"E",
                                pALI>=mc.i~"ALI",
                                T~"CBD"))
  y<-xdf %>% filter(TentClass==tent.i) %>% nrow()
  y/nrow(xdf)
})

cutpoint_summary$pct_Correct <-sapply(1:nrow(cutpoint_summary), function(i){
  # set.i<-cutpoint_summary$Set[i]
  modname.i<-cutpoint_summary$ModName2[i]
  modtype.i<-cutpoint_summary$ModType[i]
  mc.i<-cutpoint_summary$mincut[i]
  tent.i<-cutpoint_summary$TentClass[i]
  xdf<-my_frf2_prediction2 %>%
    filter(ModName2==modname.i & ModType==modtype.i ) %>%
    mutate(TentClass= case_when(P>=mc.i~"P",
                                I>=mc.i~"I",
                                E>=mc.i~"E",
                                pALI>=mc.i~"ALI",
                                T~"CBD"))
  if(tent.i!="ALI")
    y<-xdf %>% filter(TentClass==tent.i & DetFinal.f==tent.i) %>% nrow()
  else
    y<-xdf %>% filter(TentClass=="ALI" & DetFinal.f %in% c("P","I")) %>% nrow()
  y/nrow(xdf)
})

cutpoint_summary$pct_Incorrect <-sapply(1:nrow(cutpoint_summary), function(i){
  # set.i<-cutpoint_summary$Set[i]
  modname.i<-cutpoint_summary$ModName2[i]
  modtype.i<-cutpoint_summary$ModType[i]
  mc.i<-cutpoint_summary$mincut[i]
  tent.i<-cutpoint_summary$TentClass[i]
  xdf<-my_frf2_prediction2 %>%
    filter(ModName2==modname.i & ModType==modtype.i ) %>%
    mutate(TentClass= case_when(P>=mc.i~"P",
                                I>=mc.i~"I",
                                E>=mc.i~"E",
                                pALI>=mc.i~"ALI",
                                T~"CBD"))
  if(tent.i!="ALI")
    y<-xdf %>% filter(TentClass==tent.i & DetFinal.f!=tent.i) %>% nrow()
  else
    y<-xdf %>% filter(TentClass=="ALI" & !DetFinal.f %in% c("P","I")) %>% nrow()
  y/nrow(xdf)
})

cutpoint_summary %>% filter(TentClass==c("CBD"))
cutpoint_summary %>% group_by(mincut) %>% summarise(sum(pct_Correct))
cutpoint_summary %>% mutate(summa=pct_Correct + pct_Incorrect) %>% group_by(mincut) %>% summarise(sum(summa))
cutpoint_summary$TentClass<-factor(cutpoint_summary$TentClass, levels=rev(c("ALI","P","I","E","CBD")))

library(RColorBrewer)
pie_mc_classification_plot<-ggplot(data=cutpoint_summary, aes(x=mincut, y=pct_classifications))+
  scale_fill_manual(name="Determination",labels=c("Cannot be determined","Ephemeral","Intermittent","Perennial","At least intermittent"),
                    values=c("white",
                             brewer.pal(name="Blues", n=5)[2:4],
                             brewer.pal(name="YlOrRd", n=6)[3])  )+
  geom_area(aes(fill=TentClass), position=position_stack(), stat="identity")+
  geom_vline(xintercept=c(0.5,0.67,.75,.9,1), linetype="dashed")+
  xlab("Minimum proportion of votes for a determination")+
  ylab("% classified sites")
ggsave(pie_mc_classification_plot, filename="Figures/pie_mc_classification_plot.jpg", dpi=300, height=4, width=6.5)
  # facet_wrap(~Set)

cutpoint_summary_p<-cutpoint_summary %>%
  pivot_longer(cols=c(pct_Correct,pct_Incorrect)) %>%
  mutate(Outcome = case_when(name=="pct_Correct"~paste(TentClass,"Correct"),
                             T~paste(TentClass,"Incorrect"))         ) %>%
  filter(Outcome!="CBD Correct")

cutpoint_summary_p$Outcome<-factor(cutpoint_summary_p$Outcome, 
                                   levels=rev(c("ALI Correct","P Correct","I Correct","E Correct",
                                                "ALI Incorrect", "P Incorrect", "I Incorrect","E Incorrect",
                                                "CBD Incorrect")))
pie_pal<-c("white",
           brewer.pal(name="YlOrBr", n=5)[2:5],
           brewer.pal(name="Blues", n=5)[2:5])  
pie_mc_accuracy_plot<-ggplot(data=cutpoint_summary_p, aes(x=mincut, y=value))+
  scale_fill_manual(name="Determination",
                    labels=c("Need more information",
                             "Ephemeral (incorrect)","Intermittent (incorrect)","Perennial (incorrect)","At least intermittent (incorrect)",
                             "Ephemeral (correct)","Intermittent (correct)","Perennial (correct)","At least intermittent (correct)"),
                    values=pie_pal)+
  geom_area(aes(fill=Outcome ), position=position_stack(), stat="identity")+
  geom_vline(xintercept=c(0.5,0.67,.75,.9,1), linetype="dashed")+
  xlab("Minimum proportion of votes for a determination")+
  ylab("% classified sites (cumulative)")+
  theme_bw()+
  theme(panel.grid = element_blank())
pie_mc_accuracy_plot
ggsave(pie_mc_accuracy_plot, filename="Figures/pie_mc_accuracy_plot.jpg", dpi=300, height=4, width=6.5)

cutpoint_summary_wotus<-my_frf2_prediction2 %>%
  select(ModName2, ModType) %>%
  unique() %>%
  crossing(mincut=seq(.5,1,.025),
           wotusClass=c("WOTUS","NonWOTUS","CBD"))
# mutate(wotusClass=case_when(TentClass %in% c("P","I","ALI")~"WOTUS",
# TentClass=="E"~"NonWOTUS",
# T~"CBD"))

cutpoint_summary_wotus$pct_classifications <-sapply(1:nrow(cutpoint_summary_wotus), function(i){
  # set.i<-cutpoint_summary$Set[i]
  modname.i<-cutpoint_summary_wotus$ModName2[i]
  modtype.i<-cutpoint_summary_wotus$ModType[i]
  mc.i<-cutpoint_summary_wotus$mincut[i]
  tent.i<-cutpoint_summary_wotus$wotusClass[i]
  xdf<-my_frf2_prediction2 %>%
    filter(ModName2==modname.i & ModType==modtype.i ) %>%
    mutate(TentClass= case_when(P>=mc.i~"WOTUS",
                                I>=mc.i~"WOTUS",
                                E>=mc.i~"NonWOTUS",
                                pALI>=mc.i~"WOTUS",
                                T~"CBD"),
           DetFinal.f = case_when(DetFinal.f %in% c("I","P")~"WOTUS",
                                  T~"NonWOTUS"))
  y<-xdf %>% filter(TentClass==tent.i) %>% nrow()
  y/nrow(xdf)
})

cutpoint_summary_wotus$pct_Correct <-sapply(1:nrow(cutpoint_summary_wotus), function(i){
  # set.i<-cutpoint_summary$Set[i]
  modname.i<-cutpoint_summary_wotus$ModName2[i]
  modtype.i<-cutpoint_summary_wotus$ModType[i]
  mc.i<-cutpoint_summary_wotus$mincut[i]
  tent.i<-cutpoint_summary_wotus$wotusClass[i]
  xdf<-my_frf2_prediction2 %>%
    filter(ModName2==modname.i & ModType==modtype.i ) %>%
    mutate(TentClass= case_when(P>=mc.i~"WOTUS",
                                I>=mc.i~"WOTUS",
                                E>=mc.i~"NonWOTUS",
                                pALI>=mc.i~"WOTUS",
                                T~"CBD"),
           DetFinal.f = case_when(DetFinal.f %in% c("I","P")~"WOTUS",T~"NonWOTUS"))
  y<-xdf %>% filter(TentClass==tent.i & DetFinal.f==tent.i) %>% nrow()
  y/nrow(xdf)
})

cutpoint_summary_wotus$pct_Incorrect <-sapply(1:nrow(cutpoint_summary_wotus), function(i){
  # set.i<-cutpoint_summary$Set[i]
  modname.i<-cutpoint_summary_wotus$ModName2[i]
  modtype.i<-cutpoint_summary_wotus$ModType[i]
  mc.i<-cutpoint_summary_wotus$mincut[i]
  tent.i<-cutpoint_summary_wotus$wotusClass[i]
  xdf<-my_frf2_prediction2 %>%
    filter(ModName2==modname.i & ModType==modtype.i ) %>%
    mutate(TentClass= case_when(P>=mc.i~"WOTUS",
                                I>=mc.i~"WOTUS",
                                E>=mc.i~"NonWOTUS",
                                pALI>=mc.i~"WOTUS",
                                T~"CBD"),
           DetFinal.f = case_when(DetFinal.f %in% c("I","P")~"WOTUS",T~"NonWOTUS"))
  y<-xdf %>% filter(TentClass==tent.i & DetFinal.f!=tent.i) %>% nrow()
  y/nrow(xdf)
})

cutpoint_summary_wotus_p<-cutpoint_summary_wotus %>% 
  pivot_longer(cols=c(pct_Correct,pct_Incorrect)) %>%
  mutate(Outcome = case_when(name=="pct_Correct"~paste(wotusClass,"Correct"),
                             T~paste(wotusClass,"Incorrect"))         ) %>%
  filter(Outcome!="CBD Correct")


cutpoint_summary_wotus_p$Outcome<-factor(cutpoint_summary_wotus_p$Outcome,
                                         levels=rev(c("WOTUS Correct","NonWOTUS Correct",
                                                      "WOTUS Incorrect","NonWOTUS Incorrect",
                                                      "CBD Incorrect")))

wotus_mc_plot<-ggplot(data=cutpoint_summary_wotus_p, aes(x=mincut, y=value))+
  scale_fill_manual(name="Determination",
                    labels=c("Cannot be determined",
                             "Non-WOTUS (incorrect)","WOTUS (incorrect)",
                             "Non-WOTUS (correct)","WOTUS (correct)"),
                    values=c("white",
                             brewer.pal(name="YlOrBr", n=5)[c(2,4)],
                             brewer.pal(name="Blues", n=5)[c(2,4)])  )+
  geom_area(aes(fill=Outcome ), position=position_stack(), stat="identity")+
  geom_vline(xintercept=c(0.5,0.67,.75,.9,1), linetype="dashed")+
  xlab("Minimum proportion of votes for a determination")+
  ylab("% classified sites (cumulative)")
ggsave(wotus_mc_plot, filename="Figures/wotus_mc_plot.jpg", dpi=300, height=4, width=6.5)

########

mydf2<-mydf

mydf2$SingleInd_PNW<-case_when((mydf2$fishabund_score2 + mydf2$amphib_score + mydf2$snake_score)>0~1,T~0)
mydf2$SingleInd_NM<-case_when((mydf2$fishabund_score2 + mydf2$bmiabund_score)>0~1,T~0)
mydf2$SingleInd_fish<-case_when((mydf2$fishabund_score2)>0~1,T~0)
mydf2$SingleInd_bmi<-case_when((mydf2$bmiabund_score)>0~1,T~0)
mydf2$SingleInd_alg<-#case_when((mydf2$algabund_score)>0~1,T~0)
  case_when((mydf2$alglivedead_cover_score)>2~1,T~0)
mydf2$SingleInd_iofb<-case_when((mydf2$iofb_score)>0~1,T~0)
mydf2$SingleInd_hydric<-case_when((mydf2$hydric_score)>0~1,T~0)
mydf2$SingleInd_amphib<-case_when((mydf2$amphib_score)>0~1,T~0)
mydf2$SingleInd_snakes<-case_when((mydf2$snake_score)>0~1,T~0)
mydf2$SingleInd_turtles<-case_when((mydf2$snake_score)>0~1,T~0)
mydf2$SingleInd_verts<-case_when((mydf2$vert_score)>0~1,T~0)
mydf2$SingleInd_hydrophytes<-case_when((mydf2$hydrophytes_present_any)>0~1,T~0)
mydf2$SingleInd_hydrophytes_flag<-case_when((mydf2$hydrophytes_present_noflag)>0~1,T~0)
mydf2$SingleInd_peren<-case_when((mydf2$perennial_abundance)>0~1,T~0)
mydf2$SingleInd_liveperen<-case_when((mydf2$perennial_live_abundance)>0~1,T~0)

si_predictions<-my_frf2_prediction2 %>% 
  select(Region, ModName2, ModType, globalid, SITECODE, collection_date, DetFinal.f, Visit_No, Prediction=TentClass) %>%
  bind_rows(
    all_predictions %>%
      select(Region, ModName2, ModType, globalid, SITECODE, collection_date, DetFinal.f, Visit_No, Prediction) %>%
      filter(ModName2 %in% c("PNW","NM"))
  )



si_summary<-crossing(
  data.frame(
    ModType=c("FRF2","PNW","NM"),
    ModName2=c("FRF2","PNW","NM")
  ),
  # Set=c("Training","Testing"),
  SingleIndicator=c("PNW","NM","verts","fish", "snakes","turtles", "amphib","bmi", "peren","liveperen","hydrophytes","hydrophytes_flag","alg", "iofb","hydric")
) 
si_summary$n_ALI_true<-sapply(1:nrow(si_summary), function(i){
  modname.i=si_summary$ModName2[i]
  modtype.i=si_summary$ModType[i]
  # set.i=si_summary$Set[i]
  ind.i=paste0("SingleInd_",si_summary$SingleIndicator[i])
  
  xdf<-si_predictions %>%
    filter(Region=="ASW") %>%
    # filter(Set==set.i) %>%
    # filter(Stratum==evalstrat.i) %>%
    filter(ModName2==modname.i) %>%
    filter(ModType==modtype.i)
  xdf %>%
    filter(DetFinal.f %in% c("I","P")) %>%
    nrow()
  # print(paste(i, modname.i, modtype.i, set.i, nrow(xdf)))
})

si_summary$n_ALI_true_missed<-sapply(1:nrow(si_summary), function(i){
  modname.i=si_summary$ModName2[i]
  modtype.i=si_summary$ModType[i]
  # set.i=si_summary$Set[i]
  ind.i=paste0("SingleInd_",si_summary$SingleIndicator[i])
  
  xdf<-si_predictions %>%
    filter(Region=="ASW") %>%
    # filter(Set==set.i) %>%
    # filter(Stratum==evalstrat.i) %>%
    filter(ModName2==modname.i) %>%
    filter(ModType==modtype.i)
  xdf %>%
    filter(DetFinal.f %in% c("I","P")) %>%
    filter(Prediction=="E") %>%
    nrow()
  # print(paste(i, modname.i, modtype.i, set.i, nrow(xdf)))
})

si_summary$n_ALI_true_missed_has_SI<-sapply(1:nrow(si_summary), function(i){
  modname.i=si_summary$ModName2[i]
  modtype.i=si_summary$ModType[i]
  # set.i=si_summary$Set[i]
  ind.i=paste0("SingleInd_",si_summary$SingleIndicator[i])
  
  xdf<-si_predictions %>%
    filter(Region=="ASW") %>%
    # filter(Set==set.i) %>%
    # filter(Stratum==evalstrat.i) %>%
    filter(ModName2==modname.i) %>%
    filter(ModType==modtype.i) 
  ydf<-mydf2 %>%
    select(globalid, SI=all_of(ind.i))
  xdf<-left_join(xdf, ydf) 
  
  xdf %>%
    filter(DetFinal.f %in% c("I","P")) %>%
    filter(Prediction=="E" & 
             SI==1) %>%
    nrow()
  # print(paste(i, modname.i, modtype.i, set.i, nrow(xdf)))
})



si_summary$n_Eph_true<-sapply(1:nrow(si_summary), function(i){
  modname.i=si_summary$ModName2[i]
  modtype.i=si_summary$ModType[i]
  # set.i=si_summary$Set[i]
  ind.i=paste0("SingleInd_",si_summary$SingleIndicator[i])
  
  xdf<-si_predictions %>%
    filter(Region=="ASW") %>%
    # filter(Set==set.i) %>%
    # filter(Stratum==evalstrat.i) %>%
    filter(ModName2==modname.i) %>%
    filter(ModType==modtype.i)
  xdf %>%
    filter(DetFinal.f %in% c("E")) %>%
    nrow()
  # print(paste(i, modname.i, modtype.i, set.i, nrow(xdf)))
})

si_summary$n_Eph_true_notmissed<-sapply(1:nrow(si_summary), function(i){
  modname.i=si_summary$ModName2[i]
  modtype.i=si_summary$ModType[i]
  # set.i=si_summary$Set[i]
  ind.i=paste0("SingleInd_",si_summary$SingleIndicator[i])
  
  xdf<-si_predictions %>%
    filter(Region=="ASW") %>%
    # filter(Set==set.i) %>%
    # filter(Stratum==evalstrat.i) %>%
    filter(ModName2==modname.i) %>%
    filter(ModType==modtype.i)
  xdf %>%
    filter(DetFinal.f %in% c("E")) %>%
    filter(Prediction=="E") %>%
    nrow()
  # print(paste(i, modname.i, modtype.i, set.i, nrow(xdf)))
})

si_summary$n_Eph_true_notmissed_has_SI<-sapply(1:nrow(si_summary), function(i){
  modname.i=si_summary$ModName2[i]
  modtype.i=si_summary$ModType[i]
  # set.i=si_summary$Set[i]
  ind.i=paste0("SingleInd_",si_summary$SingleIndicator[i])
  
  xdf<-si_predictions %>%
    filter(Region=="ASW") %>%
    # filter(Set==set.i) %>%
    # filter(Stratum==evalstrat.i) %>%
    filter(ModName2==modname.i) %>%
    filter(ModType==modtype.i) 
  ydf<-mydf2 %>%
    select(globalid, SI=all_of(ind.i))
  xdf<-left_join(xdf, ydf) 
  xdf %>%
    filter(DetFinal.f %in% c("E")) %>%
    filter(Prediction=="E" & 
             SI==1) %>%
    nrow()
  # print(paste(i, modname.i, modtype.i, set.i, nrow(xdf)))
})

#How many ephemerals will falsely be called ALI, as a fraction of all ephemerals
si_summary$SI_FalsePos<-100*si_summary$n_Eph_true_notmissed_has_SI/si_summary$n_Eph_true

#How many ALIs will no longer be missed, as a fraction of all ALIs?
si_summary$SI_TruePos<-100*si_summary$n_ALI_true_missed_has_SI/si_summary$n_ALI_true
#How many ALIs will no longer be missed, as a fraction of missed ALIs?
si_summary$SI_TrueCorrections<-100*si_summary$n_ALI_true_missed_has_SI/si_summary$n_ALI_true_missed


si_summary_plot.df<-si_summary %>%
  # select(ModType, Set, SingleIndicator, FalsePos=SI_FalsePos, TruePos=SI_TruePos, Corrected = SI_TrueCorrections) %>%
  # pivot_longer(cols=c("FalsePos","TruePos","Corrected"))
  select(ModType, SingleIndicator, ErrorsCorrected=n_ALI_true_missed_has_SI,ErrorsIntroduced=n_Eph_true_notmissed_has_SI) %>%
  pivot_longer(cols=c(ErrorsCorrected,ErrorsIntroduced))

si_summary_plot.df$SingleIndicator<-
  factor(si_summary_plot.df$SingleIndicator,
         levels=rev(c("PNW","NM","verts","fish", "snakes","turtles", "amphib","bmi","peren","liveperen","hydrophytes","hydrophytes_flag","alg", "iofb","hydric")))

si_summary_plot.df$ModType2<-case_when(si_summary_plot.df$ModType=="FRF2"~"Simplified Random Forest",
                                       T~as.character(si_summary_plot.df$ModType))
si_summary_plot.df2<-si_summary_plot.df %>%
  filter(SingleIndicator %in% c("PNW","NM","verts","fish", "amphib","bmi","liveperen","alg","hydric"))
single_indicator_plot<-ggplot(data=si_summary_plot.df2, 
       aes(x=SingleIndicator, y=value))+
  geom_point(aes(fill=name, shape=name), size=3, position=position_dodge(width=.25))+
  geom_point(data=si_summary_plot.df2 %>% filter(name=="ErrorsCorrected"), aes(fill=name, shape=name), size=3, position=position_dodge(width=.25), alpha=.5)+
  facet_wrap(~ModType2, ncol=1)+
  coord_flip()+
  scale_fill_brewer(palette="Set2", name="Errors", labels=c("Corrected","Introduced"))+
  scale_size_manual(values=c(1.5, 3))+
  scale_shape_manual(values=c(24,25),name="Errors", labels=c("Corrected","Introduced"))+
  ylab("Number of errors corrected or introducted")+
  ggtitle("Influence of single indicators\nEphemeral vs. At least intermittent")+
  theme_bw()+
  scale_x_discrete(labels=rev(c("PNW","NM","Aquatic vertebrates","Fish","Amphibians","BMI (live)","Perennial indicator BMI (live)","Algae (live or dead)","Hydric soils")),
                   name="")
ggsave(single_indicator_plot, filename="Figures/single_indicator_plot.jpg", height=8, width=6, dpi=300)
########
#Head to head comparisons


method_predictions<-my_frf2_prediction2 %>% 
  select(Region, ModName2, ModType, Stratum, wet,globalid, SITECODE, collection_date, DetFinal.f, Visit_No, Prediction=TentClass) %>%
  bind_rows(
    all_predictions %>%
      select(Region, ModName2, ModType, Stratum, wet,globalid, SITECODE, collection_date, DetFinal.f, Visit_No, Prediction) %>%
      filter(ModName2 %in% c("NM"))
  ) %>%
  arrange(Stratum) %>%
  left_join(mydf %>%
              select(globalid, nm_class1, nm_score_final) %>%
              mutate(nm_class1x=case_when(nm_class1=="P"~"P",
                               nm_class1=="I"~"I",
                               nm_class1=="E"~"E",
                               nm_class1=="It"~"CBD",
                               nm_class1=="Pt"~"ALI",
                               T~NA_character_))) %>%
  mutate(Prediction2=case_when(ModName2=="NM"~nm_class1x,T~Prediction))

method_predictions$Prediction<-factor(method_predictions$Prediction,
                                      levels=c("P","ALI","I","E","CBD"))
method_predictions$Prediction<-factor(method_predictions$Prediction,
                                      levels=c("P","ALI","I","E","CBD"))

method_predictions$Prediction2<-factor(method_predictions$Prediction2,
                                      levels=c("P","ALI","I","CBD","E"))
levels(method_predictions$Prediction2)[4]<-"NMI"
table(method_predictions$Stratum, method_predictions$Prediction)



nm_asw_agreement_plot<-ggplot(data=method_predictions,
       aes(x=SITECODE, y=Prediction2))+
  geom_point(aes(shape=ModType, fill=ModType), size=2)+
  scale_shape_manual(values=c(24,25), name="Tool", labels=c("ASW","NM"))+
  scale_fill_manual(values=c("#e41a1c80","#377eb880"), name="Tool", labels=c("ASW","NM"))+
  facet_grid(DetFinal.f~., scales="free", space="free")+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y = element_text(size=8))+
  xlab("")+ylab("")+
  scale_y_discrete(labels=c("P","ALI/Pt","I","NMI/It","E"))


nm_asw_agreement_plot2<-ggplot(data=method_predictions %>% filter(ModType=="FRF2"),
                              aes(x=Prediction2, y=nm_score_final))+
  geom_rect(ymin=19, ymax=22, xmin=-Inf, xmax=Inf, fill="#cccccc50")+
  geom_rect(ymin=9, ymax=12, xmin=-Inf, xmax=Inf, fill="#cccccc50")+
  geom_boxplot(outlier.color = NA)+
  geom_point(position=position_jitterdodge(jitter.height=0, jitter.width=.1), size=1.5, aes(fill=DetFinal.f), shape=21)+
  xlab("AWS Interim Method Classification\n(core indicators)")+
  ylab("NM index scores")+
  geom_hline(yintercept=c(9, 12, 19, 22), linetype="dashed")+
  scale_fill_manual(values=brewer.pal(5,"Blues")[c(1,3,5)], name="True Class")
nm_asw_agreement_plot2
ggsave(nm_asw_agreement_plot2, filename="Figures/nm_asw_agreement_plot2.jpg",
       dpi=300, height=4, width=6)


junk<-method_predictions %>%
  left_join(
    mydf %>% select(globalid, SITECODE, sitename, sitecode_entry, algcover_dead, alglive_cover_score, algdead_cover_score,alglivedead_cover_score, source)
  ) %>%
  filter(ModType=="FRF2") %>%
  filter(Prediction %in% c("E", "CBD") & alglivedead_cover_score>0)

junk<-method_predictions %>%
  filter(ModType=="FRF2") %>%
  select(globalid, SITECODE, Region, Stratum, wet, collection_date, DetFinal.f, Visit_No, ASW=Prediction2, ASW2=Prediction) %>%
  left_join(method_predictions %>%
              filter(ModType=="NM") %>%
              select(globalid, SITECODE, Region, Stratum, wet, collection_date, DetFinal.f, Visit_No, NM=Prediction2, NM2=Prediction)
            )%>%
  mutate(Agree=ASW==NM,
         Agree2=ASW2==NM2,
         ASW.n=as.numeric(ASW),
         NM.n=as.numeric(NM),
         Difference=ASW.n-NM.n) %>%
  arrange(Stratum)

ggplot(data=junk, aes(x=Stratum, y=Difference))+
  geom_hline(yintercept = 0, linetype="dotted")+
  geom_point(aes(color=wet), position = position_jitter(height=0.025, width=.05), alpha=.75)+
  scale_color_brewer(palette="Set1")+
  facet_grid(DetFinal.f~.,  space="free")+
  scale_y_continuous(breaks=c(-3,0,2), labels=c("ASW > NM","Same","NM > ASW"))+
  xlab("")+ylab("")+
  ggtitle("Differences between ASW and NM predictions")+
  # coord_flip()+
  theme_bw()#+
  # theme(axis.text.y = element_blank())

junk2<-junk %>%
  # filter(wet) %>%
  group_by(DetFinal.f, Stratum, Agree) %>%
  tally() %>%
  pivot_wider(names_from = Agree, values_from=n)
names(junk2)[c(3:4)]<-c("Disagree","Agree")
junk2$Disagree[is.na(junk2$Disagree)]<-0
junk2$Agree[is.na(junk2$Agree)]<-0
junk2$PctAgree=100*junk2$Agree / (junk2$Agree+junk2$Disagree)



ggplot(data=junk2, aes(x=Stratum, y=PctAgree))   +
  geom_bar(stat="identity", aes(fill=DetFinal.f), position = position_dodge(), color="black")+
  scale_fill_brewer(palette="Blues", name="True class")+
  xlab("")+ylab("% agree")
  
  

#All possible outcomes

all_outcomes<-crossing(hydrophytes_3pa=c(0,.5,1),
                       BMI_20=c(0,.5,1),
                       EPT_pa=c(0,1),
                    livedeadalg_pa=c(0,1)) %>%
  filter(!(EPT_pa==1 & BMI_20==0)) %>%
  bind_cols(
    predict(frf_2, newdata = all_outcomes, type="prob") %>%  as.data.frame()
  ) %>%
  mutate(pALI=P+I,
         Determination = case_when(P>=0.667~"Perennial",
                                   I>=0.667~"Intermittent",
                                   E>=0.667~"Ephemeral",
                                   pALI>=0.667~"At least intermittent",
                                   T~"Need more information"),
         Case=row_number())


all_outcomes$n<-sapply(1:nrow(all_outcomes), function(i){
  hydro.i<-all_outcomes$hydrophytes_3pa[i]
  ept.i<-all_outcomes$EPT_pa[i]
  bmi.i<-all_outcomes$BMI_20[i]
  alg.i<-all_outcomes$livedeadalg_pa[i]
  xdf<-mydf %>%
    filter(hydrophytes_3pa==hydro.i & EPT_pa==ept.i & BMI_20==bmi.i & livedeadalg_pa==alg.i)
  nrow(xdf)
})

all_outcomes %>% 
  filter(n>0) %>%
  filter(Determination=="At least intermittent")

all_outcomes_p<-all_outcomes %>%
  transmute(Hydrophytes = hydrophytes_3pa,
         EPT=EPT_pa,
         BMI=BMI_20,
         Algae=livedeadalg_pa,
         Case=Case,
         Determination=Determination) %>%
  mutate(Determination2=0) %>%
  pivot_longer(cols=c(Hydrophytes,EPT, BMI, Algae, Determination2), names_to="Indicator") %>%
  mutate(value2=case_when(value==0~"Absent",
                          value==0.5~"Weak",
                          T~"Strong"), 
         Case2=paste(Determination, Case))
all_outcomes_p$value2<-factor(all_outcomes_p$value2, levels=c("Absent","Weak","Strong"))
all_outcomes_p$Indicator<-factor(all_outcomes_p$Indicator, levels=c("Hydrophytes","BMI","EPT","Algae","Determination2"))


ggplot(data=all_outcomes_p, aes(y=Case, x=Indicator))+
  geom_tile(aes(fill=value2), color="white")+
  scale_fill_brewer(palette="Greys", name="Strength of\nIndicator")+
  # coord_flip()+
  geom_text(data=all_outcomes_p%>% filter(Indicator=="Determination2"), aes(label=Determination), hjust=0) +
  theme_classic()
  


all_outcomes_p2<-all_outcomes %>%
  transmute(Hydrophytes = hydrophytes_3pa, E=E,I=I,P=P,n=n,
            EPT=EPT_pa,
            BMI=BMI_20,
            Algae=livedeadalg_pa,
            Case=Case,
            Determination=Determination) %>%
  mutate(HydroLab=case_when(Hydrophytes==0~"No plants", Hydrophytes==0.5~"Few plants", T~"Many plants"),
         BMILab=case_when(BMI==0~"No BMI", BMI==.5~"Few BMI",T~"Many BMI"),
         EPTLab=case_when(EPT==0~"No EPT",T~"Yes EPT"),
         AlgLab=case_when(Algae==0~"No Algae",T~"Yes Algae"),
         CaseLab=paste(HydroLab,BMILab, EPTLab,AlgLab, sep="-")) %>%
    pivot_longer(cols=c(E,I,P), names_to="Class", values_to="Votes") 

all_outcomes_p2$CaseLab<-factor(all_outcomes_p2$CaseLab, levels=all_outcomes_p2$CaseLab %>% unique())



ggplot(data=all_outcomes_p2, aes(x=CaseLab, y=Votes))+
  geom_bar(aes(fill=Class), position=position_stack(), stat="identity")+
  geom_bar(data=all_outcomes_p2 %>% filter(Determination==c("At least intermittent") ),
           aes(y=1), position=position_stack(), stat="identity", color="black", fill=NA)+
  # geom_bar(data=all_outcomes_p2 %>% filter(Determination==c("At least intermittent") ),
           # aes(group=Class), position=position_stack(), stat="identity", color="black", fill=NA)+
  
  
  scale_fill_brewer(palette="RdYlBu")+
  coord_flip()+
  xlab("")+
  scale_y_continuous("Proportion of votes", limits=c(-.05,1), breaks=c(0,0.25,.5,.75,1))+
  geom_hline(yintercept=c(0.333,0.667), linetype="dashed")+
  geom_hline(yintercept=c(0.5), linetype="dashed", color="gray")+
  geom_text(data=all_outcomes_p2 %>% filter(Class=="E"), aes(label=n), y=-0.05)+
  theme(legend.position = "bottom")



########
# save.image("tool.finalization.Rdata")
load("tool.finalization.Rdata")
write.csv(final_mod_summary_modtype_allstrata, file="Output/final_mod_summary_modtype_allstrata.csv", row.names=F)
write.csv(final_mod_summary_modtype_eachstratum, file="Output/final_mod_summary_modtype_eachstratum.csv", row.names=F)


table_6_data<-mydf %>% 
  select(globalid, Stratum, SITECODE, sitecode_entry, collection_date, DetFinal.f, wet, hydrophytes_3pa, BMI_20, EPT_pa, livedeadalg_pa, SingleInd_fish, alglive_cover_score, algdead_cover_score)  %>%
  mutate(Type = case_when(globalid %in% mydata_train$globalid~"Training",
                          globalid %in% mydata_test$globalid~"Testing",
                          globalid %in% mydf_revisits$globalid~"Testing",
                          T~"Other"),
         SingleInd_ASW=case_when(SingleInd_fish>0~1,
                                 alglive_cover_score>2~1,
                                 algdead_cover_score>2~1,
                                 T~0),
         Classification=case_when((hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==0)~"E",
                   (hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==1 & SingleInd_ASW==0)~"NMI",
                   (hydrophytes_3pa==0 & BMI_20==0 & livedeadalg_pa==1 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==0 & SingleInd_ASW==0)~"NMI",
                   (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==0 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==1 & SingleInd_ASW==0)~"NMI",
                   (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==1 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==.5 & EPT_pa==1 )~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 0 & SingleInd_ASW==0)~"NMI",
                   (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 0 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 1 & SingleInd_ASW==0)~"NMI",
                   (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa == 1 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==0 & BMI_20==1 & EPT_pa==1 )~"ALI",
                   (hydrophytes_3pa==.5 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW ==0)~"NMI",
                   (hydrophytes_3pa==.5 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW ==1)~"ALI",
                   
                   (hydrophytes_3pa==.5 & BMI_20==0 & livedeadalg_pa==1 & SingleInd_ASW ==0)~"ALI",
                   
                   (hydrophytes_3pa==.5 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==0)~"I",
                   (hydrophytes_3pa==.5 & BMI_20==.5 & EPT_pa==0 & livedeadalg_pa==1)~"ALI",
                   (hydrophytes_3pa==.5 & BMI_20==.5 & EPT_pa==1 )~"I",
                   (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa==0)~"I",
                   (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==0 & livedeadalg_pa==1)~"ALI",
                   (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==1 & livedeadalg_pa==0)~"ALI",
                   (hydrophytes_3pa==.5 & BMI_20==1 & EPT_pa==1 & livedeadalg_pa==1)~"I",
                   (hydrophytes_3pa==1 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==0)~"NMI",
                   (hydrophytes_3pa==1 & BMI_20==0 & livedeadalg_pa==0 & SingleInd_ASW==1)~"ALI",
                   (hydrophytes_3pa==1 & BMI_20==0 & livedeadalg_pa==1)~"ALI",
                   (hydrophytes_3pa==1 & BMI_20==.5 & EPT_pa == 0)~"ALI",
                   (hydrophytes_3pa==1 & BMI_20==.5 & EPT_pa == 1)~"P",
                   (hydrophytes_3pa==1 & BMI_20==1 & EPT_pa == 0)~"ALI",
                   (hydrophytes_3pa==1 & BMI_20==1 & EPT_pa == 1)~"P",
                   T~"Error")
         )
table_6_data %>% filter(Classification=="Error")

write.table(table_6_data,file="clipboard", sep="\t", row.names = F)
