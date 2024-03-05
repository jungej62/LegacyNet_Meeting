library(DImodels)
library(tidyverse)
library(readxl)
dw<-read_xlsx("data/LegacyNet_DataJJ.xlsx", sheet="Wide", na="NA")
str(dw)
library(vegan)
#Turn empty (NA) cover values to 0 for shandiv
zzz<-colnames(dw[c(17:22,27:32,37:42,47:52)])
dw2 <- dw %>% 
  dplyr::mutate_at(zzz, ~replace_na(., 0))

dw2$H_cut1<-diversity(dw2[,17:22])
dw2$H_cut2<-diversity(dw2[,27:32])
dw2$H_cut3<-diversity(dw2[,37:42])
dw2$H_cut4<-diversity(dw2[,47:52])
str(dw2)

dwl<-read_xlsx("data/LegacyNet_DataJJ.xlsx", sheet="Long", na=".", skip=1)
dwl$ShanDiv<-c(dw2$H_cut1,
                   dw2$H_cut2,
                   dw2$H_cut3,
                   dw2$H_cut4,
               rep(NA,104))
dwl$SR<-c(dw2$SR_cut1,
               dw2$SR_cut2,
               dw2$SR_cut3,
               dw2$SR_cut4,
               rep(NA,104))
dwl$system<-c(rep("ley",208),rep("follow",104))
dwl %>% 
  filter(system=="ley") %>% 
  ggplot(aes(x=SR, y=DMYield, color=as.factor(HarvestN)))+
  geom_point()+
  geom_smooth(se=F)
dwl %>% 
  filter(system=="ley") %>% 
  ggplot(aes(x=ShanDiv, y=DMYield, color=as.factor(HarvestN)))+
  geom_point()+
  geom_smooth(se=F)

dwl %>% 
  filter(system=="ley") %>% 
  pivot_longer(G1vis:Weedvis, )
  ggplot(aes(x=ShanDiv, y=DMYield, color=as.factor(HarvestN)))+
  geom_point()+
  geom_smooth(se=F)

#Follow on cut yields
dw2 %>% 
  group_by(RichnessSP) %>% 
  summarise(sd_fc1 = sd(Yld_fc1, na.rm=T), 
            n_fc1 = n(),
            g_fc1 = mean(Yld_fc1, na.rm=T)) %>% #order matters if you want to keep the g_ry column name, must do mean after sd
  mutate(se_fc1=sd_fc1/sqrt(n_fc1)) %>%
  ggplot(aes(x=RichnessSP, y=g_fc1))+
  geom_bar(stat="identity", show.legend = F, width=.5)+
  geom_errorbar(aes(ymin=g_fc1-se_fc1, ymax=g_fc1+se_fc1),
                stat="identity", show.legend = F, width=.5)+
  ggtitle("Fallow-on cut yield 1")
