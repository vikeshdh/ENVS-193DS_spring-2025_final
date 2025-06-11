setwd("D:/R/starlings in swifty boxes")
dir()
boxes<-read.csv("occdist.csv", header=TRUE)
str(boxes)
boxes<-factor(data$sp)
boxes$season<-factor(boxes$season)
str(boxes)


library(tidyverse)
library(MASS)
library(lmerTest)
library(emmeans)
library(car)
library(dabestr)
library(wesanderson)
library(ggeffects)


table(boxes$box.occupant,boxes$season)

#swift parrot
spn<-glm(sp~1, family=binomial(), data=boxes)
sp1<-glm(sp~edge.distance, family=binomial(),data=boxes)
sp2<-glm(sp~season, family=binomial(),data=boxes)
sp3<-glm(sp~edge.distance*season,family=binomial(), data=boxes)#best
sp4<-glm(sp~edge.distance+season,family=binomial(), data=boxes)

AIC(spn,sp1,sp2,sp3,sp4)
Anova(sp4)
summary(sp4)

dat =  ggpredict(sp4, terms = c("edge.distance", "season"))
plot(dat, color=c("red", "blue")) + 
  labs(x = "Distance from the forest edge (m)",y = "Probability of box occupancy") + 
  theme_classic() + 
  #geom_point(data=boxes, aes(x=edge.distance, y=sp), inherit.aes = FALSE, color=sp)  + 
  #geom_point(data=subset(trees, DBH =="> 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='gray1') +
  labs(title = "swift parrot")

datint =  ggpredict(sp3, terms = c("edge.distance", "season"))
plot(datint, color=c("red", "blue")) + 
  labs(x = "Distance from the forest edge (m)",y = "Probability of box occupancy") + 
  theme_classic() + 
  #geom_point(data=boxes, aes(x=edge.distance, y=sp), inherit.aes = FALSE, color=sp)  + 
  #geom_point(data=subset(trees, DBH =="> 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='gray1') +
  labs(title = "swift parrot")
  
  #common starling
csn<-glm(cs~1, family=binomial(), data=boxes)
cs1<-glm(cs~edge.distance, family=binomial(),data=boxes)
cs2<-glm(cs~season, family=binomial(),data=boxes)
cs3<-glm(cs~edge.distance*season,family=binomial(), data=boxes)#best
cs4<-glm(cs~edge.distance+season,family=binomial(), data=boxes)

AIC(csn,cs1,cs2,cs3,cs4)
Anova(cs4)
summary(cs4)

dat2 =  ggpredict(cs4, terms = c("edge.distance", "season"))
plot(dat2, color=c("red", "blue")) + 
  labs(x = "Distance from the forest edge (m)",y = "Probability of box occupancy") + 
  theme_classic() + 
  #geom_point(data=subset(trees, DBH =="< 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='blue1')  + 
  #geom_point(data=subset(trees, DBH =="> 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='gray1') +
  labs(title = "common starling")


dat2int =  ggpredict(cs3, terms = c("edge.distance", "season"))
plot(dat2int, color=c("red", "blue")) + 
  labs(x = "Distance from the forest edge (m)",y = "Probability of box occupancy") + 
  theme_classic() + 
  #geom_point(data=subset(trees, DBH =="< 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='blue1')  + 
  #geom_point(data=subset(trees, DBH =="> 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='gray1') +
  labs(title = "common starling")


#tree martin
tmn<-glm(tm~1, family=binomial(), data=boxes)
tm1<-glm(tm~edge.distance, family=binomial(),data=boxes)
tm2<-glm(tm~season, family=binomial(),data=boxes)
tm3<-glm(tm~edge.distance*season,family=binomial(), data=boxes)#best
tm4<-glm(tm~edge.distance+season,family=binomial(), data=boxes)

AIC(tmn,tm1,tm2,tm3,tm4)
Anova(tm3)
summary(tm3)

dat3 =  ggpredict(tm3, terms = c("edge.distance", "season"))
plot(dat3, color=c("red", "blue")) + 
  labs(x = "Distance from the forest edge (m)",y = "Probability of box occupancy") + 
  theme_classic() + 
  #geom_point(data=subset(trees, DBH =="< 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='blue1')  + 
  #geom_point(data=subset(trees, DBH =="> 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='gray1') +
  labs(title = "tree martin")


#empty
en<-glm(e~1, family=binomial(), data=boxes)
e1<-glm(e~edge.distance, family=binomial(),data=boxes)
e2<-glm(e~season, family=binomial(),data=boxes)
e3<-glm(e~edge.distance*season,family=binomial(), data=boxes)#best
e4<-glm(e~edge.distance+season,family=binomial(), data=boxes)

AIC(en,e1,e2,e3,e4)
Anova(tm3)
summary(tm3)

dat4 =  ggpredict(e4, terms = c("edge.distance", "season"))
plot(dat4, color=c("red", "blue")) + 
  labs(x = "Distance from the forest edge (m)",y = "Probability of box occupancy") + 
  theme_classic() + 
  #geom_point(data=subset(trees, DBH =="< 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='blue1')  + 
  #geom_point(data=subset(trees, DBH =="> 110 cm"), aes(x=climbentry, y=groundcount), inherit.aes = FALSE, color='gray1') +
  labs(title = "empty")



library(janitor)
summary(boxes)
ggplot(boxes, aes(x=edge.distance))+
  geom_density()+
  facet_wrap(~season)
sort(boxes$edge.distance)
boxes$distanceF <-  ifelse(boxes$edge.distance <500,0,1) %>%
  factor(labels = c("less than 500m","more than 500m"))
tabyl(boxes, box.occupant, season, distanceF) %>%
  adorn_percentages("col")



# ####Ordinal regression example below but can't use as this isn't ordered response variables####
# sn<-polr(factor(box.occupant)~1, data=boxes)
# s1<-polr(factor(box.occupant)~edge.distance, data=boxes)
# s2<-polr(factor(box.occupant)~season, data=boxes)
# s3<-polr(factor(box.occupant)~edge.distance*season, data=boxes)#best
# s4<-polr(factor(box.occupant)~edge.distance+season, data=boxes)
# 
# AIC(sn,s1,s2,s3,s4)
# Anova(s3)
# summary(s3)
# 
# 
# #Predicted outcomes: note that pred is 5 columns
# bird.pred<-predict(s3, type = "probs",  se.fit = TRUE)
# 
# 
# #Can also predict for every value of burned_unburned:period
# newdat<-data.frame(season = rep(c("2016","2019"),each=151),
#                    edge.distance = seq(0,1500,by=10))
# 
# #newdat$edge.distance<- factor(newdat$period, order = TRUE,
# #                       levels = c(1,2,3,4,5),
# #                       labels = c("before","0 months","8 months","12 months","20 months"))
# newdat<-cbind(newdat,predict(s3, newdat, type = "probs"))
# newdat_long<-gather(newdat,key=Species, value=probability, 3:6)
# 
# birdplot<- ggplot(newdat_long, aes(x = edge.distance, y = probability, colour = Species)) +
#   #geom_point(size=2)+
#   geom_line(aes(x=as.numeric(edge.distance)),size=1) + 
#   facet_grid( ~ season)+
#   theme_classic()+
#   theme(legend.position = "top")
# birdplot + scale_x_continuous(name="Distance to forest edge (m)") +
#   scale_y_continuous(name="Probability")+
#   scale_color_brewer(palette="RdGy")
# 
#   
# 



