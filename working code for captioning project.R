setwd("C:/Users/cathr/Dropbox/OU YT project/Summer 2022 LPZ")

library(readxl)
library(DescTools)
library(MASS)
library(AER)
dat <- read_excel("no_duplicate_dataset.xlsx")

dat$image<-as.factor(dat$Treatment.shown)
summary(dat$Finished<-as.factor(dat$Finished))
#exclude rows where people did not finish, drops 142 rows
dat2<-dat[which(dat$Finished==1),]

dat2$qresearch<-dat2$X...This.post.depicts.wildlife.research.
dat2$qinteract<-dat2$X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.
dat2$qgoodpet<-dat2$X...This.animal.would.make.a.good.pet.
dat2$qhavepet<-dat2$X...I.would.like.to.have.this.animal.as.a.pet.
dat2$age<-dat2$What.is.your.age.
dat2$gender<-dat2$To.which.gender.identity.do.you.most.identify.
dat2$ethnicity<-dat2$Please.specify.your.ethnicity.
dat2$education<-dat2$What.is.the.highest.degree.or.level.of.school.you.have.completed..If.currently.enrolled..what.is.your.highest.degree.received.

#group male + trans male (1 and 4), female + trans female (2-3), gender variant+not listed (5-6), code 7 as NA
dat2$gender<-as.factor(ifelse(dat2$gender==1,"male",ifelse(dat2$gender==4,"male",ifelse(dat2$gender==2,"female",ifelse(dat2$gender==3,"female",
                                                                                              ifelse(dat2$gender==5,"other",ifelse(dat2$gender==6,"other",                                                                                              ifelse(dat2$gender==7,NA,dat2$gender))))))))
#group education 1-3 (associate's degree or below), keep 4, 5, code 6 as NA
dat2$education.coded<-as.factor(ifelse(dat2$education<4,1,ifelse(dat2$education==6,NA,dat2$education)))

#code ethicity to match gender and ed so "prefer not to say" are excluded
dat2$ethnicity<-as.factor(ifelse(dat2$ethnicity==7,NA,dat2$ethnicity))

dat.gorilla<-dat2[which(dat2$image == "Image1" | dat2$image=="Image2"),]
dat.gorilla$image<-droplevels(dat.gorilla$image)
dat.lemur<-dat2[which(dat2$image == "Image3" | dat2$image=="Image4"),]
dat.lemur$image<-droplevels(dat.lemur$image)

#In probit regression you are predicting the z-score change of your outcome as a function of your independent variables.
#https://web.pdx.edu/~newsomj/mvclass/ho_ordinal%20examples.pdf
research.g<-polr(as.factor(qresearch)~image+age+education.coded+gender+ethnicity,
           data=dat.gorilla,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(research.g)

interact.g<-polr(as.factor(qinteract)~image+age+education.coded+gender+ethnicity,
                 data=dat.gorilla,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(interact.g)

goodpet.g<-polr(as.factor(qgoodpet)~image+age+education.coded+gender+ethnicity,
                data=dat.gorilla,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(goodpet.g)

havepet.g<-polr(as.factor(qhavepet)~image+age+education.coded+gender+ethnicity,
                data=dat.gorilla,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(havepet.g)

research.l<-polr(as.factor(qresearch)~image+age+education.coded+gender+ethnicity,
                 data=dat.lemur,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(research.l)

interact.l<-polr(as.factor(qinteract)~image+age+education.coded+gender+ethnicity,
                 data=dat.lemur,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(interact.l)

goodpet.l<-polr(as.factor(qgoodpet)~image+age+education.coded+gender+ethnicity,
                data=dat.lemur,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(goodpet.l)

havepet.l<-polr(as.factor(qhavepet)~image+age+education.coded+gender+ethnicity,
                 data=dat.lemur,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(havepet.l)

library(erer)
library(ggplot2)
library(reshape2)
library(fishualize)
ocME(research.g)
ocME(research.l)

#make a graph for the marginal effects for gorilla and lemur
rg<-ocME(research.g)$out$ME.all[1,]
ig<-ocME(interact.g)$out$ME.all[1,]
hg<-ocME(havepet.g)$out$ME.all[1,]
gg<-ocME(goodpet.g)$out$ME.all[1,]
marginals.gorilla<-rbind(rg,ig,hg,gg)
mg.dat<-as.data.frame(melt(marginals.gorilla,value.name="ProbChange"))
marginals.gorilla.p<-ggplot(data=mg.dat, aes(x=Var1,y=(ProbChange*100),by=Var2,fill=Var2))+
  geom_col(position = position_dodge(0.85),width=0.7,color="black")+
  theme_bw(base_size=16)+
  ylab("Probability change")+
  xlab("")+
  scale_fill_fish_d(option="Epibulus_insidiator",labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  scale_x_discrete(labels=c("Post depicts research","Would seek to interact",
                            "Have as pet","Good pet"))+
  theme(axis.text.x = element_text(color="black",size=15))
marginals.gorilla.p

rl<-ocME(research.l)$out$ME.all[1,]
il<-ocME(interact.l)$out$ME.all[1,]
hl<-ocME(havepet.l)$out$ME.all[1,]
gl<-ocME(goodpet.l)$out$ME.all[1,]
marginals.lemur<-rbind(rl,il,hl,gl)
ml.dat<-as.data.frame(melt(marginals.lemur,value.name="ProbChange"))
marginals.lemur.p<-ggplot(data=ml.dat, aes(x=Var1,y=(ProbChange*100),by=Var2,fill=Var2))+
  geom_col(position = position_dodge(0.85),width=0.7,color="black")+
  theme_bw(base_size=16)+
  ylab("Probability change")+
  xlab("")+
  scale_fill_fish_d(option="Epibulus_insidiator",labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  scale_x_discrete(labels=c("Post depicts research","Would seek to interact",
                            "Have as pet","Good pet"))+
  theme(axis.text.x = element_text(color="black",size=15))
marginals.lemur.p


https://www.st-andrews.ac.uk/media/ceed/students/mathssupport/OrdinalexampleR.pdf
https://peopleanalytics-regression-book.org/ord-reg.html
https://users.stat.ufl.edu/~aa/articles/agresti_tarantola.pdf



























