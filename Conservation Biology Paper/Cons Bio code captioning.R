#Set your working directory and load some packages you will need,
#then load the dataset. Here we call it dat.

library(readxl)
library(DescTools)
library(MASS)
library(AER)
library(ggplot2)
library(dplyr)

#Set image as a factor and check the distribution of responses
dat$image<-as.factor(dat$Treatment.shown)
summary(dat$Finished<-as.factor(dat$Finished))
#Exclude rows where people did not finish, which drops 142 rows
dat2<-dat[which(dat$Finished==1),]
#Exclude rows w/ recaptcha score less than 0.5
dat2<-dat2[which(dat2$Q_RecaptchaScore>=0.5),]
#look for duplicates in responses
#this will remove all duplicated rows, leaving only the original response
dat2<-cbind(dat2,duplicated(dat2[,12:29]))
dat2<-dat2[which(dat2$`duplicated(dat2[, 12:29])`=="FALSE"),]

#check distribution of image
summary(dat2$image)

#The rows of the dataset are inelegantly named, so rename them
#Also code them as factors for the analysis where appropriate
dat2$qresearch<-as.factor(dat2$X...This.post.depicts.wildlife.research.)
dat2$qinteract<-as.factor(dat2$X...I.would.seek.out.an.experience.to.personally.interact.with.this.animal.)
dat2$qgoodpet<-as.factor(dat2$X...This.animal.would.make.a.good.pet.)
dat2$qhavepet<-as.factor(dat2$X...I.would.like.to.have.this.animal.as.a.pet.)
dat2$qendangered<-as.factor(dat2$X...This.animal.is.an.endangered.species.)
dat2$age<-dat2$What.is.your.age.
dat2$gender<-dat2$To.which.gender.identity.do.you.most.identify.
dat2$ethnicity<-dat2$Please.specify.your.ethnicity.
dat2$education<-dat2$What.is.the.highest.degree.or.level.of.school.you.have.completed..If.currently.enrolled..what.is.your.highest.degree.received.

#Group gender data according to description in Supplement 
#Combine male + trans male (1 and 4), female + trans female (2-3), gender variant+not listed (5-6), code 7 as NA
dat2$gender<-as.factor(ifelse(dat2$gender==1,"male",ifelse(dat2$gender==4,"male",ifelse(dat2$gender==2,"female",ifelse(dat2$gender==3,"female",
                                                                                              ifelse(dat2$gender==5,"other",ifelse(dat2$gender==6,"other",                                                                                              ifelse(dat2$gender==7,NA,dat2$gender))))))))
#Group education 1-3 (associate's degree or below), keep 4, 5, code 6 as NA
dat2$education.coded<-as.factor(ifelse(dat2$education<4,1,ifelse(dat2$education==6,NA,dat2$education)))

#Code ethicity to match gender and ed so that "prefer not to say" are excluded from the analysis
dat2$ethnicity<-as.factor(ifelse(dat2$ethnicity==7,NA,dat2$ethnicity))

#Build age graph for supplement
age.plot<-ggplot(data=dat2,aes(age))+
  geom_histogram(binwidth=1,fill="#d8a828",col="black")+
  xlab("Participant age")+
  ylab("Number of participants")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())
age.plot
  
##Look at social media use among participants, also for supplement
social<-dat2$Which.social.media.do.you.have.use..select.all.that.apply..  
library(stringr)
max(lengths(strsplit(social, ',')))
social.data<-as.data.frame(str_split_fixed(social, ",", 6))

social.fb<-social.data %>% filter_all(any_vars(. %in% c(1)))
social.insta<-social.data %>% filter_all(any_vars(. %in% c(2)))
social.tw<-social.data %>% filter_all(any_vars(. %in% c(3)))
social.m<-social.data %>% filter_all(any_vars(. %in% c(4)))
social.tt<-social.data %>% filter_all(any_vars(. %in% c(5)))
social.none<-social.data %>% filter_all(any_vars(. %in% c(6)))
social.all<-social.data[which(social.data$V5 == 5),]

#Continue with the main data analysis

#Separate into gorilla and loris datasets and drop unused levels of the image factor
dat.gorilla<-dat2[which(dat2$image == "Image1" | dat2$image=="Image2"),]
dat.gorilla$image<-droplevels(dat.gorilla$image)
dat.loris<-dat2[which(dat2$image == "Image3" | dat2$image=="Image4"),]
dat.loris$image<-droplevels(dat.loris$image)

#Calculate percentages of various classes to make Table 2
#(combine 4 and 5 for agree + strongly agree)
table(dat.gorilla$qinteract,dat.gorilla$image)
(344+164)/726
(361+173)/754
table(dat.gorilla$qendangered,dat.gorilla$image)
(408+173)/726
(384+215)/754
table(dat.gorilla$qhavepet,dat.gorilla$image)
(272+135)/726
(287+145)/754
table(dat.gorilla$qgoodpet,dat.gorilla$image)
(301+114)/726
(289+138)/754

table(dat.loris$qinteract,dat.loris$image)
(357+163)/747
(374+165)/750
table(dat.loris$qendangered,dat.loris$image)
(410+131)/747
(390+146)/750
table(dat.loris$qhavepet,dat.loris$image)
(322+152)/747
(309+151)/750
table(dat.loris$qgoodpet,dat.loris$image)
(341+129)/747
(296+149)/750

table(dat.gorilla$qgoodpet)
(590+252)/1480
table(dat.loris$qgoodpet)
(637+278)/1497
table(dat.gorilla$qhavepet)
(559+280)/1480
table(dat.loris$qhavepet)
(631+303)/1497
table(dat.gorilla$qinteract)
(705+337)/1480
table(dat.loris$qinteract)
(731+328)/1497

#Run probit regression using Hess = TRUE to return the observed information matrix
#Use coef test to get significance
#These are the gorilla models
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

endanger.g<-polr(as.factor(qendangered)~image+age+education.coded+gender+ethnicity,
                data=dat.gorilla,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(endanger.g)

#These are the loris models
research.l<-polr(as.factor(qresearch)~image+age+education.coded+gender+ethnicity,
                 data=dat.loris,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(research.l)

interact.l<-polr(as.factor(qinteract)~image+age+education.coded+gender+ethnicity,
                 data=dat.loris,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(interact.l)

goodpet.l<-polr(as.factor(qgoodpet)~image+age+education.coded+gender+ethnicity,
                data=dat.loris,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(goodpet.l)

havepet.l<-polr(as.factor(qhavepet)~image+age+education.coded+gender+ethnicity,
                 data=dat.loris,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(havepet.l)

endanger.l<-polr(as.factor(qendangered)~image+age+education.coded+gender+ethnicity,
                 data=dat.loris,method="probit",na.action=na.omit,Hess = TRUE)
coeftest(endanger.l)

#Load more packages to make figures
library(erer)
library(reshape2)
library(fishualize)
library(cowplot)

#Use ocME function to calculate marginal effects in terms of probability
ocME(research.g)
ocME(research.l)

#make a graph for the marginal effects for gorilla (denoted with g) and loris (denoted with l)
rg<-ocME(research.g)$out$ME.all[1,]
eg<-ocME(endanger.g)$out$ME.all[1,]
ig<-ocME(interact.g)$out$ME.all[1,]
hg<-ocME(havepet.g)$out$ME.all[1,]
gg<-ocME(goodpet.g)$out$ME.all[1,]
marginals.gorilla<-rbind(rg,eg,ig,hg,gg)
mg.dat<-as.data.frame(melt(marginals.gorilla,value.name="ProbChange"))
marginals.gorilla.p<-ggplot(data=mg.dat, aes(x=Var1,y=(ProbChange*100),by=Var2,fill=Var2))+
  geom_col(position = position_dodge(0.85),width=0.7,color="black")+
  theme_bw(base_size=16)+
  ylab("Probability change (%)")+
  xlab("")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  scale_x_discrete(labels=c("This post depicts \nwildife research","This animal is an \nendangered species",
                            "I would seek out \nan opportunity \nto interact with \nthis animal",
                            "I would like \nto have this animal \nas a pet","This animal would \nmake a good pet"))+
  theme(axis.text.x = element_text(color="black",size=15),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5),linetype="longdash")
marginals.gorilla.p

rl<-ocME(research.l)$out$ME.all[1,]
el<-ocME(endanger.l)$out$ME.all[1,]
il<-ocME(interact.l)$out$ME.all[1,]
hl<-ocME(havepet.l)$out$ME.all[1,]
gl<-ocME(goodpet.l)$out$ME.all[1,]
marginals.loris<-rbind(rl,el,il,hl,gl)
ml.dat<-as.data.frame(melt(marginals.loris,value.name="ProbChange"))
marginals.loris.p<-ggplot(data=ml.dat, aes(x=Var1,y=(ProbChange*100),by=Var2,fill=Var2))+
  geom_col(position = position_dodge(0.85),width=0.7,color="black")+
  theme_bw(base_size=16)+
  ylab("Probability change (%)")+
  xlab("")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  scale_x_discrete(labels=c("This post depicts \nwildife research","This animal is an \nendangered species",
                            "I would seek out \nan opportunity \nto interact with \nthis animal",
                            "I would like \nto have this animal \nas a pet","This animal would \nmake a good pet"))+
  theme(axis.text.x = element_text(color="black",size=15),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5),linetype="longdash")
marginals.loris.p

#Plot everything together
marginals<-plot_grid(marginals.gorilla.p,marginals.loris.p,
                     labels=c("A","B"),ncol=1)
marginals


#Make stacked barplots for the four primate conservation questions
g.i.bar<-ggplot(data=dat.gorilla,aes(x=image,fill=qinteract))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend=FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("Seek out opportunity to interact")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)

g.i.bar

g.e.bar<-ggplot(data=dat.gorilla,aes(x=image,fill=qendangered))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend = FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("This animal is endangered")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
g.e.bar

g.hp.bar<-ggplot(data=dat.gorilla,aes(x=image,fill=qhavepet))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend = FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("Would have as pet")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
g.hp.bar

g.gp.bar<-ggplot(data=dat.gorilla,aes(x=image,fill=qgoodpet))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend = FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("Would make a good pet")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
g.gp.bar


gorilla.bars<-plot_grid(g.e.bar,g.i.bar,
                        g.hp.bar,g.gp.bar,labels=c("A","B","C","D"))
gorilla.bars


l.i.bar<-ggplot(data=dat.loris,aes(x=image,fill=qinteract))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend = FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("Seek out opportunity to interact")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
l.i.bar

l.e.bar<-ggplot(data=dat.loris,aes(x=image,fill=qendangered))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend = FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("This animal is endangered")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
l.e.bar

l.hp.bar<-ggplot(data=dat.loris,aes(x=image,fill=qhavepet))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend=FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("Would have as pet")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
l.hp.bar

l.gp.bar<-ggplot(data=dat.loris,aes(x=image,fill=qgoodpet))+
  geom_bar(position="fill", stat="count",width=0.6,show.legend=FALSE,color="black")+
  scale_fill_fish_d(option="Epibulus_insidiator",direction=-1,labels=c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree"),
                    name="Category")+
  ylab("Percentage of responses")+
  xlab("Would make a good pet")+
  theme_bw(base_size=16)+
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels=c("Control","Test"))+
  scale_y_continuous(labels = scales::percent)
l.gp.bar

loris.bars<-plot_grid(l.e.bar,l.i.bar,
                      l.hp.bar,l.gp.bar,labels=c("A","B","C","D"))
loris.bars





















