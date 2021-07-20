## ----include = FALSE, results= 'hide'---------------------------------------------------------------------------------------------------
library(tidyverse)
library(here) # builds OS agnostic file paths
library(lme4) # MLM package
library(broom) # tidyverse package for results objects
library(stargazer) # output formatting
library(lattice)
library(ggplot2) 
library(broom.mixed) # relatively new pkg for tidying lme4 objects
# https://cran.r-project.org/web/packages/broom.mixed/index.html
library(kableExtra) # export results
# library(broomExtra) # install but do not load
library(haven) # import STATA data
library(ggeffects);library(sjPlot);library(sjmisc); library(interplot);library(effects);library(magrittr);library(lme4)
setwd("C:/Users/dima/OneDrive - Emory University/gender_matching")
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
# knitr::opts_chunk$set(cache = TRUE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m1 = lmer(knowledge ~ age + (age | childID), data = df, REML = FALSE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m2 = lmer(knowledge ~ age*language + age*hours + (age | childID), data = df, REML = FALSE)


## ---- message = FALSE-------------------------------------------------------------------------------------------------------------------
#alc = read_csv(here("data", "SingerWillettalcohol1_pp.csv"))
#dat <- read.csv("~/Dropbox/MLM-2020/data/dz_mlmdata.csv")
dat <- read.csv("~/OneDrive - Emory University/critics_success/imdb_data_realSent.2_newspapers.csv")
#dat <- read.csv("~/OneDrive - Emory University/critics_success/newspaper_dat_w_rating.csv")
 # dat <- subset(dz_mlmdata_only_newspapers, rating2!="No Rating")

## ---- echo = TRUE-----------------------------------------------------------------------------------------------------------------------
model.b <- lmer(alcuse ~ age_14 + (age_14 | id) , data=alc, REML = FALSE)
summary(model.b)

model.b <- lmer(ave_sentiment ~ year2 + (year2|newspaper.name), data = dat, REML = T)

model.c <- lmer(ave_sentiment ~ lead.actor.gender + ( 1 |newspaper.name) + (lead.actor.gender| year2), data = dat, REML = T)

model.d <- lmer(ave_sentiment ~ lead.actor.gender + author.female + (1|newspaper.name) +(lead.actor.gender|year2),data=dat,REML=T)

model.e <- lmer(ave_sentiment ~ lead.actor.gender*author.female + (author.female|newspaper.name) +(lead.actor.gender|year2),data=dat,REML=T)

model.f <- lmer(ave_sentiment ~ lead.actor.gender*author.female + num.reviews.per.author + (1|newspaper.name) +(lead.actor.gender|year2),data=dat,REML=T)

model.g <- lmer(ave_sentiment ~ lead.actor.gender + runtimeMinutes+ author.female + num.reviews.per.author +(1|newspaper.name)+(lead.actor.gender|year2),data=dat,REML=T)

model.e <- glmer(ave_sentiment2 ~ lead.actor.gender +(1|newspaper.name) +(1|year2),data=dat, family = "binomial")

model.h <- glmer(favorable ~ lead.actor.gender + female.director +(1|newspaper.name/year2),data=dat, family = "binomial")


model.a <- glmer(real_sentiment2 ~ 1 + (1|newspaper.name), data=dat, family = "binomial")

model.aa <- glmer(real_sentiment2 ~ 1 + (1|newspaper.name), data=dat, family="binomial")


model.i <- glmer(real_sentiment2 ~ lead.actor.gender + female.director + author.female + comedy + drama + 
				 + (1|newspaper.name) + (1|year2),data=dat, family = "binomial")

model.i <- glmer(real_sentiment2 ~ lead.actor.gender + female.director + author.female + comedy + drama + 
				 	+ (1|author.id.x/film.id/year2),data=dat, family = "binomial")

model.j <- glmer(real_sentiment2 ~ lead.actor.gender 
				 + (1|newspaper.name) + (1|year2),data=dat, family = "binomial",control = glmerControl(optCtrl=list(maxfun=20000)))

model.jj <- glmer(real_sentiment2 ~ lead.actor.gender*author.female + female.director + gender.terms + author.female + drama + 
				 + (1|newspaper.name) + (1|year2),data=dat, family = "binomial")

model.jk <- glmer(real_sentiment2 ~ lead.actor.gender #+ author.female
				  #+ lead.actor.age 
				  + drama + comedy + action #+ exp 
				  	+ lead.actor.gender #+ female.director
				  + (1|newspaper.name) + (1|year2),data=dat, family = "binomial")

model.kl <- glmer(real_sentiment2 ~ lead.actor.gender + author.female + lead.actor.age + drama + comedy + action + exp 
				  + lead.actor.gender + female.director
				  + (1|media.name) + (1|film.id),data=dat, family = "binomial")

model.ll <- glmer(real_sentiment2 ~ lead.actor.gender*year2 + female.director + gender.terms + author.female + drama + elite.np +
				  	+ (1|newspaper.name:np.region:elite.np),data=dat, family = "binomial")

model.mm <- glmer(real_sentiment2 ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender + drama + comedy +
				  	  exp+ sum.art.critical.terms*lead.actor.gender + lead.actor.gender*author.female
				  	+ (1|newspaper.name/year2) + (lead.actor.gender|year2),data=dat, family = "binomial")

model.mm <- glmer(real_sentiment2 ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender + drama + comedy +
				  	elite.np + sum.art.critical.terms + gender.terms + year2
				  + (1|media.name) + (1|film.id) + (female.director|author.id.x),data=dat, family = "binomial")

model.nn <- glmer(real_sentiment2 ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender + drama + elite.np +
				  	+ comedy + exp + sum.art.critical.terms + gender.terms + lead.actor.gender*year2
				  + (1|newspaper.name) ,data=dat, family = "binomial")

model.ln <- glmer(real_sentiment2 ~ lead.actor.gender + second.actor.gender + lead.actor.age + lead.actor.gender*author.female
				  	+ comedy + action + drama + exp + art.terms + lead.actor.gender*author.female*exp
				  + (1|newspaper.name:elite.np),data=dat, family = "binomial")

tab_model(model.ln)

model.llk <- glmer(real_sentiment2 ~ lead.actor.gender + second.actor.gender + drama + lead.actor.age + author.female
                   + comedy + action + exp
                   + (1|newspaper.name:elite.np) + (1|year2),data=dat, family = "binomial")

model.llm <- glmer(real_sentiment2 ~ lead.actor.gender + second.actor.gender + drama + lead.actor.age + author.female
				   + comedy + action + exp + lead.actor.gender*author.female*exp
				   + (1|newspaper.name:elite.np) + (1|year2),data=dat, family = "binomial")

pl <- c(
  `(Intercept)` = "Intercept",
  lead.actor.gender = "Female Leading Actor",
  second.actor.gender = "Female Supporting Actor", 
  drama = "Drama", 
  lead.actor.age = "Leading Actor's Age (Older)",
  author.female = "Review Author Female ",
  comedy = "Comedy", 
  action = "Action",
  expmid.exp = "Reviewing Frequency: Mid",
  explittle.exp = "Reviewing Frequency: Low",
  lead.actor.gender:author.female = "Female Leading Actor x Review Author Female",
  lead.actor.gender:explittle.exp="Female Leading Actor x Reviewing Frequency Low",
  lead.actor.gender:expmid.exp="Female Leading Actor x Reviewing Frequency Mid",
  author.female:explittle.exp="Review Author Female x Reviewing Frequency Low",
  author.female:expmid.exp="Review Author Female x Reviewing Frequency Mid",
  lead.actor.gender:author.female:explittle.exp = "Female Leading Actor x Review Author Female x Reviewing Frequency Low",
  lead.actor.gender:author.female:expmid.exp = "Female Leading Actor x Review Author Female x Reviewing Frequency Mid"
)


tab <- tab_model(model.llk,model.llm, 
          #pred.labels=pl,
          pred.labels = c("Intercept", "Female Leading Actor", "Female Supporting Actor", 
                          "Drama","Leading Actor's Age (Older)","Review Author Female ","Comedy",
                          "Action","Reviewing Frequency: Low","Reviewing Frequency: Mid",
                          "Female Leading Actor x Review Author Female","Female Leading Actor x Reviewing Frequency Low",
                          "Female Leading Actor x Reviewing Frequency Mid","Review Author Female x Reviewing Frequency Low",
                          "Review Author Female x Reviewing Frequency Mid","Female Leading Actor x Review Author Female x Reviewing Frequency Low",
                          "Female Leading Actor x Review Author Female x Reviewing Frequency Mid"
                          ),
          transform = NULL, show.ci=F, use.viewer = F,show.p = T,show.icc=F,show.se=T,show.stat=F,
          dv.labels = c("Model 1", "Model 2"))


anova(model.llm, model.ll)

ggpredict(model.ln, c("author.female","lead.actor.gender")) %>% plot()

model.ll <- glmer(real_sentiment2 ~ lead.actor.gender + drama + lead.actor.age + lead.actor.gender*author.female
				  + comedy + action  
				  + lead.actor.gender*year2*author.female
				  + (year2|newspaper.name),data=dat, family = "binomial", 
				  control = glmerControl(optimizer ="Nelder_Mead"))


plot_model(model.ll, type = "pred", terms = c("exp", "author.female", "lead.actor.gender"))
plot_model(model.ll, type = "pred", terms = c( "lead.actor.gender", "year2", "author.female"))

interplot(m = model.ll, var1 = "author.female", var2 = "year2")

#--- models with author.female as dv
model.k <- glmer(author.female ~ lead.actor.gender + (1|newspaper.name/year2),data=dat, family = "binomial")

model.l <- glmer(author.female ~ lead.actor.gender + female.director + (1|newspaper.name/year2),data=dat, family = "binomial")

model.m <- glmer(author.female ~ lead.actor.gender + second.actor.gender + (1|newspaper.name/year2),data=dat, family = "binomial")

model.n <- glmer(author.female ~ lead.actor.gender + female.director + comedy + 
				 	(1|newspaper.name) + (1|year2),data=dat, family = "binomial")

model.o <- glmer(author.female ~ lead.actor.gender*second.actor.gender + lead.actor.age + elite.np +
				 	(1|newspaper.name),data=dat, family = "binomial")

model.r <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
				 	(1|newspaper.name),data=dat, family = "binomial")

model.rr <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
				  	comedy*romance + drama + action + crime + adventure + thriller +
				 	(1|newspaper.name),data=dat, family = "binomial")

model.rr <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
				  	comedy*romance + drama + action + crime + adventure + thriller +
				  	(1|newspaper.name),data=dat, family = "binomial")

model.s <- glmer(author.female ~ lead.actor.gender + second.actor.gender + lead.actor.age + comedy
					+ drama + action +
				 	(1|np.region:elite.np),data=dat, family = "binomial")

model.t <- glmer(author.female ~ lead.actor.gender + lead.actor.age + art.terms
				  + comedy  + exp + real_sentiment2
				  +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
				 control = glmerControl(optimizer ="Nelder_Mead"))

model.oo <- glmer(author.female ~ lead.actor.gender + lead.actor.age + comedy + drama + action#+ second.actor.gender #+ art.terms
				  + exp 
				 +	(1|newspaper.name/year2),data=dat, family = "binomial", 
				 control = glmerControl(optimizer ="Nelder_Mead"))

model.ox <- glmer(author.female ~ lead.actor.gender + lead.actor.age + comedy #+ second.actor.gender #+ art.terms
				  + exp
				  +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
				  control = glmerControl(optimizer ="Nelder_Mead"))

model.x <- glmer(author.female ~ lead.actor.gender + lead.actor.age + real_sentiment2 + exp
				  + comedy + drama + action + elite.np
				  +	(1|newspaper.name:np.region) + (1|year2),data=dat, family = "binomial", 
				  control = glmerControl(optimizer ="Nelder_Mead"))

model.y <- glmer(author.female ~  
				 + exp + year2*lead.actor.gender + year2*comedy + year2*drama  #+ year2*lead.actor.age 
				 +	(1|newspaper.name:author.id.x),data=dat, family = "binomial"
					,control = glmerControl(optimizer ="Nelder_Mead"))

model.z <- glmer(author.female ~ lead.actor.gender + lead.actor.age + real_sentiment2 + exp
				 + comedy + drama 
				 +	(1|newspaper.name:author.id.x:year2),data=dat, family = "binomial"
				 ,control = glmerControl(optimizer ="Nelder_Mead"))


plot_model(model.y, type = "pred", terms = c( "lead.actor.gender", "year2"))


# model.bR <- lmer(alcuse ~ age_14 + (age_14 | id) , data=alc, REML = TRUE)
# summary(model.bR)


## ----echo = TRUE------------------------------------------------------------------------------------------------------------------------
summary(model.x)$coefficients
print(VarCorr(model.x),comp=c("Variance","Std.Dev."),digits=3)
mb.re = ranef(model.x, condVar=TRUE)


## ---- fig.width=2.5, fig.height=2.5-----------------------------------------------------------------------------------------------------
dotplot(mb.re, main = FALSE)

## ---------------------------------------------------------------------------------------------------------------------------------------
model.c <- lmer(alcuse ~ coa*age_14  + (age_14 | id), data=alc, REML = FALSE)
summary(model.c)
# model.cR <- lmer(alcuse ~ coa*age_14  + (age_14 | id), data=alc, REML = TRUE)
# summary(model.cR)


## ----echo = FALSE, results= 'asis'------------------------------------------------------------------------------------------------------
stargazer(model.b, model.c, header=FALSE, type='latex', font.size = "scriptsize", 
    single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
    float = FALSE)


## ----echo = TRUE------------------------------------------------------------------------------------------------------------------------
summary(model.x)$coefficients
print(VarCorr(model.x),comp=c("Variance","Std.Dev."),digits=3)
mc.re = ranef(model.x, condVar=TRUE)


## ---- fig.width=2.5, fig.height=2.5-----------------------------------------------------------------------------------------------------
dotplot(mc.re, main = FALSE)


## ---- echo = TRUE-----------------------------------------------------------------------------------------------------------------------
print(VarCorr(model.b),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.c),comp=c("Variance","Std.Dev."),digits=3)
anova(model.b, model.c)


## ---------------------------------------------------------------------------------------------------------------------------------------
model.d <- lmer(alcuse ~ coa*age_14 + peer*age_14 + (age_14 | id), data=alc, REML = FALSE)
# summary(model.d)
# model.dR <- lmer(alcuse ~ coa*age_14 + peer*age_14 + (age_14 | id), data=alc, REML = TRUE)
# summary(model.dR)


## ----echo = TRUE------------------------------------------------------------------------------------------------------------------------
summary(model.d)$coefficients
print(VarCorr(model.d),comp=c("Variance","Std.Dev."),digits=3)
md.re = ranef(model.d, condVar=TRUE)


## ---- fig.width=2.5, fig.height=2.5-----------------------------------------------------------------------------------------------------
dotplot(md.re, main = FALSE)


## ----echo = FALSE, results= 'asis'------------------------------------------------------------------------------------------------------
stargazer(model.x, model.ox, header=FALSE, type='text', font.size = "scriptsize", 
    single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
    float = FALSE)


## ---- echo = TRUE-----------------------------------------------------------------------------------------------------------------------
print(VarCorr(model.b),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.c),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.d),comp=c("Variance","Std.Dev."),digits=3)
anova(model.b, model.c, model.d)


## ---------------------------------------------------------------------------------------------------------------------------------------
model.e <- lmer(alcuse ~ coa + age_14*peer + (age_14 | id), data=alc, REML = FALSE)
summary(model.e)
# model.eR <- lmer(alcuse ~ coa + peer*age_14 + (age_14 | id), data=alc, REML = TRUE)
# summary(model.eR)


## ----echo = FALSE, results= 'asis'------------------------------------------------------------------------------------------------------
stargazer(model.c, model.d, model.e, header=FALSE, type='latex', font.size = "scriptsize", 
    single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
    float = FALSE, order=c("age_14","coa","peer"))


## ---- echo = TRUE-----------------------------------------------------------------------------------------------------------------------
print(VarCorr(model.c),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.d),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.e),comp=c("Variance","Std.Dev."),digits=3)
anova(model.c, model.d, model.e)


## ----echo = FALSE, results= 'asis'------------------------------------------------------------------------------------------------------
stargazer(model.x, header=FALSE, type='text', font.size = "scriptsize", 
    single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
    float = FALSE, order=c("Leading Actor Gender","Leading Actor Age","Favorable Rating", 
    					   "Lower-level Frequency of Publishing", "Mid-level Frequency of Publishing","Comedy","Drama","Action",
    					   "Elite Newspaper"))


## ----echo = TRUE------------------------------------------------------------------------------------------------------------------------
print(VarCorr(model.e),comp=c("Variance","Std.Dev."),digits=3)
me.re = ranef(model.e, condVar=TRUE)


## ---- fig.width=2.5, fig.height=2.5-----------------------------------------------------------------------------------------------------
dotplot(me.re, main = FALSE)


## ---- fig.width=6, fig.height=4.5-------------------------------------------------------------------------------------------------------
dotplot(me.re, main = FALSE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m1 = lmer(y ~ X1 + W1 + W2 + (X1 | personID), data = df, REML = FALSE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m1 = lmer(grade ~ SES + ( 1 | hoodID) + (1 | schoolID), data = df, REML = FALSE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m1 = lmer(grade ~ SES + crime + voced + ( 1 | hoodID) + (1 | schoolID), data = df, REML = FALSE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m1 = lmer(grade ~ SES + crime + voced + ( SES | hoodID) + (1 | schoolID), data = df, REML = FALSE)


## ----echo = TRUE, eval = FALSE----------------------------------------------------------------------------------------------------------
## # R command
## m1 = lmer(grade ~ SES*voced + crime + ( SES | hoodID) + (SES | schoolID), data = df, REML = FALSE)


## ---- message = FALSE-------------------------------------------------------------------------------------------------------------------
cls = read_csv(here("data","class_school.txt"), na="NA", trim_ws=TRUE)
cls$class = as.factor(cls$class)
cls$school = as.factor(cls$school)
kable(table(cls$class, cls$school))


## ---------------------------------------------------------------------------------------------------------------------------------------
m1 <- lmer(extro ~ open + agree + social + (1 | school) + (1 | class), data = cls, REML = FALSE)
summary(m1)


## ---- fig.width=1.75, fig.height=1.75---------------------------------------------------------------------------------------------------
m1.re = ranef(m1, condVar=TRUE)
dotplot(m1.re, main = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------
m2 <- lmer(extro ~ open + agree + social + (1 | school:class), data = cls, REML = FALSE)
summary(m2)


## ---------------------------------------------------------------------------------------------------------------------------------------
m2.re = ranef(m2, condVar=TRUE)
dotplot(m2.re, main = FALSE)


## ----echo = FALSE, results= 'asis'------------------------------------------------------------------------------------------------------
stargazer(m1, m2, header=FALSE, type='latex', font.size = "scriptsize", 
    single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
    float = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------
print(VarCorr(m1),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(m2),comp=c("Variance","Std.Dev."),digits=3)


## ---- fig.width=6, fig.height=4.5-------------------------------------------------------------------------------------------------------
dotplot(m2.re, main = FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------------
knitr::knit_exit()

