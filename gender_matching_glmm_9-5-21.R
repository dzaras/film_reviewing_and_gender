# Dimitrios Zaras - Gender matching glmm models - 11/19/20ddddddd

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
library(performance);library(see)
library(ggeffects);library(sjPlot);library(sjmisc); library(interplot);library(effects);library(magrittr);library(lme4)
setwd("C:/Users/dima/OneDrive - Emory University/critics_success")
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
# knitr::opts_chunk$set(cache = TRUE)


## ---- message = FALSE-------------------------------------------------------------------------------------------------------------------

dat <- read.csv("~/OneDrive - Emory University/critics_success/imdb_data_realSent.2_newspapers.csv") #import dataset with full data on newspaper film reviews and authors

## ---- echo = TRUE-----------------------------------------------------------------------------------------------------------------------

model.b <- lmer(ave_sentiment ~ year2 + (year2|newspaper.name), data = dat, REML = T)

model.c <- lmer(ave_sentiment ~ lead.actor.gender + ( 1 |newspaper.name) + (lead.actor.gender| year2), data = dat, REML = T)

model.d <- lmer(ave_sentiment ~ lead.actor.gender + author.female + (1|newspaper.name) +(lead.actor.gender|year2),data=dat,REML=T)

model.e <- lmer(ave_sentiment ~ lead.actor.gender*author.female + (author.female|newspaper.name) +(lead.actor.gender|year2),data=dat,REML=T)

model.f <- lmer(ave_sentiment ~ lead.actor.gender*author.female + num.reviews.per.author + (1|newspaper.name) +(lead.actor.gender|year2),data=dat,REML=T)

model.g <- lmer(ave_sentiment ~ lead.actor.gender + runtimeMinutes+ author.female + num.reviews.per.author +(1|newspaper.name)+(lead.actor.gender|year2),data=dat,REML=T)

model.e <- glmer(ave_sentiment2 ~ lead.actor.gender +(1|newspaper.name) +(1|year2),data=dat, family = "binomial")

model.h <- glmer(favorable ~ lead.actor.gender + female.director +(1|newspaper.name/year2),data=dat, family = "binomial")


model.a <- glmer(real_sentiment2 ~ 1 + (1|newspaper.name), data=dat, family = "binomial")

summary(model.aa <- glmer(real_sentiment ~ lead.actor.gender
                  + female.director
                  #+ second.actor.age
                  + lead.actor.age
                  + drama
                  #+ comedy
                  #+ action
                  + minutes
                  #+ genre.number
                  + sum.art.critical.terms
                  + (1|newspaper.name) + (1|year2), data=dat, family="binomial"))

summary(model.ab <- glmer(real_sentiment ~ lead.actor.gender
                          + female.director
                          + second.actor.gender*lead.actor.gender
                          + lead.actor.age
                          + drama
                          + comedy
                          + action
                          + minutes
                          #+ genre.number
                          + sum.art.critical.terms
                          + word.cat
                          + (1|newspaper.name) #+ (1|year2)
                          , data=dat, family="binomial"))

summary(model.ac <- glmer(real_sentiment ~ lead.actor.gender
                          + female.director
                          + second.actor.gender*lead.actor.gender
                          + lead.actor.age
                          + drama
                          + comedy
                          + action
                          + minutes
                          #+ genre.number
                          + sum.art.critical.terms
                          + word.cat
                          + (1|newspaper.name) + (1|year2), data=dat, family="binomial"))

summary(model.ad <- glmer(real_sentiment ~ lead.actor.gender
                  + female.director
                  + lead.actor.age
                  + drama
                  + comedy
                  + action
                  + exp#*lead.actor.gender
                  + minutes
                  + genre.number
                  + (1|newspaper.name:year2), data=dat, family="binomial"))


summary(model.i <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender
                 + lead.actor.age 
                 + author.female + #minutes 
                 + drama + comedy + action 
                 + exp2*author.female
                 + (1|newspaper.name)# + (1|year2)
                 ,data=dat, family = "binomial"))

summary(model.i2 <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender
                         + lead.actor.age 
                         + author.female #+  runtimeMinutes + word.count +
                         + drama + comedy + action 
                         + exp2
                         + year2
                         + word.cat
                         + (1|newspaper.name)# + (1|year2)
                         ,data=dat, family = "binomial"))

summary(model.j <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender 
                 + lead.actor.age 
                 #+ female.director
                 + author.female
                 + drama + comedy + action
                 + exp2
                 + (1|newspaper.name) #+ (1|year2)
                 ,data=dat, family = "binomial"))


summary(model.jj <- glmer(real_sentiment ~ lead.actor.gender*female.director 
                  + lead.actor.age 
                  + author.female*exp2
                  + drama + comedy  + action
                  + elite.np*lead.actor.gender
                  + (1|newspaper.name:year2),data=dat, family = "binomial"))

model.jk <- glmer(real_sentiment ~ lead.actor.gender #+ author.female
                  #+ lead.actor.age 
                  + drama + comedy + action #+ exp 
                  + lead.actor.gender #+ female.director
                  + (1|newspaper.name) + (1|year2),data=dat, family = "binomial")

model.kl <- glmer(real_sentiment2 ~ lead.actor.gender + author.female + lead.actor.age + drama + comedy + action + exp 
                  + lead.actor.gender + female.director
                  + (1|media.name) + (1|film.id),data=dat, family = "binomial")

model.ll <- glmer(real_sentiment ~ lead.actor.gender*year2 + female.director + author.female + drama + elite.np +
                    + (1|newspaper.name:np.region:elite.np),data=dat, family = "binomial")

model.mm <- glmer(real_sentiment ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender + drama + comedy +
                    exp + lead.actor.gender*author.female
                  + (1|newspaper.name:year2) + (lead.actor.gender|year2),data=dat, family = "binomial")

model.mm2 <- glmer(favorable ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender*author.female +
                     drama + comedy + action +
                     exp*author.female + lead.actor.gender*author.female
                   + (1|newspaper.name:year2) ,data=dat, family = "binomial")


model.mm <- glmer(real_sentiment ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender + drama + comedy +
                    elite.np + sum.art.critical.terms + gender.terms + year2
                  + (1|media.name) + (1|film.id) + (female.director|author.id.x),data=dat, family = "binomial")

model.nn <- glmer(real_sentiment ~ lead.actor.gender*female.director + lead.actor.gender*second.actor.gender + drama + elite.np +
                    + comedy + exp + lead.actor.gender*year2
                  + (1|newspaper.name) ,data=dat, family = "binomial")

model.ln <- glmer(real_sentiment ~ lead.actor.gender + second.actor.gender + lead.actor.age + lead.actor.gender*author.female
                  + comedy + action + drama + exp + art.terms + lead.actor.gender*author.female*exp
                  + (1|newspaper.name:elite.np),data=dat, family = "binomial")

tab_model(model.ln)

model.llk <- glmer(real_sentiment ~ lead.actor.gender + second.actor.gender + drama + lead.actor.age + author.female
                   + comedy + action + exp
                   + (1|newspaper.name:elite.np) + (1|year2),data=dat, family = "binomial")

model.llm <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender + drama + lead.actor.age + author.female
                   + comedy + action + exp + lead.actor.gender*author.female*exp
                   + (1|newspaper.name:elite.np) + (1|year2),data=dat, family = "binomial")

pl <- c(
  #`(Intercept)` = "Intercept",
  dat$lead.actor.gender = "Female Leading Actor",
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
  lead.actor.gender:author.female:expmid.exp = "Female Leading Actor x Review Author Female x Reviewing Frequency Mid")


tab <- tab_model(model.s2b, 
                 #pred.labels=pl,
                 pred.labels = c("Intercept", "Female Leading Actor", 
                                 "Leading Actor's Age (Older)",
                                  "Drama", "Comedy","Action", 
                                 #"Elite Newspaper",
                                 "Artistic & Critical Terms",
                                 "Word Count: Low", "Word Count: Mid", 
                                 "Reviewing Frequency: Low","Reviewing Frequency: Mid",
                                 "Year: 2008","Year: 2018", 
                                 "Female Leading Actor x Reviewing Frequency: Low",
                                 "Female Leading Actor x Reviewing Frequency: Mid"
                                 #"Female Leading Actor x Year: 2008", "Female Leading Actor x Year: 2018"
                                 ),
                  show.ci=F, use.viewer = F,show.p = T,show.icc=T,show.se=T,show.stat=F,
                 dv.labels = c("Model 3"))


anova(model.llm, model.ll)

ggpredict(model.ln, c("author.female","lead.actor.gender")) %>% plot()

model.ll <- glmer(real_sentiment ~ lead.actor.gender + drama + lead.actor.age + lead.actor.gender*author.female
                  + comedy + action  
                  + lead.actor.gender*year2*author.female
                  + (year2|newspaper.name),data=dat, family = "binomial", 
                  control = glmerControl(optimizer ="Nelder_Mead"))


plot_model(model.ll, type = "pred", terms = c("exp", "author.female", "lead.actor.gender"))
plot_model(model.ll, type = "pred", terms = c( "lead.actor.gender", "year2", "author.female"))

interplot(m = model.ll, var1 = "author.female", var2 = "year2")

#--- models with author.female as dv
summary(model.k <- glmer(author.female ~  (1|newspaper.name),data=dat, family = "binomial"))

model.l <- glmer(author.female ~ lead.actor.gender*female.director + (1|newspaper.name:year2),data=dat, family = "binomial")

model.m <- glmer(author.female ~ lead.actor.gender + second.actor.gender + (1|newspaper.name/year2),data=dat, family = "binomial")

summary(model.n <- glmer(author.female ~ lead.actor.gender + female.director + comedy + 
                   (1|newspaper.name) + (1|year2),data=dat, family = "binomial"))

model.o <- glmer(author.female ~ lead.actor.gender*second.actor.gender + lead.actor.age + elite.np +
                   (1|newspaper.name),data=dat, family = "binomial")

model.r <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
                   (1|newspaper.name),data=dat, family = "binomial")

model.rr <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
                    comedy*romance + drama + action + crime + adventure + thriller +
                    (1|newspaper.name),data=dat, family = "binomial")

model.rr <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
                    comedy + drama + action +
                    (1|newspaper.name:year2),data=dat, family = "binomial")

model.s <- glmer(author.female ~ lead.actor.gender + second.actor.gender + lead.actor.age + comedy
                 + drama + action +
                   (1|np.region:elite.np),data=dat, family = "binomial")

summary(model.s2 <- glmer(author.female ~ lead.actor.gender
                  + lead.actor.age#*second.actor.age.cat
                  #+ female.director
                  #+ female.writer
                  #+ second.actor.gender
                  + drama
                  + comedy
                  + action
                  + elite.np
                  #+ minutes
                  #+ genre.number
                  #+ art.terms
                  + critical.terms
                  + word.cat
                  #+ genre.number
                  + exp*lead.actor.gender
                  #+ year2
                  + (1|newspaper.name), data=dat, family="binomial"))

summary(model.s2b <- glm(author.female ~ lead.actor.gender
                          + lead.actor.age#*second.actor.age.cat
                          #+ female.director
                          #+ female.writer
                          #+ second.actor.gender
                          + drama
                          + comedy
                          + action
                          #+ elite.np
                          #+ minutes
                          #+ genre.number
                          #+ art.terms
                          + critical.terms
                          + word.cat
                          #+ genre.number
                          + exp*lead.actor.gender
                          #+ year2*lead.actor.gender
                          + media.name
                          , data= dat18, family="binomial"))

# dat2 <- dat[dat$media.name == 'nyt' | dat$media.name== 'Houston Chronicle' | dat$media.name=='Star-Ledger' | dat$media.name== 'lat' | dat$media.name== 'San Francisco Chronicle',]

dat98 %>% count(author.id.x)  

## model.s3 includes variables for the level-2 variable = newspaper.name

summary(model.s3a <- glmer(author.female ~ lead.actor.gender
                          + lead.actor.age#*second.actor.age.cat
                          #+ female.director
                          #+ female.writer
                          #+ second.actor.gender
                          + drama
                          + comedy
                          + action
                          + elite.np
                          #+ minutes
                          #+ genre.number
                          + sum.art.critical.terms
                          + word.cat
                          #+ genre.number
                          + exp*lead.actor.gender
                          + #year2#*lead.actor.gender
                          + (1|newspaper.name) + (1|year2), data=dat, family="binomial"))

check_model(model.s3a)

summary(model.t <- glmer(author.female ~ lead.actor.gender#*exp 
                 + lead.actor.age 
                 + comedy + drama 
                 + elite.np
                 + year2
                 +	(1|newspaper.name),data=dat, family = "binomial", 
                 control = glmerControl(optimizer ="Nelder_Mead")))

model.o <- glmer(author.female ~ lead.actor.gender*exp 
                 + year2*lead.actor.gender
                 + lead.actor.age 
                 + comedy + drama 
                 + elite.np
                 +	(1|newspaper.name),data=dat, family = "binomial", 
                 control = glmerControl(optimizer ="Nelder_Mead"))

model.oo <- glmer(author.female ~ lead.actor.gender + lead.actor.age + comedy + drama + action#+ second.actor.gender #+ art.terms
                  + exp 
                  +	(1|newspaper.name/year2),data=dat, family = "binomial", 
                  control = glmerControl(optimizer ="Nelder_Mead"))

summary(model.ox <- glmer(author.female ~ lead.actor.gender 
                  + lead.actor.age 
                  + female.director
                  + drama + comedy 
                  + elite.np 
                  #+ sum.art.critical.terms
                  + exp2
                  
                  +	(1|newspaper.name:year2),data=dat, family = "binomial", 
                  control = glmerControl(optimizer ="Nelder_Mead")))

summary(model.oxy <- glmer(author.female ~ lead.actor.gender
                  + lead.actor.age 
                  #+ second.actor.gender 
                  + female.director
                  + comedy + drama 
                  #+ elite.np 
                  + minutes
                  #+ genre.number
                  #+ word.cat
                  +	(1|newspaper.name)# + (1|year2)
                  ,data=dat, family = "binomial", 
                  control = glmerControl(optimizer ="Nelder_Mead")))

model.oxz <- glmer(author.female ~ lead.actor.gender + lead.actor.age 
                   + comedy + drama + action 
                   + elite.np + np.region
                   +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
                   control = glmerControl(optimizer ="Nelder_Mead"))

summary(model.oz <- glmer(author.female ~ lead.actor.gender
                   + lead.actor.age 
                   + second.actor.gender*lead.actor.gender
                   + female.director
                   + comedy + drama #+ action
                   #+ sum.art.critical.terms
                   + elite.np 
                   + exp
                   #+ year2*lead.actor.gender
                   +	(1|newspaper.name) #+ (1 |year2)
                   ,data=dat, family = "binomial", 
                   control = glmerControl(optimizer ="Nelder_Mead")))

exponentiate <- function(x) exp(x)
stargazer(model.oz, header=FALSE, type='latex', font.size =
            "scriptsize", single.row = TRUE, model.numbers = FALSE,
          apply.coef=exponentiate, float = FALSE, star.char = "", omit.table.layout = "l",
          column.labels = c("model.oz"))

model.s3.re = ranef(model.s3, condVar=TRUE)
pa2.re.df = as.data.frame(model.s3.re)
qqnorm(pa2.re.df$condval)

summary(model.t <- glmer(author.female ~ lead.actor.gender 
                 + lead.actor.age 
                 + comedy + drama 
                 + elite.np 
                 + exp2
                 +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
              control = glmerControl(optimizer ="Nelder_Mead")))

summary(model.x <- glmer(author.female ~ lead.actor.gender + lead.actor.age 
                 + comedy + elite.np + exp
                 +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
                 control = glmerControl(optimizer ="Nelder_Mead")))

model.x2 <- glmer(author.female ~ lead.actor.gender + lead.actor.age 
                 + comedy + drama + action + elite.np + np.region + exp 
                 +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
                 control = glmerControl(optimizer ="Nelder_Mead"))

model.y <- glmer(author.female ~  
                   year2 + lead.actor.gender + lead.actor.age + elite.np 
                 +	(1|newspaper.name),data=dat, family = "binomial"
                 ,control = glmerControl(optimizer ="Nelder_Mead"))

model.y2 <- glmer(author.female ~  
                   year2 + lead.actor.gender + lead.actor.age + elite.np 
                  + second.actor.gender + female.director
                  #+ np.region
                   + num.reviews.per.author
                  + (1|newspaper.name),data=dat, family = "binomial"
                 ,control = glmerControl(optimizer ="Nelder_Mead"))


model.z <- glmer(author.female ~  
                   year2 +lead.actor.gender + lead.actor.age + elite.np 
                 + second.actor.gender + female.director
                 + np.region
                 + sum.art.critical.terms
                 #+ num.reviews.per.author
                 + (1|newspaper.name),data=dat, family = "binomial"
                 ,control = glmerControl(optimizer ="Nelder_Mead"))

model.z2 <- glmer(author.female ~  
                   lead.actor.gender + lead.actor.age + elite.np 
                 + second.actor.gender + female.director
                 + 
                 #+ sum.art.critical.terms
                 + exp2
                 + action + comedy + drama
                 + (1|newspaper.name) + (1|year2),data=dat, family = "binomial"
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


