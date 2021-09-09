# Dimitrios Zaras - Film Criticism and Gender - glmmm analysis - 11/21/20

# This script is an analysis of data on the relationship between professional film critics and the effect that gender has on film review assignment within a media organization
# I employ generalized linear mixed-effects models because I use data (film reviews) that are nested within the newspapers that they were publisehd in and within the year they published in
# I use two different dependent variables for the two different sections of the analysis: "real_sentiment" that is adummy variable measuring whether a film is favorable or not
# "author.female" which measures whether the author (film critic) of a review is male or female

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
library(performance);library(see)
library(ggeffects);library(sjPlot);library(sjmisc); library(interplot);library(effects);library(magrittr);library(lme4)
setwd("C:/Users/dima/OneDrive - Emory University/critics_success")

# the dataset I import in the next line contains data on film reviews published in 7 newspapers in the US in 3 points in time: 1998, 2008, 2018
dat <- read.csv("~/OneDrive - Emory University/critics_success/imdb_data_realSent.2_newspapers.csv") #import dataset with full data on newspaper film reviews and authors

# the level 2 variables are "newspaper.name", which contains the names of the seven newspapers from wihch I have collected data for this part of the project, 
# and "year2", which has three levels - 1998, 2008, and 2018
# I use a cross-nested design with data points, i.e. film reviews, being nested within both newspapers and years
# The DV is "real_sentiment" and it is a variable that measures whether a review is favorable or not. 
# The film reviews collected had either a quantitative rating (e.g. 3 out of 4 stars) or a qualitative rating (favorable, mized, unfavorable). 
# The "real_sentiment" variable is a dummy variable with values of 1 for based on qualitative ('favorable') or quantitative rating (>0.67). 
# The quantitative ratings included in the film reviews were normalized so that they have a range from 0 to 1. 

# DV: favorability of film review
# Level 1: film reviews , favorability of film review (favorable/unfavarable) Level 2: Newspaper  +  Year of publishing
# The small number of groups at Level 2 (7 different newspaper titles and 3 years) is another reason for using a cross-nested design since with a cross-nested design 
# the groups for each one of the two level 2 variables are multiplied. Thus, we end up with 21 groups at level 2. 

model.e <- glmer(real_sentiment ~ 1 +(1|newspaper.name) +(1|year2),data=dat, family = "binomial")
summary(model.e)

# adding more predictors to the original model

model.aa <- glmer(real_sentiment ~ lead.actor.gender
                  + female.director
                  + lead.actor.age
                  + drama
                  + minutes
                  + sum.art.critical.terms
                  + (1|newspaper.name) + (1|year2), data=dat, family="binomial")

summary(model.aa)$coefficients
print(VarCorr(model.aa),comp=c("Variance","Std.Dev."),digits=3)
mb.re = ranef(model.aa, condVar=TRUE)

dotplot(mb.re, main = FALSE)

# removing some of the predictors and adding others due to lack of a significant relationship with the DV

model.ab <- glmer(real_sentiment ~ lead.actor.gender
                          + female.director
                          + second.actor.gender*lead.actor.gender
                          + lead.actor.age
                          + drama
                          + comedy
                          + action
                          + minutes
                          + sum.art.critical.terms
                          + word.cat
                          + (1|newspaper.name) + (1|year2)
                          , data=dat, family="binomial")

summary(model.ab)$coefficients
print(VarCorr(model.ab),comp=c("Variance","Std.Dev."),digits=3)
mb.re = ranef(model.ab, condVar=TRUE)

dotplot(mb.re, main = FALSE)

model.ac <- glmer(real_sentiment ~ lead.actor.gender
                          + female.director
                          + second.actor.gender
                          + lead.actor.age
                          + drama
                          + comedy
                          + action
                          + minutes
                          + sum.art.critical.terms
                          + word.cat
                          + (1|newspaper.name) + (1|year2), data=dat, family="binomial")

summary(model.ac)$coefficients
print(VarCorr(model.ac),comp=c("Variance","Std.Dev."),digits=3)
mb.re = ranef(model.ac, condVar=TRUE)

dotplot(mb.re, main = FALSE)

print(VarCorr(model.aa),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.ab),comp=c("Variance","Std.Dev."),digits=3)
print(VarCorr(model.ac),comp=c("Variance","Std.Dev."),digits=3)
anova(model.aa, model.ab, model.ac)

stargazer(model.ac, header=FALSE, type='text', font.size = "scriptsize", 
          single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
          float = FALSE, order=c("Leading Actor Gender","Female Director", "Supporting Actor Female", "Leading Actor Age","Drama", 
                                 "Comedy", "Action","Duration of Film in Minutes","Total Number of Artistic Terms in Review","Review Length",
                                 "Elite Newspaper"))

# aadding the "exp2" variable that measures the experience or reviewing frequency of a film critics by taking into account the total number of reviews by a certain critic included in the sample
# within the 3 years that have included in the study (1998, 2008, 2018). The exp2 variable is a categorical variable with 3 (high,mid,low experience) categories. The distribution of experience
# among film critics in the dataset was right-skewed, so the conversion to a categorical variable that simplifies the interpretation of the results

summary(model.ad <- glmer(real_sentiment ~ lead.actor.gender
                  + female.director
                  + lead.actor.age
                  + drama
                  + comedy
                  + action
                  + exp2#*lead.actor.gender
                  + minutes
                  + genre.number
                  + (1|newspaper.name) + (1|year2), data=dat, family="binomial"))

summary(model.i2 <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender
                  + lead.actor.age 
                  + author.female +  runtimeMinutes + word.count +
                  + drama + comedy + action 
                  + exp2
                  + year2
                  + word.cat
                  + (1|newspaper.name) + (1|year2)
                  ,data=dat, family = "binomial"))

# in the following model I have included a random slope for the gender of the leading actor
model.mm <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender + drama + comedy + lead.actor.age +
                    exp2
                  + (1|newspaper.name) + (lead.actor.gender|year2),data=dat, family = "binomial")


model.llm <- glmer(real_sentiment ~ lead.actor.gender*second.actor.gender + drama + comedy + lead.actor.age + 
                   exp2 
                   + (1|newspaper.name) + (1|year2),data=dat, family = "binomial")

# running an anova to compare between model.mm and model.llm to examine whether the inclusion of a random inctercept for the gender of the leading actor improves the mdoel
anova(model.llm, model.mm)


# in the following chunk of code I assign the labels for the variables in the dataset so that the output of the tab_mdoel function is easier to udnerstand
pl <- c(
  #`(Intercept)` = "Intercept",
  dat$lead.actor.gender = "Female Leading Actor",
  second.actor.gender = "Female Supporting Actor", 
  drama = "Drama", 
  lead.actor.age = "Leading Actor's Age (Older)",
  author.female = "Review Author Female ",
  comedy = "Comedy", 
  action = "Action",
  exp2mid.exp2 = "Reviewing Frequency: Mid",
  exp2little.exp2 = "Reviewing Frequency: Low",
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

# using the function tab_model for better presentation of the regression results as a HTML table
tab_model(model.mm)

# use ggpredict to visualize the relationship between 
ggpredict(model.ln, c("comedy","lead.actor.gender")) %>% plot()

# in the next model I include a three-way interaction term between lead.actor.gender, year2 (the year a review was published, and 
# author.female (whether the author of a film review is male or female)
model.ll <- glmer(real_sentiment ~ lead.actor.gender + drama + lead.actor.age 
                  + comedy + action  
                  + lead.actor.gender*year2*author.female
                  + (year2|newspaper.name),data=dat, family = "binomial", 
                  control = glmerControl(optimizer ="Nelder_Mead"))

# plotting the 3-way interaction term using the plot_model function to produce a series of graphs regarding the three variables 
plot_model(model.ll, type = "pred", terms = c("exp", "author.female", "lead.actor.gender"))
#plot_model(model.ll, type = "pred", terms = c( "lead.actor.gender", "year2", "author.female"))

interplot(m = model.ll, var1 = "author.female", var2 = "year2")

# models with author.female as dv
summary(model.k <- glmer(author.female ~  (1|newspaper.name),data=dat, family = "binomial"))

model.l <- glmer(author.female ~ lead.actor.gender*female.director + (1|newspaper.name:year2),data=dat, family = "binomial")

model.m <- glmer(author.female ~ lead.actor.gender + second.actor.gender + (1|newspaper.name/year2),data=dat, family = "binomial")

# comparison between models with cross-nested 2-level design and 3-level design - with year2 as the level-2 and newspaper.name as the level-3 variable
summary(model.n <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender +
                      comedy + drama + action +
                   (1|newspaper.name) + (1|year2),data=dat, family = "binomial"))


model.rr <- glmer(author.female ~ lead.actor.gender + female.director + second.actor.gender + 
                    comedy + drama + action +
                    (1|newspaper.name:year2),data=dat, family = "binomial")


# fixed-effexts regression model for the data from 2018 only
summary(model.s2b <- glm(author.female ~ lead.actor.gender
                          + lead.actor.age*second.actor.age
                          #+ female.director
                          + second.actor.gender
                          + drama
                          + comedy
                          + action
                          + critical.terms
                          + exp2*lead.actor.gender
                          + year2*lead.actor.gender
                          + newspaper.name
                          , data= dat18, family="binomial"))

# count the unique values for author id within one 
dat98 %>% count(author.id.x)  

## model.s3 includes variables for the level-2 variable = newspaper.name

summary(model.s3 <- glmer(author.female ~ lead.actor.gender
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
                          + exp2*lead.actor.gender
                          + #year2#*lead.actor.gender
                          + (1|newspaper.name) + (1|year2), data=dat, family="binomial"))

# diagnostics for model s3
model.s3.re = ranef(model.s3, condVar=TRUE)
pa2.re.df = as.data.frame(model.s3.re)
qqnorm(pa2.re.df$condval)

check_model(model.s3a)


model.oo <- glmer(author.female ~ lead.actor.gender + lead.actor.age + comedy + drama + action#+ second.actor.gender #+ art.terms
                  + exp2 
                  +	(1|newspaper.name:year2),data=dat, family = "binomial", 
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
                   + exp2
                   #+ year2*lead.actor.gender
                   +	(1|newspaper.name) #+ (1 |year2)
                   ,data=dat, family = "binomial", 
                   control = glmerControl(optimizer ="Nelder_Mead")))

exponentiate <- function(x) exp(x)
stargazer(model.oz, header=FALSE, type='latex', font.size =
            "scriptsize", single.row = TRUE, model.numbers = FALSE,
          apply.coef=exponentiate, float = FALSE, star.char = "", omit.table.layout = "l",
          column.labels = c("model.oz"))



summary(model.t <- glmer(author.female ~ lead.actor.gender 
                 + lead.actor.age 
                 + comedy + drama 
                 + elite.np 
                 + exp2
                 +	(1|newspaper.name) + (1|year2),data=dat, family = "binomial", 
              control = glmerControl(optimizer ="Nelder_Mead")))

model.z2 <- glmer(author.female ~  
                   lead.actor.gender + lead.actor.age + elite.np 
                 + second.actor.gender + female.director
                 + exp2
                 + action + comedy + drama
                 + (1|newspaper.name) + (1|year2),data=dat, family = "binomial"
                 ,control = glmerControl(optimizer ="Nelder_Mead"))


plot_model(model.z2, type = "pred", terms = c( "lead.actor.gender", "year2"))


summary(model.x)$coefficients
print(VarCorr(model.x),comp=c("Variance","Std.Dev."),digits=3)
mb.re = ranef(model.x, condVar=TRUE)

dotplot(mb.re, main = FALSE)

stargazer(model.b, model.c, header=FALSE, type='latex', font.size = "scriptsize", 
          single.row = TRUE, omit.table.layout = "l", model.numbers = FALSE, 
          float = FALSE)
