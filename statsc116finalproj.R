#STATS C116 Final Project 
# Emma Chi

library(readxl)
library(rstanarm)
library(foreign)
library(MASS)
library(car)
library(mlmRev)
library(lme4)
library(dplyr)
library(car)
library(loo)


#------load data------#
setwd("~/Desktop/statsc116finalproj")
data <- read_excel("ISL Acculturation’s Effects on Eating Disorders - Research Survey (Responses).xlsx")
head(data)

ethnicity <- c(data$"ETHNICITY")
generation <- c(data$"GENERATION")
smas <- c(data$"SMAS SCORE")
restraint <- c(data$"EDEQ 1 - RESTRAINT")
eating <- c(data$"EDEQ 2 - EATING CONCERN")
shape <- c(data$"EDEQ 3 - SHAPE CONCERN")
weight <- c(data$"EDEQ 4 - WEIGHT CONCERN")
global<- c(data$"EDEQ GLOBAL SCORE") 

#------Factor Data-----#
generation <- factor(generation)
#generation <- (unclass(generation2)) # make factor instead of unclass
class(generation)
head(generation)

head(ethnicity)
ethnicity <- factor(ethnicity)
class(ethnicity)
head(ethnicity)

df <- data.frame(generation, smas, restraint, eating, shape, weight, global)
head(df)

df2 <- data.frame(ethnicity, smas, restraint, eating, shape, weight, global)
head(df2)


#-----exploratory analysis-----#
#-----scatterplot-----#
plot(smas, ylab="SMAS Score")

#-----histogram-----#
hist(smas,
     breaks=8,
     main=" ",
     xlab="SMAS Score",
     ylab="Frequency",
     xlim=c(40,140),
     ylim=c(0,0.048),
     col="grey",
     freq=FALSE
)

#-----side by side boxplot-----#
boxplot(df$smas ~ df$generation,
        col="grey",
        main=' ',
        xlab='Generation',
        ylab='SMAS Score')

boxplot(df$global ~ df$generation,
        col="grey",
        main=' ',
        xlab='Generation',
        ylab='Global EDE-Q Score')

#-----ethnicity grouped bar plot-----#
table(ethnicity, useNA="always")

ethnicity.1 <- data %>% filter(generation=="First Generation") %>% select("ETHNICITY")
table(ethnicity.1,useNA="always")
ethn.1 <- c(29, 1, 41, 5, 1)

ethnicity.2 <- data %>% filter(generation=="Second Generation") %>% select("ETHNICITY")
table(ethnicity.2,useNA="always")
ethn.2 <- c(24, 0, 26, 6, 0)

ethn <- c(ethn.1, ethn.2)
ethn

gen <- c("Gen 1", "Gen 2")

gfg <- data.frame(ethn, 
                  grp = rep(gen, each = 5),
                  subgroup = LETTERS[1:5])

gfg <- reshape(gfg,idvar = "subgroup",
               timevar = "grp",
               direction = "wide")

row.names(gfg) <- gfg$subgroup
gfg <- gfg[ , 2:ncol(gfg)]
colnames(gfg) <- gen
gfg <- as.matrix(gfg)

color <- colorRampPalette(colors = c("orange", "light blue"))(5)

barplot(height = gfg,beside = TRUE, ylim = c(0,50), xlab = "Generation", ylab = "Count", 
        main = " ", col = color, 
        legend=c("East Asian", "North Asian", "South Asian", "Southeast Asian", "West Asian"),  
        args.legend = list(x = "topright", inset = c( 0, -0.2)))

#-----edeq subscores bar chart-----#
pdf("edeqsubscores.pdf", width=20, height=10)

gen1 <- df %>% filter(generation=="First Generation")
gen2 <- df %>% filter(generation=="Second Generation")

avgrestraint.1 <- mean(gen1$restraint)
avgrestraint.2 <- mean(gen2$restraint)
avgrestraint <- c(avgrestraint.1, avgrestraint.2)
avgrestraint

avgeating.1 <- mean(gen1$eating)
avgeating.2 <- mean(gen2$eating)
avgeating <- c(avgeating.1, avgeating.2)
avgeating

avgshape.1 <- mean(gen1$shape)
avgshape.2 <- mean(gen2$shape)
avgshape <- c(avgshape.1, avgshape.2)
avgshape

avgweight.1 <- mean(gen1$weight)
avgweight.2 <- mean(gen2$weight)
avgweight <- c(avgweight.1, avgweight.2)
avgweight

avg <- c(avgrestraint, avgeating, avgshape, avgweight)
score <- c("Avg Restraint", "Avg Eating Concern", "Avg Shape Concern", "Avg Weight Concern")

gfg <- data.frame(avg, 
                  grp = rep(score, each = 2),
                  subgroup = LETTERS[1:2])

gfg <- reshape(gfg,idvar = "subgroup",
               timevar = "grp",
               direction = "wide")

row.names(gfg) <- gfg$subgroup
gfg <- gfg[ , 2:ncol(gfg)]
colnames(gfg) <- score
gfg <- as.matrix(gfg)

color <- colorRampPalette(colors = c("light grey", "dark grey"))(2)

barplot(height = gfg,beside = TRUE, ylim = c(0,3), xlab = "EDE-Q Subscore", ylab = "Score", 
        main = " ", col = color, 
        legend = c("First Generation", "Second Generation"), 
        args.legend = list(x = "topright", inset = c( 0, -0.3)))

dev.off()


#------Model Fitting------#
#------simple linear model------#
lm <- lm(global ~ smas + restraint + eating + shape + weight)
#print(lm)
summary(lm)
vif(lm) 
anova(lm)

lm2 <- lm(smas ~ generation)
summary(lm2)

lm3 <- lm(global ~ ethnicity)
summary(lm3)

lm4 <- lm(global ~ generation)
summary(lm4)

lm <- lm(global ~ ethnicity + restraint + eating + shape + weight)
print(lm)
summary(lm)


#--------linear mixed effects regression-------# 
fit0 <- stan_lmer(global ~ 1 + smas + (1 + smas | generation),
  data = df,
  prior_intercept = normal(location = 0, 
                           scale = 10, 
                           autoscale = TRUE))
summary(fit0, digits=3)
prior_summary(fit0, digits=3)


#------horseshoe prior-----#
simplefit <- stan_glm(global ~ generation + ethnicity + smas + restraint + eating  + shape + weight,
                 data=df, 
                 prior = normal(location = 1, 
                                scale = 1,
                                autoscale = TRUE),
                prior_intercept  = normal(location = 1, 
                               scale = 2.5,
                               autoscale = TRUE),
                 seed=1, refresh=0)

plot(simplefit, "areas", prob = 0.95, prob_outer = 1)

round(coef(simplefit), 2)
round(posterior_interval(simplefit, prob=0.9), 2)


#-----revised linear mixed effects models-----#
oldfit <- stan_lmer(global ~ smas +  (smas | generation), 
                  data = df,
                  seed = 345)
pp_check(oldfit)

fit1 <- stan_lmer(global ~ smas + shape + (smas | generation), 
                         data = df, 
                         prior = normal(location = 0, # prior on the variable; scale invariant prior; can change location to be vector - separate location for each var
                                        scale = 10, #forces sd of 10; pos relationship if there's a relationship between var and outcome; location >0
                                        autoscale = TRUE),
                         prior_intercept = normal(location = 0, #specifies prior on intercept; int is constant
                                                  scale = 10, 
                                                  autoscale = TRUE),
                         seed = 349)
summary(fit1)
plot(fit1, par="smas")

fit2 <- stan_lmer(global ~ smas + shape + weight + (smas | generation), 
                  data = df,
                  seed = 349)
prior_summary(fit2, digits=3) 

oldfit2 <- stan_lmer(global ~ smas + restraint + eating + shape + weight + 
                       (smas | generation), 
                     data = df, 
                     prior = normal(location = 1, 
                                    scale = 1, 
                                    autoscale = TRUE),
                     prior_intercept = normal(location = 2,
                                              scale = 2.5, 
                                              autoscale = TRUE),
                     seed = 349)
pp_check(oldfit2)
summary(oldfit2)

newfit1 <- stan_lmer(global ~ smas + restraint + eating + shape + weight + 
                       (smas + restraint + eating + shape + weight | generation), 
                     data = df, 
                     prior = normal(location = 1, 
                                    scale = 1, 
                                    autoscale = TRUE),
                     prior_intercept = normal(location = 1, 
                                              scale = 2.5, 
                                              autoscale = TRUE),
                     seed = 345)
pp_check(newfit1)
summary(newfit1)

fit3 <- stan_lmer(global ~ smas + shape + weight + (smas | ethnicity), 
                  data = df2,
                  seed = 345)
prior_summary(fit3, digits=3)


newfit2 <- stan_lmer(global ~ smas + restraint + eating + shape + weight + (smas + restraint + eating + shape + weight | ethnicity), 
                     data = df2, 
                     prior = normal(location = 1,
                                    scale = 2.5, 
                                    autoscale = TRUE),
                     prior_intercept = normal(location = 1, 
                                              scale = 2.5, 
                                              autoscale = TRUE),
                     seed = 345)
pp_check(newfit2)

plot(newfit2,plotfun="dens",
     pars="Intercept")

#------model plots------#
plot(newfit1, par="eating")
plot(newfit2, par="eating")

plot(newfit1,plotfun="trace",
     pars="smas")
plot(newfit2,plotfun="trace",
     pars="smas")


#------evaluating models------#
loo.1 <- loo(newfit1)
print(loo.1)

loo.2 <- loo(newfit2)
print(loo.2)

loo_compare(loo.1, loo.2)
