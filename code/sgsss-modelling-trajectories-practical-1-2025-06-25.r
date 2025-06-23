name <- readline(prompt="Enter name: ")
print(paste("Hi,", name, "enjoy learning more about R and statistical models!"))

x * 5

x <- 10 # create an object called 'x' and give it the value '10'

x * 5 # multiply 'x' by 5

install.packages("aod")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lme4")
install.packages("lmerTest")
install.packages("lmtest")

library(aod)      # Conducting Wald tests
library(tidyverse) # Various data management functions
library(ggplot2)  # Graphs
library(lme4)     # Estimating multilevel models
library(lmerTest) # Printing p-values for the regression coefficients
library(lmtest)   # Conducting likelihood ratio tests

options(scipen=999) # disable scientific notation

df <- read.csv("https://raw.githubusercontent.com/DiarmuidM/sgsss-modeling-trajectories-2024/main/data/la-stats-1971-2021-analysis.csv", header=TRUE, na="NA", sep=",")
head(df, n=12) # view the first twelve observations

str(df)

df$pph_cat <- factor(df$pph_cat, levels = c(1,2,3,4,5), labels = c("Most Urban", "Very Urban", "Urban", "Rural", "Most Rural"))

df$region <- factor(df$region, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("North East", "North West", "Yorkshire and The Humber", 
                                                                            "East Midlands", "West Midlands", "East of England",
                                                                           "London", "South East", "South West", "Wales"))

length(unique(df$la_id))

length(unique(df$year))

df %>% count(year)

summary(df$charpop)

summary(df$town)

summary(df$qual)

summary(df$nonbw)

table(df$region)

table(df$pph_cat)

df[df$la_id == 258 | df$la_id == 91, 
       c("la_name" , "year", "charpop" , "town" , "qual" , "nonbw", "region", "pph_cat")]

# INSERT CODE HERE #

df_means <- df %>% group_by(la_id) %>% summarise(charpop=mean(charpop))

mn_charpop <- mean(df_means$charpop)

ggplot(df_means, aes(x = la_id, y = charpop)) + 
    geom_point() +
    geom_hline(yintercept=mn_charpop) + 
    theme_classic()

# INSERT CODE HERE #

df$sample <- df$la_id %in% c(258, 91, 4, 329, 155, 33)

ggplot(df[df$sample == TRUE, ], aes(x = year, y = charpop)) +
    geom_point() +
    geom_line() +
    facet_wrap(~la_id) +
    theme_classic()

ggplot(df, aes(x = year, y = charpop, group = la_id)) +
    geom_point() +
    geom_line() +
    theme_classic()

m1 <- lm(charpop ~ 1, data = df)
summary(m1)

logLik(m1) # log-likelihood of the model - used in comparing nested models

-2*logLik(m1) # deviance of the model - used in comparing nested models

m2 <- lmer(charpop ~ 1 + (1 | la_id), data = df, REML = FALSE)
summary(m2)

rpm2 <- as.data.frame(VarCorr(m2))

# VPC/ICC = var(u)/[var(u) +var(e)]
rpm2$vcov[rpm2$grp == "la_id"] / sum(rpm2$vcov)

lrtest(m1, m2)

df$period2 <- df$period^2

m3 <- lmer(charpop ~ period + period2 + (1 | la_id), data = df, REML = FALSE)
summary(m3)

m4 <- lmer(charpop ~ period + period2 + (1 + period + period2 | la_id), data = df, REML = FALSE)
summary(m4)

m4 <- lmer(charpop ~ period + period2 + (1 + period | la_id), data = df, REML = FALSE)
summary(m4)

lrtest(m2, m4)

df$xbu1 <- predict(m4)

# Plot the fitted growth curves
ggplot(df, aes(x = year, y = xbu1, group = la_id)) +
    geom_line() +
    theme_classic()

df$region <- relevel(df$region, "London") # set London as the base / reference category for the region variable for model estimation

m5 <- lmer(charpop ~ period + period2 + region + (1 + period | la_id), data = df, REML = FALSE)
summary(m5)

# INSERT CODE HERE #

# INSERT CODE HERE # 

df$pph_cat <- relevel(df$pph_cat, "Rural") # set Rural as the base / reference category for the urban/rural variable for model estimation

m6 <- lmer(charpop ~ period + period2 + region + pph_cat + town + qual + nonbw + (1 + period | la_id), data = df, REML = FALSE)
summary(m6)

lrtest(m5, m6)

rpm6 <- as.data.frame(VarCorr(m6))

# VPC/ICC = var(u)/[var(u) +var(e)]
rpm6$vcov[rpm6$grp == "la_id"] / sum(rpm6$vcov)
# note we are only interested in first result

df$xbu1 <- predict(m6)

# Plot the fitted growth curves
ggplot(df, aes(x = year, y = xbu1, group = la_id)) +
    geom_line() +
    theme_classic()

ggplot(df, aes(x = year, y = xbu1, group = la_id)) +
    geom_line() +
    facet_wrap(~region) +
    theme_classic()

u01m6 <- data.frame(ranef(m6), condVar = TRUE)
sample_n(u01m6, 20) # sample 20 observations from the random effects dataset

u0m6 <- u01m6[u01m6$term=="(Intercept)",]
u1m6 <- u01m6[u01m6$term=="period",]

# Calculate the lower and upper values of the confidence intervals
u0m6$lower <- u0m6$condval - 1.96*u0m6$condsd
u0m6$upper <- u0m6$condval + 1.96*u0m6$condsd
# By multiplying  the standard errors by 1.96 we calculate 95% confidence
# intervals

# Rank the local authority effects
u0m6$rank <- rank(u0m6$condval)
sample_n(u0m6, 10)

ggplot(u0m6, aes(x = rank, y = condval, ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0) +
    geom_errorbar() +
    geom_point() +
    theme_classic()

# Calculate the lower and upper values of the confidence intervals
u1m6$lower <- u1m6$condval - 1.96*u1m6$condsd
u1m6$upper <- u1m6$condval + 1.96*u1m6$condsd
# By multiplying  the standard errors by 1.96 we calculate 95% confidence
# intervals

# Rank the local authority effects
u1m6$rank <- rank(u1m6$condval)
sample_n(u1m6, 10)

ggplot(u1m6, aes(x = rank, y = condval, ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0) +
    geom_errorbar() +
    geom_point() +
    theme_classic()
