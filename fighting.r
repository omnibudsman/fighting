library(tidyverse)


####### Insheet

fight.raw <- read.csv("archive/data.csv")

####### Clean
fight.raw$R.B.dif <- fight.raw$R_Weight_lbs - fight.raw$B_Weight_lbs
fight.raw$R.B.reach.dif <- fight.raw$R_Reach_cms - fight.raw$B_Reach_cms
fight.raw$R.B.height.dif <- fight.raw$R_Height_cms - fight.raw$B_Height_cms
fight.raw$R.won <- as.numeric(fight.raw$Winner == "Red")

####### Pare

bigdif <- fight.raw %>% subset(abs(R.B.dif) >= 15)

####### Analyze weight (models)
m <- glm(formula = R.won ~ R.B.dif, data=fight.raw, family=binomial(link="logit"))
summary(m)

m2 <- lm(formula = R.won ~ R.B.dif, data=fight.raw)
summary(m2)

####### Analyze reach (models)
m.reach <- glm(formula = R.won ~ R.B.reach.dif, data=fight.raw, family=binomial(link="logit"))
summary(m.reach)

####### Full model

m3 <- glm(formula = R.won ~  R.B.height.dif + R.B.dif, data=fight.raw, family=binomial(link="logit"))
summary(m3)

m3.lpm <- lm(formula = R.won ~ R.B.height.dif + R.B.dif, data=fight.raw)
summary(m3.lpm)

####### Analyze subsets (models)
full.subset <- glm(formula = R.won ~  R.B.height.dif + R.B.dif, data=bigdif, family=binomial(link="logit"))
summary(full.subset)

####### Analyze exploratory)

# general weight dif
hist(fight.raw$R.B.dif)

# subset weight dif
hist(bigdif$R.B.dif)

# subset height dif
hist(bigdif$R.B.height.dif)

