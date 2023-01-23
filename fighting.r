library(tidyverse)


####### Insheet

ufc.raw <- read.csv("archive/data.csv")
fights.only.raw <- read.csv("archive/raw_total_fight_data.csv", sep=";")

####### Clean

# set up mrtge to get missing win_by data
fights.only <- fights.only.raw %>% select(R_fighter, B_fighter, win_by, date) %>% mutate(date = as.character(as.Date(date, "%B %d, %Y")))

# set up main dataset
fight.raw <- merge(ufc.raw, fights.only, by=c("R_fighter", "B_fighter", "date"))

fight.raw$R.B.dif <- fight.raw$R_Weight_lbs - fight.raw$B_Weight_lbs
fight.raw$R.B.reach.dif <- fight.raw$R_Reach_cms - fight.raw$B_Reach_cms
fight.raw$R.B.height.dif <- fight.raw$R_Height_cms - fight.raw$B_Height_cms
fight.raw$R.won <- as.numeric(fight.raw$Winner == "Red")

####### Pare

# Note that favored fighters sit in the red corner!!!

paring <- c("R_fighter", 'B_fighter', "R_Weight_lbs", "B_Weight_lbs", "R.B.dif", "R.B.reach.dif", "R.B.height.dif", "R_Reach_cms", "B_Reach_cms", "R_Height_cms", "B_Height_cms", "R.won")

# only tkos or submissions
real.finish.only <- fight.raw %>% subset(win_by == "Submission" | win_by == "KO/TKO")

# only big differences
bigdif <- real.finish.only %>% subset(abs(R.B.dif) >= 15) %>% select(paring)

# only heavyweights
heavy <- real.finish.only %>% subset(weight_class == "Heavyweight")


####### Analyze KOs/submissions only

m.kos.only <- glm(formula = R.won ~  R.B.height.dif + R.B.dif, data=real.finish.only, family=binomial(link="logit"))
summary(m.kos.only)

####### Analyze KOs/submissions only where there's a big weight difference

m.kos.only.bigdif <- glm(formula = R.won ~  R.B.height.dif + R.B.dif, data=bigdif, family=binomial(link="logit"))
summary(m.kos.only.bigdif)

####### Analyze KOs/submissions only among heavyweights

m.heavy <- glm(formula = R.won ~  R.B.height.dif + R.B.dif, data=heavy, family=binomial(link="logit"))
summary(m.heavy)

# among heavyweights, not even height matters!


####### Analyze big weight differences
heaviest <- heavy %>% subset(abs(R.B.dif) >= 50)
heaviest$heavier.won <- as.numeric(as.numeric(heaviest$R.B.dif <= 0) != heaviest$R.won)
mean(heaviest$heavier.won)

####### Predictions

newdata.means <- mean(real.finish.only$R.B.dif, na.rm=T)
newdata.heightdif <- c(0,1,5,10,15)
newdata <- expand.grid(R.B.dif = 0, R.B.height.dif=newdata.heightdif)

height.predictions <- predict.lm(m.kos.only, newdata=newdata, interval="prediction")
predict.frame <- cbind(newdata, height.predictions)

ggplot(predict.frame) +
  geom_point(aes(x = R.B.height.dif, y = fit - 0.82))
  #geom_errorbar(aes(x = R.B.height.dif, ymin = lwr - 0.82, ymax = upr - 0.82))


# notes

# simpson vs herman - bigger guy got hurt
# https://mmabouts.fandom.com/wiki/Aaron_Simpson_vs._Ed_Herman







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

