source("mars.R")
source("predict_mars.R")
source("summary_mars.R")
source("plot_mars.R")
source("anova_mars.R")
source("print_mars.R")
library(ISLR)

# First test
data(Wage)
mc <- mars.control(Mmax = 10)
mout <- mars(wage ~ age + education, data=Wage, control=mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout,newdata=data.frame(age = Wage$age, education = Wage$education))
head(cbind(ff,p1,p2)) # columns should be identical
mout # tests print method
summary(mout) #test summary method
anova(mout) # test anova method
plot(mout, "education", color = "lightgreen") # test plot method
plot(mout, "age")


# Second test
premier_league <- read.csv("premier_league_stats.csv") # Premier league team stats from season 2006/2007 to 2017/2018
mc1 <- mars.control(Mmax = 10)
mout1 <- mars(wins ~ goals + clean_sheet, data = premier_league, control = mc1) # Clean_sheet: Number of matches that the team did not concede a goal
ff1 <- fitted(mout1)
p11 <- predict(mout1)
p21 <- predict(mout1,newdata=data.frame(goals = premier_league$goals, clean_sheet = premier_league$clean_sheet))
head(cbind(ff1,p11,p21))
mout1
summary(mout1)
anova(mout1)
plot(mout1, "clean_sheet", color = "purple")


# Third Test
polar_bear <- read.csv("polar_bear.csv") # This dataset details polar bears captured in the Chukchi and Beaufort Seas, 1981-2017 by the U.S. Geological Survey and U.S. Fish and Wildlife Service
mc2 <- mars.control(Mmax = 10)
mout2 <- mars(Mass ~ Age + Sex, data=polar_bear, control=mc2)
ff2 <- fitted(mout2)
p12 <- predict(mout2)
p22 <- predict(mout2,newdata=data.frame(Age = polar_bear$Age, Sex = polar_bear$Sex))
head(cbind(ff2,p12,p22))
mout2
summary(mout2)
anova(mout2)
plot(mout2, color = "blue")
