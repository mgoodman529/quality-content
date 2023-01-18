# Evaluation Question #1- How has the average age of a Bachelor Contestant changed over time (from 2002-as close to present as data allows)? 
## *This data covers Seasons 1-23 (2002-2019)
## Use ANCOVA

# Evaluation Question #2- Is a Bachelor contestant’s hometown region a predictor for success on the show?
## Use Independent Chi Square

# Import data

# Load Libraries for ANCOVA- Evaluation Question #1

library("rcompanion")
library("car")
library("effects")
library("multcomp")
library("dplyr")

# Load Libraries for Independent Chi Square- Evaluation Question #2

install.packages("gmodels")
library("gmodels")

# ANCOVA Data Wrangling

## Subset of data- Only seasons

bcageseasononly <- c("age", "season")
bachelorcontestants2 <- bachelorcontestants[bcageseasononly]

str(bachelorcontestants$season)

## Output shows that it is an INTEGER- Need to convert to a factor

bachelorcontestants$season <- as.factor(bachelorcontestants$season)

str(bachelorcontestants$season)

## Success!  Now IV is a factor

## Make sure CV's are factors

str(bachelorcontestants$hometown.region)
str(bachelorcontestants$occupation)

### Both not a factor

bachelorcontestants$hometown.region <- as.factor(bachelorcontestants$hometown.region)
bachelorcontestants$occupation <- as.factor(bachelorcontestants$occupation)

str(bachelorcontestants$hometown.region)
str(bachelorcontestants$occupation)

### Success! Both CV's now a factor

# ANCOVA Testing Assumptions

## Normality

plotNormalHistogram(bachelorcontestants$age)

### Not too bad- but could probably be a little more normal- Try square formation

bachelorcontestants$ageSQ <- bachelorcontestants$age * bachelorcontestants$age
plotNormalHistogram(bachelorcontestants$ageSQ)

### Still not great- Try log

bachelorcontestants$ageLOG <- log(bachelorcontestants$age)
plotNormalHistogram(bachelorcontestants$ageLOG)

### Best option!  Go with log- Now test meets the assumptions of normality

## Homogeneity of Variance

leveneTest(ageLOG~season, data=bachelorcontestants)

### This test is not significant- Assumption met

## Homogeneity of Regression Slopes ***

Homogeneity_RegrSlp = lm(ageLOG~hometown.region, data=bachelorcontestants)
anova(Homogeneity_RegrSlp)

### P-value is significant, so assumption not met- Do NOT use 'hometown region' as covariate; Instead use as second IV? ***

Homogeneity_RegrSlp1 = lm(ageLOG~occupation, data=bachelorcontestants)
anova(Homogeneity_RegrSlp1)

### P-value is not significant, so assumption is met- 'occupation' good to go as CV

# Independent Chi Square Data Wrangling- Should I subset for just 'Winners/Runner Ups" or leave as is? (see "as is" at 245)

## Subset data for only 'eliminated' and 'hometown.region'

bcwinnersrunnersup <- c("hometown.region", "eliminated")
bachelorcontestants3 <- bachelorcontestants[bcwinnersrunnersup]

## Filter out so only left with Winners/Runner Ups

bachelorcontestants4 <- subset(bachelorcontestants3, eliminated %in% c("Winner", "Runner-Up", "Runner-up"))

# Change all "Runner-up" to "Runner-Up"

bachelorcontestants4[bachelorcontestants4 == "Runner-up"] <- "Runner-Up"

# Independent Chi Square Testing Assumptions

## First assumption is to have independent data- Each person/object must be able to fit in only one cell; Met? ***

## Second assumption is that your expected frequencies must be greater than 5 for each cell

CrossTable(bachelorcontestants4$hometown.region, bachelorcontestants4$eliminated, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format= "SPSS")

## Many Expected Values less than 5- Does not meet assumption > account for it in explanation (say "it requires additional research)

## Results: p-Value > 0.05 (Analysis not significant; Hometown region does not impact contestant success on the show); No need to do Post-Hocs since results not significant

# ANCOVA Data Wrangling- Try 2 (Remove 'hometown' column to see if all assumptions met)

## Subset of data- without 'hometown'

bcwithouthometown <- c("column_a", "age", "eliminated", "hometown.region", "name", "occupation", "season")
bachelorcontestants5 <- bachelorcontestants[bcwithouthometown]

str(bachelorcontestants5$season)

## Output shows that it is a FACTOR- good! IV is a factor

## Make sure CV's are factors

str(bachelorcontestants5$hometown.region)
str(bachelorcontestants5$occupation)

### Both factors- success!

# ANCOVA Testing Assumptions- Try 2 (without 'hometown' column)

## Normality

plotNormalHistogram(bachelorcontestants5$age)

### Not too bad- but could probably be a little more normal- Try square formation

bachelorcontestants5$ageSQ <- bachelorcontestants5$age * bachelorcontestants5$age
plotNormalHistogram(bachelorcontestants5$ageSQ)

### Still not great- Try log

bachelorcontestants5$ageLOG <- log(bachelorcontestants5$age)
plotNormalHistogram(bachelorcontestants5$ageLOG)

### Best option!  Go with log- Now test meets the assumption of normality

## Homogeneity of Variance

leveneTest(ageLOG~season, data=bachelorcontestants5)

### This test is not significant- Assumption met

## Homogeneity of Regression Slopes ***

Homogeneity_RegrSlp2 = lm(ageLOG~hometown.region, data=bachelorcontestants5)
anova(Homogeneity_RegrSlp2)

### P-value is still significant, so assumption still not met- Do NOT use 'hometown region' as covariate

Homogeneity_RegrSlp3 = lm(ageLOG~occupation, data=bachelorcontestants5)
anova(Homogeneity_RegrSlp3)

### P-value is not significant, so assumption is met- 'occupation' good to go as CV

## This ended up the same as original

# Try 3- Instead of ANCOVA, try running as separate ANOVA with 'hometown.region' only

# ANOVA Data Wrangling- Try  3 (do only 'age' and 'season'); One Way "Within Subjects/Repeated Measures" ANOVA

str(bachelorcontestants2$season)
bachelorcontestants2$season <- as.factor(bachelorcontestants2$season)
str(bachelorcontestants2$season)
 
## Subset of data- with only 'age' and 'season'
### Already subset in bachelorcontestants2

# Split into baseline data and follow-up data

## Getting the data in the right shape for the baseline measure- Rows 1-177 (Seasons 1-12, data excludes Seasons 6-8)

bcbaseline <- bachelorcontestants2[1:177, 1:2]
bcbaseline$contrasts <- "T1"

# Getting the data in the right shape for the follow-up measure- Rows 178-476

bcfollowup <- bachelorcontestants2[178:476, 1:2]
bcfollowup$contrasts <- "T2"

# Then smoosh 'em back together with binding
bcbaselinefollowup <- rbind(bcbaseline, bcfollowup)

# ANOVA Testing Assumptions- Try 3

## Normality for Baseline

plotNormalHistogram(bcbaseline$age)

### Not too bad- but could probably be a little more normal- Try square formation

bcbaseline$ageSQ <- bcbaseline$age * bcbaseline$age
plotNormalHistogram(bcbaseline$ageSQ)

### Still not great- Try log

bcbaseline$ageLOG <- log(bcbaseline$age)
plotNormalHistogram(bcbaseline$ageLOG)

### Go with log for Baseline

## Normality for Follow Up

plotNormalHistogram(bcfollowup$age)

### Not too bad- but could probably be a little more normal- Try square formation

bcfollowup$ageSQ <- bcfollowup$age * bcfollowup$age
plotNormalHistogram(bcfollowup$ageSQ)

### Still not great- Try log

bcfollowup$ageLOG <- log(bcfollowup$age)
plotNormalHistogram(bcfollowup$ageLOG)

### Best option!  Go with log- Now test meets the assumption of normality

## Homogeneity of Variance

leveneTest(age~season*contrasts, data=bcbaselinefollowup)

### This test is not significant- Assumption met

## Sample Size- A repeated measures ANOVA requires a sample size of at least 20 per independent variable. We have that, so this assumption has been met.

## Sphericity- Assumption automatically included; Good to go

# This all looks pretty good!  But try again with a subset of 'age', 'season', and 'column_a' to use 'column_a' as 'Error' in ANOVA analysis

# Independent Chi Sq.- Run 'as is'

# Independent Chi Square Data Wrangling- Should I subset for just 'Winners/Runner Ups" or leave as is? (see "as is" at 245)

# Subset without 'ageSQ' and 'ageLOG" / Change all "Runner-up" to "Runner-Up"

## Subset of data- without 'hometown'

bcwithoutagesqagelog <- c("column_a", "age", "eliminated", "hometown", "hometown.region", "name", "occupation", "season")
bachelorcontestants6 <- bachelorcontestants[bcwithoutagesqagelog]

bachelorcontestants6[bachelorcontestants6 == "Runner-up"] <- "Runner-Up"

# Independent Chi Square Testing Assumptions

## First assumption is to have independent data- Each person/object must be able to fit in only one cell; Met

## Second assumption is that your expected frequencies must be greater than 5 for each cell

CrossTable(bachelorcontestants6$hometown.region, bachelorcontestants6$eliminated, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format= "SPSS")

## Expected values mostly over 5 but really difficult to read- Go with original Ind. Chi Sq

# Evaluation Question #1- Repeated Measures ANOVA

## Re-do Data Wrangling to have Subset of data- with only 'age' and 'season' and 'column_a'

bcageseasoncolumnaonly <- c("age", "season", "column_a")
bachelorcontestants7 <- bachelorcontestants[bcageseasoncolumnaonly]

str(bachelorcontestants7$season)

# Already a factor

# Split into baseline data and follow-up data

## Getting the data in the right shape for the baseline measure- Rows 1-177 (Seasons 1-12, data excludes Seasons 6-8)

bcbaseline1 <- bachelorcontestants7[1:177, 1:3]
bcbaseline1$contrasts <- "T1"

# Getting the data in the right shape for the follow-up measure- Rows 178-476

bcfollowup1 <- bachelorcontestants7[178:476, 1:3]
bcfollowup1$contrasts <- "T2"

# Then smoosh 'em back together with binding
bcbaselinefollowup1 <- rbind(bcbaseline1, bcfollowup1)

# ANOVA Testing Assumptions

## Normality for Baseline

plotNormalHistogram(bcbaseline1$age)

### Not too bad- but could probably be a little more normal- Try square formation

bcbaseline1$ageSQ <- bcbaseline1$age * bcbaseline1$age
plotNormalHistogram(bcbaseline1$ageSQ)

### Still not great- Try log

bcbaseline1$ageLOG <- log(bcbaseline1$age)
plotNormalHistogram(bcbaseline1$ageLOG)

### Go with log for Baseline

## Normality for Follow Up

plotNormalHistogram(bcfollowup1$age)

### Not too bad- but could probably be a little more normal- Try square formation

bcfollowup1$ageSQ <- bcfollowup1$age * bcfollowup1$age
plotNormalHistogram(bcfollowup1$ageSQ)

### Still not great- Try log

bcfollowup1$ageLOG <- log(bcfollowup1$age)
plotNormalHistogram(bcfollowup1$ageLOG)

### Best option!  Go with log- Now test meets the assumption of normality

## Homogeneity of Variance

leveneTest(age~season*contrasts, data=bcbaselinefollowup1)

### This test is not significant- Assumption met

## Sample Size- A repeated measures ANOVA requires a sample size of at least 20 per independent variable. We have that, so this assumption has been met.

## Sphericity- Assumption automatically included; Good to go

# Evaluation Question #1- Repeated Measures ANOVA; RUN THE ANALYSIS

library(tidyverse)
library(ggpubr)
library(rstatix)

BCanova <- aov(age~season+Error(column_a), bcbaselinefollowup1)
summary(BCanova)

# There is not a significant effect of season (time) on age of contestants
