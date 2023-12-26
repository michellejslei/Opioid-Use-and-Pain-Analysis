library(tidyverse)
library(readxl)
library(aod)
library(DescTools)
library(car)

#read dataset
dat <- read_excel("~/Desktop/DIRECTED STUDIES DATASETS/pain test.xlsx")
head(dat)
summary(dat)
complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny, dat$EthnicityNum)
data2 <- dat[complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny, dat$EthnicityNum), ]
summary(data2$EthnicityNum == 4)
summary(data2$Income < 1800)

#factor variables
data2$OpioidNPAny = factor(data2$OpioidNPAny)

#################################################################################################
##PAIN
NPOp_Pain <- glm(OpioidNPAny ~ MaudsleyMaxPain, data = data2, family=binomial(link="logit"))
summary(NPOp_Pain)
wald.test(b = coef(NPOp_Pain), Sigma = vcov(NPOp_Pain), Terms = 1)
exp(cbind(OR = coef(NPOp_Pain), confint(NPOp_Pain)))
exp(cbind(OR = coef(NPOp_Pain), confint.default(NPOp_Pain, level= 0.85)))
Anova(NPOp_Pain, type="II", test="Wald")
PseudoR2(NPOp_Pain, c("McFadden", "Nagel", "CoxSnell"))
#R2 ~0 --> model doesn't explain data at all
#R2 ~0.15-0.25 --> model could be linear regression model
#R2 ~0.3-0.6 --> good fit for logistic model
#R2 ~1 --> perfect ideal


# create new data that has the same range as the old data
newdat_pain <- data.frame(MaudsleyMaxPain=seq(min(dat$MaudsleyMaxPain), max(dat$MaudsleyMaxPain),len=100))

# predict based on new data
newdat_pain <- newdat_pain %>% 
  mutate(OpioidNPAny = predict(object = NPOp_Pain, newdata = newdat_pain, type = "response"))

summary(newdat_pain)

# plot
NPOp_Pain_Logit <- ggplot(data = newdat_pain, mapping = aes(x = MaudsleyMaxPain, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##INCOME
NPOp_Income <- glm(OpioidNPAny ~ Income, data = data2, family=binomial(link="logit"))
summary(NPOp_Income)
wald.test(b = coef(NPOp_Income), Sigma = vcov(NPOp_Income), Terms = 1)
exp(cbind(OR = coef(NPOp_Income), confint(NPOp_Income)))
exp(cbind(OR = coef(NPOp_Income), confint.default(NPOp_Income, level= 0.85)))
Anova(NPOp_Income, type="II", test="Wald")
PseudoR2(NPOp_Income, c("McFadden", "Nagel", "CoxSnell"))


# create new data that has the same range as the old data
newdat_income <- data.frame(Income=seq(min(0), max(5000),len=100))

# predict based on new data
newdat_income <- newdat_income %>% 
  mutate(OpioidNPAny = predict(object = NPOp_Income, newdata = newdat_income, type = "response"))

summary(newdat_income)

# plot
NPOp_Income_Logit <- ggplot(data = newdat_income, mapping = aes(x = Income, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)


#################################################################################################
##AGE
NPOp_Age <- glm(OpioidNPAny ~ EntryAge, data = data2, family=binomial(link="logit"))
summary(NPOp_Age)
wald.test(b = coef(NPOp_Age), Sigma = vcov(NPOp_Age), Terms = 1)
exp(cbind(OR = coef(NPOp_Age), confint(NPOp_Age)))
exp(cbind(OR = coef(NPOp_Age), confint.default(NPOp_Age, level= 0.85)))
Anova(NPOp_Age, type="II", test="Wald")
PseudoR2(NPOp_Age, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_age <- data.frame(EntryAge=seq(min(dat$EntryAge), max(dat$EntryAge),len=100))

# predict based on new data
newdat_age <- newdat_age %>% 
  mutate(OpioidNPAny = predict(object = NPOp_Age, newdata = newdat_age, type = "response"))

summary(newdat_age)

# plot
NPOp_Age_Logit <- ggplot(data = newdat_age, mapping = aes(x = EntryAge, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##EDUC
NPOp_Edu <- glm(OpioidNPAny ~ HighestDegree, data = data2, family=binomial(link="logit"))
summary(NPOp_Edu)
NPOp_Edu
wald.test(b = coef(NPOp_Edu), Sigma = vcov(NPOp_Edu), Terms = 1)
exp(cbind(OR = coef(NPOp_Edu), confint(NPOp_Edu)))
exp(cbind(OR = coef(NPOp_Edu), confint.default(NPOp_Edu, level= 0.85)))
Anova(NPOp_Edu, type="II", test="Wald")
PseudoR2(NPOp_Edu, c("McFadden", "Nagel", "CoxSnell"))

data2$HighestDegree = factor(data2$HighestDegree)
summary(data2$HighestDegree)

# create new data that has the same range as the old data
newdat_edu <- data.frame(HighestDegree=seq(min(1), max(6),len=100))

# predict based on new data
newdat_edu <- newdat_edu %>% 
  mutate(OpioidNPAny = predict(object = NPOp_Edu, newdata = newdat_edu, type = "response"))

summary(newdat_edu)

# plot
NPOp_Edu_Logit <- ggplot(data = newdat_edu, mapping = aes(x = HighestDegree, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##OPIOIDBF18
NPOp_obf18 <- glm(OpioidNPAny ~ OpioidBefore18, data = data2, family=binomial(link="logit"))
summary(NPOp_obf18)
NPOp_obf18
wald.test(b = coef(NPOp_obf18), Sigma = vcov(NPOp_obf18), Terms = 1)
exp(cbind(OR = coef(NPOp_obf18), confint(NPOp_obf18)))
exp(cbind(OR = coef(NPOp_obf18), confint.default(NPOp_obf18, level= 0.85)))
Anova(NPOp_obf18, type="II", test="Wald")
PseudoR2(NPOp_obf18, c("McFadden", "Nagel", "CoxSnell"))

data2$OpioidBefore18 = factor(data2$OpioidBefore18)


# create new data that has the same range as the old data
newdat_obf18 <- data.frame(OpioidBefore18=seq(min(0), max(1),len=100))

# predict based on new data
newdat_obf18 <- newdat_obf18 %>% 
  mutate(OpioidNPAny = predict(object = NPOp_obf18, newdata = newdat_obf18, type = "response"))

summary(newdat_obf18)

# plot
NPOp_OBF18_Logit <- ggplot(data = newdat_obf18, mapping = aes(x = OpioidBefore18, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##GENDER
NPOp_Gender <- glm(OpioidNPAny ~ GenderTwo, data = data2, family=binomial(link="logit"))
summary(NPOp_Gender)
wald.test(b = coef(NPOp_Gender), Sigma = vcov(NPOp_Gender), Terms = 1)
exp(cbind(OR = coef(NPOp_Gender), confint(NPOp_Gender)))
exp(cbind(OR = coef(NPOp_Gender), confint.default(NPOp_Gender, level= 0.85)))
Anova(NPOp_Gender, type="II", test="Wald")
PseudoR2(NPOp_Gender, c("McFadden", "Nagel", "CoxSnell"))

data2$GenderTwo = factor(data2$GenderTwo)
count(data2, vars=data2$GenderTwo)
##female=1; male=0

# create new data that has the same range as the old data
newdat_gender <- data.frame(GenderTwo=seq(min(0), max(1),len=100))

# predict based on new data
newdat_gender <- newdat_gender %>% 
  mutate(OpioidNPAny = predict(object = NPOp_Gender, newdata = newdat_gender, type = "response"))

summary(newdat_gender)

# plot
NPOp_Gender_Logit <- ggplot(data = newdat_gender, mapping = aes(x = GenderTwo, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##POp
NPOp_POp <- glm(OpioidNPAny ~ OpioidPrescDays, data = data2, family=binomial(link="logit"))
summary(NPOp_POp)
wald.test(b = coef(NPOp_POp), Sigma = vcov(NPOp_POp), Terms = 1)
exp(cbind(OR = coef(NPOp_POp), confint(NPOp_POp)))
exp(cbind(OR = coef(NPOp_POp), confint.default(NPOp_POp, level= 0.85)))
Anova(NPOp_POp, type="II", test="Wald")
PseudoR2(NPOp_POp, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_pop <- data.frame(OpioidPrescDays=seq(min(0), max(30),len=100))

# predict based on new data
newdat_pop <- newdat_pop %>% 
  mutate(OpioidNPAny = predict(object = NPOp_POp, newdata = newdat_pop, type = "response"))

summary(newdat_pop)

# plot
NPOp_POp_Logit <- ggplot(data = newdat_pop, mapping = aes(x = OpioidPrescDays, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##Combined Plots
library(gridExtra)
grid.arrange(NPOp_Pain_Logit, NPOp_Income_Logit, NPOp_Age_Logit, NPOp_Edu_Logit, NPOp_OBF18_Logit, NPOp_Gender_Logit, NPOp_POp_Logit)


#################################################################################################
##ETHNICITY
NPOp_Ethn <- glm(OpioidNPAny ~ EthnicityNum, data = data2, family=binomial(link="logit"))
summary(NPOp_Ethn)
NPOp_Ethn
wald.test(b = coef(NPOp_Ethn), Sigma = vcov(NPOp_Ethn), Terms = 1)
exp(cbind(OR = coef(NPOp_Ethn), confint(NPOp_Ethn)))
exp(cbind(OR = coef(NPOp_Ethn), confint.default(NPOp_Ethn, level= 0.85)))
Anova(NPOp_Ethn, type="II", test="Wald")
PseudoR2(NPOp_Edu, c("McFadden", "Nagel", "CoxSnell"))

data2$EthnicityNum = factor(data2$EthnicityNum)
summary(data2$EthnicityNum)

# create new data that has the same range as the old data
newdat_ethn <- data.frame(EthnicityNum=seq(min(1), max(13),len=100))

# predict based on new data
newdat_ethn <- newdat_ethn %>% 
  mutate(OpioidNPAny = predict(object = NPOp_Ethn, newdata = newdat_ethn, type = "response"))

summary(newdat_ethn)

# plot
NPOp_Ethn_Logit <- ggplot(data = newdat_ethn, mapping = aes(x = EthnicityNum, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##NPOp Multivar
NPOp_mv <- glm(OpioidNPAny ~   MaudsleyMaxPain + GenderTwo + EntryAge + OpioidPrescDays, data = data2, family = "binomial")
summary(NPOp_mv)
Anova(NPOp_mv, type="II", test="Wald")
exp(cbind(OR = coef(NPOp_mv), confint(NPOp_mv)))
exp(cbind(OR = coef(NPOp_mv), confint.default(NPOp_mv, level= 0.85)))

library(DescTools)
PseudoR2(NPOp_mv, c("McFadden", "Nagel", "CoxSnell"))

library(car)
Anova(NPOp_mv, type="II", test="Wald")

library(car)
vif(NPOp_mv)


library(ggplot2)
NPOp_confint <- exp(cbind(OR = coef(NPOp_mv), confint.default(NPOp_mv, level= 0.85))) %>% as.data.frame()
NPOp_confint <- rename(NPOp_confint, c("OR" = "OR",
                                       "CILow" = "7.5 %",
                                       "CIHigh" = "92.5 %"))
row_to_keep = c(FALSE, TRUE, TRUE, TRUE, TRUE)
NPOp_confint = NPOp_confint[row_to_keep,]
NPOp_labels <- c("Pain", "Sex", "Age", "Presc opioid days")
NPOp_confint <- cbind(NPOp_labels, NPOp_confint)
num <- c(4, 3, 2, 1)
NPOp_confint <- cbind(num, NPOp_confint)
NPOp_confint

# Plot
OR_NPOp <- ggplot(NPOp_confint, aes(x=OR, y=num))
OR_NPOp + geom_vline(aes(xintercept = 1), size=.25, linetype="dashed") + 
  geom_errorbarh(aes(xmax=CIHigh, xmin=CILow), size=.5, height=.2, color="gray50") +
  geom_point(size=3.5, color="blue") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(labels=c("Presc opioid days", "Age", "Sex", "Pain")) +
  scale_x_continuous(breaks=seq(0, 2.8, by=0.05)) +
  ylab("") +
  xlab("Odds Ratio") +
  ggtitle("Probability of Non-Prescribed Opioid Use")
