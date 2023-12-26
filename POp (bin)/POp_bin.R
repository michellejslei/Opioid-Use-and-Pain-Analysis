library(tidyverse)
library(readxl)
library(aod)

#read dataset
dat <- read_excel("~/Desktop/DIRECTED STUDIES DATASETS/pain test.xlsx")
head(dat)
complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny)
data2 <- dat[complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny), ]

#factor variables
data2$OpioidPrescAny = factor(data2$OpioidPrescAny)

#################################################################################################
##PAIN
POp_Pain <- glm(OpioidPrescAny ~ MaudsleyMaxPain, data = data2, family=binomial(link="logit"))
summary(POp_Pain)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
exp(cbind(OR = coef(POp_Pain), confint.default(POp_Pain, level= 0.85)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_pain1 <- data.frame(MaudsleyMaxPain=seq(min(dat$MaudsleyMaxPain), max(dat$MaudsleyMaxPain),len=100))

# predict based on new data
newdat_pain1 <- newdat_pain1 %>% 
  mutate(OpioidPrescAny = predict(object = POp_Pain, newdata = newdat_pain1, type = "response"))

summary(newdat_pain1)

# plot
POp_Pain_Logit <- ggplot(data = newdat_pain1, mapping = aes(x = MaudsleyMaxPain, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##INCOME
POp_Income <- glm(OpioidPrescAny ~ Income, data = data2, family=binomial(link="logit"))
summary(POp_Income)
wald.test(b = coef(POp_Income), Sigma = vcov(POp_Income), Terms = 1)
exp(cbind(OR = coef(POp_Income), confint(POp_Income)))
exp(cbind(OR = coef(POp_Income), confint.default(POp_Income, level= 0.85)))
Anova(POp_Income, type="II", test="Wald")
PseudoR2(POp_Income, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_income1 <- data.frame(Income=seq(min(0), max(5000),len=100))

# predict based on new data
newdat_income1 <- newdat_income1 %>% 
  mutate(OpioidPrescAny = predict(object = POp_Income, newdata = newdat_income1, type = "response"))

summary(newdat_income1)

# plot
POp_Income_Logit <- ggplot(data = newdat_income1, mapping = aes(x = Income, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)


#################################################################################################
##AGE
POp_Age <- glm(OpioidPrescAny ~ EntryAge, data = data2, family=binomial(link="logit"))
summary(POp_Age)
wald.test(b = coef(POp_Age), Sigma = vcov(POp_Age), Terms = 1)
exp(cbind(OR = coef(POp_Age), confint(POp_Age)))
exp(cbind(OR = coef(POp_Age), confint.default(POp_Age, level= 0.85)))
Anova(POp_Age, type="II", test="Wald")
PseudoR2(POp_Age, c("McFadden", "Nagel", "CoxSnell"))


# create new data that has the same range as the old data
newdat_age1 <- data.frame(EntryAge=seq(min(dat$EntryAge), max(dat$EntryAge),len=100))

# predict based on new data
newdat_age1 <- newdat_age1 %>% 
  mutate(OpioidPrescAny = predict(object = POp_Age, newdata = newdat_age1, type = "response"))

summary(newdat_age1)

# plot
POp_Age_Logit <- ggplot(data = newdat_age1, mapping = aes(x = EntryAge, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##EDUC
POp_Edu <- glm(OpioidPrescAny ~ HighestDegree, data = data2, family=binomial(link="logit"))
summary(POp_Edu)
wald.test(b = coef(POp_Edu), Sigma = vcov(POp_Edu), Terms = 1)
exp(cbind(OR = coef(POp_Edu), confint(POp_Edu)))
exp(cbind(OR = coef(POp_Edu), confint.default(POp_Edu, level= 0.85)))
Anova(POp_Edu, type="II", test="Wald")
PseudoR2(POp_Edu, c("McFadden", "Nagel", "CoxSnell"))

data2$HighestDegree = factor(data2$HighestDegree)

# create new data that has the same range as the old data
newdat_edu1 <- data.frame(HighestDegree=seq(min(1), max(6),len=100))

# predict based on new data
newdat_edu1 <- newdat_edu1 %>% 
  mutate(OpioidPrescAny = predict(object = POp_Edu, newdata = newdat_edu1, type = "response"))

summary(newdat_edu1)

# plot
POp_Edu_Logit <- ggplot(data = newdat_edu1, mapping = aes(x = HighestDegree, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##OPIOIDBF18
POp_obf18 <- glm(OpioidPrescAny ~ OpioidBefore18, data = data2, family=binomial(link="logit"))
summary(POp_obf18)
wald.test(b = coef(POp_obf18), Sigma = vcov(POp_obf18), Terms = 1)
exp(cbind(OR = coef(POp_obf18), confint(POp_obf18)))
exp(cbind(OR = coef(POp_obf18), confint.default(POp_obf18, level= 0.85)))
Anova(POp_obf18, type="II", test="Wald")
PseudoR2(POp_obf18, c("McFadden", "Nagel", "CoxSnell"))

data2$OpioidBefore18 = factor(data2$OpioidBefore18)

# create new data that has the same range as the old data
newdat_obf181 <- data.frame(OpioidBefore18=seq(min(0), max(1),len=100))

# predict based on new data
newdat_obf181 <- newdat_obf181 %>% 
  mutate(OpioidPrescAny = predict(object = POp_obf18, newdata = newdat_obf181, type = "response"))

summary(newdat_obf181)

# plot
POp_OBF18_Logit <- ggplot(data = newdat_obf181, mapping = aes(x = OpioidBefore18, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##GENDER
POp_Gender <- glm(OpioidPrescAny ~ GenderTwo, data = data2, family=binomial(link="logit"))
summary(POp_Gender)
wald.test(b = coef(POp_Gender), Sigma = vcov(POp_Gender), Terms = 1)
exp(cbind(OR = coef(POp_Gender), confint(POp_Gender)))
exp(cbind(OR = coef(POp_Gender), confint.default(POp_Gender, level= 0.85)))
Anova(POp_Gender, type="II", test="Wald")
PseudoR2(POp_Gender, c("McFadden", "Nagel", "CoxSnell"))

data2$GenderTwo = factor(data2$GenderTwo)

# create new data that has the same range as the old data
newdat_gender1 <- data.frame(GenderTwo=seq(min(0), max(1),len=100))

# predict based on new data
newdat_gender1 <- newdat_gender1 %>% 
  mutate(OpioidPrescAny = predict(object = POp_Gender, newdata = newdat_gender1, type = "response"))

summary(newdat_gender1)

# plot
POp_Gender_Logit <- ggplot(data = newdat_gender1, mapping = aes(x = GenderTwo, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##NPOp
POp_NPOp <- glm(OpioidPrescAny ~ OpioidNPDays, data = data2, family=binomial(link="logit"))
summary(POp_NPOp)
wald.test(b = coef(POp_NPOp), Sigma = vcov(POp_NPOp), Terms = 1)
exp(cbind(OR = coef(POp_NPOp), confint(POp_NPOp)))
exp(cbind(OR = coef(POp_NPOp), confint.default(POp_NPOp, level= 0.85)))
Anova(POp_NPOp, type="II", test="Wald")
PseudoR2(POp_NPOp, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_npop <- data.frame(OpioidNPDays=seq(min(0), max(30),len=100))

# predict based on new data
newdat_npop <- newdat_npop %>% 
  mutate(OpioidPrescAny = predict(object = POp_NPOp, newdata = newdat_npop, type = "response"))

summary(newdat_npop)

# plot
POp_NPOp_Logit <- ggplot(data = newdat_npop, mapping = aes(x = OpioidNPDays, y = OpioidPrescAny)) +
  ylab("Prob PrescOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##Combined Plots
library(gridExtra)
grid.arrange(POp_Pain_Logit, POp_Income_Logit, POp_Age_Logit, POp_Edu_Logit, POp_OBF18_Logit, POp_Gender_Logit, POp_NPOp_Logit)


#################################################################################################
##ETHNICITY
POp_Ethn <- glm(OpioidPrescAny ~ EthnicityNum, data = data2, family=binomial(link="logit"))
summary(POp_Ethn)
POp_Ethn
wald.test(b = coef(POp_Ethn), Sigma = vcov(POp_Ethn), Terms = 1)
exp(cbind(OR = coef(POp_Ethn), confint(POp_Ethn)))
exp(cbind(OR = coef(POp_Ethn), confint.default(POp_Ethn, level= 0.85)))
Anova(POp_Ethn, type="II", test="Wald")
PseudoR2(POp_Ethn, c("McFadden", "Nagel", "CoxSnell"))

data2$EthnicityNum = factor(data2$EthnicityNum)
summary(data2$EthnicityNum)

# create new data that has the same range as the old data
newdat_ethn <- data.frame(EthnicityNum=seq(min(1), max(13),len=100))

# predict based on new data
newdat_ethn <- newdat_ethn %>% 
  mutate(OpioidPrescAny = predict(object = POp_Ethn, newdata = newdat_ethn, type = "response"))

summary(newdat_ethn)

# plot
POp_Ethn_Logit <- ggplot(data = newdat_ethn, mapping = aes(x = EthnicityNum, y = OpioidPrescAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##POp Multivar
POp_mv <- glm(OpioidPrescAny ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidNPDays, data = data2, family = "binomial")
summary(POp_mv)
Anova(POp_mv, type="II", test="Wald")
exp(cbind(OR = coef(POp_mv), confint(POp_mv)))
exp(cbind(OR = coef(POp_mv), confint.default(POp_mv, level= 0.85)))
PseudoR2(POp_mv, c("McFadden", "Nagel", "CoxSnell"))


library(car)
Anova(POp_mv, type="II", test="Wald")

library(car)
vif(POp_mv)

library(ggplot2)
POp_confint <- exp(cbind(OR = coef(POp_mv), confint.default(POp_mv, level= 0.85))) %>% as.data.frame()
POp_confint <- rename(POp_confint, c("OR" = "OR",
                                       "CILow" = "7.5 %",
                                       "CIHigh" = "92.5 %"))
row_to_keep = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
POp_confint = POp_confint[row_to_keep,]
POp_labels <- c("Pain", "Sex", "Age", "Income", "Opioid before 18", "Non-presc opioid days")
POp_confint <- cbind(POp_labels, POp_confint)
num <- c(6, 5, 4, 3, 2, 1)
POp_confint <- cbind(num, POp_confint)
POp_confint

# Plot
OR_POp <- ggplot(POp_confint, aes(x=OR, y=num))
OR_POp + geom_vline(aes(xintercept = 1), size=.25, linetype="dashed") + 
  geom_errorbarh(aes(xmax=CIHigh, xmin=CILow), size=.5, height=.2, color="gray50") +
  geom_point(size=3.5, color="blue") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(breaks=num, labels=POp_labels) +
  scale_x_continuous(breaks=seq(0, 3, by=0.05)) +
  ylab("") +
  xlab("Odds Ratio") +
  ggtitle("Probability of Prescribed Opioid Use")

