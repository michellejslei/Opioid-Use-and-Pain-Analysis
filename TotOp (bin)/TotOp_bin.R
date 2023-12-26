library(tidyverse)
library(readxl)
library(aod)

#read dataset
dat <- read_excel("~/Desktop/DIRECTED STUDIES DATASETS/pain test.xlsx")
head(dat)head(dat)
complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny)
data2 <- dat[complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny), ]

#factor variables
data2$BothOpioid = factor(data2$BothOpioid)

#################################################################################################
##PAIN
TotOp_Pain <- glm(BothOpioid ~ MaudsleyMaxPain, data = data2, family=binomial(link="logit"))
summary(TotOp_Pain)
wald.test(b = coef(TotOp_Pain), Sigma = vcov(TotOp_Pain), Terms = 1)
exp(cbind(OR = coef(TotOp_Pain), confint(TotOp_Pain)))
exp(cbind(OR = coef(TotOp_Pain), confint.default(TotOp_Pain, level= 0.85)))
Anova(TotOp_Pain, type="II", test="Wald")
PseudoR2(TotOp_Pain, c("McFadden", "Nagel", "CoxSnell"))


# create new data that has the same range as the old data
newdat_pain2 <- data.frame(MaudsleyMaxPain=seq(min(dat$MaudsleyMaxPain), max(dat$MaudsleyMaxPain),len=100))

# predict based on new data
newdat_pain2 <- newdat_pain2 %>% 
  mutate(BothOpioid = predict(object = TotOp_Pain, newdata = newdat_pain2, type = "response"))

summary(newdat_pain2)

# plot
TotOp_Pain_Logit <- ggplot(data = newdat_pain2, mapping = aes(x = MaudsleyMaxPain, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##INCOME
TotOp_Income <- glm(BothOpioid ~ Income, data = data2, family=binomial(link="logit"))
summary(TotOp_Income)
wald.test(b = coef(TotOp_Income), Sigma = vcov(TotOp_Income), Terms = 1)
exp(cbind(OR = coef(TotOp_Income), confint(TotOp_Income)))
exp(cbind(OR = coef(TotOp_Income), confint.default(TotOp_Income, level= 0.85)))
Anova(TotOp_Income, type="II", test="Wald")
PseudoR2(TotOp_Income, c("McFadden", "Nagel", "CoxSnell"))


# create new data that has the same range as the old data
newdat_income2 <- data.frame(Income=seq(min(0), max(5000),len=100))

# predict based on new data
newdat_income2 <- newdat_income2 %>% 
  mutate(BothOpioid = predict(object = TotOp_Income, newdata = newdat_income2, type = "response"))

summary(newdat_income2)

# plot
TotOp_Income_Logit <- ggplot(data = newdat_income2, mapping = aes(x = Income, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)


#################################################################################################
##AGE
TotOp_Age <- glm(BothOpioid ~ EntryAge, data = data2, family=binomial(link="logit"))
summary(TotOp_Age)
wald.test(b = coef(TotOp_Age), Sigma = vcov(TotOp_Age), Terms = 1)
exp(cbind(OR = coef(TotOp_Age), confint(TotOp_Age)))
exp(cbind(OR = coef(TotOp_Age), confint.default(TotOp_Age, level= 0.85)))
Anova(TotOp_Age, type="II", test="Wald")
PseudoR2(TotOp_Age, c("McFadden", "Nagel", "CoxSnell"))


# create new data that has the same range as the old data
newdat_age2 <- data.frame(EntryAge=seq(min(dat$EntryAge), max(dat$EntryAge),len=100))

# predict based on new data
newdat_age2 <- newdat_age2 %>% 
  mutate(BothOpioid = predict(object = TotOp_Age, newdata = newdat_age2, type = "response"))

summary(newdat_age2)

# plot
TotOp_Age_Logit <- ggplot(data = newdat_age2, mapping = aes(x = EntryAge, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##EDUC
TotOp_Edu <- glm(BothOpioid ~ HighestDegree, data = data2, family=binomial(link="logit"))
summary(TotOp_Edu)
wald.test(b = coef(TotOp_Edu), Sigma = vcov(TotOp_Edu), Terms = 1)
exp(cbind(OR = coef(TotOp_Edu), confint(TotOp_Edu)))
exp(cbind(OR = coef(TotOp_Edu), confint.default(TotOp_Edu, level= 0.85)))
Anova(TotOp_Edu, type="II", test="Wald")
PseudoR2(TotOp_Edu, c("McFadden", "Nagel", "CoxSnell"))

data2$HighestDegree = factor(data2$HighestDegree)

# create new data that has the same range as the old data
newdat_edu2 <- data.frame(HighestDegree=seq(min(1), max(6),len=100))

# predict based on new data
newdat_edu2 <- newdat_edu2 %>% 
  mutate(BothOpioid = predict(object = TotOp_Edu, newdata = newdat_edu2, type = "response"))

summary(newdat_edu2)

# plot
TotOp_Edu_Logit <- ggplot(data = newdat_edu2, mapping = aes(x = HighestDegree, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##OPIOIDBF18
TotOp_obf18 <- glm(BothOpioid ~ OpioidBefore18, data = data2, family=binomial(link="logit"))
summary(TotOp_obf18)
wald.test(b = coef(TotOp_obf18), Sigma = vcov(TotOp_obf18), Terms = 1)
exp(cbind(OR = coef(TotOp_obf18), confint(TotOp_obf18)))
exp(cbind(OR = coef(TotOp_obf18), confint.default(TotOp_obf18, level= 0.85)))
Anova(TotOp_obf18, type="II", test="Wald")
PseudoR2(TotOp_obf18, c("McFadden", "Nagel", "CoxSnell"))

data2$OpioidBefore18 = factor(data2$OpioidBefore18)

# create new data that has the same range as the old data
newdat_obf182 <- data.frame(OpioidBefore18=seq(min(0), max(1),len=100))

# predict based on new data
newdat_obf182 <- newdat_obf182 %>% 
  mutate(BothOpioid = predict(object = TotOp_obf18, newdata = newdat_obf182, type = "response"))

summary(newdat_obf182)

# plot
TotOp_OBF18_Logit <- ggplot(data = newdat_obf182, mapping = aes(x = OpioidBefore18, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##GENDER
TotOp_Gender <- glm(BothOpioid ~ GenderTwo, data = data2, family=binomial(link="logit"))
summary(TotOp_Gender)
wald.test(b = coef(TotOp_Gender), Sigma = vcov(TotOp_Gender), Terms = 1)
exp(cbind(OR = coef(TotOp_Gender), confint(TotOp_Gender)))
exp(cbind(OR = coef(TotOp_Gender), confint.default(TotOp_Gender, level= 0.85)))
Anova(TotOp_Gender, type="II", test="Wald")
PseudoR2(TotOp_Gender, c("McFadden", "Nagel", "CoxSnell"))

data2$GenderTwo = factor(data2$GenderTwo)

# create new data that has the same range as the old data
newdat_gender2 <- data.frame(GenderTwo=seq(min(0), max(1),len=100))

# predict based on new data
newdat_gender2 <- newdat_gender2 %>% 
  mutate(BothOpioid = predict(object = TotOp_Gender, newdata = newdat_gender2, type = "response"))

summary(newdat_gender2)

# plot
TotOp_Gender_Logit <- ggplot(data = newdat_gender2, mapping = aes(x = GenderTwo, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 


#################################################################################################
##POp
TotOp_POp <- glm(BothOpioid ~ OpioidPrescDays, data = data2, family=binomial(link="logit"))
summary(TotOp_POp)
wald.test(b = coef(TotOp_POp), Sigma = vcov(TotOp_POp), Terms = 1)
exp(cbind(OR = coef(TotOp_POp), confint(TotOp_POp)))
exp(cbind(OR = coef(TotOp_POp), confint.default(TotOp_POp, level= 0.85)))
Anova(TotOp_POp, type="II", test="Wald")
PseudoR2(TotOp_POp, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_pop2 <- data.frame(OpioidPrescDays=seq(min(0), max(30),len=100))

# predict based on new data
newdat_pop2 <- newdat_pop2 %>% 
  mutate(BothOpioid = predict(object = TotOp_POp, newdata = newdat_pop2, type = "response"))

summary(newdat_pop2)

# plot
TotOp_POp_Logit <- ggplot(data = newdat_pop2, mapping = aes(x = OpioidPrescDays, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##NPOp
TotOp_NPOp <- glm(BothOpioid ~ OpioidNPDays, data = data2, family=binomial(link="logit"))
summary(TotOp_NPOp)
wald.test(b = coef(TotOp_NPOp), Sigma = vcov(TotOp_NPOp), Terms = 1)
exp(cbind(OR = coef(TotOp_NPOp), confint(TotOp_NPOp)))
exp(cbind(OR = coef(TotOp_NPOp), confint.default(TotOp_NPOp, level= 0.85)))
Anova(TotOp_NPOp, type="II", test="Wald")
PseudoR2(TotOp_NPOp, c("McFadden", "Nagel", "CoxSnell"))



# create new data that has the same range as the old data
newdat_npop2 <- data.frame(OpioidNPDays=seq(min(0), max(30),len=100))

# predict based on new datas
newdat_npop2 <- newdat_npop2 %>% 
  mutate(BothOpioid = predict(object = TotOp_NPOp, newdata = newdat_npop2, type = "response"))

summary(newdat_npop2)

# plot
TotOp_NPOp_Logit <- ggplot(data = newdat_npop2, mapping = aes(x = OpioidNPDays, y = BothOpioid)) +
  ylab("Prob TotOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE) 

#################################################################################################
##Combined Plots
library(gridExtra)
grid.arrange(TotOp_Pain_Logit, TotOp_Income_Logit, TotOp_Age_Logit, TotOp_Edu_Logit, TotOp_OBF18_Logit, TotOp_Gender_Logit, TotOp_POp_Logit, TotOp_NPOp_Logit)


#################################################################################################
##ETHNICITY
TotOp_Ethn <- glm(BothOpioid ~ EthnicityNum, data = data2, family=binomial(link="logit"))
summary(TotOp_Ethn)
TotOp_Ethn
wald.test(b = coef(TotOp_Ethn), Sigma = vcov(TotOp_Ethn), Terms = 1)
exp(cbind(OR = coef(TotOp_Ethn), confint(TotOp_Ethn)))
exp(cbind(OR = coef(TotOp_Ethn), confint.default(TotOp_Ethn, level= 0.85)))
Anova(TotOp_Ethn, type="II", test="Wald")
PseudoR2(TotOp_Ethn, c("McFadden", "Nagel", "CoxSnell"))

data2$EthnicityNum = factor(data2$EthnicityNum)
summary(data2$EthnicityNum)


#################################################################################################
##TotOp Multivar
TotOp_mv <- glm(BothOpioid ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidNPDays, data = data2, family = "binomial")
summary(TotOp_mv)
Anova(TotOp_mv, type="II", test="Wald")
exp(cbind(OR = coef(TotOp_mv), confint(TotOp_mv)))
exp(cbind(OR = coef(TotOp_mv), confint.default(TotOp_mv, level= 0.85)))
PseudoR2(TotOp_mv, c("McFadden", "Nagel", "CoxSnell"))


library(car)
Anova(TotOp_mv, type="II", test="Wald")

library(car)
vif(TotOp_mv)

library(ggplot2)
COp_confint <- exp(cbind(OR = coef(TotOp_mv), confint.default(TotOp_mv, level= 0.85))) %>% as.data.frame()
COp_confint <- rename(COp_confint, c("OR" = "OR",
                                     "CILow" = "7.5 %",
                                     "CIHigh" = "92.5 %"))
row_to_keep = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
COp_confint = COp_confint[row_to_keep,]
COp_labels <- c("Pain", "Sex", "Age", "Income", "Opioid before 18", "Non-presc opioid days")
COp_confint <- cbind(COp_labels, COp_confint)
num1 <- c(6, 5, 4, 3, 2, 1)
COp_confint <- cbind(num1, COp_confint)
COp_confint

# Plot
OR_COp <- ggplot(COp_confint, aes(x=OR, y=num1))
OR_COp + geom_vline(aes(xintercept = 1), size=.25, linetype="dashed") + 
  geom_errorbarh(aes(xmax=CIHigh, xmin=CILow), size=.5, height=.2, color="gray50") +
  geom_point(size=3.5, color="blue") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(breaks=num1, labels=COp_labels) +
  scale_x_continuous(breaks=seq(0, 3, by=0.05)) +
  ylab("") +
  xlab("Odds Ratio") +
  ggtitle("Probability of Combined Opioid Use")
