library(tidyverse)
library(readxl)
library(MASS)
library(car)
library(pscl)
library(boot)

dat <- read_excel("~/Desktop/DIRECTED STUDIES DATASETS/pain test.xlsx")
head(dat)
complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny)
data2 <- dat[complete.cases(dat$OpioidNPDays, dat$GenderTwo, dat$EntryAge, dat$MaudsleyMaxPain, dat$Income, dat$HighestDegree, dat$OpioidBefore18, dat$OpioidPrescDays, dat$OpioidNPAny, dat$OpioidPrescAny), ]
count(data2, vars=data2$BothOpioid)
count(data2, vars=data2$OpioidNPAny)
count(data2, vars=data2$GenderTwo)
count(dat, vars=dat$BothOpioid)

data2$GenderTwo = factor(data2$GenderTwo)
data2$OpioidBefore18 = factor(data2$OpioidBefore18)
data2$HighestDegree = factor(data2$HighestDegree)

NPOp_p_Pain <- glm(OpioidNPDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidPrescDays, data = data2, family="poisson")
summary(NPOp_p_Pain)
Anova(NPOp_p_Pain, type="II", test="Wald")
exp(cbind(OR = coef(NPOp_p_Pain), confint(NPOp_p_Pain)))
PseudoR2(NPOp_p_Pain, c("McFadden", "Nagel", "CoxSnell"))

NPOp_nb_Pain <- glm.nb(OpioidNPDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidPrescDays, data = data2)
summary(NPOp_nb_Pain)

NPOp_zip_Pain <- zeroinfl(OpioidNPDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidPrescDays, data = data2)
summary(NPOp_zip_Pain)
AIC(NPOp_zip_Pain)

NPOp_zinb_Pain <- zeroinfl(OpioidNPDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + OpioidPrescDays, data = data2, dist = "negbin")
summary(NPOp_zinb_Pain)
AIC(NPOp_zinb_Pain)
exp(cbind(OR = coef(NPOp_zinb_Pain), confint(NPOp_zinb_Pain)))
exp(cbind(OR = coef(NPOp_zinb_Pain), confint.default(NPOp_zinb_Pain, level= 0.85)))
pR2(NPOp_zinb_Pain)

library(ggplot2)
NPOpf_confint <- exp(cbind(OR = coef(NPOp_zinb_Pain), confint.default(NPOp_zinb_Pain, level= 0.85))) %>% as.data.frame()
NPOpf_confint <- rename(NPOpf_confint, c("OR" = "OR",
                                       "CILow" = "7.5 %",
                                       "CIHigh" = "92.5 %"))
row_to_keep = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
NPOpf_confint = NPOpf_confint[row_to_keep,]
NPOp_labels <- c("Pain", "Sex", "Age", "Presc opioid days")
NPOpf_confint <- cbind(NPOp_labels, NPOpf_confint)
num <- c(4, 3, 2, 1)
NPOpf_confint <- cbind(num, NPOpf_confint)
NPOpf_confint

# Plot
OR_NPOpf <- ggplot(NPOpf_confint, aes(x=OR, y=num))
OR_NPOpf + geom_vline(aes(xintercept = 1), size=.25, linetype="dashed") + 
  geom_errorbarh(aes(xmax=CIHigh, xmin=CILow), size=.5, height=.2, color="gray50") +
  geom_point(size=3.5, color="orange") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(labels=c("Presc opioid days", "Age", "Sex", "Pain")) +
  scale_x_continuous(breaks=seq(0, 2.8, by=0.05)) +
  ylab("") +
  xlab("Relative Risk Ratio") +
  ggtitle("Freuency of Non-Prescribed Opioid Use")


#################################################################################################
POp_p_Pain <- glm(OpioidPrescDays ~   MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidNPDays, data = data2, family="poisson")
summary(POp_p_Pain)
Anova(POp_p_Pain, type="II", test="Wald")
exp(cbind(OR = coef(POp_p_Pain), confint(POp_p_Pain)))
PseudoR2(POp_p_Pain, c("McFadden", "Nagel", "CoxSnell"))

POp_nb_Pain <- glm.nb(OpioidPrescDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidNPDays, data = data2)
summary(POp_nb_Pain)

POp_zip_Pain <- zeroinfl(OpioidPrescDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + Income + OpioidBefore18 + OpioidNPDays, data = data2)
summary(POp_zip_Pain)
AIC(POp_zip_Pain)

POp_zinb_Pain <- zeroinfl(OpioidPrescDays ~  MaudsleyMaxPain + GenderTwo + EntryAge + OpioidBefore18 + OpioidNPDays, data = data2, dist = "negbin")
summary(POp_zinb_Pain)
AIC(POp_zinb_Pain)
exp(cbind(OR = coef(POp_zinb_Pain), confint(POp_zinb_Pain)))
exp(cbind(OR = coef(POp_zinb_Pain), confint.default(POp_zinb_Pain, level= 0.85)))
pR2(POp_zinb_Pain)

library(ggplot2)
POpf_confint <- exp(cbind(OR = coef(POp_zinb_Pain), confint.default(POp_zinb_Pain, level= 0.85))) %>% as.data.frame()
POpf_confint <- rename(POpf_confint, c("OR" = "OR",
                                     "CILow" = "7.5 %",
                                     "CIHigh" = "92.5 %"))
row_to_keep = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
POpf_confint = POpf_confint[row_to_keep,]
POpf_labels <- c("Pain", "Sex", "Age", "Opioid before 18", "Non-presc opioid days")
POpf_confint <- cbind(POp_labels, POpf_confint)
num <- c(5, 4, 3, 2, 1)
POpf_confint <- cbind(num, POpf_confint)
POpf_confint

# Plot
OR_POpf <- ggplot(POpf_confint, aes(x=OR, y=num))
OR_POpf + geom_vline(aes(xintercept = 1), size=.25, linetype="dashed") + 
  geom_errorbarh(aes(xmax=CIHigh, xmin=CILow), size=.5, height=.2, color="gray50") +
  geom_point(size=3.5, color="orange") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  scale_y_continuous(breaks=num, labels=POpf_labels) +
  scale_x_continuous(breaks=seq(0, 3, by=0.05)) +
  ylab("") +
  xlab("Relative Risk Ratio") +
  ggtitle("Frequency of Prescribed Opioid Use")


#################################################################################################
m1 <- zeroinfl(OpioidNPDays ~ MaudsleyMaxPain + GenderTwo + EntryAge + Income + HighestDegree + OpioidBefore18 + OpioidPrescDays, data = data2, dist = "negbin")
summary(m1)

#################################################################################################
NPf_mp <- glm(OpioidNPDays ~  MusclePain, data = data2, family="poisson")
summary(NPf_mp)
Anova(NPf_mp, type="II", test="Wald")
exp(cbind(OR = coef(NPf_mp), confint(NPf_mp)))

NPf_cp <- glm(OpioidNPDays ~  ChestPain, data = data2, family="poisson")
summary(NPf_cp)
Anova(NPf_cp, type="II", test="Wald")
exp(cbind(OR = coef(NPf_cp), confint(NPf_cp)))

NPf_jbp <- glm(OpioidNPDays ~  JointBonePain, data = data2, family="poisson")
summary(NPf_jbp)
Anova(NPf_jbp, type="II", test="Wald")
exp(cbind(OR = coef(NPf_jbp), confint(NPf_jbp)))

NPf_sp <- glm(OpioidNPDays ~  StomachPain, data = data2, family="poisson")
summary(NPf_sp)
Anova(NPf_sp, type="II", test="Wald")
exp(cbind(OR = coef(NPf_sp), confint(NPf_sp)))

#################################################################################################
Pf_mp <- glm(OpioidPrescDays ~  MusclePain, data = data2, family="poisson")
summary(Pf_mp)
Anova(Pf_mp, type="II", test="Wald")
exp(cbind(OR = coef(Pf_mp), confint(Pf_mp)))

Pf_cp <- glm(OpioidPrescDays ~  ChestPain, data = data2, family="poisson")
summary(Pf_cp)
Anova(Pf_cp, type="II", test="Wald")
exp(cbind(OR = coef(Pf_cp), confint(Pf_cp)))

Pf_jbp <- glm(OpioidPrescDays ~  JointBonePain, data = data2, family="poisson")
summary(Pf_jbp)
Anova(Pf_jbp, type="II", test="Wald")
exp(cbind(OR = coef(Pf_jbp), confint(Pf_jbp)))

Pf_sp <- glm(OpioidPrescDays ~  StomachPain, data = data2, family="poisson")
summary(Pf_sp)
Anova(Pf_sp, type="II", test="Wald")
exp(cbind(OR = coef(Pf_sp), confint(Pf_sp)))

attach(data2)
plot(MaudsleyMaxPain, OpioidNPDays, main="test",
     xlab="pain", ylab="POpProb")
