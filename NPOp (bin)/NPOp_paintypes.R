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
data2$BothOpioid = factor(data2$BothOpioid)


#################################################################################################
###NPOp
##chestpain
NP_cp <- glm(OpioidNPAny ~ ChestPain, data = data2, family=binomial(link="logit"))
summary(NP_cp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_npcp <- data.frame(ChestPain=seq(min(dat$ChestPain), max(dat$ChestPain),len=100))

# predict based on new data
newdat_npcp <- newdat_npcp %>% 
  mutate(OpioidNPAny = predict(object = NP_cp, newdata = newdat_npcp, type = "response"))
summary(newdat_npcp)

# plot
NP_cp <- ggplot(data = newdat_npcp, mapping = aes(x = ChestPain, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##musclepain
NP_mp <- glm(OpioidNPAny ~ MusclePain, data = data2, family=binomial(link="logit"))
summary(NP_mp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_npmp <- data.frame(MusclePain=seq(min(dat$MusclePain), max(dat$MusclePain),len=100))

# predict based on new data
newdat_npmp <- newdat_npmp %>% 
  mutate(OpioidNPAny = predict(object = NP_mp, newdata = newdat_npmp, type = "response"))
summary(newdat_npmp)

# plot
NP_mp <- ggplot(data = newdat_npmp, mapping = aes(x = MusclePain, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##jointbonepain
NP_jbp <- glm(OpioidNPAny ~ JointBonePain, data = data2, family=binomial(link="logit"))
summary(NP_jbp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_npjbp <- data.frame(JointBonePain=seq(min(dat$JointBonePain), max(dat$JointBonePain),len=100))

# predict based on new data
newdat_npjbp <- newdat_npjbp %>% 
  mutate(OpioidNPAny = predict(object = NP_jbp, newdata = newdat_npjbp, type = "response"))
summary(newdat_npjbp)

# plot
NP_jbp <- ggplot(data = newdat_npjbp, mapping = aes(x = JointBonePain, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##stomachpain
NP_sp <- glm(OpioidNPAny ~ StomachPain, data = data2, family=binomial(link="logit"))
summary(NP_sp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_npsp <- data.frame(StomachPain=seq(min(dat$StomachPain), max(dat$StomachPain),len=100))

# predict based on new data
newdat_npsp <- newdat_npsp %>% 
  mutate(OpioidNPAny = predict(object = NP_sp, newdata = newdat_npsp, type = "response"))
summary(newdat_npsp)

# plot
NP_sp <- ggplot(data = newdat_npsp, mapping = aes(x = StomachPain, y = OpioidNPAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)


library(gridExtra)
grid.arrange(NP_sp, NP_cp, NP_mp, NP_jbp)


#################################################################################################
###POp
##chestpain
P_cp <- glm(OpioidPrescAny ~ ChestPain, data = data2, family=binomial(link="logit"))
summary(P_cp)
wald.test(b = coef(P_cp), Sigma = vcov(P_cp), Terms = 1)
exp(cbind(OR = coef(P_cp), confint(P_cp)))
Anova(P_cp, type="II", test="Wald")
PseudoR2(P_cp, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_pcp <- data.frame(ChestPain=seq(min(dat$ChestPain), max(dat$ChestPain),len=100))

# predict based on new data
newdat_pcp <- newdat_pcp %>% 
  mutate(OpioidPrescAny = predict(object = P_cp, newdata = newdat_pcp, type = "response"))
summary(newdat_pcp)

# plot
P_cp <- ggplot(data = newdat_pcp, mapping = aes(x = ChestPain, y = OpioidPrescAny)) +
  ylab("Prob POp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##musclepain
P_mp <- glm(OpioidPrescAny ~ MusclePain, data = data2, family=binomial(link="logit"))
summary(P_mp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_pmp <- data.frame(MusclePain=seq(min(dat$MusclePain), max(dat$MusclePain),len=100))

# predict based on new data
newdat_pmp <- newdat_pmp %>% 
  mutate(OpioidPrescAny = predict(object = P_mp, newdata = newdat_pmp, type = "response"))
summary(newdat_pmp)

# plot
P_mp <- ggplot(data = newdat_pmp, mapping = aes(x = MusclePain, y = OpioidPrescAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##jointbonepain
P_jbp <- glm(OpioidPrescAny ~ JointBonePain, data = data2, family=binomial(link="logit"))
summary(P_jbp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_pjbp <- data.frame(JointBonePain=seq(min(dat$JointBonePain), max(dat$JointBonePain),len=100))

# predict based on new data
newdat_pjbp <- newdat_pjbp %>% 
  mutate(OpioidPrescAny = predict(object = P_jbp, newdata = newdat_pjbp, type = "response"))
summary(newdat_pjbp)

# plot
P_jbp <- ggplot(data = newdat_pjbp, mapping = aes(x = JointBonePain, y = OpioidPrescAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##stomachpain
P_sp <- glm(OpioidPrescAny ~ StomachPain, data = data2, family=binomial(link="logit"))
summary(P_sp)
wald.test(b = coef(POp_Pain), Sigma = vcov(POp_Pain), Terms = 1)
exp(cbind(OR = coef(POp_Pain), confint(POp_Pain)))
Anova(POp_Pain, type="II", test="Wald")
PseudoR2(POp_Pain, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_psp <- data.frame(StomachPain=seq(min(dat$StomachPain), max(dat$StomachPain),len=100))

# predict based on new data
newdat_psp <- newdat_psp %>% 
  mutate(OpioidPrescAny = predict(object = P_sp, newdata = newdat_psp, type = "response"))
summary(newdat_psp)

# plot
P_sp <- ggplot(data = newdat_psp, mapping = aes(x = StomachPain, y = OpioidPrescAny)) +
  ylab("Prob NPOp Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)


library(gridExtra)
grid.arrange(P_sp, P_cp, P_mp, P_jbp)

#################################################################################################
###COp
##chestpain
C_cp <- glm(BothOpioid ~ ChestPain, data = data2, family=binomial(link="logit"))
summary(C_cp)
wald.test(b = coef(C_cp), Sigma = vcov(C_cp), Terms = 1)
exp(cbind(OR = coef(C_cp), confint(C_cp)))
Anova(C_cp, type="II", test="Wald")
PseudoR2(C_cp, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_ccp <- data.frame(ChestPain=seq(min(dat$ChestPain), max(dat$ChestPain),len=100))

# predict based on new data
newdat_ccp <- newdat_ccp %>% 
  mutate(BothOpioid = predict(object = C_cp, newdata = newdat_ccp, type = "response"))
summary(newdat_ccp)

# plot
C_cp <- ggplot(data = newdat_ccp, mapping = aes(x = ChestPain, y = BothOpioid)) +
  ylab("Prob Combined Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##musclepain
C_mp <- glm(BothOpioid ~ MusclePain, data = data2, family=binomial(link="logit"))
summary(C_mp)
wald.test(b = coef(C_mp), Sigma = vcov(C_mp), Terms = 1)
exp(cbind(OR = coef(C_mp), confint(C_mp)))
Anova(C_mp, type="II", test="Wald")
PseudoR2(C_mp, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_cmp <- data.frame(MusclePain=seq(min(dat$MusclePain), max(dat$MusclePain),len=100))

# predict based on new data
newdat_cmp <- newdat_cmp %>% 
  mutate(BothOpioid = predict(object = C_mp, newdata = newdat_cmp, type = "response"))
summary(newdat_cmp)

# plot
C_mp <- ggplot(data = newdat_cmp, mapping = aes(x = MusclePain, y = BothOpioid)) +
  ylab("Prob Combined Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##jointbonepain
C_jbp <- glm(BothOpioid ~ JointBonePain, data = data2, family=binomial(link="logit"))
summary(C_jbp)
wald.test(b = coef(C_jbp), Sigma = vcov(C_jbp), Terms = 1)
exp(cbind(OR = coef(C_jbp), confint(C_jbp)))
Anova(C_jbp, type="II", test="Wald")
PseudoR2(C_jbp, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_cjbp <- data.frame(JointBonePain=seq(min(dat$JointBonePain), max(dat$JointBonePain),len=100))

# predict based on new data
newdat_cjbp <- newdat_cjbp %>% 
  mutate(BothOpioid = predict(object = C_jbp, newdata = newdat_cjbp, type = "response"))
summary(newdat_cjbp)

# plot
C_jbp <- ggplot(data = newdat_cjbp, mapping = aes(x = JointBonePain, y = BothOpioid)) +
  ylab("Prob Combined Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)

#################################################################################################
##stomachpain
C_sp <- glm(BothOpioid ~ StomachPain, data = data2, family=binomial(link="logit"))
summary(C_sp)
wald.test(b = coef(C_sp), Sigma = vcov(C_sp), Terms = 1)
exp(cbind(OR = coef(C_sp), confint(C_sp)))
Anova(C_sp, type="II", test="Wald")
PseudoR2(C_sp, c("McFadden", "Nagel"))
# create new data that has the same range as the old data
newdat_csp <- data.frame(StomachPain=seq(min(dat$StomachPain), max(dat$StomachPain),len=100))

# predict based on new data
newdat_csp <- newdat_csp %>% 
  mutate(BothOpioid = predict(object = C_sp, newdata = newdat_csp, type = "response"))
summary(newdat_csp)

# plot
C_sp <- ggplot(data = newdat_csp, mapping = aes(x = StomachPain, y = BothOpioid)) +
  ylab("Prob Ccombined Use (%)") +
  stat_smooth(method = glm, method.args=list(family="binomial"), se=TRUE)


library(gridExtra)
grid.arrange(C_sp, C_cp, C_mp, C_jbp)
