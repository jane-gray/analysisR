################ ANOVA for comparison the mean between three+ groups ##############
# One-way ANOVA
sample <- aov(AUDIT ~ AVISalcool, familydata2)
summary(sample)


# One-way ANOVA and post-hoc analysis
install.packages("DescTools")
library(DescTools)

PostHocTest(sample, method='lsd')
PostHocTest(m, method='bonferroni')


# Two-way ANOVA
sample <- aov(AUDIT ~ *ALCOOLpere, familydata)
summary(sample)


############ repeated two-way ANOVA #########################  

familydata2$AUTORITEmereb <- ifelse(familydata2$AUTORITEmere < 12, 1, 2)
familydata2$AUTORITEpereb <- ifelse(familydata2$AUTORITEpere < 8, 1, 2)

install.packages("ez")
install.packages("multcomp")
install.packages("nlme")
install.packages("pastecs")
install.packages("reshape")
install.packages("WRS", repos="http://R-Forge.R-project.org")

library(ez) ; library(multcomp) ; library(nlme) ; library(pastecs); library(reshape) ; library(WRS)

ANOVAdata <- familydata2 [, c("SEXE","AUDITb","AUTORITEmereb","AUTORITEpereb","AUDIT","AUTORITEmere","AUTORITEpere")]

ANOVAdata$SEXE <- as.factor(ANOVAdata$SEXE)
ANOVAdata$AUDITb <- as.factor(ANOVAdata$AUDITb)
ANOVAdata$AUTORITEmereb <- as.factor(ANOVAdata$AUTORITEmereb)
ANOVAdata$AUTORITEpereb <- as.factor(ANOVAdata$AUTORITEpereb)

View(ANOVAdata)


##################### Box plot #########################################################
p <- ggplot(ANOVAdata, aes(x=SEXE, y=AUDIT, fill=SEXE)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")




################# Z-test for comparison the mean between two groups ###############
install.packages("BSDA")
library(BSDA)

describe(familydata$AUDIT) # sigma is the SD of population

z.test(x=female$AUDIT, y=male$AUDIT, mu=0, sigma.x=2.5, sigma.y=2.5)



