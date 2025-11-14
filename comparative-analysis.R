##### t-test for mean comparison between two groups with continuous variables ####

# t-test when normal distribution + equal variances assumed (O)
t.test(score ~ group, data = df, var.equal = TRUE)

# t-test when normal distribution + equal variances assumed (x)
t.test(score ~ group, data = df, var.equal = FALSE)

# Wilcoxon rank-sum test when not normal distribution (non-parametric test)
wilcox.test(score ~ group, data = df, exact = FALSE)


################ ANOVA for comparison the mean between three + groups ##############
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




################# Z-test for comparing proportions between two groups #################
#### Two-proportion Z-test
# Compare the proportions of a binary variable between two groups
# prop.test performs the two-proportion Z-test using a chi-square statistic
# (p < 0.05 indicates a statistically significant difference)
pro_test = prop.test(x = c(pro_stu_fr, pro_stu_ko), n = c(pro_to_fr, pro_to_ko), p = NULL, alternative = "two.sided", correct = FALSE) 

# Extract the Z-score (prop.test returns a chi-square statistic with df = 1)
z_value <- sqrt(pro_test$statistic)   # Z = sqrt(chi-square)
z_value




