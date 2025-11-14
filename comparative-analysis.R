#### Before taking comparative analysis, check Assumption & Parametric Tests


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




############# Chi-squared test for comparing proportions between two groups or more groups (k x m) #########
ist_table <- table(cont_base$IST, cont_base$PAYS)
chisq.test(ist_table)  # p < 0.05 indicates that the proportion of IST differs significantly across PAYS


################# Z-test for comparing proportions between two groups #################
#### Two-proportion Z-test
# Compare the proportions of a binary variable between two groups
# prop.test performs the two-proportion Z-test using a chi-square statistic
# (p < 0.05 indicates a statistically significant difference)
pro_test = prop.test(x = c(pro_stu_fr, pro_stu_ko), n = c(pro_to_fr, pro_to_ko), p = NULL, alternative = "two.sided", correct = FALSE) 

# Extract the Z-score (prop.test returns a chi-square statistic with df = 1)
z_value <- sqrt(pro_test$statistic)   # Z = sqrt(chi-square)
z_value



############## Z-test (Fisher's Z transformation) to compare correlation coefficients from two independent samples

rs1 <- 0.12; n1 <- 325  # rs1 = Spearman's rho for sample 1; n1 = sample size
rs2 <- 0.14; n2 <- 291  # rs2 = Spearman's rho for sample 2

# Convert to Fisher Z-scores
Z1 <- 0.5 * log((1 + rs1) / (1 - rs1))
Z2 <- 0.5 * log((1 + rs2) / (1 - rs2))

# Standard error of the difference
SE_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))

# Z-statistic for the difference in correlations
Z <- (Z1 - Z2) / SE_diff

# Two-tailed p-value
p_value <- 2 * (1 - pnorm(abs(Z)))

Z
p_value

# 95% confidence interval for the difference in Fisher Z
Z_95 <- 1.96
CI_lower <- (Z1 - Z2) - Z_95 * SE_diff
CI_upper <- (Z1 - Z2) + Z_95 * SE_diff

# Back-transform CI bounds to the correlation scale
CI_lower_corr <- (exp(2 * CI_lower) - 1) / (exp(2 * CI_lower) + 1)
CI_upper_corr <- (exp(2 * CI_upper) - 1) / (exp(2 * CI_upper) + 1)

list(CI_lower = CI_lower_corr, CI_upper = CI_upper_corr)


