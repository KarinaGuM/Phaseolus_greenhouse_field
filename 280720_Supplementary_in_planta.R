# karina.gutierrez@cinvestav.mx
# Karina Gutierrez Moreno
# PhD Student on Integrative Biology (Cinvestav Irapuato Mexico)
# Supervisor: Martin Heil
# Statistical analysis of Greenhouse and field experiments (2018-2019)
# July 28th 2020 
# Edited: July 28th 2020


# Library
# Install if necessary
#########################################################
### A) Installing and loading required packages
#########################################################
if (!require("Rmisc")) {
  install.packages("Rmisc", dependencies = TRUE)
  library(Rmisc)
}

if (!require("plyr")) {
  install.packages("plyr", dependencies = TRUE)
  library(plyr)
}

if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

if (!require("ggpubr")) {
  install.packages("ggpubr", dependencies = TRUE)
  library(ggpubr)
}

## ANALYZING FIRST GREENHOUSE EXPERIMENTS
setwd("~/R/Invernaderotodos_2018")
kd <- read.table("220119_allsetskarina.txt", header=T, sep="\t")
head(kd)
names(kd)
nrow(kd)
summary(kd)

kd$Set <- as.factor(kd$Set)
summary(kd$Set)

# Dependent variables are "Spores", "Area", "Dweight".
# Design is (complete factorial) Set * Cultivar * Treatment
# (use model.tables)

# Analyses of "Spores"
spo.aov <- aov(Spores ~ Set * Cultivar * Treatment, data=kd)
anova(spo.aov)

shapiro.test(spo.aov$residuals)
# El test de Shapiro-Wilk se usa para contrastar si un conjunto de datos siguen una 
# distribucion normal o no. Este hecho es de vital importancia porque otros muchos analisis 
# estadisticos requieren de la normalidad de los datos para poder llevarlos a cabo.
# ¬_¬

hist(spo.aov$residuals)
# Interpretation: The non-normality is possibly due to a few residuals larger than 30 units.

plot(spo.aov$fitted.values, spo.aov$residuals)
# Interpretation: Presents strong evidence of heteroscedasticity
# In statistics, a collection of random variables is heteroscedastic if there are 
# sub-populations that have different variabilities from others.  Here "variability" could 
# be quantified by the variance or any other measure of statistical dispersion.

summary(kd$Spores)
summary(log10(kd$Spores+1))
length(kd$Spores[!is.na(kd$Spores)])
length(kd$Spores[(!is.na(kd$Spores))&(kd$Spores>0)])

model.tables(spo.aov, type="means", se=T)

TukeyHSD(spo.aov, which="Set")

TukeyHSD(spo.aov, which="Cultivar")

TukeyHSD(spo.aov, which="Treatment")

# General interpretation of the analyses of the "Spores" variable.
# As can be seen from the ANOVA table all terms and interactions are highly significant, 
# meaning that there is not a linear response to any of the main factors (all interactions 
# are significant), i.e., the behavior of the "Spores" variable responds in different way 
# to each level of each factor.
# The "Spores" variable has a high prevalence of zeroes, 156 of the 714 non-missing values 
# are equal to 0. This means that in around 22% of the cases no spores were detected. As an 
# indirect result of this fact there is a non-normality of residuals and strong 
# heteroscedasticity (i.e., variance is strongly correlated with number of spores). Even 
# when this does not completely invalidate the ANOVA, we need to be cautious in the 
# interpretation. I explored the possibility to transform the variable "Spores", but not 
# satisfactory transformation was found.
# Tukey tests, presented above, show the contrast for means of treatments in the main factors. 
# Interestingly, results of sets 3 and 2 are NOT significant, implying that these two assays 
# are consistent (gave in average the same results). Also, applying the kruskal test, a 
# non-parametric alternative to ANOVA, the less significant main factor is "Set" (see below). 
# I will analyze it set as an independent experiment.

# Non-parametric alternative to ANOVA (one main factor at the time)
kruskal.test(Spores ~ Set, data=kd)

kruskal.test(Spores ~ Cultivar, data=kd)

kruskal.test(Spores ~ Treatment, data=kd)


# Segregating each set into different data frames.
kd.S1 <- kd[as.integer(kd$Set)==1,] # Only Set 1
kd.S2 <- kd[as.integer(kd$Set)==2,] # Only Set 2
kd.S3 <- kd[as.integer(kd$Set)==3,] # Only Set 3

# NON-PARAMETRIC TEST FOR PERCENTAGE OF GERMINATED CONIDIA

# SET 1: GREENHOUSE SPRING 2018 
kruskal.test(Spores ~ Cultivar, data=kd.S1)

# Means comparison with Wilcoxon test
compare_means(Spores ~ Cultivar,  data = kd.S1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"


# SET 2: GREENHOUSE AUTUMN 2018
kruskal.test(Spores ~ Cultivar, data=kd.S2)

# Means comparison with Wilcoxon test
compare_means(Spores ~ Cultivar,  data = kd.S2) 


# SET 3: GREENHOUSE STERILE SOIL AUTUMN 2018
kruskal.test(Spores ~ Cultivar, data=kd.S3)

# Means comparison with Wilcoxon test
compare_means(Spores ~ Cultivar,  data = kd.S3)


## NON-PARAMETRIC TEST FOR DAMAGED AREA (DISEASE SEVERITY)
kruskal.test(Area ~ Set, data=kd)

kruskal.test(Area ~ Cultivar, data=kd)

kruskal.test(Area ~ Treatment, data=kd)


# SET 1: GREENHOUSE SPRING 2018 
kruskal.test(Area ~ Cultivar, data=kd.S1)

# Means comparison with Wilcoxon test
compare_means(Area ~ Cultivar,  data = kd.S1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"


# SET 2: GREENHOUSE AUTUMN 2018
kruskal.test(Area ~ Cultivar, data=kd.S2)

# Means comparison with Wilcoxon test
compare_means(Area ~ Cultivar,  data = kd.S2) 


# SET 3: GREENHOUSE STERILE SOIL AUTUMN 2018
kruskal.test(Area ~ Cultivar, data=kd.S3)

# Means comparison with Wilcoxon test
compare_means(Area ~ Cultivar,  data = kd.S3)


#########################################################################

# Analyzing now Field data
setwd("~/R/FIELD_2019")
fd <- read.table("150919_FIELD_noPv4.txt", header=T, sep="\t")
head(fd)
names(fd)
nrow(fd)
summary(fd)

fd$Colletotrichum <- as.factor(fd$Colletotrichum)
summary(fd$Colletotrichum)

# Dependent variables are "LOF.CFU", "D.AREA", "H.AREA".
# Since field data don't have a normal distribution we'll perform
# non-parametric tests.

# Segregating each set into different data frames.
fd.C1 <- fd[as.integer(fd$Colletotrichum)==1,] # Only Colletotrichum condition
fd.C2 <- fd[as.integer(fd$Colletotrichum)==2,] # Only No Colletotrichum condition

# NON-PARAMETRIC TEST FOR COLONY FORMING UNITS LOG.CFU

# CONDITION 1: COLLETOTRICHUM CONDITION 
kruskal.test(LOG.CFU ~ Genotype, data=fd.C1)

# Means comparison with Wilcoxon test
compare_means(LOG.CFU ~ Genotype, data=fd.C1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"

# SINCE NO COLLETOTRICHUM CFU WERE FOUND ON NO INOCULATED PLOTS
# WE WILL NOT PERFORM ANALYSIS FOR CONDITION 2

# NON-PARAMETRIC TEST FOR DISEASE SEVERITY D.AREA
# BOTH CONDITIONS
kruskal.test(D.AREA ~ Genotype, data=fd)

kruskal.test(D.AREA ~ Trichoderma, data=fd)

kruskal.test(D.AREA ~ Colletotrichum, data=fd)


# ANALYSIS FOR EACH CONDITION

# CONDITION 1: COLLETOTRICHUM CONDITION 
kruskal.test(D.AREA ~ Genotype, data=fd.C1)

# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Genotype, data=fd.C1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"


# CONDITION 2: NO-COLLETOTRICHUM CONDITION 
kruskal.test(D.AREA ~ Genotype, data=fd.C2)

# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Genotype, data=fd.C2) 
# Change to method = "anova" if necessary, default method is "wilcox.test"


# NON-PARAMETRIC TEST FOR HERBIVORY H.AREA
# BOTH CONDITIONS
kruskal.test(H.AREA ~ Genotype, data=fd)

kruskal.test(H.AREA ~ Trichoderma, data=fd)

kruskal.test(H.AREA ~ Colletotrichum, data=fd)

# ANALYSIS FOR EACH CONDITION
# CONDITION 1: COLLETOTRICHUM CONDITION 
kruskal.test(H.AREA ~ Genotype, data=fd.C1)

# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Genotype, data=fd.C1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"


# CONDITION 2: NO-COLLETOTRICHUM CONDITION 
kruskal.test(H.AREA ~ Genotype, data=fd.C2)

# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Genotype, data=fd.C2) 
# Change to method = "anova" if necessary, default method is "wilcox.test"



#########################################################################

# ANALYSIS FOR YIELD (SEED PRODUCTION)
# FIELD EXPERIMENTS

setwd("~/R/FIELD_2019")
yd <- read.table("050320_NewYield.txt", header=T, sep="\t")
head(yd)
names(yd)
nrow(yd)
summary(yd)

yd$Colletotrichum <- as.factor(yd$Colletotrichum)
summary(yd$Colletotrichum)

# Dependent variables are "Pods", "Seeds", "Yield".
# Since yield data don't have a normal distribution we'll perform
# non-parametric tests.

# Segregating each set into different data frames.
yd.C1 <- yd[as.integer(yd$Colletotrichum)==1,] # Only Colletotrichum condition
yd.C2 <- yd[as.integer(yd$Colletotrichum)==2,] # Only No Colletotrichum condition

# NON-PARAMETRIC TEST FOR SEED YIELD (Yield)
# BOTH CONDITIONS
kruskal.test(Yield ~ Genotype, data=yd)

kruskal.test(Yield ~ Trichoderma, data=yd)

kruskal.test(Yield ~ Colletotrichum, data=yd)

# CONDITION 1: COLLETOTRICHUM CONDITION 
kruskal.test(Yield ~ Genotype, data=yd.C1)

# Means comparison with Wilcoxon test
compare_means(Yield ~ Genotype, data=yd.C1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"

# CONDITION 2: NO-COLLETOTRICHUM CONDITION 
kruskal.test(Yield ~ Genotype, data=yd.C2)

# Means comparison with Wilcoxon test
compare_means(Yield ~ Genotype, data=yd.C2) 
# Change to method = "anova" if necessary, default method is "wilcox.test"


# NON-PARAMETRIC TEST FOR NUMBER OF SEEDS
# BOTH CONDITIONS
kruskal.test(Seeds ~ Genotype, data=yd)

kruskal.test(Seeds ~ Trichoderma, data=yd)

kruskal.test(Seeds ~ Colletotrichum, data=yd)

# CONDITION 1: COLLETOTRICHUM CONDITION 
kruskal.test(Seeds ~ Genotype, data=yd.C1)

# Means comparison with Wilcoxon test
compare_means(Seeds ~ Genotype, data=yd.C1) 
# Change to method = "anova" if necessary, default method is "wilcox.test"

# CONDITION 2: NO-COLLETOTRICHUM CONDITION 
kruskal.test(Seeds ~ Genotype, data=yd.C2)

# Means comparison with Wilcoxon test
compare_means(Seeds ~ Genotype, data=yd.C2) 
# Change to method = "anova" if necessary, default method is "wilcox.test"