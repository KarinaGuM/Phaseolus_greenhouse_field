# karina.gutierrez@cinvestav.mx
# Karina Gutierrez Moreno
# PhD Student on Integrative Biology (Cinvestav Irapuato Mexico)
# Supervisor: Martin Heil
# Statistical analysis of Greenhouse and field experiments (2018-2019)
# July 23th 2020 
# Edited: July 23th 2020


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

# Set on your working directory
# Working with the first experimental set: Greenhouse Spring 2018
setwd("~/R/Invernadero2018_Suelo1")
set1 <- read.table("18082018_SOILone.txt", sep="\t", header=T)
head(set1)

# Working first with spores' (conidia) germination
GSPORES <- set1[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

# Segregating the different Cultivars on different data sets
GSPORES2$Cultivar <- as.factor(GSPORES2$Cultivar)
summary(GSPORES2$Cultivar)

NSL <- GSPORES2[as.factor(GSPORES2$Cultivar)=="NSL",]
summary(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# Segregating the rest of the cultivars

# FLOR DE JUNIO MARCELA
FJM <- GSPORES2[as.factor(GSPORES2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- GSPORES2[as.factor(GSPORES2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = FMA)

# PINTO VILLA
PV <- GSPORES2[as.factor(GSPORES2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=PV)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- GSPORES2[as.factor(GSPORES2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = Pv4)


# ANALYZING DISEASE SEVERITY
# Working then with disease severity (percentage of damaged leaf area)
AREA <- set1[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

# Segregating the different Cultivars on different data sets
AREA2$Cultivar <- as.factor(AREA2$Cultivar)
summary(AREA2$Cultivar)

# NEGRO SAN LUIS
NSL <- AREA2[as.factor(AREA2$Cultivar)=="NSL",]
summary(NSL)

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# Segregating the rest of the cultivars

# FLOR DE JUNIO MARCELA
FJM <- AREA2[as.factor(AREA2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- AREA2[as.factor(AREA2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = FMA)

# PINTO VILLA
PV <- AREA2[as.factor(AREA2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=PV)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- AREA2[as.factor(AREA2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = Pv4)


#######################################################################
# Working first with SET 2 (Autumn 2018 natural soil) AND SPORES
setwd("~/R/Invernadero2018_Suelo2")
set2 <- read.table("311218Allresults_Soiltwo.txt", sep="\t", header=T)
head(set2)

# Working first with spores' germination
GSPORES <- set2[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

# Segregating the different Cultivars on different data sets
GSPORES2$Cultivar <- as.factor(GSPORES2$Cultivar)
summary(GSPORES2$Cultivar)

NSL <- GSPORES2[as.factor(GSPORES2$Cultivar)=="NSL",]
summary(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# Segregating the rest of the cultivars

# FLOR DE JUNIO MARCELA
FJM <- GSPORES2[as.factor(GSPORES2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- GSPORES2[as.factor(GSPORES2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = FMA)

# PINTO VILLA
PV <- GSPORES2[as.factor(GSPORES2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=PV)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- GSPORES2[as.factor(GSPORES2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = Pv4)


# ANALYZING DISEASE SEVERITY
# Working then with disease severity (percentage of damaged leaf area)
AREA <- set2[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

# Segregating the different Cultivars on different data sets
AREA2$Cultivar <- as.factor(AREA2$Cultivar)
summary(AREA2$Cultivar)

# NEGRO SAN LUIS
NSL <- AREA2[as.factor(AREA2$Cultivar)=="NSL",]
summary(NSL)

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# Segregating the rest of the cultivars

# FLOR DE JUNIO MARCELA
FJM <- AREA2[as.factor(AREA2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- AREA2[as.factor(AREA2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = FMA)

# PINTO VILLA
PV <- AREA2[as.factor(AREA2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=PV)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- AREA2[as.factor(AREA2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = Pv4)


############################################################################
# Working first with SET 3 (Autumn 2018 sterile greenhouse mix) AND SPORES
setwd("~/R/Invernadero2018_Sustrato")
set3 <- read.table("AllResults_Mix.txt", sep="\t", header=T)
head(set3)

# Working first with spores' germination
GSPORES <- set3[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

# Segregating the different Cultivars on different data sets
GSPORES2$Cultivar <- as.factor(GSPORES2$Cultivar)
summary(GSPORES2$Cultivar)

NSL <- GSPORES2[as.factor(GSPORES2$Cultivar)=="NSL",]
summary(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# Segregating the rest of the cultivars

# FLOR DE JUNIO MARCELA
FJM <- GSPORES2[as.factor(GSPORES2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- GSPORES2[as.factor(GSPORES2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = FMA)

# PINTO VILLA
PV <- GSPORES2[as.factor(GSPORES2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=PV)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- GSPORES2[as.factor(GSPORES2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(Spores ~ Treatment, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(Spores ~ Treatment,  data = Pv4)


# ANALYZING DISEASE SEVERITY
# Working then with disease severity (percentage of damaged leaf area)
AREA <- set3[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

# Segregating the different Cultivars on different data sets
AREA2$Cultivar <- as.factor(AREA2$Cultivar)
summary(AREA2$Cultivar)

# NEGRO SAN LUIS
NSL <- AREA2[as.factor(AREA2$Cultivar)=="NSL",]
summary(NSL)

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# Segregating the rest of the cultivars

# FLOR DE JUNIO MARCELA
FJM <- AREA2[as.factor(AREA2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- AREA2[as.factor(AREA2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = FMA)

# PINTO VILLA
PV <- AREA2[as.factor(AREA2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=PV)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- AREA2[as.factor(AREA2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(Area ~ Treatment, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(Area ~ Treatment,  data = Pv4)

##########################################################################
# Working now with Field Experiments (Spring 2019)
setwd("~/R/FIELD_2019")
field<-read.table("150919_FIELD_noPv4.txt", sep="\t", header=T)
head(field)

# Global test with Kruskal-Wallis for disease severity
kruskal.test(D.AREA ~ Trichoderma, data=field)
kruskal.test(D.AREA ~ Colletotrichum, data=field)
kruskal.test(D.AREA ~ Genotype, data=field)

# Global test with Kruskal-Wallis for herbivory
kruskal.test(H.AREA ~ Trichoderma, data=field)
kruskal.test(H.AREA ~ Colletotrichum, data=field)
kruskal.test(H.AREA ~ Genotype, data=field)

# Means comparison with Wilcoxon test

# Segregating each condition into different data frames.
field.C1 <- field[as.integer(field$Colletotrichum)==1,] # Only Condition 1: With Colletotrichum
field.C2 <- field[as.integer(field$Colletotrichum)==2,] # Only Condition 2: No Colletotrichum

C1<-na.omit(field.C1)
field1<-C1[,c(2,3,4,5,6,7)]
head(field1)

C2<-na.omit(field.C2)
field2<-C2[,c(2,3,4,5,6,7)]
head(field2)

# Working first with LOG.CFU on field1
LOGCFU <- field1[,c(1,2,3,4)]
head(LOGCFU)
LOGCFU2 <- na.omit(LOGCFU)
head(LOGCFU2)

# Segregating the different Cultivars on different data sets
LOGCFU2$Genotype <- as.factor(LOGCFU2$Genotype)
summary(LOGCFU2$Genotype)

NSL <- LOGCFU2[as.factor(LOGCFU2$Genotype)=="NSL",]
summary(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(LOG.CFU ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(LOG.CFU ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- LOGCFU2[as.factor(LOGCFU2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(LOG.CFU ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(LOG.CFU ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- LOGCFU2[as.factor(LOGCFU2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(LOG.CFU ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(LOG.CFU ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- LOGCFU2[as.factor(LOGCFU2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(LOG.CFU ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(LOG.CFU ~ Trichoderma,  data = PV)

###### Working now with damaged area due to disease on field1
DAREA <- field1[,c(1,2,3,5)]
head(DAREA)
DAREA2 <- na.omit(DAREA)
head(DAREA2)

# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Genotype, data = DAREA2)

###### Working now with damaged area due to disease on field1
DAREA <- field2[,c(1,2,3,5)]
head(DAREA)
DAREA2 <- na.omit(DAREA)
head(DAREA2)

# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Genotype, data = DAREA2)



# Segregating the different Cultivars on different data sets
DAREA2$Genotype <- as.factor(DAREA2$Genotype)
summary(DAREA2$Genotype)

NSL <- DAREA2[as.factor(DAREA2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(D.AREA ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- DAREA2[as.factor(DAREA2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(D.AREA ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- DAREA2[as.factor(DAREA2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(D.AREA ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- DAREA2[as.factor(DAREA2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(D.AREA ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(D.AREA ~ Trichoderma,  data = PV)


### # Finally we will work for the Herbivory damage for Colletotrichum condition
# FIELD1
HAREA <- field1[,c(1,2,3,6)]
head(HAREA)
HAREA2 <- na.omit(HAREA)
head(HAREA2)

# Segregating the different Cultivars on different data sets
HAREA2$Genotype <- as.factor(HAREA2$Genotype)
summary(HAREA2$Genotype)

NSL <- HAREA2[as.factor(HAREA2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- HAREA2[as.factor(HAREA2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- HAREA2[as.factor(HAREA2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- HAREA2[as.factor(HAREA2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = PV)

# Since I only had interesting results for Herbivory in the experimental set with and 
# without Colletotrichum I will use the second condition to plot these results
# FIELD2
HAREA <- field2[,c(1,2,3,6)]
head(HAREA)
HAREA2 <- na.omit(HAREA)
head(HAREA2)

# Segregating the different Cultivars on different data sets
HAREA2$Genotype <- as.factor(HAREA2$Genotype)
summary(HAREA2$Genotype)

NSL <- HAREA2[as.factor(HAREA2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- HAREA2[as.factor(HAREA2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- HAREA2[as.factor(HAREA2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- HAREA2[as.factor(HAREA2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(H.AREA ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(H.AREA ~ Trichoderma,  data = PV)

######################### Let's analyze seed yield ########################
# Analyzing seed yield on field
## Both plots
setwd("~/R/In planta 2020")
Field <- read.table("110321_FIELDYIELD_Zero.txt", sep="\t", header=T)
head(Field)

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=Field)
kruskal.test(Yield ~ Colletotrichum, data=Field)
kruskal.test(Yield ~ Genotype, data=Field)


# WORKING NOW WITH INDIVIDUALL SETS FOR MAKE NEW INDIVIDUAL PLOTS WITH ERROR BARS
head(Field)
Field$Colletotrichum <- as.factor(Field$Colletotrichum)
summary(Field$Colletotrichum)

# Segregating each condition into different data frames.
Field.C1 <- Field[as.integer(Field$Colletotrichum)==1,] # Only Condition 1: With Colletotrichum
Field.C2 <- Field[as.integer(Field$Colletotrichum)==2,] # Only Condition 2: No Colletotrichum

# # Working first with Condition 1 (Plots with Colletotrichum)
head(Field.C1)
Yield <- Field.C1[,c(1,2,3,7)]
head(Yield)
Yield2 <- na.omit(Yield)
head(Yield2)

# Segregating the different Cultivars on different data sets
Yield2$Genotype <- as.factor(Yield2$Genotype)
summary(Yield2$Genotype)

NSL <- Yield2[as.factor(Yield2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- Yield2[as.factor(Yield2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- Yield2[as.factor(Yield2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- Yield2[as.factor(Yield2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = PV)


# Working first with Condition 2 (without Colletotrichum)
head(Field.C2)

# Working first with spores' germination
Yield.C2 <- Field.C2[,c(1,3,7)]
head(Yield.C2)
Yield.C2.2 <- na.omit(Yield.C2)
head(Yield.C2.2)

# Segregating the different Cultivars on different data sets
Yield.C2.2$Genotype <- as.factor(Yield.C2.2$Genotype)
summary(Yield.C2.2$Genotype)

NSL <- Yield.C2.2[as.factor(Yield.C2.2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- Yield.C2.2[as.factor(Yield.C2.2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- Yield.C2.2[as.factor(Yield.C2.2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- Yield.C2.2[as.factor(Yield.C2.2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Yield ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(Yield ~ Trichoderma,  data = PV)

##################################################
# WORKING JUST WITH NUMBER OF SEEDS
Field<- read.table("050320_NewYield.txt", sep="\t", header=T)
head(Field)

Field$Colletotrichum <- as.factor(Field$Colletotrichum)
summary(Field$Colletotrichum)

# We are going to substitute ALL values of Yield==0 by NA <--- Note: Dr. Octavio
Field$Seeds[Field$Seeds==0] <- NA

# Segregating each condition into different data frames.
Field.C1 <- Field[as.integer(Field$Colletotrichum)==1,] # Only Condition 1: With Colletotrichum
Field.C2 <- Field[as.integer(Field$Colletotrichum)==2,] # Only Condition 2: No Colletotrichum

# # Working first with Condition 1 (Plots with Colletotrichum)
head(Field.C1)
Seeds1 <- Field.C1[,c(1,3,6)]
head(Seeds1)
Seeds2 <- na.omit(Seeds1)
head(Seeds2) 

# Segregating the different Cultivars on different data sets
Seeds2$Genotype <- as.factor(Seeds2$Genotype)
summary(Seeds2$Genotype)

NSL <- Seeds2[as.factor(Seeds2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- Seeds2[as.factor(Seeds2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- Seeds2[as.factor(Seeds2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- Seeds2[as.factor(Seeds2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = PV)

###### Working then with Condition 2 (without Colletotrichum) ######
head(Field.C2)

# Working first with spores' germination
Seeds.C2 <- Field.C2[,c(1,3,6)]
head(Seeds.C2)
Seeds.2 <- na.omit(Seeds.C2)
head(Seeds.2)

# Segregating the different Cultivars on different data sets
Seeds.2$Genotype <- as.factor(Seeds.2$Genotype)
summary(Seeds.2$Genotype)

NSL <- Seeds.2[as.factor(Seeds.2$Genotype)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- Seeds.2[as.factor(Seeds.2$Genotype)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- Seeds.2[as.factor(Seeds.2$Genotype)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- Seeds.2[as.factor(Seeds.2$Genotype)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(Seeds ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(Seeds ~ Trichoderma,  data = PV)


######################################################################
# WORKING NOW WITH PLANT EMERGENCY ON FIELD
# Set on your working directory
setwd("~/R/FIELD_2019")

# Create a dataset

# Reading the table for this experiment
EMER<-read.table("190819_Emergence_Field.txt", sep="\t", header=T)
head(EMER)
summary(EMER)

EMER$Colletotrichum <- as.factor(EMER$Colletotrichum)
summary(EMER$Colletotrichum)

# Segregating each condition into different data frames.
emer.C1 <- EMER[as.integer(EMER$Colletotrichum)==1,] # Only Condition 1: With Colletotrichum
emer.C2 <- EMER[as.integer(EMER$Colletotrichum)==2,] # Only Condition 2: No Colletotrichum


# We will work first with % of plant emergence in Colletotrichum Condition (1)
head(emer.C1)

# Segregating the different Cultivars on different data sets
emer.C1$Cultivar <- as.factor(emer.C1$Cultivar)
summary(emer.C1$Cultivar)

NSL <- emer.C1[as.factor(emer.C1$Cultivar)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- emer.C1[as.factor(emer.C1$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- emer.C1[as.factor(emer.C1$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- emer.C1[as.factor(emer.C1$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- emer.C1[as.factor(emer.C1$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = Pv4)


# We will work first with % of plant emergence in Colletotrichum Condition (2)
head(emer.C2)

# Segregating the different Cultivars on different data sets
emer.C2$Cultivar <- as.factor(emer.C2$Cultivar)
summary(emer.C2$Cultivar)

NSL <- emer.C2[as.factor(emer.C2$Cultivar)=="NSL",]
summary(NSL)
head(NSL)

# NEGRO SAN LUIS
# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=NSL)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = NSL) 
# Change to method = "anova" if necessary, default methos is "wilcox.test"

# FLOR DE JUNIO MARCELA
FJM <- emer.C2[as.factor(emer.C2$Cultivar)=="FJM",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=FJM)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = FJM) 

# FLOR DE MAYO ANITA
FMA <- emer.C2[as.factor(emer.C2$Cultivar)=="FMA",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=FMA)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = FMA)

# PINTO VILLA
PV <- emer.C2[as.factor(emer.C2$Cultivar)=="PV",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=PV)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = PV)

# PHASEOLUS VULGARIS 4
Pv4 <- emer.C2[as.factor(emer.C2$Cultivar)=="Pv4",]

# Global test with Kruskal-Wallis
kruskal.test(P.Emergence ~ Trichoderma, data=Pv4)
# Means comparison with Wilcoxon test
compare_means(P.Emergence ~ Trichoderma,  data = Pv4)