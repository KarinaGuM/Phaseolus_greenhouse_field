# karina.gutierrez@cinvestav.mx
# Karina Gutierrez Moreno
# PhD Student on Integrative Biology (Cinvestav Irapuato Mexico)
# Supervisor: Martin Heil
# Statistical analysis of Greenhouse and field experiments (2018-2019)
# June 26th 2020 
# Edited: August 7th 2020


# Library
# Install if necessary
#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}

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
setwd("~/R/Invernadero2018_Suelo1")
set1 <- read.table("18082018_SOILone.txt", sep="\t", header=T)
head(set1)


# Working first with spores' germination
GSPORES <- set1[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

p2 <- ggplot(GSPORES2, aes(x=Treatment, y=Spores, fill=Treatment)) +
  geom_boxplot(show.legend = FALSE) + # Quit argument to show leyend
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(), # QUITAR ESTO PARA QUE APAREZCAN 
        axis.title.y = element_blank(), # LAS LEYENDAS EN EJES X y Y
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p2

p2+stat_compare_means(label.x = 1.5, label.y = 85, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(66,52,50,45), size=7)


# Working then with disease severity (percentage of damaged leaf area)
AREA <- set1[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

p3 <- ggplot(AREA2, aes(x=Treatment, y=Area, fill=Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p3

p3 + stat_compare_means(label.x = 1.5, label.y = 115, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(89,103,103,62), size=7)


# Working first with SET 2 (Autumn 2018 natural soil) AND SPORES
setwd("~/R/Invernadero2018_Suelo2")
set2 <- read.table("311218Allresults_Soiltwo.txt", sep="\t", header=T)
head(set2)

# Working first with spores' germination
GSPORES <- set2[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

p4 <- ggplot(GSPORES2, aes(x=Treatment, y=Spores, fill=Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p4

p4 + stat_compare_means(label.x = 1.5, label.y = 60, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(35,34,21,32), size=7)

# Working then with disease severity (percentage of damaged leaf area)
AREA <- set2[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

p5 <- ggplot(AREA2, aes(x=Treatment, y=Area, fill=Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p5

p5 + stat_compare_means(label.x = 1.5, label.y = 115, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(40,54,7,99), size=7)


# Working first with SET 3 (Autumn 2018 sterile greenhouse mix) AND SPORES
setwd("~/R/Invernadero2018_Sustrato")
set3 <- read.table("AllResults_Mix.txt", sep="\t", header=T)
head(set3)

# Working first with spores' germination
GSPORES <- set3[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

p6 <- ggplot(GSPORES2, aes(x=Treatment, y=Spores, fill=Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p6

p6 + stat_compare_means(label.x = 1.5, label.y = 60, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(22,25,21,58), size=7)

# Working then with disease severity (percentage of damaged leaf area)
AREA <- set3[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

p7 <- ggplot(AREA2, aes(x=Treatment, y=Area, fill=Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p7

p7 + stat_compare_means(label.x = 1.5, label.y = 60, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(30,40,25,28), size=7)

# Working now with Field Experiments (Spring 2019)
setwd("~/R/FIELD_2019")
field<-read.table("150919_FIELD_noPv4.txt", sep="\t", header=T)
head(field)

# Segregating each condition into different data frames.
field.C1 <- field[as.integer(field$Colletotrichum)==1,] # Only Condition 1: With Colletotrichum
field.C2 <- field[as.integer(field$Colletotrichum)==2,] # Only Condition 2: No Colletotrichum

C1<-na.omit(field.C1)
field1<-C1[,c(2,3,4,5,6,7)]
head(field1)

C2<-na.omit(field.C2)
field2<-C2[,c(2,3,4,5,6,7)]
head(field2)

# We will work first with field1 and LOG CFU
# Then, you can substitute with Area or Dweight, D.AREA, H.AREA, LOG.CFU, Etc.
p8 <- ggplot(field1, aes(x=Trichoderma, y=LOG.CFU, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "LOG[CFU/fresh leaf weight (g)]", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p8

p8 + stat_compare_means(label.x = 1.5, label.y = 2.0, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(0.1,1.4,1.05,1.7), size=7)


# Working now with damaged area due to disease
p9 <- ggplot(field1, aes(x=Trichoderma, y=D.AREA, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
p9

p9 + stat_compare_means(label.x = 1.5, label.y = 43, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(14,6,39,7), size=7)

# Finally we will work for the Herbivory damage for Colletotrichum condition
h1 <- ggplot(field1, aes(x=Trichoderma, y=H.AREA, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel1") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=28, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
h1

h1 + stat_compare_means(label.x = 1.5, label.y = 5, size=7) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(4.7), size=7) + ylim(0,5)


# Since I only had interesting results for Herbivory in the experimental set with and 
# without Colletotrichum I will use the second condition to plot these results
h2 <- ggplot(field2, aes(x=Trichoderma, y=H.AREA, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel1") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=28, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
h2

h2 + stat_compare_means(label.x = 1.5, label.y = 5, size=7) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(4.7), size=7) + ylim(0,5)

######################### Let's analyze seed yield ########################
# Set on your working directory
setwd("~/R/In planta 2020")

Field<- read.table("110321_FIELDYIELD_Zero.txt", sep="\t", header=T)
head(Field)

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

### BOXPLOT FOR YIELD #### 30/july/2020
y <- ggplot(Yield2, aes(x=Trichoderma, y=Yield, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel1") +
  labs(y = "Yield (g/plant)", fill= "Trichoderma treatment") +
  theme(text = element_text(size=28),
        plot.title = element_text(size=25),
        axis.title=element_text(size=25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_text(size=25, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 28))
y

y + stat_compare_means(label.x = 1.5, label.y = 220, size=6) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(175), size=8)

# Working first with Condition 2 (without Colletotrichum)
head(Field.C2)

# Working first with spores' germination
Yield.C2 <- Field.C2[,c(1,2,3,7)]
head(Yield.C2)
Yield.C2.2 <- na.omit(Yield.C2)
head(Yield.C2.2)

### BOXPLOT FOR YIELD #### 30/july/2020
y <- ggplot(Yield.C2, aes(x=Trichoderma, y=Yield, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel1") +
  labs(y = "Yield (g/plant)", fill= "Trichoderma treatment") +
  theme(text = element_text(size=28),
        plot.title = element_text(size=25),
        axis.title=element_text(size=25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_text(size=25, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 28))
y

y + stat_compare_means(label.x = 1.5, label.y = 220, size=6) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(175), size=8)


##################################################
# WORKING JUST WITH NUMBER OF SEEDS

# WORKING NOW WITH INDIVIDUALL SETS FOR MAKE NEW INDIVIDUAL PLOTS WITH ERROR BARS
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

### BOXPLOT FOR YIELD #### 30/july/2020
s <- ggplot(Seeds2, aes(x=Trichoderma, y=Seeds, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Number of seeds", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
s

s + stat_compare_means(label.x = 1.5, label.y = 715, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(350,550,460,400), size=7)


# Working then with Condition 2 (without Colletotrichum) ######
head(Field.C2)

# Working first with spores' germination
Seeds.C2 <- Field.C2[,c(1,3,6)]
head(Seeds.C2)
Seeds.2 <- na.omit(Seeds.C2)
head(Seeds.2)

### BOXPLOT FOR YIELD #### 30/july/2020
s2 <- ggplot(Seeds.2, aes(x=Trichoderma, y=Seeds, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Number of seeds", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
s2

s2 + stat_compare_means(label.x = 1.5, label.y = 715, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(305,320,305,245), size=7)



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

e <- ggplot(emer.C1, aes(x=Trichoderma, y=P.Emergence, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of seedlings emergency", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
e

e + stat_compare_means(label.x = 1.5, label.y = 100, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(80,65,80,80), size=7)


# We will work now with % of plant emergence in Colletotrichum Condition 2: 
# Non-inoculated
head(emer.C2)

e2 <- ggplot(emer.C2, aes(x=Trichoderma, y=P.Emergence, fill=Trichoderma)) +
  geom_boxplot(show.legend = FALSE) +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of seedlings emergency", fill= "Trichoderma treatment") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size=22, color="black"),
        axis.text.y = element_text(size=22, color="black")) +
  theme(legend.position = "none") + # Quit to show legend
  theme(strip.text.x = element_text(size = 22, face = "bold"))
e2

e2 + stat_compare_means(label.x = 1.5, label.y = 100, size=5) + 
  stat_compare_means(ref.group = ".Control", label = "p.signif", label.x = 1.5,
                     label.y = c(78,90,78,78), size=7)
