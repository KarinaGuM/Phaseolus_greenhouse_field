# Complete Randomized Design
# Open field experiments
# Karina Gutierrez Moreno
# March, 2019

# For more info and how to use visit: http://r-video-tutorial.blogspot.com/2017/07/experiment-designs-for-agriculture.html

# Basic R
# NsL = Negro San Luis
# Pv4 = Phaseolus vulgaris 4
# PV = Pinto Villa
# FJM = Flor de Junio Marcela
# FMA = Flor de Mayo Anita
# CN = Control
# B6 = T. asperellum B6
# P1 = T. atroviride P1
# MK1 = T. longibrachiatum MK1
# T22 = T. harzianum T22

# Treatment1= c("NSL","Pv4","PV","FJM","FMA")
# Treatment2= c("CN","B6","P1","MK1","T22")

TR.Structure = expand.grid(rep=1:16, Treatment1=c("NSL","Pv4","PV","FJM","FMA"), Treatment2=c("CN","B6","P1","MK1","T22"))
Data.CRD = TR.Structure[sample(1:nrow(TR.Structure),nrow(TR.Structure)),]
Data.CRD = cbind(PlotN=1:nrow(Data.CRD), Data.CRD[,-1])  
write.csv(Data.CRD, "CompleteRandomDesign.csv", row.names=F) 

################################################################################
# Working with Agricolae package

if (!require("agricolae")) {
  install.packages("agricolae", dependencies = TRUE)
  library(agricolae)
}

# A = NsL = Negro San Luis
# B = Pv4 = Phaseolus vulgaris 4
# C = PV = Pinto Villa
# D = FJM = Flor de Junio Marcela
# E = FMA = Flor de Mayo Anita
# 1 = CN = Control
# 2 = B6 = T. asperellum B6
# 3 = P1 = T. atroviride P1
# 4 = MK1 = T. longibrachiatum MK1
# 5 = T22 = T. harzianum T22

# Trt1= c("A","B","C","D","E")
# Trt2= c("1","2","3","4","5")

Trt1 = c("A","B","C","D","E")
Trt2 = c("1","2","3","4","5")

TRT = as.vector(sapply(Trt1, function(x){paste0(x,Trt2)}))
TRT

# Then we can use the object TRT with the function design.crd, from which we can directly 
# obtain the data.frame with $book:

design.crd(trt=TRT, r=16)$book

# For Random Complet Block Design another option is
design.rcbd(trt=TRT, r=16)$book 

# Plotting your Design
if (!require("desplot")) {
  install.packages("desplot", dependencies = TRUE)
  library(desplot)
}

#Complete Randomized Design  
CRD = design.crd(trt=TRT, r=16)$book  # r= Number of replicates
CRD = CRD[order(CRD$r),]  
CRD$col = CRD$r  
CRD$row = rep(1:40,5) # 1:15 numer of treatments per replicate, 
# 5 times (i.e. the number of replicates)

desplot(form=TRT ~ col+row, data=CRD, text=TRT, out1=row, out2=col,   
        cex=1, main="Complete Randomized Design")  

# NO PUEDO!! AYUDAAAAA!!!!