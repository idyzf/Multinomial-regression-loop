library(openxlsx)
library(dbplyr)
library(ggplot2)
library(caTools)
library(nnet)
library(foreign)
library(reshape2)
library(tidyverse)
rm(list=ls())

traits<- read.xlsx("trait_batch.xlsx")
rownames(traits) <- traits$X1
traits<- traits[,-1]

clinical <- read.xlsx("clinical_v2.xlsx", sheet=2)
rownames(clinical) <- clinical$ID
names(clinical)
OS<- as.data.frame(clinical[,-3:-5])
#OS<- as.data.frame(OS[,-3:-63])


dados<- read.csv("ruvcorrected.csv")
rownames(dados)<- dados$Name
dados <- dados[,-1:-3]
dados <- as.data.frame(t(dados))

batch1 <- subset(traits, traits$batch1==2)
batch2 <- subset(traits, traits$batch2==2)
batch3 <- subset(traits, traits$batch3==2)

data_batch1 <- subset(dados, rownames(dados) %in% rownames(batch1))
data_batch2 <- subset(dados, rownames(dados) %in% rownames(batch2))
data_batch3 <- subset(dados, rownames(dados) %in% rownames(batch3))

OS_batch1 <- subset(OS, rownames(OS) %in% rownames(batch1))
OS_batch2 <- subset(OS, rownames(OS) %in% rownames(batch2))
OS_batch3 <- subset(OS, rownames(OS) %in% rownames(batch3))



all_batch1 <- merge(data_batch1, OS_batch1, by=0)
all_batch2 <- merge(data_batch2, OS_batch2, by=0)
all_batch3 <- merge(data_batch3, OS_batch3, by=0)

genes<- cbind(names(dados))
paste(genes, "+")
###########################################################################
############################################################################
############################################################################
############################################################################
rownames(all_batch1) <- all_batch1$Row.names
all_batch1<- all_batch1[,-1]
binary <- as.data.frame(clinical$Group_surv_bi)
rownames(binary)<- clinical$ID


data_new1<- merge(all_batch1, binary, by=0)

#data_new<- all_batch1
rownames(data_new) <- data_new[,1]
data_new<- data_new[,-1]
out <- data.frame(NULL) 
i=1
j=1
for (i in 1:ncol(data_new)){
  for(j in 1:192){
    modelo<- summary(glm(data_new$Sobrevida.Global ~ data_new[,i], data= data_new))
    out[j, 1]  <- names(data_new[i])
    out[j, 2] <- modelo$coefficients[1,1]   # intercept
    out[j, 3] <- modelo$coefficients[2,1]  #coeficient
    out[j, 4] <- modelo$coefficients[1,4]
    print(modelo)
    i=i+1
    j=j+1
    print(i)
  }
}

names(out) <- c("y.variable", "intercept", "coef.x", "pvalue")

write.xlsx(out, "log regression result.xlsx")
