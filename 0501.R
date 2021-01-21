setwd("~/R Scripts/Learning Environment")

library(psych)
library(GPArotation)
library(lavaan)   
library(lavaanPlot)
library(corrplot)
######################transfer chinese to numbers
data2 <- read.csv("almostalldata.csv",encoding = "UTF-8",header = F)

#data2[1,3]
#for (i in 1:40){
#  print(i)
#  print(data2[1,i])
#}
#levels(data2[1,14])
for (i in 1:40){
  data2[,i] <-as.numeric(data2[,i])
}

data2[,1] <- 3-data2[,1]
data2[,2] <- data2[,2]-6
data2[,4] <- 3-data2[,4]

if(data2[1,1] == 0){
  data2[1,1] <- 2
}

for (j in 1:384){
for(i in 6:9){
  if(data2[j,i]==1){
    data2[j,i] <- 4
  }else if(data2[j,i]==3){
    data2[j,i]<-7
  }else if(data2[j,i]==4){
    data2[j,i]<-3
  }else if(data2[j,i]==7){
    data2[j,i]<-1
  }
}
}

for(i in 1:384){
  if(data2[i,12]==1){
    data2[i,12]<-4
  }else if(data2[i,12]==3){
    data2[i,12]<-1
  }else if(data2[i,12]==4){
    data2[i,12]<-7
  }else if(data2[i,12]==5){
    data2[i,12]<-3
  }else if(data2[i,12]==6){
    data2[i,12]<-5
  }else if(data2[i,12]==7){
    data2[i,12]<-6
  }
}

#########
for(i in 1:384){
  if (data2[i,14]==1){
    data2[i,14]<-4
  }else if(data2[i,14]==4){
    data2[i,14]<-6
  }else if(data2[i,14]==5){
    data2[i,14]<-1
  }else if (data2[i,14]==6){
    data2[i,14]<-7
  }else if(data2[i,14]==7){
    data2[i,14]<-5
  }
}
for(j in 1:384){
  for(i in 15:40){
    if(data2[j,i]==3){
      data2[j,i]<-6
    }else if(data2[j,i]==4){
      data2[j,i]<-3
    }else if(data2[j,i] ==5){
      data2[j,i]<-7
    }else if(data2[j,i] == 6){
      data2[j,i]<-4
    }else if (data2[j,i]==7){
      data2[j,i]<-5
    }
}
}
data2<-data2[-307,]

##########
data1 <- read.csv("test.csv",header = T)
data1 <- data1[,-1]

names(data2)<-c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q20','Q21','Q22','Q23','Q24','Q25','Q26','Q27','Q28','Q29','Q30','Q31','Q32','Q33','Q34','Q35','Q36','Q37','Q38','Q39','Q40')
names(data1)<-c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q20','Q21','Q22','Q23','Q24','Q25','Q26','Q27','Q28','Q29','Q30','Q31','Q32','Q33','Q34','Q35','Q36','Q37','Q38','Q39','Q40')

data<-rbind(data1,data2)
data<-na.omit(data)

data.model <-data[,c(6:9,12,14:40)]
data.cfa <- data.model
data.efa <- data.cfa


###0523 EFA+CFA+SEM
#split dataset
data.environment<- data.model[,-30:-32]
f<-data.frame(row.names(describe(data.model)),describe(data.model)$mean,describe(data.model)$sd,describe(data.model)$skew,describe(data.model)$kurtosis)
write.table (f,file ="f.csv", sep =",", row.names = FALSE) 

N <- nrow(data.environment)
indices <- seq(1, N)
indices_EFA <- sample(indices, floor((.5*N)))
indices_CFA <- indices[!(indices %in% indices_EFA)]

environment_EFA <- data.environment[indices_EFA, ]
environment_CFA <- data.environment[indices_CFA, ]
# Use the indices from the previous exercise to create a grouping variable
group_var <- vector("numeric", N)
group_var[indices_EFA] <- 1
group_var[indices_CFA] <- 2

# Bind that grouping variable onto the gcbs dataset
environment_grouped <- cbind(data.environment, group_var)

# Compare stats across groups
describeBy(environment_grouped, group = group_var)

######H1: EFA+ CFA
#correlation
lowerCor(data.environment, use = "pairwise.complete.obs")
corr.test(data.environment, use = "pairwise.complete.obs")$p
# Use those indices to split the dataset into halves for your EFA and CFA
envi_EFA <- data.environment[indices_EFA, ]
envi_CFA <- data.environment[indices_CFA, ]
# Calculate the correlation matrix first
envi_EFA_cor <- cor(envi_EFA, use = "pairwise.complete.obs")
# Decide the number of factors
fa.parallel(envi_EFA_cor, n.obs = 261, fa = "both", n.iter = 100, main = "Scree plots with parallel analysis")
# Factor analysis, number of factors is 6
fa <- fa(envi_EFA_cor, nfactors = 6, rotate = "none", fm = "pa")
fa
# varimaxal Rotation 
fa.varimax <- fa(envi_EFA_cor, nfactors = 6, rotate = "varimax", fm = "pa")
fa.varimax
# Promaxal rotation
fa.promax<- fa(envi_EFA_cor,nfactors = 6, rotate = "promax",fm = "pa")
h<-fa.promax$loadings

fa.diagram(fa.promax,simple =TRUE)
# Cronbach alpha
alpha(envi_EFA2[,c(18:21)])

####################CFA
#delete Q12,14,15,20,27,28,33,35
envi_CFA2<-envi_CFA[,c(-5,-6,-7,-12,-19,-20,-26,-27)]
cfa.model <- 'SA =~Q6+Q7+Q8+Q9
              SS =~Q16+Q17+Q18+Q19
              SI =~Q21+Q22+Q23
              ST =~Q24+Q25+Q26
              SM =~Q29+Q30+Q31+Q32
              SR =~Q33+Q36+Q37
              
              Q16 ~~ Q17
              Q18 ~~ Q19
              Q36 ~~ Q37
              Q25 ~~ Q26 
              '
fit1_CFA <- cfa(cfa.model, data = envi_CFA2)
modindices(fit1_CFA, minimum.value = 3.841, sort=TRUE, free.remove = FALSE)
fitmeasures(fit1_CFA, c("chisq","df","pvalue","gfi","agfi","nfi","nnfi","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr", "ave"))
lavaanPlot(model = fit1_CFA, coefs = TRUE, stand=TRUE, covs = TRUE)

?fit1_CFA



####################H2 CFA + SEM
####################CFA
data.cfa$SA <- (data.cfa$Q6+data.cfa$Q7+data.cfa$Q8+data.cfa$Q9)/4
data.cfa$SS <- (data.cfa$Q16+data.cfa$Q17+data.cfa$Q18+data.cfa$Q19)/4
data.cfa$SI <- (data.cfa$Q21+data.cfa$Q22+data.cfa$Q23)/3
data.cfa$ST <- (data.cfa$Q24+data.cfa$Q25+data.cfa$Q26)/3
data.cfa$SM <- (data.cfa$Q29+data.cfa$Q30+data.cfa$Q31+data.cfa$Q32)/4
data.cfa$SR <- (data.cfa$Q33+data.cfa$Q36+data.cfa$Q37)/3


cfa.model <- 'Online_Env =~ SI+ST+SM+SR
              Ambient_Env =~ Q6+Q7+Q8+Q9
              Satisfaction =~ Q40
              
             
              ST ~~  SM
              Q6 ~~  Q7
              Q8 ~~  Q9
              
              
              '
fit2_CFA <- cfa(cfa.model, data = data.cfa)
modindices(fit2_CFA, minimum.value = 3.841, sort=TRUE, free.remove = FALSE)
fitmeasures(fit2_CFA, c("chisq","df","pvalue","gfi","agfi","nfi","nnfi","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr", "ave"))
lavaanPlot(model = fit2_CFA, coefs = TRUE, stand=TRUE)


############SEM

sem.model <- 'Satisfaction ~ Online_Env + Ambient_Env
              Online_Env ~ Ambient_Env

              Online_Env =~ SI+ST+SM+SR
              Ambient_Env =~ Q6+Q7+Q8+Q9
              Satisfaction =~ Q40
              ST ~~  SM
              Q6 ~~  Q7
              Q8 ~~  Q9
                  
              
                 '
fit2_SEM <- sem(sem.model, data = data.cfa)
fitmeasures(fit2_SEM, c("chisq","df","pvalue","gfi","agfi","nfi","nnfi","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr", "ave"))
lavaanPlot(model = fit2_SEM, coefs = TRUE, stand=TRUE, covs=TRUE)
coef(fit2_SEM)


#####################ggplot
library(ggplot2)
data$Q1 <- factor(data$Q1)
data$Q2 <- factor(data$Q2)
data$Q3 <- factor(data$Q3)
data$Q4 <- factor(data$Q4)
data$Q10 <- factor(data$Q10)


ggplot(data, aes(x=Q2,fill = Q1)) + geom_bar(stat="count",position="dodge")
ggplot(data, aes(x=Q3)) + geom_bar(stat="count")
ggplot(data, aes(x=Q4)) + geom_bar(stat="count")
ggplot(data, aes(x=Q10)) + geom_bar(stat="count")

try<-read.csv("almostalldata.csv",encoding = "UTF-8",header = F)
levels(data[1,1])

p=ggplot(data,aes(x=bmi,y=..density..,fill=sex))+geom_histogram(stat="bin",position="dodge",binwidth = 1)
p=p+geom_text(aes(label=as.character(round(..density..,3))),stat="bin",binwidth=1,vjust=-0.6)
print(p)

