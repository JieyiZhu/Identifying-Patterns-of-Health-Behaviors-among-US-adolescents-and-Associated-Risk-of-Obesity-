#libraries
library(poLCA)
#library(reshape2)
library(ggplot2)

##### Part I data manipulation
# loading data
ori_data<- read.csv("DATA-num.csv",header=TRUE) 
str(ori_data)
dim(ori_data)

## info of dataset: Look at Table 1
  ## for variables (TV3,COMP3,FR3, VEG3,SODA1,BK7DAY,PA7DAY,DLYPE): "1" is "yes"; "2" is "NO"
  ## for Sex variable : "1" is female, "2" is male

######################################################
######### data cleaning ##############################

## covert factor into char
data<- data.frame(lapply(ori_data, as.character),stringsAsFactors=FALSE)

## remove all 'NA' data 
data<-na.omit(data)

## remove all 'Missing', 'Mi','Un' values
data<-data[data$RACEETH !="Missing",]
data<-data[data$Age !="Mi",]
data<-data[data$Grade != "Mi",]
data<-data[data$Grade != "Un",]

## change "9t" in to '9' value for the Grade variable
data$Grade[data$Grade=="9t"] <- "9"

## covert Race into four categories. The minorities were combined as "Others"
data$RACEETH[data$RACEETH == "Am Indian / Alaska Native"] <- "Others"
data$RACEETH[data$RACEETH == "Native Hawaiian/other PI"] <- "Others"
data$RACEETH[data$RACEETH == "Multiple - Non-Hispanic"] <- "Others"

data$RACEETH[data$RACEETH == "Hispanic/Latino"] <- "Hispanic"
data$RACEETH[data$RACEETH == "Multiple - Hispanic"] <- "Hispanic"

# Covert "BMIPCT" variable to num, the rest of varialbes were convert to factor
data$BMIPCT <- as.numeric(data$BMIPCT)

# var_corr is a dataset used to test the intercorrelation among 8 variables: 
#    TV3,COMP3,FR3, VEG3,SODA1,BK7DAY,PA7DAY,DLYPE

var_corr <- as.data.frame(lapply(data[c(1:8)], as.numeric)) 

# except col#10"BMIPCT", the rest colums were converted to factor
data[c(-10)]<-as.data.frame(lapply(data[c(-10)], factor))




########################################################
############ Result  ###################################



# split data into females and males groups
Female <-data[data$Sex == "1",]
dim(Female)# the number of female in the sample
Male <- data[data$Sex == "2",]
dim(Male) # the number of male in the sample

###Table 2: Sample characteristics and descriptive statistics of weight-related behaviors
###

# functions for calculating percentage for variables
totPerc <- function (x){
  round(summary(x)/dim(data)[1],3) # for total data
}
fmPerc <- function (x){
  round(summary(x)/dim(Female)[1],3) # for females only 
}
mlPerc <- function (x){
  round(summary(x)/dim(Male)[1],3) # for males only
}

##  percentage of different Grade in different sex
totPerc(data$Grade)
fmPerc(Female$Grade)
mlPerc(Male$Grade)
##  percentage of different Race in different sex

totPerc(data$RACEETH)
fmPerc(Female$RACEETH)
mlPerc(Male$RACEETH)

##  percentage of different Age in different sex
totPerc(data$Age)
fmPerc(Female$Age)
mlPerc(Male$Age)

## percentage of weight status
round(nrow(subset(data, BMIPCT >= 5 & BMIPCT < 85))/dim(data)[1],3) # total
round(nrow(subset(Female, BMIPCT >= 5 & BMIPCT < 85))/dim(Female)[1],3) # Female
round(nrow(subset(Male, BMIPCT >= 5 & BMIPCT < 85))/dim(Male)[1],3) # Male

round(nrow(subset(data, BMIPCT >= 85 & BMIPCT < 95))/dim(data)[1],3) # total
round(nrow(subset(Female, BMIPCT >= 85 & BMIPCT < 95))/dim(Female)[1],3) # Female
round(nrow(subset(Male, BMIPCT >= 85 & BMIPCT < 95))/dim(Male)[1],3) # Male

round(nrow(subset(data, BMIPCT >= 95 ))/dim(data)[1],3) # total
round(nrow(subset(Female, BMIPCT >= 95))/dim(Female)[1],3) # Female
round(nrow(subset(Male, BMIPCT >= 95))/dim(Male)[1],3) # Male

## percentage of TV3 in different sex
totPerc(data$TV3[data$TV3 ==1])
fmPerc(Female$TV3[Female$TV3 ==1])
mlPerc(Male$TV3[Male$TV3 ==1])


## percentage of computer3 in different sex
totPerc(data$COMP3[data$COMP3 ==1])
fmPerc(Female$COMP3[Female$COMP3 ==1])
mlPerc(Male$COMP3[Male$COMP3 ==1])


## percentage of FR3 in different sex
totPerc(data$FR3[data$FR3 ==1])
fmPerc(Female$FR3[Female$FR3 ==1])
mlPerc(Male$FR3[Male$FR3 ==1])

## percentage of VEG3 in different sex
totPerc(data$VEG3[data$VEG3 ==1])
fmPerc(Female$VEG3[Female$VEG3 ==1])
mlPerc(Male$VEG3[Male$VEG3 ==1])

## percentage of SODA1 in different sex
totPerc(data$SODA1[data$SODA1 ==1])
fmPerc(Female$SODA1[Female$SODA1 ==1])
mlPerc(Male$SODA1[Male$SODA1 ==1])


## percentage of BK7DAY in different sex
totPerc(data$BK7DAY[data$BK7DAY ==1])
fmPerc(Female$BK7DAY[Female$BK7DAY ==1])
mlPerc(Male$BK7DAY[Male$BK7DAY ==1])

## percentage of PA7DAY in different sex
totPerc(data$PA7DAY[data$PA7DAY ==1])
fmPerc(Female$PA7DAY[Female$PA7DAY==1])
mlPerc(Male$PA7DAY[Male$PA7DAY ==1])

## percentage of BK7DAY in different sex
totPerc(data$DLYPE[data$DLYPE ==1])
fmPerc(Female$DLYPE[Female$DLYPE==1])
mlPerc(Male$DLYPE[Male$DLYPE ==1])


### Table 3: Inter-correlation of eight weight-related health behaviors
library(Hmisc)
cor <- rcorr(as.matrix(var_corr), type="pearson")
corres<-round(cor$r,3) # inter-correlation result 
corp<-round(cor$P,3)# p-value to see if the correlation is significant or not. 

#library(corrplot)
#corrplot(res,type="upper", order="hclust", tl.col="black", tl.srt=45)


##### latent class analysis
lca<-data
# Female and Male Data for LCA 
lcaF <- lca[lca$Sex==1,]
lcaM <- lca[lca$Sex==2,]

## females LCA
f <- cbind(TV3,COMP3,FR3, VEG3,SODA1,BK7DAY,PA7DAY,DLYPE)~ 1  
lcF1 <- poLCA(f, lcaF, nclass= 1, maxiter=10000)
lcF2 <- poLCA(f, lcaF, nclass=2, maxiter=10000) 
lcF3 <- poLCA(f, lcaF, nclass=3, maxiter=10000) 
lcF4 <- poLCA(f, lcaF, nclass=4, maxiter=10000) # has the lowest BIC 50598.62
lcF5 <- poLCA(f, lcaF, nclass=5, maxiter=10000) 
lcF6 <- poLCA(f, lcaF, nclass=6, maxiter=10000) 
lcF7 <- poLCA(f, lcaF, nclass=7, maxiter=10000) 

## males LCA
f <- cbind(TV3,COMP3,FR3, VEG3,SODA1,BK7DAY,PA7DAY,DLYPE)~ 1
lcM1 <- poLCA(f, lcaM, nclass= 1, maxiter=10000)
lcM2 <- poLCA(f, lcaM, nclass=2, maxiter=10000) 
lcM3 <- poLCA(f, lcaM, nclass=3, maxiter=10000) 
lcM4 <- poLCA(f, lcaM, nclass=4, maxiter=10000, nrep=10) 
lcM5 <- poLCA(f, lcaM, nclass=5, maxiter=10000, nrep=10) # has the lowest BIC 55239.04
lcM6 <- poLCA(f, lcaM, nclass=6, maxiter=10000) 
lcM7 <- poLCA(f, lcaM, nclass=7, maxiter=10000) 


### Table 4 and Table 5 for BIC, AIC , DF etc...of 7 LCA models.
## 
## Ref: http://statistics.ohlsen-web.de/latent-class-analysis-polca/
results <- data.frame(Model=c("1-class"),
                      AIC = lcF1$aic,
                      df = lcF1$resid.df,
                      BIC=lcF1$bic,
                      ABIC=  (-2*lcF1$llik) + ((log((lcF1$N + 2)/24)) * lcF1$npar),
                      CAIC = (-2*lcF1$llik) + lcF1$npar * (1 + log(lcF1$N)), 
                      Gsq=lcF1$Gsq)

results$Model<-as.integer(results$Model)

results[1,1]<-c("1-class")
results[2,1]<-c("2-class")
results[3,1]<-c("3-class")
results[4,1]<-c("4-class")
results[5,1]<-c("5-class")
results[6,1]<-c("6-class")
results[7,1]<-c("7-class")

results[2,2]<-lcF2$aic
results[3,2]<-lcF3$aic
results[4,2]<-lcF4$aic
results[5,2]<-lcF5$aic
results[6,2]<-lcF6$aic
results[7,2]<-lcF7$aic

results[2,3]<-lcF2$resid.df
results[3,3]<-lcF3$resid.df
results[4,3]<-lcF4$resid.df
results[5,3]<-lcF5$resid.df
results[6,3]<-lcF6$resid.df
results[7,3]<-lcF7$resid.df

results[2,4]<-lcF2$bic
results[3,4]<-lcF3$bic
results[4,4]<-lcF4$bic
results[5,4]<-lcF5$bic
results[6,4]<-lcF6$bic
results[7,4]<-lcF7$bic

results[2,5]<-(-2*lcF2$llik) + ((log((lcF2$N + 2)/24)) * lcF2$npar) #abic
results[3,5]<-(-2*lcF3$llik) + ((log((lcF3$N + 2)/24)) * lcF3$npar)
results[4,5]<-(-2*lcF4$llik) + ((log((lcF4$N + 2)/24)) * lcF4$npar)
results[5,5]<-(-2*lcF5$llik) + ((log((lcF5$N + 2)/24)) * lcF5$npar)
results[6,5]<-(-2*lcF6$llik) + ((log((lcF6$N + 2)/24)) * lcF6$npar)
results[7,5]<-(-2*lcF7$llik) + ((log((lcF7$N + 2)/24)) * lcF7$npar)

results[2,6]<- (-2*lcF2$llik) + lcF2$npar * (1 + log(lcF2$N)) #caic
results[3,6]<- (-2*lcF3$llik) + lcF3$npar * (1 + log(lcF3$N))
results[4,6]<- (-2*lcF4$llik) + lcF4$npar * (1 + log(lcF4$N))
results[5,6]<- (-2*lcF5$llik) + lcF5$npar * (1 + log(lcF5$N))
results[6,6]<- (-2*lcF6$llik) + lcF6$npar * (1 + log(lcF6$N))
results[7,6]<- (-2*lcF7$llik) + lcF7$npar * (1 + log(lcF7$N))

results[2,7]<-lcF2$Gsq
results[3,7]<-lcF3$Gsq
results[4,7]<-lcF4$Gsq
results[5,7]<-lcF5$Gsq
results[6,7]<-lcF6$Gsq
results[7,7]<-lcF7$Gsq

Femaleres<-results

# Male
results <- data.frame( Model=c("1-class"),
                       AIC = lcF1$aic,
                       df = lcM1$resid.df,
                       BIC=lcM1$bic,
                       ABIC=  (-2*lcM1$llik) + ((log((lcM1$N + 2)/24)) * lcM1$npar),
                       CAIC = (-2*lcM1$llik) + lcM1$npar * (1 + log(lcM1$N)), 
                       likelihood_ratio=lcM1$Gsq)

results$Model<-as.integer(results$Model)

results[1,1]<-c("1-class")
results[2,1]<-c("2-class")
results[3,1]<-c("3-class")
results[4,1]<-c("4-class")
results[5,1]<-c("5-class")
results[6,1]<-c("6-class")
results[7,1]<-c("7-class")

results[2,2]<-lcM2$aic
results[3,2]<-lcM3$aic
results[4,2]<-lcM4$aic
results[5,2]<-lcM5$aic
results[6,2]<-lcM6$aic
results[7,2]<-lcM7$aic

results[2,3]<-lcM2$resid.df
results[3,3]<-lcM3$resid.df
results[4,3]<-lcM4$resid.df
results[5,3]<-lcM5$resid.df
results[6,3]<-lcM6$resid.df
results[7,3]<-lcM7$resid.df

results[2,4]<-lcM2$bic
results[3,4]<-lcM3$bic
results[4,4]<-lcM4$bic
results[5,4]<-lcM5$bic
results[6,4]<-lcM6$bic
results[7,4]<-lcM7$bic

results[2,5]<-(-2*lcM2$llik) + ((log((lcM2$N + 2)/24)) * lcM2$npar) #abic
results[3,5]<-(-2*lcM3$llik) + ((log((lcM3$N + 2)/24)) * lcM3$npar)
results[4,5]<-(-2*lcM4$llik) + ((log((lcM4$N + 2)/24)) * lcM4$npar)
results[5,5]<-(-2*lcM5$llik) + ((log((lcM5$N + 2)/24)) * lcM5$npar)
results[6,5]<-(-2*lcM6$llik) + ((log((lcM6$N + 2)/24)) * lcM6$npar)
results[7,5]<-(-2*lcM7$llik) + ((log((lcM7$N + 2)/24)) * lcM7$npar)

results[2,6]<- (-2*lcM2$llik) + lcM2$npar * (1 + log(lcM2$N)) #caic
results[3,6]<- (-2*lcM3$llik) + lcM3$npar * (1 + log(lcM3$N))
results[4,6]<- (-2*lcM4$llik) + lcM4$npar * (1 + log(lcM4$N))
results[5,6]<- (-2*lcM5$llik) + lcM5$npar * (1 + log(lcM5$N))
results[6,6]<- (-2*lcM6$llik) + lcM6$npar * (1 + log(lcM6$N))
results[7,6]<- (-2*lcM7$llik) + lcM7$npar * (1 + log(lcM7$N))

results[2,7]<-lcM2$Gsq
results[3,7]<-lcM3$Gsq
results[4,7]<-lcM4$Gsq
results[5,7]<-lcM5$Gsq
results[6,7]<-lcM6$Gsq
results[7,7]<-lcM7$Gsq

Maleres <-results

Femaleres # Table 4
Maleres # Table 5


### Table 6 and Table 7
## Table 6 probability of each variables in each class when the model contains 4 latent classes in females
fmvarProb<- matrix(rep(NA), nrow = 8, ncol = 4)
colnames(fmvarProb)<- c("class1","class2","class3", "class4")
rownames(fmvarProb)<- c("3h/day of TV viewing (TV3)",
                        "3h/day of computer/video game (COMP3)",
                        "3 times fruits/day (FR3)",
                        "3 times vegetables/day (VEG3)",
                        "1 soda/day (SODA1)",
                        "All 7 days breakfast consumption (BK7DAY)",
                        "60min/day of physical activity (PA7DAY)",
                        "All 5 days physical education class (DLYPE)"
)

fmvarProb[1,] <- round(lcF4$probs$TV3[,1],2)
fmvarProb[2,] <- round(lcF4$probs$COMP3[,1],2)
fmvarProb[3,] <- round(lcF4$probs$FR3[,1],2)
fmvarProb[4,] <- round(lcF4$probs$VEG3[,1],2)
fmvarProb[5,] <- round(lcF4$probs$SODA1[,1],2)
fmvarProb[6,] <- round(lcF4$probs$BK7DAY[,1],2)
fmvarProb[7,] <- round(lcF4$probs$PA7DAY[,1],2)
fmvarProb[8,] <- round(lcF4$probs$DLYPE[,1],2)


library(ggplot2)
library(reshape2)

table6 <- read.csv("table6.csv", header = TRUE)
table6_melt <- melt(table6, id.vars = "Items", variable.name = 'class' )
head(table6_melt)
colnames(table6_melt)[3] <- "Probability"
ggplot(table6_melt, aes(x = Items, y= Probability, group = class)) +
    geom_point(stat = "identity",aes(colour = class)) + 
    geom_line(aes(colour = class)) +
    scale_x_discrete(limits= table6$Items) +
    geom_hline(yintercept =0.50, linetype="dashed", 
             color = "black", size=2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))




## Table 7 probability of each variables in each class when the model contains 5 latent classes in males
mvarProb<- matrix(rep(NA), nrow = 8, ncol = 5)
colnames(mvarProb)<- c("class1","class2","class3", "class4","class5")
rownames(mvarProb)<- c("3h/day of TV viewing (TV3)",
                       "3h/day of computer/video game (COMP3)",
                       "3 times fruits/day (FR3)",
                       "3 times vegetables/day (VEG3)",
                       "1 soda/day (SODA1)",
                       "All 7 days breakfast consumption (BK7DAY)",
                       "60min/day of physical activity (PA7DAY)",
                       "All 5 days physical education class (DLYPE)"
)

mvarProb[1,] <- round(lcM5$probs$TV3[,1],2)
mvarProb[2,] <- round(lcM5$probs$COMP3[,1],2)
mvarProb[3,] <- round(lcM5$probs$FR3[,1],2)
mvarProb[4,] <- round(lcM5$probs$VEG3[,1],2)
mvarProb[5,] <- round(lcM5$probs$SODA1[,1],2)
mvarProb[6,] <- round(lcM5$probs$BK7DAY[,1],2)
mvarProb[7,] <- round(lcM5$probs$PA7DAY[,1],2)
mvarProb[8,] <- round(lcM5$probs$DLYPE[,1],2)

fmvarProb # Table 6
mvarProb # Table 7



table7 <- read.csv("table7.csv", header = TRUE)
table7_melt <- melt(table7, id.vars = "Items", variable.name = 'class' )
head(table7_melt)
colnames(table7_melt)[3] <- "Probability"
ggplot(table7_melt, aes(x = Items, y= Probability, group = class)) +
  geom_point(stat = "identity",aes(colour = class)) + 
  geom_line(aes(colour = class)) +  
  scale_x_discrete(limits= table7$Items) +
  geom_hline(yintercept =0.50, linetype="dashed", 
             color = "black", size=2) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#### Table 8 and Table 9 : weight status and sociodemographic diferentces

## split males into five subgroups 
Male$class<-lcM5$predclass
Male$Grade<- as.numeric(as.character(Male$Grade))
Male$Age<- as.numeric(as.character(Male$Age))
Mclass1 <- Male[Male$class==1,]
Mclass2 <- Male[Male$class==2,]
Mclass3 <- Male[Male$class==3,]
Mclass4 <- Male[Male$class==4,]
Mclass5 <- Male[Male$class==5,]

## split females into four subgroups
Female$class <-lcF4$predclass
Female$Grade<- as.numeric(as.character(Female$Grade))
Female$Age<- as.numeric(as.character(Female$Age))
Fclass1 <- Female[Female$class==1,]
Fclass2 <- Female[Female$class==2,]
Fclass3 <- Female[Female$class==3,]
Fclass4 <- Female[Female$class==4,]


## weight status percentage 
calculateBMIPerc<- function(BMI){
  res<- matrix(rep(NA), nrow = 1,ncol=3)
  NormCount = 0
  OWCount = 0
  OBCount = 0
  for(i in 1:length(BMI)){
    
    if (BMI[i] >= 5 & BMI[i]< 85){
      NormCount <- NormCount +1
    }
    else if (BMI[i] >= 85 & BMI[i] < 95){
      OWCount <- OWCount +1
    }
    else if (BMI[i] >= 95){
      OBCount <- OBCount + 1
    }
  }
  
  res[1,1] <- round(NormCount/length(BMI), 3)*100
  res[1,2] <- round(OWCount/length(BMI), 3)*100
  res[1,3] <- round(OBCount/length(BMI), 3)*100
  
  res
}

## male weight status percentage of each subgroup
maleBMI<- matrix(rep(NA), nrow = 3, ncol=5)
maleBMI[,1]<-calculateBMIPerc(Mclass1$BMIPCT)
maleBMI[,2]<-calculateBMIPerc(Mclass2$BMIPCT)
maleBMI[,3]<-calculateBMIPerc(Mclass3$BMIPCT)
maleBMI[,4]<-calculateBMIPerc(Mclass4$BMIPCT)
maleBMI[,5]<-calculateBMIPerc(Mclass5$BMIPCT)
colnames(maleBMI)<- c("class1","class2","class3", "class4", "class5")
rownames(maleBMI)<- c("Normal","Overweight","Obese")

## female weight status percentage of each subgroup
fmBMI <- matrix(rep(NA), nrow = 3, ncol=4)
fmBMI[,1]<-calculateBMIPerc(Fclass1$BMIPCT)
fmBMI[,2]<-calculateBMIPerc(Fclass2$BMIPCT)
fmBMI[,3]<-calculateBMIPerc(Fclass3$BMIPCT)
fmBMI[,4]<-calculateBMIPerc(Fclass4$BMIPCT)
colnames(fmBMI)<- c("class1","class2","class3", "class4")
rownames(fmBMI)<- c("Normal","Overweight","Obese")


## Grade percentage 
calculateGradePerc<- function(x){
  res<- matrix(rep(NA), nrow = 1,ncol=4)
  count1 = 0
  count2 = 0
  count3 = 0
  count4 = 0
  for(i in 1:length(x)){
    if (x[i] == 9){
      count1 <- count1 + 1
    }
    else if (x[i] == 10){
      count2 <- count2 + 1
    }
    else if (x[i] == 11){
      count3 <- count3 + 1
    } 
    else if(x[i]== 12){
      count4 <- count4 +1
    }
    
  }
  
  res[1,1] <- round(count1/length(x), 3)*100
  res[1,2] <- round(count2/length(x), 3)*100
  res[1,3] <- round(count3/length(x), 3)*100
  res[1,4] <- round(count4/length(x), 3)*100
  res
}

# male grade percentage of each subgroup
maleGrade<- matrix(rep(NA), nrow = 4, ncol=5)
maleGrade[,1]<-calculateGradePerc(Mclass1$Grade)
maleGrade[,2]<-calculateGradePerc(Mclass2$Grade)
maleGrade[,3]<-calculateGradePerc(Mclass3$Grade)
maleGrade[,4]<-calculateGradePerc(Mclass4$Grade)
maleGrade[,5]<-calculateGradePerc(Mclass5$Grade)
colnames(maleGrade)<- c("class1","class2","class3", "class4", "class5")
rownames(maleGrade) <- c("9th","10th","11th","12th")

# female grade percentage of each subgroup
fmGrade <- matrix(rep(NA), nrow = 4, ncol=4)
fmGrade[,1]<-calculateGradePerc(Fclass1$Grade)
fmGrade[,2]<-calculateGradePerc(Fclass2$Grade)
fmGrade[,3]<-calculateGradePerc(Fclass3$Grade)
fmGrade[,4]<-calculateGradePerc(Fclass4$Grade)
colnames(fmGrade)<- c("class1","class2","class3", "class4")
rownames(fmGrade) <- c("9th","10th","11th","12th")

## Age percentage.
calculateAgePerc<- function(x){
  res<- matrix(rep(NA), nrow = 1,ncol=7)
  count1 = 0
  count2 = 0
  count3 = 0
  count4 = 0
  count5 = 0
  count6 = 0
  count7 = 0
  for(i in 1:length(x)){
    if (x[i] == 12){
      count1 <- count1 + 1
    }
    else if (x[i] == 13){
      count2 <- count2 + 1
    }
    else if (x[i] == 14){
      count3 <- count3 + 1
    } 
    else if(x[i]== 15){
      count4 <- count4 +1
    }
    else if(x[i]== 16){
      count5 <- count5 +1
    }
    else if(x[i]== 17){
      count6 <- count6 +1
    }
    else if(x[i]== 18){
      count7 <- count7 +1
    }
  }
  
  res[1,1] <- round(count1/length(x), 3)*100
  res[1,2] <- round(count2/length(x), 3)*100
  res[1,3] <- round(count3/length(x), 3)*100
  res[1,4] <- round(count4/length(x), 3)*100
  res[1,5] <- round(count5/length(x), 3)*100
  res[1,6] <- round(count6/length(x), 3)*100
  res[1,7] <- round(count7/length(x), 3)*100
  
  res
}

# male age percentage of each subgroup
maleAge<- matrix(rep(NA), nrow = 7, ncol=5)

maleAge[,1]<-calculateAgePerc(Mclass1$Age)
maleAge[,2]<-calculateAgePerc(Mclass2$Age)
maleAge[,3]<-calculateAgePerc(Mclass3$Age)
maleAge[,4]<-calculateAgePerc(Mclass4$Age)
maleAge[,5]<-calculateAgePerc(Mclass5$Age)
colnames(maleAge)<- c("class1","class2","class3", "class4", "class5")
rownames(maleAge) <- c("12","13","14","15", "16","17","18")

# female age percentage of each subgroup
fmAge<- matrix(rep(NA), nrow = 7, ncol=4)

fmAge[,1]<-calculateAgePerc(Fclass1$Age)
fmAge[,2]<-calculateAgePerc(Fclass2$Age)
fmAge[,3]<-calculateAgePerc(Fclass3$Age)
fmAge[,4]<-calculateAgePerc(Fclass4$Age)
colnames(fmAge)<- c("class1","class2","class3", "class4")
rownames(fmAge) <- c("12","13","14","15", "16","17","18")

#

## Race percentage 
calculateRacePerc<- function(x){
  res<- matrix(rep(NA), nrow = 1,ncol=5)
  count1 = 0
  count2 = 0
  count3 = 0
  count4 = 0
  count5 = 0
  
  for(i in 1:length(x)){
    if (x[i] == "White"){
      count1 <- count1 + 1
    }
    else if (x[i] == "Hispanic"){
      count2 <- count2 + 1
    }
    else if (x[i] == "Black or African American"){
      count3 <- count3 + 1
    } 
    else if(x[i]== "Asian"){
      count4 <- count4 +1
    }
    else if(x[i]== "Others"){
      count5 <- count5 +1
    }
    
  }
  
  res[1,1] <- round(count1/length(x), 3)*100
  res[1,2] <- round(count2/length(x), 3)*100
  res[1,3] <- round(count3/length(x), 3)*100
  res[1,4] <- round(count4/length(x), 3)*100
  res[1,5] <- round(count5/length(x), 3)*100
  
  res
}
# male race percentage of each subgroup
maleRace<- matrix(rep(NA), nrow = 5, ncol=5)

maleRace[,1]<-calculateRacePerc(Mclass1$RACEETH)
maleRace[,2]<-calculateRacePerc(Mclass2$RACEETH)
maleRace[,3]<-calculateRacePerc(Mclass3$RACEETH)
maleRace[,4]<-calculateRacePerc(Mclass4$RACEETH)
maleRace[,5]<-calculateRacePerc(Mclass5$RACEETH)
colnames(maleRace)<- c("class1","class2","class3", "class4", "class5")
rownames(maleRace) <- c("White", "Hispanic ","Black or African American",
                        "Asian", "Others ")
# female race percentage of each subgroup                    
fmRace<- matrix(rep(NA), nrow = 5, ncol=4)

fmRace[,1]<-calculateRacePerc(Fclass1$RACEETH)
fmRace[,2]<-calculateRacePerc(Fclass2$RACEETH)
fmRace[,3]<-calculateRacePerc(Fclass3$RACEETH)
fmRace[,4]<-calculateRacePerc(Fclass4$RACEETH)
colnames(fmRace)<- c("class1","class2","class3", "class4")
rownames(fmRace) <- c("White", "Hispanic ","Black or African American",
                      "Asian", "Others ")

#Table 8
rbind(fmBMI,fmGrade,fmAge,fmRace)
#Table 9
rbind(maleBMI, maleGrade, maleAge,maleRace)

