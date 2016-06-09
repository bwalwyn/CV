#Ben Walwyn
#BMI, overweight and obesity in SA: model
options(scipen=999)

install.packages("R2OpenBUGS")
install.packages("coda")
install.packages("norm")
library("coda")
library("norm")
library("R2OpenBUGS")
library(car)

male_means <- read.csv("male_means.csv",header=T)
female_means <- read.csv("female_means.csv",header=T) 

male_overwght <- read.csv("male_overwght.csv",header=T)
male_overwght <- cbind(male_overwght,male_means$mean)
female_overwght <- read.csv("female_overwght.csv",header=T) 
female_overwght <- cbind(female_overwght,female_means$mean)

male_obese <- read.csv("male_obese.csv",header=T) 
male_obese <- cbind(male_obese,male_means$mean)
female_obese <- read.csv("female_obese.csv",header=T) 
female_obese <- cbind(female_obese,female_means$mean)

#Fix standard error formats
male_means[1,]
male_means$std_error<-as.numeric(levels(male_means$std_error))[male_means$std_error]
male_means$ci_l<-as.numeric(levels(male_means$ci_l))[male_means$ci_l]
male_means$ci_u<-as.numeric(levels(male_means$ci_u))[male_means$ci_u]

female_means$std_error<-as.numeric(levels(female_means$std_error))[female_means$std_error]
female_means$ci_l<-as.numeric(levels(female_means$ci_l))[female_means$ci_l]
female_means$ci_u<-as.numeric(levels(female_means$ci_u))[female_means$ci_u]

#Fix proportion and CI for overweight and obese
#MALE
male_overwght$prop<-as.numeric(levels(male_overwght$prop))[male_overwght$prop]
male_overwght$std_error<-as.numeric(levels(male_overwght$std_error))[male_overwght$std_error]
male_obese$prop<-as.numeric(levels(male_obese$prop))[male_obese$prop]
male_obese$std_error<-as.numeric(levels(male_obese$std_error))[male_obese$std_error]
#FEMALE
female_overwght$prop<-as.numeric(levels(female_overwght$prop))[female_overwght$prop]
female_overwght$std_error<-as.numeric(levels(female_overwght$std_error))[female_overwght$std_error]
female_obese$prop<-as.numeric(levels(female_obese$prop))[female_obese$prop]
female_obese$std_error<-as.numeric(levels(female_obese$std_error))[female_obese$std_error]


# --- Number of observations and missing observations --- #

#Numbers of means for different years by sex
nrow(subset(male_means, year==1998))
nrow(subset(male_means, year==2003))
nrow(subset(male_means, year==2008))
nrow(subset(male_means, year==2010))
nrow(subset(male_means, year==2012))
nrow(subset(female_means, year==1998))
nrow(subset(female_means, year==2003))
nrow(subset(female_means, year==2008))
nrow(subset(female_means, year==2010))
nrow(subset(female_means, year==2012))

#overweight and obese
nrow(male_overwght_sd)
nrow(female_overwght_sd)
nrow(male_obese_sd)
nrow(female_obese_sd)

#missing means
nrow(subset(male_means, std_error=='.'| std_error==0 & year==1998))
nrow(subset(male_means, std_error=='.'| std_error=='0' & year==2003))
nrow(subset(male_means, std_error=='.'| std_error=='0' & year==2008))
nrow(subset(male_means, std_error=='.'| std_error=='0' & year==2010))
nrow(subset(male_means, std_error=='.'| std_error=='0' & year==2012))
nrow(subset(female_means, std_error=='.'| std_error=='0' & year==1998))
nrow(subset(female_means, std_error=='.'| std_error=='0' & year==2003))
nrow(subset(female_means, std_error=='.'| std_error=='0' & year==2008))
nrow(subset(female_means, std_error=='.'| std_error=='0' & year==2010))
nrow(subset(female_means, std_error=='.'| std_error=='0' & year==2012))

#--- Number of observations with zero standard error ---#

#means
sum(((male_means_sd$std_error==0) & (male_means_sd$year==1998))*1)
sum(((male_means_sd$std_error==0) & (male_means_sd$year==2003))*1)
sum(((male_means_sd$std_error==0) & (male_means_sd$year==2008))*1)
sum(((male_means_sd$std_error==0) & (male_means_sd$year==2010))*1)
sum(((male_means_sd$std_error==0) & (male_means_sd$year==2012))*1)

sum(((female_means_sd$std_error==0) & (female_means_sd$year==1998))*1)
sum(((female_means_sd$std_error==0) & (female_means_sd$year==2003))*1)
sum(((female_means_sd$std_error==0) & (female_means_sd$year==2008))*1)
sum(((female_means_sd$std_error==0) & (female_means_sd$year==2010))*1)
sum(((female_means_sd$std_error==0) & (female_means_sd$year==2012))*1)

#overweight
sum((is.na(male_overwght$std_error)&male_overwght$year==1998)*1)
sum((is.na(male_overwght$std_error)&male_overwght$year==2003)*1)
sum((is.na(male_overwght$std_error)&male_overwght$year==2008)*1)
sum((is.na(male_overwght$std_error)&male_overwght$year==2010)*1)
sum((is.na(male_overwght$std_error)&male_overwght$year==2012)*1)

sum((is.na(female_overwght$std_error)&female_overwght$year==1998)*1)
sum((is.na(female_overwght$std_error)&female_overwght$year==2003)*1)
sum((is.na(female_overwght$std_error)&female_overwght$year==2008)*1)
sum((is.na(female_overwght$std_error)&female_overwght$year==2010)*1)
sum((is.na(female_overwght$std_error)&female_overwght$year==2012)*1)

#obese
sum((is.na(male_obese$std_error)&male_obese$year==1998)*1)
sum((is.na(male_obese$std_error)&male_obese$year==2003)*1)
sum((is.na(male_obese$std_error)&male_obese$year==2008)*1)
sum((is.na(male_obese$std_error)&male_obese$year==2010)*1)
sum((is.na(male_obese$std_error)&male_obese$year==2012)*1)

sum((is.na(female_obese$std_error)&female_obese$year==1998)*1)
sum((is.na(female_obese$std_error)&female_obese$year==2003)*1)
sum((is.na(female_obese$std_error)&female_obese$year==2008)*1)
sum((is.na(female_obese$std_error)&female_obese$year==2010)*1)
sum((is.na(female_obese$std_error)&female_obese$year==2012)*1)

# --- Remove observations without means, proportions and/or standard errors ---#

male_means_sd<-na.omit(male_means)
male_means_sd<-male_means_sd[-which(male_means_sd$std_error==0),]
male_overwght_sd<-na.omit(male_overwght)
male_overwght_sd<-male_overwght_sd[-which(male_overwght_sd$std_error==0),]

male_obese_sd<-na.omit(male_obese)
female_means_sd<-na.omit(female_means)
female_overwght_sd<-na.omit(female_overwght)
female_obese_sd<-na.omit(female_obese)

# --- Prepare factor variables for mean analysis --- #

male_means=male_means_sd
female_means=female_means_sd

#Code mean year variables
male_stdy_yr <- male_means$year-1998
female_stdy_yr <- female_means$year-1998

#Survey numbers
male_yr<-as.factor(male_means$year)
male_yr<-cbind((male_yr==1998)*1,model.matrix(~male_yr)[,2:5])
female_yr<-as.factor(female_means$year)
female_yr<-cbind((female_yr==1998)*1,model.matrix(~female_yr)[,2:5])

#Ages
male.means.agecat<-cbind((male_means$agecat=='15-24')*1,model.matrix(~male_means$agecat)[,2:6])
male.means.age=c()
for (i in 1:length(male_means$agecat)){
  if(male_means$agecat[i]=='15-24'){
    male.means.age[i]=20
  }
  if(male_means$agecat[i]=='25-34'){
    male.means.age[i]=30
  }
  if(male_means$agecat[i]=='35-44'){
    male.means.age[i]=40
  }
  if(male_means$agecat[i]=='45-54'){
    male.means.age[i]=50
  }
  if(male_means$agecat[i]=='55-64'){
    male.means.age[i]=60
  }
  if(male_means$agecat[i]=='65+'){
    male.means.age[i]=70
  }
}
male.means.age

female.means.agecat<-cbind((female_means$agecat=='15-24')*1,model.matrix(~female_means$agecat)[,2:6])
female.means.age=c()
for (i in 1:length(female_means$agecat)){
  if(female_means$agecat[i]=='15-24'){
    female.means.age[i]=20
  }
  if(female_means$agecat[i]=='25-34'){
    female.means.age[i]=30
  }
  if(female_means$agecat[i]=='35-44'){
    female.means.age[i]=40
  }
  if(female_means$agecat[i]=='45-54'){
    female.means.age[i]=50
  }
  if(female_means$agecat[i]=='55-64'){
    female.means.age[i]=60
  }
  if(female_means$agecat[i]=='65+'){
    female.means.age[i]=70
  }
}
female.means.age

#Provinces
prov.names<-list('Eastern Cape',  "Free State",'Gauteng', 'KwaZulu Natal',
           'Limpopo', 'Mpumalanga', 'North West','Northern Cape', 'Western Cape')
prov.v.names<-c('Eastern Cape',  "Free State",'Gauteng', 'KwaZulu Natal',
                'Limpopo', 'Mpumalanga', 'North West','Northern Cape', 'Western Cape')

#MALE MEANS provinces
dummies.male.means<- model.matrix(~male_means$prov)
male.means.dummy.prov <- cbind((male_means$prov=='Eastern Cape')*1,dummies.male.means[,2:9])
colnames(male.means.dummy.prov)=prov.v.names
male.means.dummy.prov[1,]

#FEMALE MEANS provinces
female.means.dummy.prov <- cbind((female_means$prov=='Eastern Cape')*1,model.matrix(~female_means$prov)[,2:9])
colnames(female.means.dummy.prov)<-prov.v.names
female.means.dummy.prov[1,]

#Population Groups
pop.v.names <- c('African','Asian/Indian','Coloured','White')

#MALE MEAN pop group
dummies.male.means<- model.matrix(~male_means$race)
male.means.dummy.pop <- cbind((male_means$race=='African')*1,dummies.male.means[,2:4])
colnames(male.means.dummy.pop)=pop.v.names
male.means.dummy.pop[2,]

#FEMALE MEAN pop group
dummies.female.means<- model.matrix(~female_means$race)
female.means.dummy.pop <- cbind((female_means$race=='African')*1,dummies.female.means[,2:4])
colnames(female.means.dummy.pop)=pop.v.names


#---  Bugs analysis of means --- #

##MALE MEANS WITH AGE CATEGORY

N <- 603 #number of means
y <- male_means$mean #obs
sigma2.y <- male_means$std_error^2 #std error on each mean
agecat <- male.means.agecat
prov <- male.means.dummy.prov
t<-male_stdy_yr

data <- list("N","y","sigma2.y",'agecat','t','prov')

inits <- function(){
  list(a = rnorm(3, 25, 5), b = rnorm(1, 0, 0.5),
       c1 = rnorm(3, 0, 2),
       c2 = rnorm(3, 0, 2),
       c3 = rnorm(3, 0, 2),
       c4 = rnorm(3, 0, 2),
       c5 = rnorm(3, 0, 2),
       c6 = rnorm(3, 0, 2),
       d1 = rnorm(3, 0, 2),
       d2 = rnorm(3, 0, 2),
       d3 = rnorm(3, 0, 2),
       d4 = rnorm(3, 0, 2),
       d5 = rnorm(3, 0, 2),
       d6 = rnorm(3, 0, 2),
       d7 = rnorm(3, 0, 2),
       d8 = rnorm(3, 0, 2),
       d9 = rnorm(3, 0, 2)
       )
}

inits()

male.means.sim <- bugs(data, inits, model.file = "means.agecat.txt",
                    parameters = c("a", "b", "c1",'c2','c3','c4','c5','c6',"d1",'d2','d3','d4','d5','d6','d7','d8','d9'),
                    n.chains = 3, n.iter = 100000,debug=T)

##MALE MEANS WITH AGE CONTINUOUS

N <- 603                  #number of means
y <- male_means$mean      #obs
tau.y <- male_means$std_error^(-2) #observed precision on each mean
age <- male.means.age - 40
prov <- cbind(1,male.means.dummy.prov[,-1])
pop <- cbind(1,male.means.dummy.pop[,-1])
t<-male_stdy_yr

data <- list("N","y","tau.y",'age','t','prov','pop')

inits <- function(){
  list(b = rnorm(1, 0, 100),
       c = rnorm(1, 0, 100),
       d1 = rnorm(1, 0, 100),
       d2 = rnorm(1, 0, 100),
       d3 = rnorm(1, 0, 100),
       d4 = rnorm(1, 0, 100),
       d5 = rnorm(1, 0, 100),
       d6 = rnorm(1, 0, 100),
       d7 = rnorm(1, 0, 100),
       d8 = rnorm(1, 0, 100),
       d9 = rnorm(1, 0, 100),
       e2 = rnorm(1, 0, 100),
       e3 = rnorm(1, 0, 100),
       e4 = rnorm(1, 0, 100)
  )
}

inits()

male.means.sim2 <- bugs(data, inits, model.file = "means.age.txt",
                       parameters = c("b", "c","d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                      'e2','e3','e4'),
                       n.chains = 1, n.iter = 100000,debug=T)


##MALE MEANS WITH AGE SPLINE AT 40 ONLY 

#observations
N <- 603                  #number of means
y <- male_means$mean      #obs
tau.y <- male_means$std_error^(-2) #observed precision on each mean
#age variables
age <- male.means.age-40
age.k1 <- age
age.k1[(male.means.age<=40)]<-0
age.k2 <- age
age.k2[(male.means.age<=60)]<-0 
#covariates
prov <- cbind(1,male.means.dummy.prov[,-1])
pop <- cbind(1,male.means.dummy.pop[,-1])
t<-male_stdy_yr

data <- list("N","y","tau.y",'age','age.k1','t','prov','pop')

inits <- function(){
  list(b = rnorm(3, 0, 100),
       c1 = rnorm(3, 0, 100),
       c2 = rnorm(3, 0, 100),
       c3 = rnorm(3, 0, 100),
       c4 = rnorm(3, 0, 100),
       d1 = rnorm(3, 0, 100),
       d2 = rnorm(3, 0, 100),
       d3 = rnorm(3, 0, 100),
       d4 = rnorm(3, 0, 100),
       d5 = rnorm(3, 0, 100),
       d6 = rnorm(3, 0, 100),
       d7 = rnorm(3, 0, 100),
       d8 = rnorm(3, 0, 100),
       d9 = rnorm(3, 0, 100),
       e2 = rnorm(3, 0, 100),
       e3 = rnorm(3, 0, 100),
       e4 = rnorm(3, 0, 100)
  )
}

inits()

male.means.sim3 <- bugs(data, inits, model.file = "means.age3.txt",
                        parameters = c("b", "c1","c2","c3","c4",
                                       "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                       'e2','e3','e4'),
                        n.chains = 1, n.iter = 100000,debug=T)

##MALE MEANS WITH AGE SPLINES AT K1=40 and K2=60

#observations
N <- 603                  #number of means
y <- male_means$mean      #obs
tau.y <- male_means$std_error^(-2) #observed precision on each mean
#age variables
age <- male.means.age-40
age.k1 <- age
age.k1[(male.means.age<=40)]<-0
age.k2 <- age
age.k2[(male.means.age<=60)]<-0 
#covariates
prov <- cbind(1,male.means.dummy.prov[,-1])
pop <- cbind(1,male.means.dummy.pop[,-1])
t<-male_stdy_yr

data <- list("N","y","tau.y",'age','age.k1','age.k2','t','prov','pop')

inits <- function(){
  list(b = rnorm(1, 0, 1),
       c1 = rnorm(1, 0, 100),
       c2 = rnorm(3, 0, 100),
       c3 = rnorm(3, 0, 100),
       c4 = rnorm(3, 0, 100),
       c5 = rnorm(3, 0, 100),
       d1 = rnorm(3, 0, 100),
       d2 = rnorm(3, 0, 100),
       d3 = rnorm(3, 0, 100),
       d4 = rnorm(3, 0, 100),
       d5 = rnorm(3, 0, 100),
       d6 = rnorm(3, 0, 100),
       d7 = rnorm(3, 0, 100),
       d8 = rnorm(3, 0, 100),
       d9 = rnorm(3, 0, 100),
       e2 = rnorm(3, 0, 100),
       e3 = rnorm(3, 0, 100),
       e4 = rnorm(3, 0, 100)
  )
}

inits()

male.means.sim4 <- bugs(data, inits, model.file = "means.age2.txt",
                        parameters = c("b", "c1","c2","c3","c4","c5",
                                       "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                       'e2','e3','e4'),
                        n.chains = 1, n.iter =200000,n.burnin=140000,debug=T)



###MALE MEANS WITH TAUi


#observations
N <- 603                  #number of means
y <- male_means$mean      #obs
sigma2.y <- male_means$std_error^(2) #observed precision on each mean
#age variables
age <- male.means.age-40
age.k1 <- age
age.k1[(male.means.age<=40)]<-0
age.k2 <- age
age.k2[(male.means.age<=60)]<-0 
#covariates
prov <- cbind(1,male.means.dummy.prov[,-1])
pop <- cbind(1,male.means.dummy.pop[,-1])
t<-male_stdy_yr
year<- male_yr

data <- list("N","y","sigma2.y",'age','age.k1','age.k2','t','prov','pop','year')

inits <- function(){
  list(b = rnorm(3, 0, 100),
       c1 = rnorm(3, 0, 100),
       c2 = rnorm(3, 0, 100),
       c3 = rnorm(3, 0, 100),
       c4 = rnorm(3, 0, 100),
       c5 = rnorm(3, 0, 100),
       d1 = rnorm(3, 0, 100),
       d2 = rnorm(3, 0, 100),
       d3 = rnorm(3, 0, 100),
       d4 = rnorm(3, 0, 100),
       d5 = rnorm(3, 0, 100),
       d6 = rnorm(3, 0, 100),
       d7 = rnorm(3, 0, 100),
       d8 = rnorm(3, 0, 100),
       d9 = rnorm(3, 0, 100),
       e2 = rnorm(3, 0, 100),
       e3 = rnorm(3, 0, 100),
       e4 = rnorm(3, 0, 100),
       tau1 = rgamma(3,0.1,1),
       tau2 = rgamma(3,0.1,1),
       tau3 = rgamma(3,0.1,1),
       tau4 = rgamma(3,0.1,1),
       tau5 = rgamma(3,0.1,1)
  )
}

inits()

male.means.sim5 <- bugs(data, inits, model.file = "means.tau.txt",
                        parameters = c("b", "c1","c2","c3","c4","c5",
                                       "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                       'e2','e3','e4',
                                       'tau1','tau2','tau3','tau4','tau5'),
                        n.chains = 3, n.iter = 10000, n.burnin=6000,debug=T)

##FEMALE MEANS WITH AGE CATEGORY

N <- 672 #number of means
y <- female_means$mean #obs
sigma2.y <- female_means$std_error^2 #std error on each mean
agecat <- female.means.agecat
prov <- female.means.dummy.prov
pop <- cbind(1,female.means.dummy.pop[,-1])
t<-female_stdy_yr

data <- list("N","y","sigma2.y",'agecat','t','prov','pop')

inits <- function(){
  list(a = rnorm(1, 25, 5), b = rnorm(1, 0, 0.5),
       c1 = rnorm(1, 0, 2),
       c2 = rnorm(1, 0, 2),
       c3 = rnorm(1, 0, 2),
       c4 = rnorm(1, 0, 2),
       c5 = rnorm(1, 0, 2),
       c6 = rnorm(1, 0, 2),
       d1 = rnorm(1, 0, 2),
       d2 = rnorm(1, 0, 2),
       d3 = rnorm(1, 0, 2),
       d4 = rnorm(1, 0, 2),
       d5 = rnorm(1, 0, 2),
       d6 = rnorm(1, 0, 2),
       d7 = rnorm(1, 0, 2),
       d8 = rnorm(1, 0, 2),
       d9 = rnorm(1, 0, 2),
       e2 = rnorm(1, 0, 100),
       e3 = rnorm(1, 0, 100),
       e4 = rnorm(1, 0, 100)
  )
}

inits()

female.means.sim <- bugs(data, inits, model.file = "means.agecat.txt",
                       parameters = c("a", "b", "c1",'c2','c3','c4','c5','c6',
                                      "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                      "e2","e3","e4"),
                       n.chains = 1, n.iter = 200000,n.burnin=150000,debug=T)

##FEMALE MEANS WITH AGE CONTINUOUS

N <- 672                  #number of means
y <- female_means$mean      #obs
tau.y <- female_means$std_error^(-2) #observed precision on each mean
age <- female.means.age - 40
prov <- cbind(1,female.means.dummy.prov[,-1])
pop <- cbind(1,female.means.dummy.pop[,-1])
t<-female_stdy_yr

data <- list("N","y","tau.y",'age','t','prov','pop')

inits <- function(){
  list(b = 0.11,
       c = 0.1,
       d1 =25,
       d2 = rnorm(1, 0, 100),
       d3 = rnorm(1, 0, 100),
       d4 = rnorm(1, 0, 100),
       d5 = rnorm(1, 0, 100),
       d6 = rnorm(1, 0, 100),
       d7 = rnorm(1, 0, 100),
       d8 = rnorm(1, 0, 100),
       d9 = rnorm(1, 0, 100),
       e2 = rnorm(1, 0, 100),
       e3 = rnorm(1, 0, 100),
       e4 = rnorm(1, 0, 100)
  )
}

inits()

female.means.sim2 <- bugs(data, inits, model.file = "means.age.txt",
                        parameters = c("b", "c","d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                       'e2','e3','e4'),
                        n.chains = 1, n.iter = 100000,debug=T)


##FEMALE MEANS WITH AGE SPLINE AT 40 ONLY 

#observations
N <- 603                  #number of means
y <- male_means$mean      #obs
tau.y <- male_means$std_error^(-2) #observed precision on each mean
#age variables
age <- male.means.age-40
age.k1 <- age
age.k1[(male.means.age<=40)]<-0
age.k2 <- age
age.k2[(male.means.age<=60)]<-0 
#covariates
prov <- cbind(1,male.means.dummy.prov[,-1])
pop <- cbind(1,male.means.dummy.pop[,-1])
t<-male_stdy_yr

data <- list("N","y","tau.y",'age','age.k1','t','prov','pop')

inits <- function(){
  list(b = rnorm(3, 0, 100),
       c1 = rnorm(3, 0, 100),
       c2 = rnorm(3, 0, 100),
       c3 = rnorm(3, 0, 100),
       c4 = rnorm(3, 0, 100),
       d1 = rnorm(3, 0, 100),
       d2 = rnorm(3, 0, 100),
       d3 = rnorm(3, 0, 100),
       d4 = rnorm(3, 0, 100),
       d5 = rnorm(3, 0, 100),
       d6 = rnorm(3, 0, 100),
       d7 = rnorm(3, 0, 100),
       d8 = rnorm(3, 0, 100),
       d9 = rnorm(3, 0, 100),
       e2 = rnorm(3, 0, 100),
       e3 = rnorm(3, 0, 100),
       e4 = rnorm(3, 0, 100)
  )
}

inits()

male.means.sim3 <- bugs(data, inits, model.file = "means.age3.txt",
                        parameters = c("b", "c1","c2","c3","c4",
                                       "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                       'e2','e3','e4'),
                        n.chains = 1, n.iter = 100000,debug=T)

##FEMALE MEANS WITH AGE SPLINES AT K1=40 and K2=60

#observations
N <- 603                  #number of means
y <- male_means$mean      #obs
tau.y <- male_means$std_error^(-2) #observed precision on each mean
#age variables
age <- male.means.age-40
age.k1 <- age
age.k1[(male.means.age<=40)]<-0
age.k2 <- age
age.k2[(male.means.age<=60)]<-0 
#covariates
prov <- cbind(1,male.means.dummy.prov[,-1])
pop <- cbind(1,male.means.dummy.pop[,-1])
t<-male_stdy_yr

data <- list("N","y","tau.y",'age','age.k1','age.k2','t','prov','pop')

inits <- function(){
  list(b = rnorm(1, 0, 1),
       c1 = rnorm(1, 0, 100),
       c2 = rnorm(3, 0, 100),
       c3 = rnorm(3, 0, 100),
       c4 = rnorm(3, 0, 100),
       c5 = rnorm(3, 0, 100),
       d1 = rnorm(3, 0, 100),
       d2 = rnorm(3, 0, 100),
       d3 = rnorm(3, 0, 100),
       d4 = rnorm(3, 0, 100),
       d5 = rnorm(3, 0, 100),
       d6 = rnorm(3, 0, 100),
       d7 = rnorm(3, 0, 100),
       d8 = rnorm(3, 0, 100),
       d9 = rnorm(3, 0, 100),
       e2 = rnorm(3, 0, 100),
       e3 = rnorm(3, 0, 100),
       e4 = rnorm(3, 0, 100)
  )
}

inits()

male.means.sim4 <- bugs(data, inits, model.file = "means.age2.txt",
                        parameters = c("b", "c1","c2","c3","c4","c5",
                                       "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                       'e2','e3','e4'),
                        n.chains = 1, n.iter =200000,n.burnin=140000,debug=T)




###FEMALE MEANS WITH TAU

N <- 672                  #number of means
y <- female_means$mean      #obs
sigma2.y <- female_means$std_error^(2) #observed precision on each mean
#age variables
age <- female.means.age-40
age.k1 <- age
age.k1[(female.means.age<=40)]<-0
age.k2 <- age
age.k2[(female.means.age<=60)]<-0 
#covariates
prov <- cbind(1,female.means.dummy.prov[,-1])
pop <- cbind(1,female.means.dummy.pop[,-1])
t<-female_stdy_yr
year<- female_yr

data <- list("N","y","sigma2.y",'age','age.k1','age.k2','t','prov','pop','year')

inits <- function(){
  list(b = rnorm(1, 0, 100),
       c1 = 0.1,
       c2 = 0.015,
       c3 = 0.001,
       c4 = 0,
       c5 = 0,
       d1 = rnorm(1, 0, 100),
       d2 = rnorm(1, 0, 100),
       d3 = rnorm(1, 0, 100),
       d4 = rnorm(1, 0, 100),
       d5 = rnorm(1, 0, 100),
       d6 = rnorm(1, 0, 100),
       d7 = rnorm(1, 0, 100),
       d8 = rnorm(1, 0, 100),
       d9 = rnorm(1, 0, 100),
       e2 = rnorm(1, 0, 100),
       e3 = rnorm(1, 0, 100),
       e4 = rnorm(1, 0, 100),
       tau1 = 0.5,
       tau2 = 0.5,
       tau3 = 0.5,
       tau4 = 0.5,
       tau5 = 0.5
  )
}

inits()

female.means.sim3 <- bugs(data, inits, model.file = "means.tau.txt",
                          parameters = c("b", "c1","c2","c3","c4","c5",
                                         "d1",'d2','d3','d4','d5','d6','d7','d8','d9',
                                         'e2','e3','e4',
                                         'tau1','tau2','tau3','tau4','tau5'),
                          n.chains = 1, n.iter = 20000, n.burnin=12000,debug=T)


# --- Prepare factor variables for overweight analysis --- #

#Survey numbers
male_yr<-as.factor(male_overwght_sd$year)
male_yr<-cbind((male_yr==1998)*1,model.matrix(~male_yr)[,2:5])
female_yr<-as.factor(female_overwght_sd$year)
female_yr<-cbind((female_yr==1998)*1,model.matrix(~female_yr)[,2:5])

#Ages
male.overwght_sd.agecat<-cbind((male_overwght_sd$agecat=='15-24')*1,model.matrix(~male_overwght_sd$agecat)[,2:6])
male.overwght_sd.age=c()
for (i in 1:length(male_overwght_sd$agecat)){
  if(male_overwght_sd$agecat[i]=='15-24'){
    male.overwght_sd.age[i]=20
  }
  if(male_overwght_sd$agecat[i]=='25-34'){
    male.overwght_sd.age[i]=30
  }
  if(male_overwght_sd$agecat[i]=='35-44'){
    male.overwght_sd.age[i]=40
  }
  if(male_overwght_sd$agecat[i]=='45-54'){
    male.overwght_sd.age[i]=50
  }
  if(male_overwght_sd$agecat[i]=='55-64'){
    male.overwght_sd.age[i]=60
  }
  if(male_overwght_sd$agecat[i]=='65+'){
    male.overwght_sd.age[i]=70
  }
}
male.overwght_sd.age

female.overwght_sd.agecat<-cbind((female_overwght_sd$agecat=='15-24')*1,model.matrix(~female_overwght_sd$agecat)[,2:6])
female.overwght_sd.age=c()
for (i in 1:length(female_overwght_sd$agecat)){
  if(female_overwght_sd$agecat[i]=='15-24'){
    female.overwght_sd.age[i]=20
  }
  if(female_overwght_sd$agecat[i]=='25-34'){
    female.overwght_sd.age[i]=30
  }
  if(female_overwght_sd$agecat[i]=='35-44'){
    female.overwght_sd.age[i]=40
  }
  if(female_overwght_sd$agecat[i]=='45-54'){
    female.overwght_sd.age[i]=50
  }
  if(female_overwght_sd$agecat[i]=='55-64'){
    female.overwght_sd.age[i]=60
  }
  if(female_overwght_sd$agecat[i]=='65+'){
    female.overwght_sd.age[i]=70
  }
}
female.overwght_sd.age


#Provinces
prov.names<-list('Eastern Cape',  "Free State",'Gauteng', 'KwaZulu Natal',
                 'Limpopo', 'Mpumalanga', 'North West','Northern Cape', 'Western Cape')
prov.v.names<-c('Eastern Cape',  "Free State",'Gauteng', 'KwaZulu Natal',
                'Limpopo', 'Mpumalanga', 'North West','Northern Cape', 'Western Cape')

#MALE overwght_sd provinces
dummies.male.overwght_sd<- model.matrix(~male_overwght_sd$prov)
male.overwght_sd.dummy.prov <- cbind((male_overwght_sd$prov=='Eastern Cape')*1,dummies.male.overwght_sd[,2:9])
colnames(male.overwght_sd.dummy.prov)=prov.v.names
male.overwght_sd.dummy.prov[1,]

#FEMALE overwght_sd provinces
female.overwght_sd.dummy.prov <- cbind((female_overwght_sd$prov=='Eastern Cape')*1,model.matrix(~female_overwght_sd$prov)[,2:9])
colnames(female.overwght_sd.dummy.prov)<-prov.v.names
female.overwght_sd.dummy.prov[1,]

#Population Groups
pop.v.names <- c('African','Asian/Indian','Coloured','White')

#MALE overweight pop group
dummies.male.overwght_sd<- model.matrix(~male_overwght_sd$race)
male.overwght_sd.dummy.pop <- cbind((male_overwght_sd$race=='African')*1,dummies.male.overwght_sd[,2:4])
colnames(male.overwght_sd.dummy.pop)=pop.v.names
male.overwght_sd.dummy.pop[2,]

#FEMALE overweight pop group
dummies.female.overwght_sd<- model.matrix(~female_overwght_sd$race)
female.overwght_sd.dummy.pop <- cbind((female_overwght_sd$race=='African')*1,dummies.female.overwght_sd[,2:4])
colnames(female.overwght_sd.dummy.pop)=pop.v.names

#--- Linear Regression Analysis of Overweight --- #

######MALES#####

#logit(p)
logit.p<-log(male_overwght_sd$prop/(1-male_overwght_sd$prop))
#year
year<-male_overwght_sd$year-1998
year2<-year^2
#age and age spline
age=male.overwght_sd.age
age.k1=age
age.k1[(age<=40)]<-0
age2=(age)^2
age2.k1=age.k1^2
age3=(age)^3
age3.k1=age.k1^3
#bmi and bmi splines
bmi=male_overwght_sd[,'male_means$mean']
bmi.k1=bmi
bmi.k1[(bmi<=23)]<-0
bmi.k2=bmi
bmi.k2[(bmi<=28)]<-0
bmi2<-bmi^2
bmi2.k1<-bmi.k1^2
bmi2.k2<-bmi.k2^2
bmi3<-bmi^3
bmi3.k1<-bmi.k1^3
bmi3.k2<-bmi.k2^3

#Preliminary analysis of proportion, logit(p) and predictors

#Plot histograms (male and female)
par(mfrow=c(2,2))
par(oma=c(0,0,2,0))
#male
hist(male_overwght_sd$prop,main="",xlab="Males overweight",freq=F)
hist(logit.p,breaks=30,main='',xlab="logit (p)",freq=F)
#female
hist(female_overwght_sd$prop,main="",xlab="Females overweight",freq=F)
hist(log(female_overwght_sd$prop/(1-female_overwght_sd$prop)),
     ,freq=F,breaks=30,main='',xlab="logit (p)")
mtext("Histograms of Overweight Prevalences in all subpopulations",side=3,line=1,outer=T)
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))

#Covariates
par(mfrow=c(2,2))
#Plot bmi against proportions
plot(bmi,male_overwght_sd$prop,ylab='proportion overweight')
#Plot for age categories
plot(male_overwght_sd$agecat,male_overwght_sd$prop,xlab='age category')
#Plot logit(p) against others
plot(bmi,logit.p,ylab='logit (p)')
plot(male_overwght_sd$agecat,logit.p,xlab='age category')
par(mfrow=c(1,1))

##Both age and BMI have a curve relationship
##Apply the equations from webtable 4 with splines at appropriate places

m.overwght <- lm(logit.p~bmi+bmi2+bmi3+bmi3.k1+bmi3.k2+race+age+age2+age3+age3.k1+prov+year,data=male_overwght_sd)
summary(m.overwght)
anova(m.overwght)

m.overwght2 <- lm(logit.p~bmi+bmi2+bmi2.k1+bmi2.k2+race+age+age2+age2.k1+prov+year,data=male_overwght_sd)
summary(m.overwght2)
anova(m.overwght2)

m.overwght3 <- lm(logit.p~bmi+bmi2+race+age+age2+prov+year,data=male_overwght_sd)
summary(m.overwght3)
anova(m.overwght3)

par(mfrow=c(2,2))
#predicted vs fitted
plot(m.overwght3$fitted,logit.p,xlab="Predicted",ylab="Observed",main="A")
abline(a=0,b=1,lty=2,lwd=1.5)

#residual analysis
plot(m.overwght$fitted,m.overwght$resid,xlab="Fitted values",ylab="Residuals",main="B")
abline(h=c(1.96,-1.96),lty=2)
hist(m.overwght3$resid,breaks=40,freq=F,main="C",xlab='Residuals')
qqPlot(m.overwght3,main="D",ylab="Studentised Residuals")
par(mfrow=c(1,1))

#identify the outliers
male_overwght_sd[which(abs(m.overwght3$resid)>2),]

###FEMALES

#logit(p)
logit.p<-log(female_overwght_sd$prop/(1-female_overwght_sd$prop))
#year
year<-female_overwght_sd$year-1998
#age and age spline
age=female.overwght_sd.age
age.k1=age
age.k1[(age<=50)]<-0
age2=(age)^2
age2.k1=age.k1^2
age3=(age)^3
age3.k1=age.k1^3
#bmi and bmi splines
bmi=female_overwght_sd[,'female_means$mean']
bmi.k1=bmi
bmi.k1[(bmi<=23)]<-0
bmi.k2=bmi
bmi.k2[(bmi<=28)]<-0
bmi2<-bmi^2
bmi2.k1<-bmi.k1^2
bmi2.k2<-bmi.k2^2
bmi3<-bmi^3
bmi3.k1<-bmi.k1^3
bmi3.k2<-bmi.k2^3

#Preliminary analysis of proportion, logit(p) and predictors

#Covariates
par(mfrow=c(2,2))
#Plot bmi against proportions
plot(bmi,female_overwght_sd$prop,ylab='proportion overweight')
#Plot for age categories
plot(female_overwght_sd$agecat,female_overwght_sd$prop,xlab='age category')
#Plot logit(p) against others
plot(bmi,logit.p,ylab='logit (p)')
plot(female_overwght_sd$agecat,logit.p,xlab='age category')
par(mfrow=c(1,1))

##Both age and BMI have a curve relationship, almost log related.
##Apply the equations from webtable 4

f.overwght <- lm(logit.p~bmi+bmi2+bmi3+bmi3.k1+bmi3.k2+race+age+age2+age3+age3.k1+prov+year,data=female_overwght_sd)
anova(f.overwght)
summary(f.overwght)

f.overwght2 <- lm(logit.p~bmi+bmi2+bmi2.k1+bmi2.k2+race+age+age2+age2.k1+prov+year,data=female_overwght_sd)
summary(f.overwght2)
anova(f.overwght2)

#NOTE YEAR COEFFICIENT IS SENSITIVE TO THE SPLINES!!!

f.overwght3 <- lm(logit.p~bmi+bmi2+age+age2+year,data=female_overwght_sd)
summary(f.overwght3)
anova(f.overwght3,f.overwght)

f.overwght4 <- lm(logit.p~race+age+age2+prov+year,data=female_overwght_sd)
summary(f.overwght3)
anova(f.overwght3,f.overwght)

par(mfrow=c(2,2))
#predicted vs fitted
plot(f.overwght$fitted,logit.p,xlab="Predicted",ylab="Observed",main="A")
abline(a=0,b=1,lty=2,lwd=1.5)

#residual analysis
plot(f.overwght$fitted,f.overwght$resid,xlab="Fitted values",ylab="Residuals",main="B")
abline(h=c(1.96,-1.96),lty=2)
hist(f.overwght$resid,breaks=40,freq=F,main="C",xlab='Residuals')
qqPlot(f.overwght,main="D",ylab="Studentised Residuals")
par(mfrow=c(1,1))

#identify the outliers
female_overwght_sd[which(abs(f.overwght$resid)>2),]

# --- Prepare factor variables for obesity analysis --- #

#Survey numbers
male_yr<-as.factor(male_obese_sd$year)
male_yr<-cbind((male_yr==1998)*1,model.matrix(~male_yr)[,2:5])
female_yr<-as.factor(female_obese_sd$year)
female_yr<-cbind((female_yr==1998)*1,model.matrix(~female_yr)[,2:5])

#Ages
male.obese_sd.agecat<-cbind((male_obese_sd$agecat=='15-24')*1,model.matrix(~male_obese_sd$agecat)[,2:6])
male.obese_sd.age=c()
for (i in 1:length(male_obese_sd$agecat)){
  if(male_obese_sd$agecat[i]=='15-24'){
    male.obese_sd.age[i]=20
  }
  if(male_obese_sd$agecat[i]=='25-34'){
    male.obese_sd.age[i]=30
  }
  if(male_obese_sd$agecat[i]=='35-44'){
    male.obese_sd.age[i]=40
  }
  if(male_obese_sd$agecat[i]=='45-54'){
    male.obese_sd.age[i]=50
  }
  if(male_obese_sd$agecat[i]=='55-64'){
    male.obese_sd.age[i]=60
  }
  if(male_obese_sd$agecat[i]=='65+'){
    male.obese_sd.age[i]=70
  }
}
male.obese_sd.age

female.obese_sd.agecat<-cbind((female_obese_sd$agecat=='15-24')*1,model.matrix(~female_obese_sd$agecat)[,2:6])
female.obese_sd.age=c()
for (i in 1:length(female_obese_sd$agecat)){
  if(female_obese_sd$agecat[i]=='15-24'){
    female.obese_sd.age[i]=20
  }
  if(female_obese_sd$agecat[i]=='25-34'){
    female.obese_sd.age[i]=30
  }
  if(female_obese_sd$agecat[i]=='35-44'){
    female.obese_sd.age[i]=40
  }
  if(female_obese_sd$agecat[i]=='45-54'){
    female.obese_sd.age[i]=50
  }
  if(female_obese_sd$agecat[i]=='55-64'){
    female.obese_sd.age[i]=60
  }
  if(female_obese_sd$agecat[i]=='65+'){
    female.obese_sd.age[i]=70
  }
}
female.obese_sd.age


#Provinces
prov.names<-list('Eastern Cape',  "Free State",'Gauteng', 'KwaZulu Natal',
                 'Limpopo', 'Mpumalanga', 'North West','Northern Cape', 'Western Cape')
prov.v.names<-c('Eastern Cape',  "Free State",'Gauteng', 'KwaZulu Natal',
                'Limpopo', 'Mpumalanga', 'North West','Northern Cape', 'Western Cape')

#MALE obese_sd provinces
dummies.male.obese_sd<- model.matrix(~male_obese_sd$prov)
male.obese_sd.dummy.prov <- cbind((male_obese_sd$prov=='Eastern Cape')*1,dummies.male.obese_sd[,2:9])
colnames(male.obese_sd.dummy.prov)=prov.v.names
male.obese_sd.dummy.prov[1,]

#FEMALE obese_sd provinces
female.obese_sd.dummy.prov <- cbind((female_obese_sd$prov=='Eastern Cape')*1,model.matrix(~female_obese_sd$prov)[,2:9])
colnames(female.obese_sd.dummy.prov)<-prov.v.names
female.obese_sd.dummy.prov[1,]

#Population Groups
pop.v.names <- c('African','Asian/Indian','Coloured','White')

#MALE overweight pop group
dummies.male.obese_sd<- model.matrix(~male_obese_sd$race)
male.obese_sd.dummy.pop <- cbind((male_obese_sd$race=='African')*1,dummies.male.obese_sd[,2:4])
colnames(male.obese_sd.dummy.pop)=pop.v.names
male.obese_sd.dummy.pop[2,]

#FEMALE overweight pop group
dummies.female.obese_sd<- model.matrix(~female_obese_sd$race)
female.obese_sd.dummy.pop <- cbind((female_obese_sd$race=='African')*1,dummies.female.obese_sd[,2:4])
colnames(female.obese_sd.dummy.pop)=pop.v.names

#--- Linear Regression Analysis of Obesity --- #

######MALES#####

#logit(p)
logit.p<-log(male_obese_sd$prop/(1-male_obese_sd$prop))
#year
year<-male_obese_sd$year-1998
#age and age spline
age=male.obese_sd.age
age.k1=age
age.k1[(age<=40)]<-0
age2=(age)^2
age2.k1=age.k1^2
age3=(age)^3
age3.k1=age.k1^3
#bmi and bmi splines
bmi=male_obese_sd[,'male_means$mean']
bmi.k1=bmi
bmi.k1[(bmi<=23)]<-0
bmi.k2=bmi
bmi.k2[(bmi<=28)]<-0
bmi2<-bmi^2
bmi2.k1<-bmi.k1^2
bmi2.k2<-bmi.k2^2
bmi3<-bmi^3
bmi3.k1<-bmi.k1^3
bmi3.k2<-bmi.k2^3

#Preliminary analysis of proportion, logit(p) and predictors

#Plot histograms
par(mfrow=c(2,2))
par(oma=c(0,0,2,0))
#male
hist(male_obese_sd$prop,main="",xlab="Males obese",freq=F)
hist(logit.p,breaks=30,main='',xlab="logit (p)",freq=F)
#female
hist(female_obese_sd$prop,main="",xlab="Females obese",freq=F)
hist(log(female_obese_sd$prop/(1-female_obese_sd$prop)),
     ,freq=F,breaks=30,main='',xlab="logit (p)")
mtext("Histograms of Obesity Prevalences in all subpopulations",side=3,line=1,outer=T)
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))

#Covariates
par(mfrow=c(2,2))
#Plot bmi against proportions
plot(bmi,male_obese_sd$prop,ylab='proportion obese')
#Plot for age categories
plot(male_obese_sd$agecat,male_obese_sd$prop,xlab='age category')
#Plot logit(p) against others
plot(bmi,logit.p,ylab='logit (p)')
plot(male_obese_sd$agecat,logit.p,xlab='age category')
par(mfrow=c(1,1))


##Apply the equations from webtable 4

m.obese <- lm(logit.p~bmi+bmi2+bmi3+bmi3.k1+bmi3.k2+race+age+age2+age3+age3.k1+prov+year,data=male_obese_sd)
anova(m.obese)
summary(m.obese)

m.obese2 <- lm(logit.p~bmi+bmi2+bmi2.k1+bmi2.k2+race+age+age2+age2.k1+prov+year,data=male_obese_sd)
summary(m.obese2)
anova(m.obese2)

m.obese3 <- lm(logit.p~bmi+bmi2+race+age+age2+prov+year,data=male_obese_sd)
summary(m.obese3)
anova(m.obese3,m.obese)

#Residual analysis
par(mfrow=c(2,2))
#predicted vs fitted
plot(m.obese3$fitted,logit.p,xlab="Predicted",ylab="Observed",main="A")
abline(a=0,b=1,lty=2,lwd=1.5)
#residual analysis
plot(m.obese3$fitted,m.obese3$resid,xlab="Fitted values",ylab="Residuals",main="B")
abline(h=c(1.96,-1.96),lty=2)
hist(m.obese3$resid,breaks=40,freq=F,main="C",xlab='Residuals')
qqPlot(m.obese3,main="D",ylab="Studentised Residuals")
par(mfrow=c(1,1))
#identify the outliers
male_obese_sd[which(abs(m.obese3$resid)>2),]

###FEMALES

#logit(p)
logit.p<-log(female_obese_sd$prop/(1-female_obese_sd$prop))
#year
year<-female_obese_sd$year-1998
#age and age spline
age=female.obese_sd.age
age.k1=age
age.k1[(age<=40)]<-0
age2=(age)^2
age2.k1=age.k1^2
age3=(age)^3
age3.k1=age.k1^3
#bmi and bmi splines
bmi=female_obese_sd[,'female_means$mean']
bmi.k1=bmi
bmi.k1[(bmi<=23)]<-0
bmi.k2=bmi
bmi.k2[(bmi<=28)]<-0
bmi2<-bmi^2
bmi2.k1<-bmi.k1^2
bmi2.k2<-bmi.k2^2
bmi3<-bmi^3
bmi3.k1<-bmi.k1^3
bmi3.k2<-bmi.k2^3

#Preliminary analysis of proportion, logit(p) and predictors

#Covariates
par(mfrow=c(2,2))
#Plot bmi against proportions
plot(bmi,female_obese_sd$prop,ylab='proportion obese')
#Plot for age categories
plot(female_obese_sd$agecat,female_obese_sd$prop,xlab='age category')
#Plot logit(p) against others
plot(bmi,logit.p,ylab='logit (p)')
plot(female_obese_sd$agecat,logit.p,xlab='age category')
par(mfrow=c(1,1))

##Both age and BMI have a curve relationship, almost log related.
##Apply the equations from webtable 4

f.obese <- lm(logit.p~bmi+bmi2+bmi3+bmi3.k1+bmi3.k2+race+age+age2+age3+age3.k1+prov+year,data=female_obese_sd)
anova(f.obese)
summary(f.obese)

f.obese2 <- lm(logit.p~bmi+bmi2+bmi2.k1+bmi2.k2+race+age+age2+age2.k1+prov+year,data=female_obese_sd)
summary(f.obese2)
anova(f.obese2)

f.obese3 <- lm(logit.p~bmi+bmi2+race+age+age2+prov+year,data=female_obese_sd)
summary(f.obese3)
anova(f.obese3,f.obese)

#Residual analysis
par(mfrow=c(2,2))
#predicted vs fitted
plot(f.obese3$fitted,logit.p,xlab="Predicted",ylab="Observed",main="A")
abline(a=0,b=1,lty=2,lwd=1.5)
#residual analysis
plot(f.obese3$fitted,f.obese3$resid,xlab="Fitted values",ylab="Residuals",main="B")
abline(h=c(1.96,-1.96),lty=2)
hist(f.obese3$resid,breaks=40,freq=F,main="C",xlab='Residuals')
qqPlot(f.obese3,main="D",ylab="Studentised Residuals")
par(mfrow=c(1,1))

#identify the outliers
female_obese_sd[which(abs(f.obese3$resid)>2),]

