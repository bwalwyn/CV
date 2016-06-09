#Ben Walwyn
#27 March 2014

#Input data
age <- c(1,1.5,1.5,1.5,2.5,4,5,5,7,8,8.5,9,9.5,9.5,10,12,12,13,13,14.5,15.5,15.5,16.5,17,22.5,29,31.5)
length <- c(1.8,1.85,1.87,1.77,2.02,2.27,2.15,2.26,2.47,2.19,2.26,2.4,2.39,2.41,2.5,2.32,2.32,2.43,2.47,2.56,2.65,2.47,2.64,2.56,2.7,2.72,2.57)

#Simulation inputs
start_val <- c(1,1,1,0.9)
nsim=11000

#Non-informative priorss
tau_alpha <- 0.00001
mu_alpha <- 0
tau_beta <- 0.00001
mu_beta <- 0
a <- 0.00001
b <- 0.00001

#Conditonal distributions and Gibbs sampling functions
alpha_sample <- function(y,x,beta,gam,tau,tau_alpha,mu_alpha){
  var <- 1/(length(y)*tau+tau_alpha)
  mean <- (tau*sum(y+beta*(gam^x)) + tau_alpha*mu_alpha)*var
  rnorm(1,mean,sqrt(var))
}

beta_sample <- function(y,x,alpha,gam,tau,tau_beta,mu_beta){
  var <- 1/(tau*sum(gam^(2*x))+tau_beta)
  mean <- (tau*sum((alpha)*gam^x)-tau*sum((y)*gam^x)+tau_beta*mu_beta)*var
  rnorm(1,mean,sqrt(var))
}

tau_sample <- function(y,x,alpha,beta,gam){
  theta <- a+length(x)/2
  lambda <- b+0.5*sum((y-(alpha-beta*gam^x))^2)
  rgamma(1,theta,rate=lambda)
}

#Transformed g conditional posterior distribution
g_cond <- function(y,x,alpha,beta,tau,g){
  return(exp(-(tau/2)*sum((y-(alpha-beta*(exp(g)/(1+exp(g)))^x))^2))*exp(g)/((exp(g)+1)^2))
}

#Metropolis hastings algorithm function
M_H <- function(y,x,start_val,nsim){
  samples <- matrix(0,nsim,4)
  samples[1,] <- start_val
  colnames(samples) <- c('alpha','beta','tau','gam')
  
  #Loop through simulations
  for (i in 2:nsim){
    n <- length(x)
    samples[i,'alpha'] <- alpha_i <- alpha_sample(y,x,samples[i-1,'beta'],samples[i-1,'gam'],samples[i-1,'tau'],tau_alpha,mu_alpha)
    samples[i,'beta'] <- beta_i <- beta_sample(y,x,samples[i,'alpha'],samples[i-1,'gam'],samples[i-1,'tau'],tau_beta,mu_beta)
    samples[i,'tau'] <- tau_i <- tau_sample(y,x,samples[i,'alpha'],samples[i,'beta'],samples[i-1,'gam']) 
    
    g_old <- log(samples[i-1,'gam']/(1-samples[i-1,'gam']))
    g_new <- rnorm(1,g_old,1)
    acc_prob <- min(1,(g_cond(y,x,alpha_i,beta_i,tau_i,g_new)/g_cond(y,x,alpha_i,beta_i,tau_i,as.numeric(g_old))))
    #Acceptance decision
    if (acc_prob >= runif(1)){
      samples[i,'gam'] <- exp(g_new)/(1+exp(g_new))
    }  
    else {samples[i,'gam'] <- samples[i-1,'gam']}
  }
  return(samples)
}

samp <- M_H(length,age,start_val,nsim)[1001:nsim,]

#estimates
alpha_est<-mean(samp[,'alpha'])
beta_est<-mean(samp[,'beta'])
sigma_sq_est<-mean(1/samp[,'tau'])
sigma_sq_est
gamma_est<-mean(samp[,'gam'])
alpha_est
beta_est
gamma_est
tau_est

#standard deviations
alpha_sd<-sd(samp[,'alpha'])
beta_sd<-sd(samp[,'beta'])
tau_sd<-sd(1/samp[,'tau'])
gamma_sd<-sd(samp[,'gam'])
alpha_sd
beta_sd
tau_sd
gamma_sd


fit<-function(alpha,beta,gam,x){
  alpha - beta*gam^x
}
fitted <- fit(alpha_est,beta_est,gamma_est,age)

pdf("myfile.pdf", height=6,width=6)
plot(age,length,main="Plot of Dagong data and fitted model",ylab="Recorded Length",xlab="Age")
lines(age,fitted,type='l')
dev.off()



