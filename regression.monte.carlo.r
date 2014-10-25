
# Here's how you do a Monte Carlo Study for Sample Sizes in R
# set on the population values 
# All theory based
pop.model<-' y ~ 0.66*x1 + 0.05*x2 + -0.30*x3 
x1~~0.40*x2 + 0.60*x3 
x2~~0.05*x3 
y ~~ 0.69*y
'
#
library(lavaan)
pop.fit<-sem(pop.model, fixed.x = FALSE)
summary(pop.fit, standardized = TRUE, rsquare = TRUE)
# find the model implied covariance matrix
pop.cov <- fitted(pop.fit)$cov
# find the correlations from covariance
library(MBESS)
cov2cor(pop.cov)
# setup for MC
library(simsem)
# create model bones to simulate
analysis.model <- 'y ~ x1 + x2 + x3'
# simulate some data
# Here's what's going on
# nRep = no. samples to create
# model = model specified above
# n = sample size for each generated sample
# generate = the data generating model 
# specified originally
# lavaanfun = the lavaan function to call on 
# for analysis
# seed = random seed set so others can replicate
# you pick whatever value
# multicore = if you have more than 1 processor 
# this will speed things up quite a bit
analysis.sim <- sim(nRep = 500, 
	model=analysis.model, n = 237,
	generate = pop.model, lavaanfun = "sem",
	seed = 565, multicore = TRUE)
# let's see what happened
# what are the average results acrsoss all 500 samples?
# set p-vale to .05
summaryParam(analysis.sim, detail = TRUE, alpha = .05)
# What if we want a range of samples? 
# here we put a rep and seq command inside the n 
# this estimates a range of samples 200-400
# by increments of 25, and does this whole thing for 
# 50 samples.
analysis.n <- sim(nRep = NULL, 
	model=analysis.model, n = rep(seq(200,400,25),50),
	generate = pop.model, lavaanfun = "sem",
	seed = 565, multicore = TRUE)
summaryParam(analysis.n, detail = TRUE, alpha = .05)
# what you really want to understand better is the
# power to detect the parameter of interest at the
# specified alpha level. 
# the plotPower() command will do just this
plotPower(analysis.n, powerParam = "y~x2", alpha = 0.05)
plotCIwidth(analysis.n, c("y~x2"), assurance = 0.95)
# find the sample size for a specified power level
power.n <- getPower(analysis.n, alpha=0.05, nVal=200:300)
findPower(power.n, iv="N", power = 0.80)