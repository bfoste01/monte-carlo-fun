#----------------Structural Model--------------#
# Monte Carlo (MC) Study with a Simple CFA w/no missing data
# The idea with this MC study is to specify a theoretical model,
# by this I mean you input the hypothesized parameter estimates
# or estimates from a model you have already run.
# The Monte Carlo aspect means to examine the variability
# in the parameter estimates and fit statistics. 
# MC studies are an important element to establish
# power in a study. 
#-----MC Study of Uncertainty in Parameter Estimates-----#
# You will need two packages installed (lavaan & simsem),
# so install.packages("lavaan") or install.packages("simsem")
# if needed. 
# load simsem (lavaan is a dependency and will load
# automatically if it is installed)
library(simsem)
# set up your CFA
# here we have two factors (factor covariances)
# are estimated by default. 
analyze.model <- '
    f1 =~ y1 + y2 + y3
    f2 =~ y4 + y5 + y6
    f3 =~ y7 + y8 + y9
'
# popModel is specifying a model with hypothetical 
# parameters estimates - 
# It's good to base this off past research and be modest if you
# don't know what these values might be. Perhaps of more interest 
# is to use estimates from a model you have run to examine the 
# uncertainty in parameter estimates. 
# see how every parameter in the analyze.model is pre-
# multiplied by a hypothesized value? 
# Also note that the f1 ~~ 1*f1 command is specified to
# properly identify and set the scale of the factor
# by setting the latent variances to 1.
# I could have just as easily pre-multiplied the latent
# variances by a meaningful value, but I would have had to
# pre-multiply a factor loading by 1.0 to properly identify factor. 
pop.model <- '
    f1 =~ 0.7*y1 + 0.6*y2 + 0.7*y3
    f2 =~ 0.67*y4 + 1.1*y5 + 0.9*y6
    f3 =~ 0.86*y7 + 1.2*y8 + 1.1*y9
    f1 ~~ 1*f1
    f2 ~~ 1*f2
    f3 ~~ 1*f3
    f1 ~~ 0.4*f2
    f1 ~~ 0.2*f3
    f2 ~~ 0.3*f3
    y1 ~~ 0.5*y1
    y2 ~~ 1.1*y2
    y3 ~~ 0.8*y3
    y4 ~~ 0.4*y4
    y5 ~~ 0.4*y5
    y6 ~~ 0.8*y6
    y7 ~~ 0.8*y7
    y8 ~~ 0.5*y8
    y9 ~~ 0.6*y9
'
# Next we are going to create some simulated data to 
# examine uncertainties in our parameter estimates,
# and fit statistics. 
# What this command says is to create 1000 datasets
# with a sample size of 200. Generate this data from
# the hypothesized population model above. 
# fit the lavaan CFA function to all the datasets. 
# Don't fix the factor variances to 1, because we did 
# that in the code above already, and we don't' want to
# confuse the estimation process. 
# If you have multiple processors the multicore
# command speeds things up quite a bit, so it is
# set to TRUE here. 
# finally, you should specify a seed = 'value' if 
# you need results to replicate exactly. 
output <- sim(1000, analyze.model, n = 250,
    generate=pop.model, lavaanfun = "cfa", 
    std.lv = FALSE, multicore = TRUE)
# See the summary of the average parameter estimates over 
# the 1000 samples. 
# This helps you get an idea of the variability in the
# data. 
summary(output)
# it can also be informative to look at the sampling 
# distribution of the fit statistics
plotCutoff(output)
# distribution of the fit statistics with alpha (p) drawn
# play around with the alpha in these graphs
plotCutoff(output, .001)
plotCutoff(output, .01)
plotCutoff(output, .05)
#---------------------Power------------------------#
# In the above code we examined the variability of
# a hypothesized model. We could have done the same 
# thing with a model that we estimated, which is always
# good practice. 
# Next the goal is to examine the power to detect the 
# hypothesized estimates at various sample sizes. 
# All of the code above stays the same, except for the
# 'sim' command. 
# Notice how NULL is specified as well as a range of 
# sample sizes between 50:1000. 
output.2 <- sim(NULL, n=50:1000, analyze.model,
    generate=pop.model, lavaanfun = "cfa", 
    std.lv = FALSE, multicore = TRUE)
# Now we can look at power. 
cpower <- getPower(output)
# Here are the two questions that are usually of interest
# in grant proposals, etc. 
# 1. What kind of power do we have at a sample = 200?
cpower2 <- getPower(output, nVal = 200)
# 2. How big a sample do we need to have a power of 0.8)
# Results show on a per parameter basis. NA means you don't
# have any power at the designated sample size. INF means your
# sample size is overkill. 
findPower(cpower, "N", 0.80)
# Because the outputs can be so large it is sometimes nice
# to isolate paramters of interest, which are drawn from
# pop.model, and plot power curves.
plotPower(output.2, powerParam=c("f1~~f1", "f1~~f3"))
#--------------------Concluding Thoughts------------------------#
# This was an ideal situation (no missing data), and depending 
# on your estimation technique, you might be dropping data until 
# you have a complete dataset. 
# You can model uncertainty due to missingness with this method. 
# That Gist is forthcoming. 
# Full SEM models are just as easy with this script, just specify
# the regression relationships (e.g., f1 ~ f2) in the lavaan syntax,
# hypothesize a parameter estimate, and run the same script. 