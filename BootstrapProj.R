# We will use the bootstrap technique to generate confidence intervals

# 1. Suppose we have a sample of data from an exponential distribution 
# with parameter lambda. In this case use lambda.hat = 1/mean(X). 

# As the number of observations increases, does the estimate for lambda 
# become roughly normally distributed? We will answer this question in
# the following parts.

# 1a. (1) Generate 100 observations of test data, with lambda=3. Remember
# to set your seed before carrying out any computations.
set.seed(0)
lambda = 3
exp_n = 100
exp_sample <- rexp(n = exp_n, rate = lambda)
exp_sample

# 1b. (1) What is the mean of your test data? (give the code and the value)

exp_mean <- mean(exp_sample)
exp_mean

# 1c. (1) What is your estimate lambda.hat? (give the code and the value)

lambda.hat <- 1/exp_mean 
lambda.hat

# 2. Now use the bootstrap to estimate the distribution of 
# lambda.hat and create bootstrap confidence intervals for lambda, 
# rather than the approach in 1).

# 2a. (1) Form a set of bootstrap estimates of our parameter by generating B
# random samples as you did once in 1a but use lambda.hat since we do not
# know the true lambda in this case (keep n=100). Set B=1000, and again set
# your seed.

set.seed(0)
B = 1000
exp_sample2 <- replicate(n = B, rexp(n = exp_n, rate = lambda.hat))
exp_sample2

# 2b. (1) Get a new estimate for lambda.hat from each of the bootstrap samples
# in 2a. You'll want to create a matrix to receive each value. You should 
# have 1000 estimates for lambda.hat now.

lambda.hat2 <- matrix(1/(apply(exp_sample2, 2, mean)))
lambda.hat2

# 2c. (2) Now look at the sampling distribution for lambda.hat, using the hist
# function. Remember the graphing techniques discussed in class and use them 
# to make the plot look professional. Does the distribution look normal?

hist(lambda.hat2, breaks = 20)

# The distribution appears to be normal.

# 2d. (1) Calculate an estimate of the standard error of lambda.hat using your
# collection of bootstrap estimated parameters. What is your 95% confidence interval?

se <- sd(lambda.hat2)/sqrt(length(lambda.hat2))
m_lambda <- mean(lambda.hat2)
ci_up_limit <- m_lambda + 1.96*se
ci_low_limit <- m_lambda - 1.96*se

# Confidence Interval is 2.9261 - 2.9439


# 3a. (5) We made some decisions when we used the bootstrap above that we can now question. 
# Repeat the above creation of a confidence interval for a range of values of data
# (we had our sample size fixed at 100) and a range of bootstrap values (we had B 
# fixed at 1000). Suppose the sample size varies (100, 200, 300, .... , 1000) and 
# B varies (1000, 2000, ... , 10000). You will likely find it useful to write
# functions to carry out these calculations. Your final output should be 
# upper and lower pairs for the confidence intervals produced using the bootstrap
# method for each value of sample size and B.

# generalize 2b into a function, and vary inputs of sample size and B as we did above.

boot.sample <- function(sample.size, B){
  set.seed(0)
  lambda = 3
  exp_n = 100
  exp_sample <- rexp(n = exp_n, rate = lambda)
  exp_mean <- mean(exp_sample)
  lambda.hat <- 1/exp_mean
  high <- matrix(nrow=length(sample.size),ncol=length(B))
  low <- matrix(nrow=length(sample.size), ncol=length(B))
  sample_lambda_mean <- matrix(nrow = length(sample.size), ncol = length(B))
  for(i in 1:length(sample.size)){
    for(j in 1:length(B)){
      set.seed(0)
      sample_exp <- replicate(B[j], rexp(sample.size[i], rate=lambda.hat))
      sample_lambda <- 1/apply(sample_exp, 2, mean)
      sample_se <- sd(sample_lambda)/sqrt(length(sample_lambda))
      sample_lambda_mean[i,j] <- mean(sample_lambda)
      high[i, j] <- sample_lambda_mean[i,j] + 1.96*sample_se
      low[i, j] <- sample_lambda_mean[i,j] - 1.96*sample_se
    } 
  }
  ci <- list(sample_mean = sample_lambda_mean, ci_high = high, ci_low = low)
  return(ci)
}



# 3b. (2) Plot your CI limits to show the effect of changing the sample size and 
# changing the number of bootstrap replications. What do you conclude?

x <- c(seq(from=1000, to=10000, by=1000))
n <- c(seq(from=1000, to=10000, by=100))

# keeping sample.size constant with value = 100

c <- boot.sample( sample.size =  1000, B = x)
errbar(x, c$sample_mean, yplus = c$ci_high, yminus = c$ci_low, main = "Confidence Limits for sample size = 100 as B varies", xlab = "Bootstrap values", ylab = "Confidence Limits")

#keeping B constant with value = 1000

c <- boot.sample( sample.size =  n, B = 1000)
errbar(n, c$sample_mean, yplus = c$ci_high, yminus = c$ci_low, main = "Confidence Limits for B = 1000 as sample size varies", xlab = "Sample size values", ylab = "Confidence Limits")

# CONCLUSION
# As the value of bootstrap and sample size increases the value of standard error decreases.

