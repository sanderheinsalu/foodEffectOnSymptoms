setwd("~/Documents/yourDirectory")
library(MASS)
library(readODS)
data <- read_ods("data.ods",col_names = TRUE, row_names = FALSE,strings_as_factors = FALSE)
clnms <- colnames(data)
for (col in clnms){
  data[,col] <- as.numeric(data[,col]) #NAs introduced by coercion
}
data <- data[!is.na(rowSums(data)), ]  # truncate to nonempty rows
xrows <- length(data[,1])
datam <- as.matrix(data)
# Specify the inputs (independent variables) for which lags are included in the model. Use the column names, assuming these are the top row in the data file.
# ! means exclusion.
xdf <- datam[,! clnms %in% c("time2","time3","time4","time1","output1","output2","output3"),drop=F]
xdf <- xdf[,colSums(xdf)>7] # drop columns with few observations.
output1 <- datam[,"output1",drop=F]
output2 <- datam[,"output2",drop=F]
output3 <- datam[,"output3",drop=F]
timedummies <- datam[,clnms %in% c("time2","time3","time4"),drop=F]
lags <- 2L
xdflags <- matrix(data=NA, nrow=nrow(xdf), ncol=(lags+1)*ncol(xdf))
xdflagsnames <- rep(colnames(xdf),lags)

# Lag one of the output variables to use as an input for the other output variables. 
output3lags <- output3
output3lagsnames <- colnames(output3)
k <- 1
for (j in colnames(xdf)){
  xtemp <- xdf[,j]
  xdflags[,k] <- xdf[,j]
  xdflagsnames[k] <- j
  k <- k+1
  for (i in 1:lags){ # interleave/riffle xdflags so lags of the same variable are in adjacent columns.
    xdflags[,k] <- xts::lag.xts(xdf[,j], -i)
    xdflagsnames[k] <- paste0(j,"-",i)
    k <- k+1
  }
}
for (i in 1:lags){
  output3lags <- cbind(output3lags,xts::lag.xts(output3,-i))
  output3lagsnames <- cbind(output3lagsnames, paste0(colnames(output3),'-',i))
}
colnames(xdflags) <- xdflagsnames
colnames(output3lags) <- output3lagsnames
xdflags <- xdflags[1:(xrows-lags),]
output3lags <- output3lags[1:(xrows-lags),]

output1lag <- xts::lag.xts(output1,0)[lags:(xrows-1),]
output2lag <- xts::lag.xts(output2,0)[lags:(xrows-1),]
# Need to shorten the vector of observations of an output variable from the start and an input variable from the end to include lags in the model.
output1y <- output1[(lags+1):xrows]
output2y <- output2[(lags+1):xrows]
# Time dummy observations are shortened from the beginning like output variables, despite being inputs.
timedummies <- timedummies[(lags+1):xrows,]

# Make the matrix of input variables and their lags:
x <- cbind(output2lag ,output1y ,output1lag ,output3lags ,xdflags ,timedummies)

# Turn the output variable to use in the ordered logistic regression into a factor.
#output2y <- output2y[!output2y %in% c(0.8,0.9)] # If necessary, eliminate some factor levels with too few observations (remove # from the start of this line and the next). To keep all levels of the factor, put # at the start of this line and the next.
#x <- x[!output2y %in% c(0.8,0.9) ,]
output2f <- factor(output2y)
coeff0 <- rep(0, length(colnames(x))) # starting coefficients for the numerical search for the coefficients.
zeta0 <- 1:nlevels(output2f) # starting intercepts for the class boundaries.
start <- c(coeff0,zeta0) # combine the starting coefficients and intercepts.

# Check for NA observations in the matrix of input variables and replace these with zeroes.
any(is.na(x))
x[is.na(x)] <- 0

# Run the ordered logistic regression.
fitoloutput2 <- polr(output2f ~ x, Hess=TRUE, method="logistic")
summary(fitoloutput2)
# The parallel slopes assumption for logistic regression should be checked - here, this is omitted.

# Run the probit regression too, to compare to logistic.
fitoloutput2probit <- polr(output2f ~ x, Hess=TRUE, start=start, method="probit")
summary(fitoloutput2probit)
# The coefficients and standard errors should be about 1.7 times smaller in absolute value than for the logistic regression in fitoloutput2.
# The coefficients of logit and probit scale in a pi/sqrt(3) ratio because the standard normal distribution has variance 1 and logistic has variance pi/[sqrt]3. The tails differ for these distributions, but are rarely reached in the data.

# Confidence intervals for the logistic and probit regression coefficients.
cbind(confint(fitoloutput2),confint(fitoloutput2probit)) # Takes half a minute to "profile" the likelihood function and calculate the CIs.

# Run the cauchit regression too to compare to logistic and probit. Cauchit assumes the latent variable has a Cauchy distribution, which differs from logistic and probit, so the results probably differ.
fitoloutput2cauchit <- polr(output2f ~ x, Hess=TRUE, start=start, method="cauchit")
summary(fitoloutput2cauchit)

# The latent variable output2* may not have a symmetric distribution, in which case cloglog or loglog may be a better error distribution.
fitoloutput2 <- polr(output2f ~ x, Hess=TRUE, method="cloglog")
summary(fitoloutput2)

# Compare to ordinary least squares regression:
fitolsoutput2 <- lm(output2y ~ x)
summary(fitolsoutput2)
