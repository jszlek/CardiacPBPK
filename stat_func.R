# Function for calculating median, upper and lower confidence intervals for x
# Where x will be concentrations for API and metabolite

statFunc <- function(x, probs.low, probs.hi) {
    my.median <-  median(x)
    my.low <-  quantile(x, probs=probs.low, names=FALSE) 
    my.hi <-  quantile(x, probs=probs.hi, names=FALSE)
    my.length <-  length(x)
    result <- c("median"=my.median, "lower_CI"=my.low, "higher_CI"=my.hi, "n"=my.length)
    return(result)
}

# for 95% confidence level - probs.low = 0.025 and probs.hi = 0.975
# for 90% confidence level - probs.low = 0.05 and probs.hi = 0.95
# for 80% confidence level - probs.low = 0.1 and probs.hi = 0.9
