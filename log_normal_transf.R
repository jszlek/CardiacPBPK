  #Functions to transform normal distribution into log-normal
  #Functions of mean and CV:
  #meanlog:
  m_lognormal <-
    function(m, cv) {
      (log(m) - 0.5 * log(1 + (cv * m) ^ 2 / (m ^ 2)))
    }
  #sdlog:
  s_lognormal <- function(m, cv) {
    (sqrt(log(1 + (cv * m) ^ 2 / (m ^ 2))))
  }
  
  #Functions of mean and SD:
  #meanlog:
  m_lognormal_2 <-
    function(m, stdev) {
      (log(m) - 0.5 * log(1 + (stdev) ^ 2 / (m ^ 2)))
    }
  #sdlog:
  s_lognormal_2 <- function(m, stdev) {
    (sqrt(log(1 + (stdev) ^ 2 / (m ^ 2))))
  }

  
# Other functions goes here:
  fun <- function(a,x, y){
  as.data.frame(cbind(a, x + y))
}
