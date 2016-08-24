
# ---- test-a ----
set.seed(602) 
library(MASS) # to generate multi-var distribution
e <-  as.numeric(mvrnorm(n = 2, mu = 0, Sigma = 1))
plot(e)





## ---- chunk-1 ----
set.seed(602) 
library(MASS) # to generate multi-var distribution
e <-  as.numeric(mvrnorm(n = 100, mu = 0, Sigma = 1))
plot(e)