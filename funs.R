# Probability that A wins rally given A serves
## 1 - pr = qr = prob B wins given A serves
pr <- function(pwon, pserve) {
  mean(pwon / pserve)
}

# Probability that A wins game given A serves
## 1 - pg = qg = prob B wins given A serves
pg <- function(pr) {
  qr <- 1 - pr
  
  pr^4 * (1 + 4*qr + 10*qr^2) + 10*(pr*qr)^3 * pr^2 * (1 - 2*p2*qr)^-1
}