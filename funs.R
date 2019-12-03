
library(numbers)


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


#Prob A wins a set given A served
pASij <- function(i, j, pAG, pBG){
  if(i == 0 & j == 1){return(1)}
  if(i < 0){return(0)}
  if(j < 0){return(0)}
  for( a in 1:i){ 
    for( b in j:6){
      if(mod((i+j-1), 1) == 1) {
        if(j == 6 & i <= 5){
          return(pASij(i, j-1, pAG, pBG)*(1-pAG))
        }
        else if( i == 6 & j <= 5){
          return(pASij(i-1, j, pAG, pBG)*pAG)
        }
        else{
          return(pASij(i-1, j, pAG, pBG)*pAG + pASij(i, j-1, pAG, pBG)*(1-pAG))
        }
      }
      else{
        if(j == 6 & i <= 5){
          return(pASij(i, j-1, pAG, pBG)*pBG)
        }
        else if( i == 6 & j <= 5){
          return(pASij(i-1, j, pAG, pBG)*(1-pBG))
        }
        else{
          return(pASij(i-1, j, pAG, pBG)*(1-pBG) + pASij(i, j-1, pAG, pBG)*pBG)
        }
      }
    }
  }
  
}

