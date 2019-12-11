
library(numbers)
library(tidyverse)


# Probability that A wins rally given A serves
## 1 - pr = qr = prob B wins given A serves
pr <- function(pwon, pserve) {
  mean(pwon / pserve)
}

# Probability that A wins game given A serves
## 1 - pg = qg = prob B wins given A serves
pg <- function(pr) {
  qr <- 1 - pr
  
  pr^4 * (1 + 4*qr + 10*qr^2) + 20*(pr*qr)^3 * pr^2 * (1 - 2*pr*qr)^-1
}


#equation 6-8
pASij <- function(i, j, pAG, pBG) { #i , j , prob a wins given a served, prob b wins given b served
  # initial conditions
  if(i == 0 & j == 0) return(1)
  if(i < 0) return(0)
  if(j < 0) return(0)
  
  
  
  if( i == 7 & j==5){
    return(pASij(6,5, pAG, pBG) *(1-pBG))
  }
  
  
  
  # if(i == 6 & j == 6) {
  #   s <- 0
  #   
  #   for(x in 0:4) {
  #     s <- s + pASij(x, 6, pag, pbg) + pASij(6, x, pag, pbg)
  #   }
  #   
  #   return(1 -
  #     (s +
  #        pASij(7,5, pag, pbg) +
  #        pASij(5,7, pag, pbg)))
  # }
  
  if(mod((i-1+j), 2) == 0) {
    
    if(j == 6 & i <= 5) return( pASij(i, j-1, pAG, pBG)*(1-pAG) )
    
    else if( i == 6 & j <= 5) return( pASij(i-1, j, pAG, pBG)*pAG )
    
    return( pASij(i-1, j, pAG, pBG)*pAG + pASij(i, j-1, pAG, pBG)*(1-pAG))
    
  } else {
    
    if(j == 6 & i <= 5) return( pASij(i, j-1, pAG, pBG)*pBG )
    
    else if( i == 6 & j <= 5) return( pASij(i-1, j, pAG, pBG)*(1-pBG) )
    
    return( pASij(i-1, j, pAG, pBG)*(1-pBG) + pASij(i, j-1, pAG, pBG)*pBG )
    
  }
}



pATij <- function(i, j, pAR, pBR) { #i , j , prob a wins given a served, prob b wins given b served
  # initial conditions
  if(i == 0 & j == 0) return(1)
  if(i < 0) return(0)
  if(j < 0) return(0)
  
  MO <- mod(i+j-1, 4)
  
  if(MO == 0 | MO == 3) {
    
    if(i == 7 & j <= 6) return( pATij(i-1, j, pAR, pBR)*(pAR) )
    
    else if( j == 7 & i <= 6) return( pATij(i, j-1, pAR, pBR)*(1-pAR) )
    
    return(pATij(i-1, j, pAR, pBR)*pAR + pATij(i, j-1, pAR, pBR)*(1-pAR))
    
  } else {
    
    if(i == 7 & j <= 6) return( pATij(i-1, j, pAR, pBR)*(1-pBR) )
    
    else if( j == 7 & i <= 6) return( pATij(i, j-1, pAR, pBR)*pBR )
    
    return(pATij(i-1, j, pAR, pBR)*(1-pBR) + pATij(i, j-1, pAR, pBR)*(pBR))
    
  }
}

pT <- function(pAR, pBR) {
  sum(sapply(0:5, pATij, i=7, pAR=pAR, pBR=pBR)) +
    pATij(6,6,pAR,pBR) * pAR * (1-pBR) *
    (1 - pAR*pBR - (1-pAR)*(1-pBR))^-1
}

# Prob A wins a set given a served
ps <- function(pAR, pBR) {
  pag <- pg(pAR)
  pbg <- pg(pBR)
  
  sum(sapply(0:4, pASij, i=6, pAG=pag, pBG=pbg )) +
    pASij(7,5, pag, pbg) +
    pASij(6,6, pag, pbg) * pT(pAR, pBR)
}


#where pag is prob A wins a game given A served and 
#pbg is prob B wins a game given B served

## The probability of winning two out of three sets, resulting in winning a match
pM <- function(pAR, pBR, num_set) {
  ps_a <- ps(pAR, pBR)
  ps_b <- ps(pBR, pAR)
  
  if (num_set==2) ret <- (ps_a)^2 + 2*(ps_a)^2 * ps_b
  
  else if(num_set==3) ret <- (ps_a)^3 + 3*(ps_a)^3 * ps_b + 6*ps_a^3 * (ps_b)^2
  
  return(ret)
}

# Probability of winning a tournament
pTC <- function(pr1, pr2, pr3, pr4, numset) {
  p0 <- cbind(rep(1,4))
  
  P1 <- rbind(c(0, pM(pr1, pr2, numset), 0, 0),
              c(pM(pr2, pr1, numset), 0, 0, 0),
              c(0, 0, 0, pM(pr3, pr4, numset)),
              c(0, 0, pM(pr4, pr3, numset), 0))
  
  P2 <- rbind(c(0, 0, pM(pr1, pr3, numset), pM(pr1, pr4, numset)),
              c(0, 0, pM(pr2, pr3, numset), pM(pr2, pr4, numset)),
              c(pM(pr3, pr1, numset), pM(pr3, pr2, numset), 0, 0),
              c(pM(pr4, pr1, numset), pM(pr4, pr2, numset), 0, 0))
  
  p1 <- P1 %*% p0
  
  p2 <- P2 %*% p1
  
  p1 * p2
}

