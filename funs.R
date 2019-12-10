
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
  
  pr^4 * (1 + 4*qr + 10*qr^2) + 20 * (pr*qr)^3 * pr^2 * (1 - 2*pr*qr)^(-1)
}


#equation 6-8
pASij <- function(i, j, pAG, pBG) { #i , j , prob a wins given a served, prob b wins given b served
  # initial conditions
  if(i == 0 & j == 0) return(1)
  if(i < 0) return(0)
  if(j < 0) return(0)
  
  
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
        
    if(j == 7 & i <= 6) return( pATij(i-1, j, pAR, pBR)*(pAR) )
        
    else if( i == 7 & j <= 6) return( pATij(i, j-1, pAR, pBR)*(1-pAR) )
        
    return(pATij(i-1, j, pAR, pBR)*pAR + pATij(i, j-1, pAR, pBR)*(1-pAR))
        
  } else {
        
    if(i == 7 & j <= 6) return( pATij(i-1, j, pAR, pBR)*(1-pBR) )
        
    else if( j == 7 & i <= 6) return( pATij(i, j-1, pAR, pBR)*pBR )
        
    return(pATij(i-1, j, pAR, pBR)*(1-pBR) + pATij(i, j-1, pAR, pBR)*(pBR))
        
  }
}

pT <- function(pAR, pBR) {
  sum(sapply(0:5, pATij, i=7, pAR=pAR, pBR=pBR)) +
    pATij(6,6,pAR,pBR)*pAR*(1-pBR) *
    (1 - pAR*pBR - (1-pAR)*(1-pBR))^-1
}

#Prob A wins a set given a served 
ps <- function(pAR, pBR){
  pag <- pg(pAR)
  pbg <- pg(pBR)
  
  ret <- pASij(7,5, pag, pbg) + pASij(6,6,pAR,pBR)*pT(pAR, pBR) + sum(sapply(0:4, pASij, i=6, pAG=pag, pBG=pbg ))
  
  if(ret > 1){return(1)}
  else {return(ret)}
}




#where pag is prob A wins a game given A served and 
#pbg is prob B wins a game given B served

## The probability of winning two out of three sets, resulting in winning a match
pM <- function(pAR, pBR, num_set){
  ps_a <- ps(pAR, pBR)
  ps_b <- ps(pBR, pAR)
  
  if (num_set==2){
    ret <- (ps_a)^2 + 2*(ps_a)^2 * ps_b
  }
  if(num_set==3){
    ret <- (ps_a)^3 + 3*(ps_a)^3 * ps_b + 6*ps_a^3 * (ps_b)^2
  }
  
  return(ret)
}

# Probability of winning a tournament
pTC <- function(pr_1, pr_2, pr_3, pr_4, numset) {
  pRally <- c(pr_1, pr_2, pr_3, pr_4)
  
  
  
  p21 <- pM(pSet[2,1], pSet[1,2], numset)
  p23 <- pM(pSet[2,3], pSet[3,2], numset)
  p24 <- pM(pSet[2,4], pSet[4,2], numset)
  
  p31 <- pM(pSet[3,1], pSet[1,3], numset)
  p32 <- 1 - p23
  p34 <- pM(pSet[3,4], pSet[4,3], numset)
  
  p41 <- pM(pSet[4,1], pSet[1,4], numset)
  p42 <- 1 - p24
  p43 <- 1 - p34
  
  p14 <- 1 - p41
  p13 <- 1 - p31
  p12 <- 1 - p21
  
  tourney <- tourney %>%
    mutate(c(p12 * (p13*p34 + p14*p43),
             p21 * (p23*p34 + p24*p43),
             p34 * (p31*p12 + p32*p21),
             p43 * (p41*p12 + p42*p21)))
}


