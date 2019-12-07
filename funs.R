
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


#equation 6-8
pASij <- function(i, j, pAG, pBG){ #i , j , prob a wins given a served, prob b wins given b served
  if(i == 0 & j == 0){return(1)}
  if(i < 0){return(0)}
  if(j < 0){return(0)}
  for( a in 1:i){ 
    for( b in j:6){
      if(mod((i+j-1), 2) == 1) {
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



pATij <- function(i, j, pAR, pBR){ #i , j , prob a wins given a served, prob b wins given b served
  if(i == 0 & j == 0){return(1)}
  if(i < 0){return(0)}
  if(j < 0){return(0)}
  for( a in 0:i){ 
    for( b in j:7){
      MO <- mod(i+j-1, 4)
      if(MO == 0 | MO == 3) {
        if(j == 7 & i <= 6){
          return(pATij(i-1, j, pAR, pBR)*(pAR))
        }
        else if( i == 7 & j <= 6){
          return(pATij(i, j-1, pAR, pBR)*(1-pAR))
        }
        else{
          return(pATij(i-1, j, pAR, pBR)*pAR + pATij(i, j-1, pAR, pBR)*(1-pAR))
        }
      }
      else{
        if(j == 7 & i <= 6){
          return(pATij(i-1, j, pAR, pBR)*(1-pBR))
        }
        else if( i == 7 & j <= 6){
          return(pATij(i, j-1, pAR, pBR)*(pBR))
        }
        else{
          return(pATij(i-1, j, pAR, pBR)*(1-pBR) + pATij(i, j-1, pAR, pBR)*(pBR))
        }
      }
    }
  }
  
}





#Prob A wins a set given a served 
ps <- function(pag, pbg){
  ret <- pASij(7,5, pag, pbg) + pATij(6,6,pag, pbg) + sum(sapply(0:4, pASij, i=6, pAG=pag, pBG=pbg ))
  if(ret > 1){return(1)}
  else {return(ret)}
}




#where pag is prob A wins a game given A served and 
#pbg is prob B wins a game given B served
#still need to write pAT


## The probability of winning two out of three sets, resulting in winning a match
pM=function(ps_a, ps_b,num_set){
  if (num_sets==2){
    (ps_a)^2+2*(ps_a)^2*ps_b
  }
  if(num_sets==3){
    (ps_a)^3+3*(ps_a)^3*ps_b+6*ps_a^3*ps_b^2
  }
}