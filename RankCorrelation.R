Rank.Correlation <- function(x, alpha = 0.05){
  
  z <- qnorm(alpha/2, lower.tail = F)
  cols <- dim(x)[2]
  n <- sum(x[1,]) + sum(x[2,])
  np <- (n*(n-1))/2
  SE.LS.approx <- sqrt((2*(2*n+5))/(9*n*(n-1)))
  
  T.pairs <- sum(x[1,])*(sum(x[1,])-1)/2 + sum(x[2,])*(sum(x[2,])-1)/2
  Tied.r <- T.pairs
  
  C.pairs <- 0
  D.pairs <- 0
  Tied.c <- 0
  for(i in 1:cols){
    if(i < cols){
      for(j in (i + 1):cols){
        C.pairs <- C.pairs + x[1,i]*x[2,j]
        D.pairs <- D.pairs + x[2,i]*x[1,j]
      }
    }
    
    T.pairs <- T.pairs + x[1,i]*x[2,i]
    Tied.c <- Tied.c + sum(x[,i])*(sum(x[,i])-1)/2
    
  }
  
  vt <- 0
  v1.c <- 0
  v2.c <- 0
  for(i in 1:cols){
    s <- sum(x[,i])*(sum(x[,i])-1)
    vt <- vt + s*(2*sum(x[,i])+5)
    v1.c <- v1.c + s
    v2.c <- v2.c + s*(sum(x[,i])-2)
  }
  
  vu <- 0
  v1.r <- 0
  v2.r <- 0
  for(i in 1:2){
    s <- sum(x[i,])*(sum(x[i,])-1)
    vu <- vu + s*(2*sum(x[i,])+5)
    v1.r <- v1.r + s
    v2.r <- v2.r + s*(sum(x[i,])-2)
  }
  
  v1 <- v1.c*v1.r/(2*n*(n-1))
  v2 <- v2.c*v2.r/(9*n*(n-1)*(n-2))
  
  v0 <- n*(n-1)*(2*n+5)
  
  v <- (v0 - vt - vu) / (18) + v1 + v2
 
  
  Ktau.a <- (C.pairs - D.pairs) / np
  Ktau.b <- (C.pairs - D.pairs) / sqrt( (np - Tied.r)*(np - Tied.c) )
  
  Z.a <- as.numeric( 3*(C.pairs - D.pairs) / sqrt(n*(n-1)*(2*n+5)/2) )
  pva <- pnorm(abs(Z.a), lower.tail = F)
  Z.a.Alt <- as.numeric( Ktau.a/SE.LS.approx )
  pva.Alt <- pnorm(abs(Z.a.Alt), lower.tail = F)
  
  Ta.LB <- Ktau.a - z*SE.LS.approx
  Ta.UB <- Ktau.a + z*SE.LS.approx
  
  Z.b <- as.numeric( (C.pairs - D.pairs) / sqrt(v) )
  pvb <- pnorm(abs(Z.b), lower.tail = F)
  Z.b.Alt <- as.numeric( Ktau.b/SE.LS.approx )
  pvb.Alt <- pnorm(abs(Z.b.Alt), lower.tail = F)
  
  Tb.LB <- Ktau.b - z*SE.LS.approx
  Tb.UB <- Ktau.b + z*SE.LS.approx
  
  gamma <- (C.pairs - D.pairs)/(C.pairs + D.pairs)
  SE.gamma <- sqrt(n*(1-gamma^2)/(C.pairs+D.pairs))
  Z.gamma <- as.numeric( gamma/SE.gamma )
  pvg <- pnorm(abs(Z.gamma), lower.tail = F)
  
  G.LB <- gamma - z*SE.gamma
  G.UB <- gamma + z*SE.gamma
  
  out <- matrix(c(Ta.LB, Tb.LB, G.LB, Ktau.a, Ktau.b, gamma, Ta.UB, Tb.UB, G.UB, Z.a, Z.b, Z.gamma, 
                  pva, pvb, pvg, Z.a.Alt, Z.b.Alt, NA, pva.Alt, pvb.Alt, NA),
                nrow = 3, ncol = 7,
                dimnames = list(c("Tau.a", "Tau.b", "Gamma"), 
                                c("Lower.Bound","Estimate", "Upper Bound", "TS", "p.val", "Alt.TS", "p.val")))
  
  out
}