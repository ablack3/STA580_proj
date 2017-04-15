plot(x = c(1:11), y = Final_Odds.Ratios[,4], 
     main = "Estimated Odds Ratios", 
     ylab = "Odds Ratio", xlab = "Explanatory Variable", xlim = c(1,11), 
     ylim = c(0.8, 3.2), pch=20, axes = FALSE, cex.main = .9, cex = .7, cex.lab = .9)
box()
axis(1, at = c(1:dim(Final_Odds.Ratios)[1]), labels = c (expression("M"), expression("P"),
                                                         expression("R"), expression("As"), expression("C"), expression("Ar"), expression("B"), 
                                                         expression("LA"["S = 0"]), expression("LA"["S = 1"]), expression("S"["LA = 0"]),
                                                         expression("S"["LA = 1"])), cex.axis = .7)
axis(2, at = seq(from = 0.8, to = 3.20, by = 0.2), las = 2, cex.axis = .7)

for(r in 1:dim(Final_Odds.Ratios)[1]){
  lines(x = c(r,r), y = c(Final_Odds.Ratios[r,5], Final_Odds.Ratios[r,6]))
  lines(x = c((r-.1), (r+.1)), y = c(Final_Odds.Ratios[r,5], Final_Odds.Ratios[r,5]))
  lines(x = c((r-.1), (r+.1)), y = c(Final_Odds.Ratios[r,6], Final_Odds.Ratios[r,6]))
}
abline(h=1)




Relative.Risk <- function(x, coi = c(1), alpha = 0.05){
  
  z <- qnorm(alpha/2, lower.tail = F)
  
  n1 <- as.numeric( sum(x[1,]) )
  n2 <- as.numeric( sum(x[2,]) )
  
  p1 <- as.numeric( x[1,coi]/n1 )
  p2 <- as.numeric( x[2,coi]/n2 )
  
  rr <- p1/p2
  v <- sqrt( (1-p1)/x[1,coi] + (1-p2)/x[2,coi] )
  
  LB <- rr*exp(-z*v)
  UB <- rr*exp(z*v)
  
  vec <- c(LB, rr, UB)
  vec

}

PD.propdif <- function(x, alpha){
  
  df <- x
  
  z <- qnorm(alpha/2, lower.tail = F)
  n1 <- as.numeric( sum(df[1,2:3]) )
  n2 <- as.numeric( sum(df[2,2:3]) )
  
  p1 <- as.numeric( df[1,2]/sum(df[1,2:3]) )
  p2 <- as.numeric( df[2,2]/sum(df[2,2:3]) )
  dif <- p1 - p2
  
  LB <- dif - z*sqrt( (p1*(1-p1))/n1 + (p2*(1-p2))/n2 )
  UB <- dif + z*sqrt( (p1*(1-p1))/n1 + (p2*(1-p2))/n2 )
  
  vec <- c(LB, dif, UB)
  vec
}