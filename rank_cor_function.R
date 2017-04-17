
# Rank Correlation

# First calculate the number of concordant pairs, discordant pairs, and tied pairs

working.data <- data.frame(hlthpln1 = sort(rep(c("Y","N"),10)),  checkup1 = rep(1:5,4), x.llcpwt=1:20)
working.data
table()
summary_table <- xtabs(x.llcpwt ~ hlthpln1 + checkup1, data = working.data)

rank_cor(summary_table)

m <- matrix(c(30,20,10,10,20,30), byrow = F, ncol = 2)
rank_cor(m)

tbl <- m
"table" %in% class(m)


rank_cor <- function(tbl){

      # input must be a table
      stopifnot("table" %in% class(m) || "matrix" %in% class(m))

      # if nrows > ncol then transpose
      if(nrow(tbl)>ncol(tbl)) {
            tbl <- t(tbl)
      }
      nrows <- dim(tbl)[1]
      ncols <- dim(tbl)[2]
      
      # count number of concordant, dicordant, and tied pairs
      C.pairs <- 0
      D.pairs <- 0
      T.pairs <- 0

      # for each row
      for(r in 1:(nrows-1)){
            #calculate concordant pairs start at upper left cell
            for(i in r:(ncols-1)){
                  for(j in (i + 1):ncols){
                        print(c(r,i,j))
                        C.pairs <- C.pairs + tbl[r,i]*tbl[r+1,j]
                 }
            }
            # discordant pairs. start at top right cell
            for(i in (ncols-(r-1)):2){
                  for(j in (i - 1):1){
                        D.pairs <- D.pairs + tbl[r,i]*tbl[r+1,j]
                  }
            }
      }
      
      list(C.pairs = C.pairs, D.pairs = D.pairs)
}






K.Tied.Ins <- 0
K.Tied.Uns <- 0
K.vi <- 0
K.vu <- 0
K.v1.a <- 0
K.v1.b <- 0
K.v2.a <- 0
K.v2.b <- 0
for(i in 1:5){
      T.pairs <- T.pairs + tbl[1,i]*tbl[2,i]
      T.pairs <- T.pairs + tbl[1,i]*(tbl[1,i]-1)/2
      Tied.Ins <- Tied.Ins + tbl[1,i]*(tbl[1,i]-1)/2
      K.vi <- K.vi + tbl[1,i]*(tbl[1,i]-1)*(2*tbl[1,i]+5)
      K.v1.a <- K.v1.a + tbl[1,i]*(tbl[1,i]-1)
      K.v2.a <- K.v2.a + tbl[1,i]*(tbl[1,i]-1)*(tbl[1,i]-1)
      T.pairs <- T.pairs + tbl[2,i]*(tbl[2,i]-1)/2
      Tied.Uns <- Tied.Uns + tbl[2,i]*(tbl[2,i]-1)/2
      K.vu <- K.vu + tbl[2,i]*(tbl[2,i]-1)*(2*tbl[2,i]+5)
      K.v1.b <- K.v1.b + tbl[2,i]*(tbl[2,i]-1)
      K.v2.b <- K.v2.b + tbl[2,i]*(tbl[2,i]-1)*(tbl[2,i]-1)
}

All_Pairs <- tbl[3,6]*(tbl[3,6]-1)/2
n <- tbl[3,6]

# Kendall's tau-a (doesn't take ties into account) and tau-b (which does take ties into account)
K.tau_a <- (C.pairs - D.pairs)/All_Pairs
K.tau_b <- (C.pairs - D.pairs)/sqrt((All_Pairs - K.Tied.Ins)*(All_Pairs - K.Tied.Uns))

# Test statistics for Kendall's taus, these have approximately standard normal distributions
Z.Ka <- (3*(C.pairs - D.pairs))/sqrt(All_Pairs*(2*n+5))

K.v1 <- K.v1.a*K.v1.b/(2*n*(n-1))
K.v2 <- K.v2.a*K.v2.b/(9*n*(n-1)*(n-2))
K.vo <- n*(n-1)*(2*n+5)

Z.Kb <- (C.pairs - D.pairs)/sqrt((K.vo - K.vi - K.vu)/18 + K.v1 + K.v2)

}
