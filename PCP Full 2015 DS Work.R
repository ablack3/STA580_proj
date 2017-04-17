# Load data set

library(Hmisc)
BRFSS2015full <- sasxport.get("LLCP2015.XPT")
typeof(BRFSS2015full)
 #| [1] "list"
dim(BRFSS2015full)
 #| [1] 441456    330

# select variables of interest

data <- BRFSS2015full[,c("hlthpln1","persdoc2", "medcost", "checkup1", 
                                 "x.asthms1", "aservist", "diabete3", "diabage2",
                                 "pdiabtst", "prediab1", "insulin", "bldsugar",
                                 "feetchk2", "feetchk", "doctdiab", "chkhemo3",
                                 "eyeexam", "diabedu", "diabeye", "x.incomg",
                                 "x.educag", "x.chldcnt", "x.age.g", "x.age80", 
                                 "x.age65yr", "x.ageg5yr", "x.llcpwt", "x.state")]



## Investigating relationship between insurance status and last visit to doctor

# remove entries where the individual either didn't know, refused to respond,
# or wasn't asked about variables of interest

working.data <- data[data$hlthpln1!=7,]
working.data <- working.data[working.data$checkup1!=7,] 
  # 7 means they didn't know
working.data <- working.data[working.data$hlthpln1!=9,]
working.data <- working.data[working.data$checkup1!=9,] 
  # 9 means they refused to answer the question

table(working.data$hlthpln1, working.data$checkup1)
#|         1      2      3      4      8
#|  1 310594  45150  23695  20172   2761
#|  2  13800   5148   4702   6610   1051

chisq.test(working.data$hlthpln1, working.data$checkup1)
#|         Pearson's Chi-squared test
#|
#| data:  working.data$hlthpln1 and working.data$checkup1
#| X-squared = 23186, df = 4, p-value < 2.2e-16

# below we replace the variables with more representative values

working.data$hlthpln1[working.data$hlthpln1==1] <- "1: Yes"
working.data$hlthpln1[working.data$hlthpln1=="2"] <- "2: No"

working.data$checkup1[working.data$checkup1==1] <- "1: < 1 Year"
working.data$checkup1[working.data$checkup1=="2"] <- "2: < 2 Years"
working.data$checkup1[working.data$checkup1=="3"] <- "3: < 5 Years"
working.data$checkup1[working.data$checkup1=="4"] <- "4: > 5 Years"
working.data$checkup1[working.data$checkup1=="8"] <- "8: Never"

# recreate unweighted nationwide table
table(working.data$hlthpln1, working.data$checkup1)
#|         1: < 1 Year   2: < 2 Years   3: < 5 Years   4: > 5 Years   8: Never
#|  1: Yes      310594          45150          23695          20172       2761
#|  2: No        13800           5148           4702           6610       1051

# Import gmodel library and make a nicer looking table for unweighted nationwide data
library(gmodels)
HvC <- CrossTable(working.data$hlthpln1, working.data$checkup1, dnn = c("Health Plan", "Last Checkup"),
                  prop.chisq = FALSE, prop.c = FALSE, prop.t = FALSE)


#|        Cell Contents
#|    |-------------------------|
#|    |                       N |
#|    |           N / Row Total |
#|    |-------------------------|
#|    
#|    
#|    Total Observations in Table:  433683 
#|  
#|  
#|                 | Last Checkup 
#|     Health Plan |  1: < 1 Year | 2: < 2 Years | 3: < 5 Years | 4: > 5 Years |     8: Never |    Row Total | 
#|    -------------|--------------|--------------|--------------|--------------|--------------|--------------|
#|          1: Yes |       310594 |        45150 |        23695 |        20172 |         2761 |       402372 | 
#|                 |        0.772 |        0.112 |        0.059 |        0.050 |        0.007 |        0.928 | 
#|    -------------|--------------|--------------|--------------|--------------|--------------|--------------|
#|           2: No |        13800 |         5148 |         4702 |         6610 |         1051 |        31311 | 
#|                 |        0.441 |        0.164 |        0.150 |        0.211 |        0.034 |        0.072 | 
#|    -------------|--------------|--------------|--------------|--------------|--------------|--------------|
#|    Column Total |       324394 |        50298 |        28397 |        26782 |         3812 |       433683 | 
#|    -------------|--------------|--------------|--------------|--------------|--------------|--------------|
#|

# create a state stratified table/array of unweighted data
state_strat_HvC <- table(working.data$hlthpln1, working.data$checkup1, working.data$x.state)
# performs a Cochran-Mantel-Haenszel test of within strata independence
mantelhaen.test(state_strat_HvC, conf.level = 0.95)
#|  
#|      Cochran-Mantel-Haenszel test
#|  
#|  data:  state_strat_HvC
#|  Cochran-Mantel-Haenszel M^2 = 21976, df = 4, p-value < 2.2e-16
#|  

# since we are going to need to use the weighting variables we should check whether there are any missing
# values and if so how many
dim(working.data[is.na(working.data$x.llcpwt),])
#|[1]  1 28

working.data[is.na(working.data$x.llcpwt),]
#|  hlthpln1 persdoc2 medcost checkup1 x.asthms1 aservist diabete3 diabage2 pdiabtst prediab1 insulin
#|  <NA>     NA       NA      <NA>     NA        NA       NA       NA       NA       NA       NA
#|  bldsugar feetchk2 feetchk doctdiab chkhemo3 eyeexam diabedu diabeye x.incomg x.educag x.chldcnt
#|  NA       NA       NA      NA       NA       NA      NA      NA      NA       NA       NA      
#|  x.age.g x.age80 x.age65yr x.ageg5yr x.llcpwt x.state
#|  NA      NA      NA        NA        NA       NA      

# There is only one data point with a missing value for the weighting variable and this data point
# happens to also have missing values for all the other variables. We can safely eliminate one data
# point without biasing our data.

# remove the missing value from dataset
working.data <- working.data[!is.na(working.data$x.llcpwt),]

# create a nationwide table of weighted data
Weighted_Nation = matrix(data = c(
                                sum(working.data[working.data$hlthpln1=="1: Yes"&working.data$checkup1=="1: < 1 Year",27]),
                                sum(working.data[working.data$hlthpln1=="1: Yes"&working.data$checkup1=="2: < 2 Years",27]),
                                sum(working.data[working.data$hlthpln1=="1: Yes"&working.data$checkup1=="3: < 5 Years",27]),
                                sum(working.data[working.data$hlthpln1=="1: Yes"&working.data$checkup1=="4: > 5 Years",27]),
                                sum(working.data[working.data$hlthpln1=="1: Yes"&working.data$checkup1=="8: Never",27]),
                                sum(working.data[working.data$hlthpln1=="1: Yes",27]),
                                sum(working.data[working.data$hlthpln1=="2: No"&working.data$checkup1=="1: < 1 Year",27]),
                                sum(working.data[working.data$hlthpln1=="2: No"&working.data$checkup1=="2: < 2 Years",27]),
                                sum(working.data[working.data$hlthpln1=="2: No"&working.data$checkup1=="3: < 5 Years",27]),
                                sum(working.data[working.data$hlthpln1=="2: No"&working.data$checkup1=="4: > 5 Years",27]),
                                sum(working.data[working.data$hlthpln1=="2: No"&working.data$checkup1=="8: Never",27]),
                                sum(working.data[working.data$hlthpln1=="2: No",27]),
                                sum(working.data[working.data$checkup1=="1: < 1 Year",27]),
                                sum(working.data[working.data$checkup1=="2: < 2 Years",27]),
                                sum(working.data[working.data$checkup1=="3: < 5 Years",27]),
                                sum(working.data[working.data$checkup1=="4: > 5 Years",27]),
                                sum(working.data[working.data$checkup1=="8: Never",27]),
                                sum(working.data[,27])
                                ),
                         byrow = TRUE,
                         nrow = 3, ncol = 6,
                         dimnames = list(
                                    c("Insured", "Uninsured", "Total"), 
                                    c("1: < 1 Year", "2: < 2 Years", "3: < 5 Years", "4: > 5 Years", "8: Never", "Total")))

Weighted_Nation
#|            1: < 1 Year 2: < 2 Years 3: < 5 Years 4: > 5 Years 8: Never     Total
#|  Insured     160339386     27353809     15358862     12118123  1638083 216808262
#|  Uninsured    12546600      5364603      4640374      6014475  1203376  29769427
#|  Total       172885986     32718411     19999235     18132597  2841459 246577690
#|

#######################################
# mabye try this? Not sure if it will work
library(dplyr)
# test data
# working.data=data.frame(hlthpln1=sort(rep(c("Y","N"),10)),  checkup1 = rep(1:5,4), x.llcpwt=1:20)

# group, summarize, and transpose data to create summary table
summary_table <- working.data %>% group_by(hlthpln1, checkup1) %>% summarize(wt_sum = sum(x.llcpwt)) %>% 
      tidyr::spread(key = checkup1, value = wt_sum) %>% ungroup()

# then convert to numeric matrix and run test
summary_table %>% select(-hlthpln1) %>% as.matrix() %>% chisq.test()

### RESPONSE: Haha sweet, I've never used the %>% operator or the dplyr package. It's a bit confusing
###           at first but it does make it a lot shorter. Thanks!

## :) Hey this is even shorter. 
summary_table <- xtabs(x.llcpwt ~ hlthpln1 + checkup1, data = working.data)
chisq.test(summary_table)
#######################################

# chi-square test of general association

chisq.test(Weighted_Nation[1:2,1:5])
#|  
#|          Pearson's Chi-squared test
#|  
#|  data:  Weighted_NationNOMARGINS
#|  X-squared = 16743000, df = 4, p-value < 2.2e-16

# This low p-value indicates that there is some sort of general associatation between insurance status
# and frequency of visits to a primary care physician

######################################################################################################

# Rank Correlation

# First calculate the number of concordant pairs, discordant pairs, and tied pairs

C.pairs <- 0
D.pairs <- 0
T.pairs <- 0
for(i in 1:4){
  for(j in (i + 1):5){
    C.pairs <- C.pairs + Weighted_Nation[1,i]*Weighted_Nation[2,j]
    D.pairs <- D.pairs + Weighted_Nation[2,i]*Weighted_Nation[1,j]
    T.pairs <- T.pairs + Weighted_Nation[1,i]*Weighted_Nation[1,j]
    T.pairs <- T.pairs + Weighted_Nation[2,i]*Weighted_Nation[2,j]
  }
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
  T.pairs <- T.pairs + Weighted_Nation[1,i]*Weighted_Nation[2,i]
  T.pairs <- T.pairs + Weighted_Nation[1,i]*(Weighted_Nation[1,i]-1)/2
  Tied.Ins <- Tied.Ins + Weighted_Nation[1,i]*(Weighted_Nation[1,i]-1)/2
  K.vi <- K.vi + Weighted_Nation[1,i]*(Weighted_Nation[1,i]-1)*(2*Weighted_Nation[1,i]+5)
  K.v1.a <- K.v1.a + Weighted_Nation[1,i]*(Weighted_Nation[1,i]-1)
  K.v2.a <- K.v2.a + Weighted_Nation[1,i]*(Weighted_Nation[1,i]-1)*(Weighted_Nation[1,i]-1)
  T.pairs <- T.pairs + Weighted_Nation[2,i]*(Weighted_Nation[2,i]-1)/2
  Tied.Uns <- Tied.Uns + Weighted_Nation[2,i]*(Weighted_Nation[2,i]-1)/2
  K.vu <- K.vu + Weighted_Nation[2,i]*(Weighted_Nation[2,i]-1)*(2*Weighted_Nation[2,i]+5)
  K.v1.b <- K.v1.b + Weighted_Nation[2,i]*(Weighted_Nation[2,i]-1)
  K.v2.b <- K.v2.b + Weighted_Nation[2,i]*(Weighted_Nation[2,i]-1)*(Weighted_Nation[2,i]-1)
}

All_Pairs <- Weighted_Nation[3,6]*(Weighted_Nation[3,6]-1)/2
n <- Weighted_Nation[3,6]

# Kendall's tau-a (doesn't take ties into account) and tau-b (which does take ties into account)
K.tau_a <- (C.pairs - D.pairs)/All_Pairs
K.tau_b <- (C.pairs - D.pairs)/sqrt((All_Pairs - K.Tied.Ins)*(All_Pairs - K.Tied.Uns))

# Test statistics for Kendall's taus, these have approximately standard normal distributions
Z.Ka <- (3*(C.pairs - D.pairs))/sqrt(All_Pairs*(2*n+5))

K.v1 <- K.v1.a*K.v1.b/(2*n*(n-1))
K.v2 <- K.v2.a*K.v2.b/(9*n*(n-1)*(n-2))
K.vo <- n*(n-1)*(2*n+5)

Z.Kb <- (C.pairs - D.pairs)/sqrt((K.vo - K.vi - K.vu)/18 + K.v1 + K.v2)




#######################################################################################################

## Investigating relationship between emergency room asthma visits and insurance status

# check for states with asthma module

for(i in unique(data[,"x.state"])){
  state <- data[data$x.state==i,]
  print(i)
  print(unique(state[, "aservist"]))
}

# Only 11 and 22 (DC and Louisiana) included asthma module

LA.asthma.data <- working.data[working.data$x.state==22,]
DC.asthma.data <- working.data[working.data$x.state==11,]

# Select only individuals who currently have asthma
LA.asthma.data <- LA.asthma.data[LA.asthma.data$x.asthms1==1,]
DC.asthma.data <- DC.asthma.data[DC.asthma.data$x.asthms1==1,]

# Remove individuals with missing values
LA.asthma.data <- LA.asthma.data[LA.asthma.data$x.asthms1!=9,]
LA.asthma.data <- LA.asthma.data[!is.na(LA.asthma.data$aservist),]
LA.asthma.data <- LA.asthma.data[LA.asthma.data$aservist!=98,]
DC.asthma.data <- DC.asthma.data[DC.asthma.data$x.asthms1!=9,]
DC.asthma.data <- DC.asthma.data[!is.na(DC.asthma.data$aservist),]
DC.asthma.data <- DC.asthma.data[DC.asthma.data$aservist!=98,]

# Rename variable values
LA.asthma.data$hlthpln1[LA.asthma.data$hlthpln1==1] <- "1: Yes"
LA.asthma.data$hlthpln1[LA.asthma.data$hlthpln1=="2"] <- "2: No"
LA.asthma.data$aservist[LA.asthma.data$aservist==88] <- "88: None"
LA.asthma.data$aservist[LA.asthma.data$aservist!="88: None"] <- "1-87: ER Visit"

DC.asthma.data$hlthpln1[DC.asthma.data$hlthpln1==1] <- "1: Yes"
DC.asthma.data$hlthpln1[DC.asthma.data$hlthpln1=="2"] <- "2: No"
DC.asthma.data$aservist[DC.asthma.data$aservist==88] <- "88: None"
DC.asthma.data$aservist[DC.asthma.data$aservist!="88: None"] <- "1-87: ER Visit"

CrossTable(LA.asthma.data$hlthpln1, LA.asthma.data$aservist, dnn = c("Health Plan", "ER Visit for Asthma Attack"),
           prop.chisq = FALSE, prop.c = FALSE, prop.t = FALSE)
CrossTable(DC.asthma.data$hlthpln1, DC.asthma.data$aservist, dnn = c("Health Plan", "ER Visit for Asthma Attack"),
           prop.chisq = FALSE, prop.c = FALSE, prop.t = FALSE)

