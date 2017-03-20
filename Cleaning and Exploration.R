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


working.data$hlthpln1[working.data$hlthpln1==1] <- "1: Yes"
working.data$hlthpln1[working.data$hlthpln1=="2"] <- "2: No"

working.data$checkup1[working.data$checkup1==1] <- "1: < 1 Year"
working.data$checkup1[working.data$checkup1=="2"] <- "2: < 2 Years"
working.data$checkup1[working.data$checkup1=="3"] <- "3: < 5 Years"
working.data$checkup1[working.data$checkup1=="4"] <- "4: > 5 Years"
working.data$checkup1[working.data$checkup1=="8"] <- "8: Never"

HvC <- CrossTable(working.data$hlthpln1, working.data$checkup1, dnn = c("Health Plan", "Last Checkup"),
                  prop.chisq = FALSE, prop.c = FALSE, prop.t = FALSE)

state_strat_HvC <- table(working.data$hlthpln1, working.data$checkup1, working.data$x.state)

mantelhaen.test(state_strat_HvC, conf.level = 0.95)


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

