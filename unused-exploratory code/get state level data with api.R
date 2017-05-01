# create dataset for analysis
#
# variables of interest
# "hlthpln1"
# "persdoc2" 
# "medcost", 
# "checkup1", 
# "x.asthms1", 
# "aservist", 
# "diabete3", 
# "diabage2",
#  "pdiabtst", 
# "prediab1", 
# "insulin", 
# "bldsugar",
#  "feetchk2", 
# "feetchk", 
# "doctdiab", 
# "chkhemo3",
#  "eyeexam", 
# "diabedu", 
# "diabeye", 
# "x.incomg",
#  "x.educag", 
# "x.chldcnt", 
# "x.age.g", 
# "x.age80", 
#  "x.age65yr", 
# "x.ageg5yr", 
# "x.llcpwt", 
# "x.state"

# install.packages("RSocrata")
full <- read.csv("/Users/adamblack/Downloads/Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present_.csv", stringsAsFactors = F)

library("RSocrata")
library(dplyr)
library(ggplot2)
# df <- read.socrata("https://chronicdata.cdc.gov/resource/gkhr-x4by.json?topic=Health Care Access/Coverge")

url_base <- "https://chronicdata.cdc.gov/resource/fn2i-3j6c.json?"
df2 <- read.socrata(paste0(url_base,
                           "question=Do you have any kind of health care coverage?",
                           "&break_out=Overall",
                           "&response=No"
))


df2.2 <- read.socrata(paste0(url_base,
                           "question=How is your general health?",
                           "&response=Poor",
                           "&break_out=Overall"
                           
))

df2.2 <- read.socrata(paste0(url_base,
                          "question=Ever told you had a stroke?",
                          "&response=Yes",
                          "&break_out=Overall"
                          
))

df3 <- rbind(df2, df2.2)
class(df3$data_value)

df3$data_value <- as.numeric(df3$data_value)

names(df3)
df3 %>% 
      ggplot(aes(x = year, y = data_value, group = questionid)) + 
      geom_line(aes(color = questionid)) + 
      facet_wrap(~ locationabbr)



df3 %>% filter(Locationabbr %in% c("US", "ME", "AL")) %>% 
      ggplot(aes(x = Year, y = Data_value, group = Locationabbr)) + 
      geom_line(aes(color = Locationabbr))

full$Question %>% unique() %>% grep("stroke",. , value = T)


names(full)
f2 <- full %>% filter(Class == "Health Care Access/Coverage", Break_Out == "Overall")
f2 %>% select(Question) %>% unique()
f2 %>% select(Break_Out) %>% unique()


df3 <-  full %>% 
      filter(Question == "Do you have any kind of health care coverage?", Response == "No") %>% 
      filter(Break_Out == "Overall")



max(as.numeric(df2$data_value))
min(as.numeric(df2$data_value))


stringr::str_extract("stroke", questions)
questions <- unique(full$Question)
matches <- grepl("stroke", questions)
questions[matches]

stringr::str_extract("Stroke", questions)
