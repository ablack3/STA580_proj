# look at association beteen unisured rate and how long

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
# read in data
full <- read.csv("/Users/adamblack/Downloads/Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present_.csv", stringsAsFactors = F)

g <- full[grepl("checkup", full$Question), "Question"] %>% unique()
head(g)

df <- full %>% filter(grepl("checkup|Do you have any kind of health care coverage?", Question)) %>% 
            filter(Break_Out == "Overall") %>% 
            filter(!(Locationabbr %in% c("US","GU","PR", "UW", "DC"))) %>%
            select(Year, Locationabbr, Topic, Response, Data_value) 
            


df2 <- df %>% mutate(weight = ifelse(grepl("Yes|No|past year", Response), 1,
                              ifelse(grepl("2 years", Response), 2,
                              ifelse(grepl("5", Response), 5, 
                              ifelse(grepl("Never", Response), 10, NA)))),
                     value = weight*Data_value) 
#replace spaces with _

df3 <- df2 %>% mutate(Topic = gsub(" ", "_", Topic)) %>% 
      filter(Response != "No") %>%
      group_by(Year, Locationabbr, Topic) %>% 
      summarise(value2 = sum(value)/18) %>%  # 18 is sum of the weights
      spread(Topic, value2) %>% ungroup() %>% 
      filter(complete.cases(.)) %>% 
      dmap_at(3:4,~./100) #scale. put percentages btwn 0 and 1
head(df3)

# correlations by year
split_df <- split(df3, df3$Year)
corr <- lapply(split_df, function(df3){ 
      cor(df3$Last_Checkup, df3$Health_Care_Coverage) %>% round(2)
}) %>% unlist()
corr


ggplot(df3, aes(Health_Care_Coverage, Last_Checkup)) + geom_point() +
      geom_smooth(method="lm") + 
      facet_grid(~Year)

ggsave("./plots/coverage by checkup as state level.png")



            