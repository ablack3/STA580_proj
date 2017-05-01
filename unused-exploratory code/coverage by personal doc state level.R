# look at association beteen unisured rate and how long

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
# read in data
full <- read.csv("/Users/adamblack/Downloads/Behavioral_Risk_Factor_Surveillance_System__BRFSS__Prevalence_Data__2011_to_present_.csv", stringsAsFactors = F)

full$Year %>% unique()

q <- full[grepl("doctor", full$Question), "Question"] %>% unique()
head(q)
q[4]

q <- full[grepl("personal doctor", full$Question), c("Question","Year")] %>% unique()

df <- full %>% filter(grepl("personal doctor|Do you have any kind of health care coverage?", Question)) %>% 
      filter(Break_Out == "Overall") %>% 
      filter(!(Locationabbr %in% c("US","GU","PR", "UW", "DC"))) %>%
      select(Year, Locationabbr, Topic, Response, Data_value) 



#replace spaces with _

df3 <- df %>% 
      filter(Response == "No") %>%
      mutate(Topic = paste0("No_", gsub(" ", "_", Topic))) %>% 
      group_by(Year, Locationabbr, Topic) %>% 
      spread(Topic, Data_value) %>% ungroup() %>% 
      filter(complete.cases(.)) %>% 
      select(-Response) %>% 
      dmap_at(3:4,~./100) #scale. put percentages btwn 0 and 1
head(df3)

# correlations by year
split_df <- split(df3, df3$Year)
corr <- lapply(split_df, function(df3){ 
      cor(df3$No_Personal_Care_Provider, df3$No_Health_Care_Coverage) %>% round(2)
}) %>% unlist()
corr


ggplot(df3, aes(No_Health_Care_Coverage, No_Personal_Care_Provider)) + geom_point() +
      geom_smooth(method="lm") + 
      facet_grid(~Year)

ggsave("./plots/coverage by checkup as state level.png")



