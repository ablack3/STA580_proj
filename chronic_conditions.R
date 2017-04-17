library(purrr)
library(dplyr)
library(tidyr)

# getwd()
# varlen <- read.csv("../brfs/2015/var_layout_2015.csv", stringsAsFactors = F )
# 
# # read in data
# df <- read.fwf("../brfs/2015/head.ASC", col.names = varlen$varname, widths = varlen$field_len, skip = 1)
# df <- readr::read_fwf("../brfs/2015/LLCP2015.ASC", readr::fwf_widths(varlen$field_len, varlen$varname), skip=1)
# df <- readr::read_fwf("../brfs/2015/head.ASC", readr::fwf_widths(varlen$field_len, varlen$varname), skip=1)
# 
# ?readr::read_fwf
# ?readr::fwf_positions

BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")
BRFSS2014full <- Hmisc::sasxport.get("../brfs/2014/LLCP2014.XPT")


# this is the way to go..
bfs_head <- head(BRFSS2015full, 100)

names(BRFSS2015full)
grep("personal doctor")

str(BRFSS2015full, max.level=2)

vars <- map(BRFSS2015full, ~attr(., "label")) %>% unlist()
class(vars)
vars
grep("DOCTOR", vars, value = T)
grep("DIABETES", vars, value = T)
grep("STROKE", vars, value = T)


analysis_vars <- c("hlthpln1", "persdoc2", "x.incomg", "x.llcpwt", "x.ageg5yr", "x.rfhype5", "diabete3",
                   "asthma3", "x.drdxar1", "x.rfhlth","chccopd1", "addepev2","cvdstrk3",
                   "cvdinfr4", "chckidny", "havarth3", "chcscncr", "chcocncr", "bphigh4")

BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")
BRFSS2014full <- Hmisc::sasxport.get("../brfs/2014/LLCP2014.XPT")
BRFSS2013full <- Hmisc::sasxport.get("../brfs/2013/LLCP2013.XPT")
BRFSS2012full <- Hmisc::sasxport.get("../brfs/2012/LLCP2012.XPT")
BRFSS2011full <- Hmisc::sasxport.get("../brfs/2011/LLCP2011.XPT")

analysis_vars %in% names(BRFSS2015full) %>% all() #cbind(analysis_vars)
analysis_vars %in% names(BRFSS2014full) %>% all() # cbind(analysis_vars)
analysis_vars %in% names(BRFSS2012full) %>% all() # cbind(analysis_vars)
analysis_vars %in% names(BRFSS2011full) %>% all() # cbind(analysis_vars)
analysis_vars %in% names(BRFSS2010full) %>% all() # cbind(analysis_vars)

analysis_vars %in% names(BRFSS2015full) %>% rbind(analysis_vars)
analysis_vars %in% names(BRFSS2014full) %>% rbind(analysis_vars)
analysis_vars %in% names(BRFSS2013full) %>% rbind(analysis_vars)
analysis_vars %in% names(BRFSS2012full) %>% rbind(analysis_vars)
analysis_vars %in% names(BRFSS2011full) %>% rbind(analysis_vars)





analysis_vars %in% names(BRFSS2013full) %>% cbind(analysis_vars)

grep("x.",names(BRFSS2014full), value = T)

BRFSS2014full$bphigh45 <- NA

clean_brfss <- function(raw_brfss){
      # make sure all brfss datasets hve the smae set of analysis vars, eve if all NA
      for(n in analysis_vars){
            if(!( n %in% names(raw_brfss))){
                  raw_brfss[ , n] <- as.numeric(NA)
            }
      }
      
      # working data
      wd <- raw_brfss %>% 
            transmute(insurance = ifelse(hlthpln1 %in% c(1,2), hlthpln1, NA), 
                   no_doctor = ifelse(persdoc2 == 3, 1, ifelse(persdoc2 %in% 1:2, 2, NA)),
                   income = ifelse(x.incomg %in% 1:2, 1,
                            ifelse(x.incomg %in% 3:4, 2,
                            ifelse(x.incomg == 5, 3, NA))),
                   income = factor(income, 1:3, labels = c("<25K", "25-50K", ">50K")), 
                   age4 = ifelse(x.ageg5yr %in% 5:7, 1, 
                          ifelse(x.ageg5yr %in% 8:9, 2, 
                          ifelse(x.ageg5yr %in% 10:11, 3,
                          ifelse(x.ageg5yr %in% 12:13, 4, NA)))),
                   age4 = factor(age4, levels = 1:4, labels = c("40-54", "55-64","65-75","75+")),
                   diabetes = ifelse(diabete3 == 1, 1, 
                              ifelse(diabete3 %in% 2:4, 2, NA)),
                   poor_health = ifelse(x.rfhlth %in% 1:2, 3-x.rfhlth, NA),
                   hypertension = ifelse(bphigh4 == 1, 1, 
                                  ifelse(bphigh4 %in% 2:4, 2, as.numeric(NA))),
                   heart_attack = ifelse(cvdinfr4 %in% 1:2, cvdinfr4, NA),
                   asthma = ifelse(asthma3 %in% 1:2, asthma3, NA),
                   arthritis = ifelse(x.drdxar1 %in% 1:2, x.drdxar1, NA),
                   copd = ifelse(chccopd1 %in% 1:2, chccopd1, NA),
                   depression = ifelse(addepev2 %in% 1:2, addepev2, NA),
                   stroke = ifelse(cvdstrk3 %in% 1:2, cvdstrk3, NA),
                   kidney_disease = ifelse(chckidny %in% 1:2, chckidny, NA),
                   arthritis = ifelse(havarth3 %in% 1:2, havarth3, NA),
                   skin_cancer = ifelse(chcscncr %in% 1:2, chcscncr, NA),
                   other_cancer = ifelse(chcocncr %in% 1:2, chcocncr, NA),
                   swt = x.llcpwt) 

      wd
}
      
bfs15 <- clean_brfss(BRFSS2015full)      
bfs14 <- clean_brfss(BRFSS2014full)
bfs13 <- clean_brfss(BRFSS2013full)      
bfs12 <- clean_brfss(BRFSS2012full)      
bfs11 <- clean_brfss(BRFSS2011full)      

class(bfs12$hypertension)
bfs14$hypertension <- as.numeric(bfs14$hypertension)
bfs12$hypertension <- as.numeric(bfs12$hypertension)

bfs_all <- union(bfs15, bfs14, bfs13, bfs12, bfs11)


xtabs(~age4+no_doctor, data=wd2) %>% as.matrix() %>% gmodels::CrossTable()


library(tidyr)
calc_rel_risk <- function(df){
      df2 <- df %>% 
      gather("condition","level", hypertension, heart_attack, skin_cancer, other_cancer,
              asthma, arthritis, copd, depression, stroke, kidney_disease, diabetes, poor_health) %>%
      select(-insurance) %>% 
      group_by(no_doctor, income, age4, condition, level) %>% 
      summarise(wt_freq = sum(swt), freq = n()) %>% 
      filter(!is.na(level)) %>% 
      ungroup() %>% 
      group_by(no_doctor, income, age4, condition) %>% 
      mutate(total = sum(wt_freq), risk = round(wt_freq/total, 3)) %>% 
      mutate(total_uw = sum(freq), risk_uw = round(freq/total_uw, 3)) %>% 
      ungroup() %>% 
      filter(level ==  1, complete.cases(.))

      # split data based on nodoc      
      df3 <- split(df2, df2$no_doctor)

      df4 <- full_join(df3[[1]], df3[[2]], by = c("age4", "income", "condition")) %>% 
            mutate(rel_risk = risk.y/risk.x, rel_risk_uw = risk_uw.y/risk_uw.x)
      df4
}

rr15 <- calc_rel_risk(bfs15)
rr14 <- calc_rel_risk(bfs14)
rr13 <- calc_rel_risk(bfs13)
rr12 <- calc_rel_risk(bfs12)
rr11 <- calc_rel_risk(bfs11)
rr_all <- calc_rel_risk(bfs_all)


plot_rel_risk <- function(df, year){
      library(ggplot2)
      pal <- RColorBrewer::brewer.pal(4,"YlOrRd")[2:4] %>% rev()
      plot <-  ggplot(df, aes(x=age4, y=rel_risk, color = income, group = income)) +
            geom_point() + geom_line() + facet_wrap(~condition, nrow = 3, ncol = 4) +
            ggtitle("Relative Risk of being diagnosed with chronic condition") +
            geom_abline(slope = 0, intercept = 1, color = "black") +
            scale_color_manual(values = pal) +
            scale_y_continuous(limits = c(.5, max(rr13$rel_risk)+.1), breaks = 1:6 ) 
      ggsave(paste0("./plots/facet_plot", year,".png"), plot=plot, width = 10, height = 8)
}



plot_rel_risk(rr15, "2015")
plot_rel_risk(rr14, "2014")
plot_rel_risk(rr13, "2013")
plot_rel_risk(rr13, "2012")
plot_rel_risk(rr12, "2011")
plot_rel_risk(rr_all, "_all")



