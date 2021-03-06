library(purrr)
library(dplyr)
library(tidyr)


analysis_vars <- c("hlthpln1", "persdoc2", "x.incomg", "x.llcpwt", "x.ageg5yr", 
                   "diabete3", "asthma3", "x.drdxar1","chccopd1", "addepev2","cvdstrk3",
                   "cvdinfr4", "chckidny", "havarth3", "chcscncr", "chcocncr", 
                   "x.rfhlth")

#drop bphigh4 since it is not in 2012 or 2014 data

BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")
BRFSS2014full <- Hmisc::sasxport.get("../brfs/2014/LLCP2014.XPT")
BRFSS2013full <- Hmisc::sasxport.get("../brfs/2013/LLCP2013.XPT")
BRFSS2012full <- Hmisc::sasxport.get("../brfs/2012/LLCP2012.XPT")
BRFSS2011full <- Hmisc::sasxport.get("../brfs/2011/LLCP2011.XPT")

analysis_vars %in% names(BRFSS2015full) %>% all() #cbind(analysis_vars)
analysis_vars %in% names(BRFSS2014full) %>% all() # cbind(analysis_vars)
analysis_vars %in% names(BRFSS2013full) %>% all() # cbind(analysis_vars)
analysis_vars %in% names(BRFSS2012full) %>% all() # cbind(analysis_vars)
analysis_vars %in% names(BRFSS2011full) %>% all() # cbind(analysis_vars)

analysis_vars %in% names(BRFSS2014full) %>% rbind(analysis_vars) #bphigh4
analysis_vars %in% names(BRFSS2012full) %>% rbind(analysis_vars) #bphigh4
analysis_vars %in% names(BRFSS2011full) %>% rbind(analysis_vars) #chccopd -> chccopd1

BRFSS2011full$chccopd1 <- BRFSS2011full$chccopd

# example code for loctating variables
vars <- map(BRFSS2015full, ~attr(., "label")) %>% unlist()
grep("DOCTOR", vars, value = T)


clean_brfss <- function(raw_brfss){
      wd <- raw_brfss %>% 
            transmute(insurance = ifelse(hlthpln1 %in% c(1,2), -hlthpln1 + 2, NA), 
                   no_doctor = ifelse(persdoc2 == 3, 1, ifelse(persdoc2 %in% 1:2, 0, NA)),
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
                              ifelse(diabete3 %in% 2:4, 0, NA)),
                   poor_health = ifelse(x.rfhlth %in% 1:2, 3-x.rfhlth, NA),
                   poor_health = -poor_health + 2,
                   # hypertension = ifelse(bphigh4 == 1, 1, 
                   #                ifelse(bphigh4 %in% 2:4, 2, as.numeric(NA))),
                   heart_attack = ifelse(cvdinfr4 %in% 1:2, -cvdinfr4 + 2, NA),
                   asthma = ifelse(asthma3 %in% 1:2, -asthma3 + 2, NA),
                   arthritis = ifelse(x.drdxar1 %in% 1:2, -x.drdxar1 + 2, NA),
                   copd = ifelse(chccopd1 %in% 1:2, -chccopd1 + 2, NA),
                   depression = ifelse(addepev2 %in% 1:2, -addepev2 + 2, NA),
                   stroke = ifelse(cvdstrk3 %in% 1:2, -cvdstrk3 + 2, NA),
                   kidney_disease = ifelse(chckidny %in% 1:2, -chckidny + 2, NA),
                   arthritis = ifelse(havarth3 %in% 1:2, -havarth3 + 2, NA),
                   skin_cancer = ifelse(chcscncr %in% 1:2, -chcscncr + 2, NA),
                   other_cancer = ifelse(chcocncr %in% 1:2, -chcocncr + 2, NA),
                   swt = x.llcpwt) 

      wd
}
      
bfs15 <- clean_brfss(BRFSS2015full)      
bfs14 <- clean_brfss(BRFSS2014full)
bfs13 <- clean_brfss(BRFSS2013full)      
bfs12 <- clean_brfss(BRFSS2012full)      
bfs11 <- clean_brfss(BRFSS2011full)      

bfs_all <- union(bfs15, bfs14, bfs13, bfs12, bfs11)

bfs_head <- head(bfs_all, 100)

######### QC and data checking  ##########
xtabs(~ age4 + no_doctor, data = bfs_all) %>% as.matrix() %>% gmodels::CrossTable()

bfs_all %>% 
      filter(age4 == "40-54", income == "<25K") %>% 
      xtabs(~ no_doctor + arthritis, data = .) %>% 
      as.matrix() %>% 
      epitools::epitab(method = "riskratio", rev="columns")

# skin cancer example
v <- (1-0.0316188)/927 + (1- 0.0169369)/164
c(lower = 1.866859*exp(-1.96*sqrt(v)), upper = 1.866859*exp(1.96*sqrt(v))) 

########## 
calc_rel_risk <- function(df){
      df2 <- df %>% 
      gather("condition","level", heart_attack, skin_cancer, other_cancer,
              asthma, arthritis, copd, depression, stroke, kidney_disease, diabetes, poor_health) %>%
      select(-insurance) %>% 
      group_by(no_doctor, income, age4, condition, level) %>% 
      summarise(freq = sum(swt), freq_uw = n()) %>% 
      filter(!is.na(level)) %>% 
      ungroup() %>% 
      group_by(no_doctor, income, age4, condition) %>% 
      mutate(total = sum(freq), risk = round(freq/total, 3)) %>% 
      mutate(total_uw = sum(freq_uw), risk_uw = round(freq_uw/total_uw, 3)) %>% 
      ungroup() %>% 
      filter(level ==  1, complete.cases(.))

      # split data based on nodoc      
      df3 <- split(df2, df2$no_doctor)

      df4 <- full_join(df3[[2]], df3[[1]], by = c("age4", "income", "condition")) %>% 
            mutate(rel_risk = risk.y/risk.x, 
                   var_rr = (1-risk.y)/freq.y + (1-risk.x)/freq.x,
                   rr_lower = rel_risk*exp(-1.96*sqrt(var_rr)),
                   rr_upper = rel_risk*exp(1.96*sqrt(var_rr)),
                   rel_risk_uw = risk_uw.y/risk_uw.x,
                   var_rr_uw = (1-risk_uw.y)/freq_uw.y + (1-risk_uw.x)/freq_uw.x,
                   rr_uw_lower = rel_risk_uw*exp(-1.96*sqrt(var_rr_uw)),
                   rr_uw_upper = rel_risk_uw*exp(1.96*sqrt(var_rr_uw))
            )
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
            scale_color_manual(values = pal) 
            # scale_y_continuous(limits = c(.5, max(rr13$rel_risk)+.1), breaks = 1:6 ) 
      ggsave(paste0("./plots/facet_plot", year,".png"), plot=plot, width = 10, height = 8)
}

plot_rel_risk(rr15, "2015")
plot_rel_risk(rr14, "2014")
plot_rel_risk(rr13, "2013")
plot_rel_risk(rr13, "2012")
plot_rel_risk(rr12, "2011")
plot_rel_risk(rr_all, "_all")


rr_table_all <- rr_all %>% 
      select(condition, age4, income, rel_risk, rr_lower, rr_upper) %>% 
      arrange(condition, age4, income) %>% 
      rename(age=age4) %>% 
      dmap_if(~class(.x)=="numeric", ~round(., 3))

rr_table_all_uw <- rr_all %>% 
      select(condition, age4, income, rel_risk_uw, rr_uw_lower, rr_uw_upper) %>% 
      arrange(condition, age4, income) %>% 
      rename(age=age4) %>% 
      dmap_if(~class(.x)=="numeric", ~round(., 3))


unique(rr_table_all_uw$condition)


# logistic regression models
regression_table <- function(df){
      df$has_doctor = 1-df$no_doctor
      
      df <- df %>% 
            mutate(has_doctor = 1-no_doctor, 
                   chronic_illness = pmax(arthritis, asthma, copd, depression, diabetes, heart_attack,
                                         kidney_disease, other_cancer, skin_cancer, stroke)) %>% 
            rename(age = age4)
      
      # table(df$chronic_illness)
      # xtabs(swt ~ chronic_illness, data = df) %>% as.matrix() %>%  gmodels::CrossTable()
      # dfh <- head(df)
      
      m1 <- glm(chronic_illness ~ age + income + has_doctor, data = df, family = "binomial")
      # summary(m1)
      # coef(m1) %>% exp() %>% .["has_doctor"]
      # stderr(coef(m1))
      
      sm <- coef(summary(m1)) %>% as.data.frame() 
      lr_table <- tibble(parameter = rownames(sm),
             odds_ratio = exp(sm$Estimate),
             or_lower = exp(sm$Estimate - 1.96*sm$`Std. Error`),
             or_upper = exp(sm$Estimate + 1.96*sm$`Std. Error`)) %>% 
            filter(parameter != "(Intercept)") %>% 
            dmap_if(~class(.x) == "numeric", ~round(.,3))
      lr_table
}

lr_tables <- map(list(bfs11=bfs11,bfs12=bfs12,bfs13=bfs13,bfs14=bfs14,bfs15=bfs15,bfs_all=bfs_all), regression_table)      
lr_tables      

?do.call
 doc_or <- do.call(rbind, lr_tables) %>% filter(parameter == "has_doctor")
doc_or$year <- factor(1:6, levels = 1:6, labels = c(2011:2015,"All years"))
doc_or

ggplot(doc_or, aes(y = odds_ratio, x = year, ymin = or_lower, ymax = or_upper)) + 
      geom_point() + geom_errorbar(width = .1) +
      geom_abline(slope = 0, intercept = 1) +
      scale_y_continuous(limits = c(1, 3)) + 
      ggtitle("Odds of being diagnosed with a chronic illness") +
      xlab("") + ylab("Odds ratio (has doctor vs. no doctor)") +
      theme_minimal()
ggsave("./plots/Odds ratio plot.png")
            ?scale_y_continuous
