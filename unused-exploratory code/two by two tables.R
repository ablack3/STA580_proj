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




# any health coverage = yes :: hlthpln1 = 1 
xtabs(x.llcpwt ~ hlthpln1, data=wd) %>% as.matrix() %>% gmodels::CrossTable()

# have personal doctor :: persdoc2 1:Yes one ;; 2:Yes >1 ;; 3:No
xtabs(x.llcpwt ~ persdoc2, data=wd) %>% as.matrix() %>% gmodels::CrossTable()


# income 5 cat::  x.incomg 
xtabs(x.llcpwt ~ x.incomg, data=wd) %>% as.matrix() %>% gmodels::CrossTable()
xtabs(x.llcpwt ~ income, data=wd) %>% as.matrix() %>% gmodels::CrossTable()


# age 5yr cat :: x.ageg5yr

xtabs(x.llcpwt ~ agegrp, data=wd) %>% as.matrix() %>% gmodels::CrossTable()

BRFSS2015full %>% filter(x.ageg5yr %in% 1:13) %>% 
      xtabs(x.llcpwt ~ x.ageg5yr, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# chronic condition diagnosis

# told have hypertension;; 1=No, 2=Yes
BRFSS2015full %>% filter(x.rfhype5 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ x.rfhype5, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# told have diabetes;; 1=No, 2=Yes
BRFSS2015full %>% filter(diabete3 %in% 1:4) %>% 
      xtabs(x.llcpwt ~ diabete3, data=.) %>% as.matrix() %>% gmodels::CrossTable()


# told have MI/CHD;; x.michd:: 1=Yes, 2=No
BRFSS2015full %>% #filter(x.rfhype5 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ x.michd, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# ever told they have asthma :: x.LTASTH1 1=no  2=yes 9=missing
BRFSS2015full %>% #filter(x.rfhype5 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ x.ltasth1, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# ever told they have arthritis:: x.DRDXAR1 1=yes 2=no
BRFSS2015full %>% #filter(x.rfhype5 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ x.drdxar1, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# health status:: x.rfhlth 1=good 2=bad 9=missing
BRFSS2015full %>% #filter(x.rfhype5 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ x.rfhlth, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# copd:: chccopd1 1=Y 2=N 7,9=missing
BRFSS2015full %>% filter(chccopd1 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ chccopd1, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# depression :: ADDEPEV2 1=Y, 2=N, 7,9 = missing
BRFSS2015full %>% filter(addepev2 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ addepev2, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# stroke :: 1=y 2=n 7,9=.
BRFSS2015full %>% filter(cvdstrk3 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ cvdstrk3, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# heart atttack :: 1=y, 2=n, 7,9=.
BRFSS2015full %>% filter(cvdinfr4 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ cvdinfr4, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# kidney disease :: 1=y, 2=n
BRFSS2015full %>% filter(chckidny %in% 1:2) %>% 
      xtabs(x.llcpwt ~ chckidny, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# arthritis 1=y 2=n 7,9=.
BRFSS2015full %>% filter(havarth3 %in% 1:2) %>% 
      xtabs(x.llcpwt ~ havarth3, data=.) %>% as.matrix() %>% gmodels::CrossTable()


# skin cancer CHCSCNCR
BRFSS2015full %>% filter(chcscncr %in% 1:2) %>% 
      xtabs(x.llcpwt ~ chcscncr, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# cancer other than skin: 
BRFSS2015full %>% filter(chcocncr %in% 1:2) %>% 
      xtabs(x.llcpwt ~ chcocncr, data=.) %>% as.matrix() %>% gmodels::CrossTable()

# two by two tables
# any helath coverage = yes :: hlthpln1 = 1 
xtabs(x.llcpwt ~ hlthpln1, data=wd) %>% as.matrix() %>% gmodels::CrossTable()



BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")
BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")
BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")
BRFSS2015full <- Hmisc::sasxport.get("../brfs/2015/LLCP2015.XPT")





# need to collapse

# ok looking good
# working data
wd <- BRFSS2015full %>% 
      mutate(insurance = ifelse(hlthpln1 %in% c(1,2), hlthpln1, NA)) %>% 
      mutate(no_doctor = ifelse(persdoc2 == 3, 1, ifelse(persdoc2 %in% 1:2, 2, NA))) %>% 
      mutate(income = ifelse(x.incomg %in% 1:2, 1,
                      ifelse(x.incomg %in% 3:4, 2,
                      ifelse(x.incomg == 5, 3, NA)))) %>%
      mutate(income = factor(income, 1:3, labels = c("<25K", "25-50K", ">50K"))) %>% 
      mutate(age = ifelse(x.ageg5yr %in% 5:13, x.ageg5yr, NA), # drop under 40
             age4 = ifelse(x.ageg5yr %in% 5:7, 1, 
                    ifelse(x.ageg5yr %in% 8:9, 2, 
                    ifelse(x.ageg5yr %in% 10:11, 3,
                    ifelse(x.ageg5yr %in% 12:13, 4, NA)))),
             age4 = factor(age4, levels = 1:4, labels = c("40-54", "55-64","65-75","75+")),
             diabetes = ifelse(diabete3 == 1, 1, 
                        ifelse(diabete3 %in% 2:4, 2, NA)),
             health_status = ifelse(x.rfhlth %in% 1:2, x.rfhlth, NA),
             hypertension = ifelse(x.rfhype5 %in% 1:2, x.rfhype5, NA),
             mi_chd = ifelse(x.michd %in% 1:2, x.michd, NA),
             asthma = ifelse(x.ltasth1 %in% 1:2, x.ltasth1, NA),
             arthritis = ifelse(x.drdxar1 %in% 1:2, x.drdxar1, NA),
             copd = ifelse(chccopd1 %in% 1:2, chccopd1, NA),
             depression = ifelse(addepev2 %in% 1:2, addepev2, NA),
             stroke = ifelse(cvdstrk3 %in% 1:2, cvdstrk3, NA),
            # mi = ifelse(cvdinfr4 %in% 1:2, cvdinfr4, NA),
             kidney_disease = ifelse(chckidny %in% 1:2, chckidny, NA),
             arthritis = ifelse(havarth3 %in% 1:2, havarth3, NA),
             skin_cancer = ifelse(chcscncr %in% 1:2, chcscncr, NA),
             other_cancer = ifelse(chcocncr %in% 1:2, chcocncr, NA),
             swt = x.llcpwt, year = iyear) 

names(wd) %>% length()

# subset variables
wd2 <- wd %>% select(331:ncol(wd)) %>% select(-health_status)
names(wd2)

xtabs(~age4+no_doctor, data=wd2) %>% as.matrix() %>% gmodels::CrossTable()

# reverse code asthsma hypertension and personal doc
wd3 <- wd2 %>% mutate(asthma = 3 - asthma, hypertension = 3 - hypertension)

# collpase doctor=y categories

# library(epitools)
names(wd3)
# wd3 %>% xtabs(swt ~  mi_chd + insurance, data=.) %>% #as.matrix() %>%
#       riskratio(method = "wald")      
#       gmodels::CrossTable()
# ?riskratio


library(tidyr)
names(wd3)
wd4 <- wd3 %>% gather("condition","level", hypertension, mi_chd, skin_cancer, other_cancer,
                      asthma, arthritis, copd, depression, stroke, kidney_disease, diabetes) %>%
      select(-insurance, -year) %>% 
      group_by(no_doctor, income, age4, condition, level) %>% summarise(wt_freq = sum(swt)) %>% 
      filter(!is.na(level)) %>% 
      ungroup() %>% 
      group_by(no_doctor, income, age4, condition) %>% 
      mutate(total = sum(wt_freq), risk = round(wt_freq/total, 3)) %>% ungroup()

wd5 <- wd4 %>% filter(level ==  1)
wd5 <- wd5[complete.cases(wd5),]
# split dataset based on no_doc
wd6 <- split(wd5, wd5$no_doctor)


table(wd5$no_doctor)
names(wd6[[1]])
table(wd6[[1]]$income)
table(wd6[[2]]$income)

table(wd6[[1]]$age4)
table(wd6[[2]]$age4)

table(wd6[[1]]$level)
table(wd6[[2]]$level)

wd7 <- full_join(wd6[[1]], wd6[[2]], by = c("age4", "income", "condition")) %>% 
      mutate(rel_risk = risk.y/risk.x, condition = factor(condition))


str(wd7)
# wd7$rel_risk <- wd7$risk.y/wd7$risk.x

#define factors
# table(wd7$age)
# wd7$age2 <- factor(wd7$age, levels = 5:13, labels = c(paste0(seq(40,75,5),"-",seq(40,75,5)+4),"80+"))

# wd7$condition <- factor(wd7$condition)
table(wd7$income)

names(wd7)
# wd7$income <- factor(wd7$income, levels=1:5, labels = c("<15K","15-25K","25-35K","35-50K",">50K"))

table(wd7$condition)
class(wd7$no_doctor.x)
wd7 <- ungroup(wd7)
library(ggplot2)

conditions <- unique(wd7$condition)
cond <- conditions[1]

rr_plot <- function(cond){
pd <- wd7 %>% filter(condition == cond) 
plot <- ggplot(pd, aes(x=age4, y=rel_risk, color = income, group = income)) +
      geom_point() + geom_line() + ggtitle(cond) + geom_abline(slope = 0, intercept = 1, color = "black") +
            scale_color_brewer(palette = "YlOrRd", direction = -1) +
            scale_y_continuous(limits = c(.5,max(pd$rel_risk)+.5))
ggsave(paste0("./plots/",cond,".png"), plot=plot)
}

max(wd7$rel_risk)

rr_plot(conditions[1])
rr_plot(conditions[2])
rr_plot(conditions[3])

library(RColorBrewer)
display.brewer.all()
pal <- brewer.pal(n=5, "YlOrRd")
display.brewer.pal(n=5, "YlOrRd")

walk(conditions, rr_plot)

walk(2, ~rr_plot(conditions[.]))

?geom_smooth

test <- wd5 %>% filter(income==1, condition=="other_cancer", no_doctor %in% 1:2, age %in% 6:13) %>% ungroup()

num <- test %>% filter(no_doctor ==2) %>% select(-no_doctor)
denom <- test %>% filter(no_doctor==1) %>% select(-no_doctor)

rr <- left_join(num, denom, by = c("income","age","condition","level")) %>% 
      mutate(rr = risk.x/risk.y)
View(rr)

pal <- brewer.pal(4,"YlOrRd")[2:4] %>% rev()
plot <-  ggplot(wd7, aes(x=age4, y=rel_risk, color = income, group = income)) +
      geom_point() + geom_line() + facet_wrap(~condition, nrow = 3, ncol = 4) +
      ggtitle("Relative Risk of being diagnosed with chronic condition") +
      geom_abline(slope = 0, intercept = 1, color = "black") +
      scale_color_manual(values = pal) +
      scale_y_continuous(limits = c(.5,max(wd7$rel_risk)+.1), breaks = 1:6 ) 
      
ggsave(paste0("./plots/","facet_plot2014",".png"), plot=plot, width = 10, height = 8)

?scale_y_continuous
names(wd4)


View(head(wd4,100))

grep("rfh",vars, ignore.case = T, value = T)
vars[c("hlthpln1","persdoc2","x.rfhype5","x.michd")]

xtabs(~ hlthpln1, data=BRFSS2015full)
xtabs(x.llcpwt ~ hlthpln1, data=BRFSS2015full)

tot_wt <- sum(BRFSS2015full$x.llcpwt)
tabl <- xtabs(x.llcpwt ~ hlthpln1 + persdoc2, data=BRFSS2015full)/tot_wt %>% round(.,2)
round(tabl,3)

 xtabs(~persdoc2, data=BRFSS2015full)

summary_table <- xtabs(x.llcpwt ~ hlthpln1 + checkup1, data = BRFSS2015full)/tot_wt
summary_table %>% round(2)
rnorm(4) %>% matrix(2,2) %>%  round(2)

chisq.test(summary_table)



?gmodels::CrossTable





