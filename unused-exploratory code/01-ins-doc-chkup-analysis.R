## Importing, clean, and organize data


#library(Hmisc)
#BRFSS2015full <- sasxport.get("LLCP2015.XPT")
#BRFSS2014full <- sasxport.get("LLCP2014.XPT")
#BRFSS2013full <- sasxport.get("LLCP2013.XPT")
#BRFSS2012full <- sasxport.get("LLCP2012.XPT")
#BRFSS2011full <- sasxport.get("LLCP2011.XPT")

#t15.data <- BRFSS2015full[,c("hlthpln1", "persdoc2", "checkup1", "x.llcpwt", "x.state")]
#t14.data <- BRFSS2014full[,c("hlthpln1", "persdoc2", "checkup1", "x.llcpwt", "x.state")]
#t13.data <- BRFSS2013full[,c("hlthpln1", "persdoc2", "checkup1", "x.llcpwt", "x.state")]
#t12.data <- BRFSS2012full[,c("hlthpln1", "persdoc2", "checkup1", "x.llcpwt", "x.state")]
#t11.data <- BRFSS2011full[,c("hlthpln1", "persdoc2", "checkup1", "x.llcpwt", "x.state")]

#write.csv(t15.data, '2015 BRFSS Data.csv')
#write.csv(t14.data, '2014 BRFSS Data.csv')
#write.csv(t13.data, '2013 BRFSS Data.csv')
#write.csv(t12.data, '2012 BRFSS Data.csv')
#write.csv(t11.data, '2011 BRFSS Data.csv')

#rm(BRFSS2011full, BRFSS2012full, BRFSS2013full, BRFSS2014full, BRFSS2015full)

t15.data <- data.frame(read.csv("2015 BRFSS Data.csv")[,2:6])
t14.data <- data.frame(read.csv("2014 BRFSS Data.csv")[,2:6])
t13.data <- data.frame(read.csv("2013 BRFSS Data.csv")[,2:6])
t12.data <- data.frame(read.csv("2012 BRFSS Data.csv")[,2:6])
t11.data <- data.frame(read.csv("2011 BRFSS Data.csv")[,2:6])




### Cleaning data ###
cleaner <- function(x){
      df <- x
      for(i in 1:5){
            df <- df[!is.na(df[,i]),] 
      }
      
      for(i in 1:3){
            df <- df[df[,i] != 7 & df[,i] != 9,] 
      }
      
      
      df
}

t11.data <- cleaner(t11.data)
t12.data <- cleaner(t12.data)
t13.data <- cleaner(t13.data)
t14.data <- cleaner(t14.data)
t15.data <- cleaner(t15.data)

# Since we are only interested in whether or not respondents have a personal doctor we will treat those who reported having multiple doctors and those who reported only having one doctor the same. Now individuals with personal doctors will be coded as '1' and those without as '3'.

persdocmod <- function(x){
      df <- x
      df[df[,2] == 2,2] <- 1
      
      df
}

t11.data <- persdocmod(t11.data)
t12.data <- persdocmod(t12.data)
t13.data <- persdocmod(t13.data)
t14.data <- persdocmod(t14.data)
t15.data <- persdocmod(t15.data)

allyears <- rbind(t11.data, t12.data, t13.data, t14.data, t15.data)


### Create Summary Tables with Weighted Values ###


library(dplyr)

persdoc.table <- function(x){
      
      tab <- x %>% group_by(hlthpln1, persdoc2) %>% dplyr::summarize(wt_sum = sum(x.llcpwt)) %>% 
            tidyr::spread(key = persdoc2, value = wt_sum) %>% ungroup()
      
      wf <- length(x$hlthpln1)/sum(x$x.llcpwt)
      
      tab[,2:ncol(tab)] <- tab[,2:ncol(tab)]*wf
      
      tab
}

ST_PD_t11 <- persdoc.table(t11.data)
ST_PD_t12 <- persdoc.table(t12.data)
ST_PD_t13 <- persdoc.table(t13.data)
ST_PD_t14 <- persdoc.table(t14.data)
ST_PD_t15 <- persdoc.table(t15.data)
ST_PD_ay <- persdoc.table(allyears)

checkup.table <- function(x){
      
      tab <- x %>% group_by(hlthpln1, checkup1) %>% dplyr::summarize(wt_sum = sum(x.llcpwt)) %>% 
            tidyr::spread(key = checkup1, value = wt_sum) %>% ungroup()
      
      wf <- length(x$hlthpln1)/sum(x$x.llcpwt)
      
      tab[,2:ncol(tab)] <- tab[,2:ncol(tab)]*wf
      
      tab
}

ST_CU_t11 <- checkup.table(t11.data)
ST_CU_t12 <- checkup.table(t12.data)
ST_CU_t13 <- checkup.table(t13.data)
ST_CU_t14 <- checkup.table(t14.data)
ST_CU_t15 <- checkup.table(t15.data)
ST_CU_ay <- checkup.table(allyears)

ST_CUgPD_t11 <- checkup.table(t11.data[t11.data$persdoc2 == 1,])
ST_CUgPD_t12 <- checkup.table(t12.data[t12.data$persdoc2 == 1,])
ST_CUgPD_t13 <- checkup.table(t13.data[t13.data$persdoc2 == 1,])
ST_CUgPD_t14 <- checkup.table(t14.data[t14.data$persdoc2 == 1,])
ST_CUgPD_t15 <- checkup.table(t15.data[t15.data$persdoc2 == 1,])
ST_CUgPD_ay <- checkup.table(allyears[allyears$persdoc2 == 1,])



PD.props <- function(x){
      vec <- rep(0,4)
      c <- 0
      for(i in 1:2){
            for(j in 2:3){
                  c <- c + 1
                  vec[c] <- as.numeric(x[i,j]/sum(x[i,2:3]))   
            }
      }
      matrix(vec, nrow = 2, ncol = 2, byrow = TRUE)
}


PD11 <- PD.props(ST_PD_t11)
PD12 <- PD.props(ST_PD_t12)
PD13 <- PD.props(ST_PD_t13)
PD14 <- PD.props(ST_PD_t14)
PD15 <- PD.props(ST_PD_t15)
PDay <- PD.props(ST_PD_ay)

PD.proportions <- matrix(c(PD11[,1],PD12[,1],PD13[,1],PD14[,1],PD15[,1],PDay[,1], 0, 0), nrow = 2, ncol = 7,
                         dimnames = list(c("Insured", "Uninsured"),
                                         c("2011", "2012", "2013", "2014", "2015", "All Years", "Empty"))
)

png("plots/Proportion_of_Individuals_with_a_Personal_Doctor.png",
    height = 240, width = 480)

par(mar = c(4, 2.5, 3, 2.5))
barplot(PD.proportions, beside = TRUE, 
        names.arg = c("2011", "2012", "2013", "2014", "2015", "All Years", ""),
        main = c('Proportion of Individuals with a Personal Doctor'), 
        col = c('green', 'forestgreen')
)
legend("topright",
       inset=0,
       cex = 1,
       c("Insured","Uninsured"),
       pch=20,
       pt.cex = 2,
       col=c("green","forestgreen"),
       bty = "n"
)
abline(h=0)

dev.off()




