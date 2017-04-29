png("plots/Google_Slides_Difference_in_Doctor_Utilization.png",
    height = 620, width = 620)

x <- cbind(DU.l1.proportions, c(0,0))

par(mfrow=c(2,1))
barplot(x, beside = TRUE, 
        names.arg = c("2011", "2012", "2013", "2014", "2015", "All Years", ""),
        main = c('Checkup within 1 Year'), 
        col = c('green', 'forestgreen'),
        width = .9, space = c(0,1.5)
)
abline(h=0)

legend("topright",
       inset=0,
       cex = 1.2,
       c("Insured","Uninsured"),
       pch=20,
       pt.cex = 2,
       col=c("green","forestgreen"),
       bty = "n"
)

barplot(DU.g1.proportions[,1:6], beside = TRUE, 
        names.arg = c("2011", "2012", "2013", "2014", "2015", "All Years"),
        main = c('No Checkup within 5 Years'), 
        col = c('green', 'forestgreen'),
        width = .9, space = c(0,1.5)
)
abline(h=0)

dev.off()
