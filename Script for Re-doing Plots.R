#### Personal Doctor ####

png("plots/Redo_PD.png",
    height = 400, width = 960)

par(mfrow = c(1,2))

plot(x = c(1:6), y = Persdoc.pdif.95CI[,2], 
     main = "Absolute Difference of Proportion of Persons with a Personal Doctor \n Insured v.s. Uninsured", 
     ylab = "Absolute Difference",
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(0.44, 0.46), 
     pch=20, col = "red",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "All Years"), cex.axis = 1)
axis(2, at = seq(from = .44, to = .46, by = 0.01), las = 2, cex.axis = .8)

segments(c(1:6), Persdoc.pdif.95CI[,1], c(1:6), Persdoc.pdif.95CI[,3], 
         lwd = 1.5)

arrows(c(1:6), Persdoc.pdif.95CI[,1], c(1:6), Persdoc.pdif.95CI[,3], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

##

plot(x = c(1:6), y = PD.RR[,2], 
     main = "Relative Difference of Proportion of Persons with a Personal Doctor \n Insured v.s. Uninsured", 
     ylab = "Relative Difference",
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(2.00, 2.25), 
     pch=20, col = "red",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "All Years"), cex.axis = 1)
axis(2, at = seq(from = 2.00, to = 2.25, by = 0.05), las = 2, cex.axis = .8)


segments(c(1:6), as.numeric(PD.RR[,1]), c(1:6), as.numeric(PD.RR[,3]), lwd = 1.5)
arrows(c(1:6), as.numeric(PD.RR[,1]), c(1:6), as.numeric(PD.RR[,3]), lwd = 1.5, angle = 90, code = 3, length = 0.03)

dev.off()


#######################################################################
######################### Checkup Within Year #########################
#######################################################################


png("plots/Redo_CU1Y.png",
    height = 540, width = 960)


par(mfrow = c(1,2))

plot(x = c(1:6), y = DU[,2], 
     sub = "Absolute Difference between Insured and Uninsured", 
     ylab = "Absolute Difference",
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(.186, .350), 
     pch=20, col = "darkgreen",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()


segments(c(1:6), DU[,1], c(1:6), DU[,3], 
         lwd = 1.5)

arrows(c(1:6), DU[,1], c(1:6), DU[,3], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)


points(x = c(1:6), y = DUgPD[,2], 
     pch=20, col = "red")

segments(c(1:6), DUgPD[,1], c(1:6), DUgPD[,3], 
         lwd = 1.5)

arrows(c(1:6), DUgPD[,1], c(1:6), DUgPD[,3], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "All Years"), cex.axis = 1)
axis(2, at = seq(from = .185, to = .350, by = .02), las = 2, cex.axis = .8)

legend(x = 1, y = .3,
       inset=0,
       cex = 1,
       c("All","Personal Doctor"),
       pch=20,
       pt.cex = 1,
       col=c("darkgreen","red"),
       text.width = 1.3
)

##

plot(x = c(1:6), y = DU[,5], 
     sub = "Relative Difference between Insured and Uninsured", 
     ylab = "Relative Difference",
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(1.28, 1.92), 
     pch=20, col = "darkgreen",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()


segments(c(1:6), DU[,4], c(1:6), DU[,6], 
         lwd = 1.5)

arrows(c(1:6), DU[,4], c(1:6), DU[,6], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)


points(x = c(1:6), y = DUgPD[,5], 
       pch=20, col = "red")

segments(c(1:6), DUgPD[,4], c(1:6), DUgPD[,6], 
         lwd = 1.5)

arrows(c(1:6), DUgPD[,4], c(1:6), DUgPD[,6], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "All Years"), cex.axis = 1)
axis(2, at = seq(from = 1.25, to = 1.95, by = .05), las = 2, cex.axis = .8)

mtext('Difference in Doctor Utilization between Insured and Uninsured:\n Proportion who have had a Checkup within 1 Year', 
      outer = T, font = 2, cex = 1.2, adj = .5, padj = 1.5)

dev.off()


#######################################################################
###################### NO Checkup Within 5 Years ######################
#######################################################################


png("plots/Redo_CU5Y.png",
    height = 540, width = 960)


par(mfrow = c(1,2))

plot(x = c(1:6), y = DU[,8], 
     sub = "Absolute Difference between Insured and Uninsured", 
     ylab = "Absolute Difference",
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(.075, .190), 
     pch=20, col = "darkgreen",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()


segments(c(1:6), DU[,7], c(1:6), DU[,9], 
         lwd = 1.5)

arrows(c(1:6), DU[,7], c(1:6), DU[,9], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)


points(x = c(1:6), y = DUgPD[,8], 
       pch=20, col = "red")

segments(c(1:6), DUgPD[,7], c(1:6), DUgPD[,9], 
         lwd = 1.5)

arrows(c(1:6), DUgPD[,7], c(1:6), DUgPD[,9], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "All Years"), cex.axis = 1)
axis(2, at = seq(from = .070, to = .190, by = .01), las = 2, cex.axis = .8)

legend(x = 1, y = .170,
       inset=0,
       cex = 1,
       c("All","Personal Doctor"),
       pch=20,
       pt.cex = 1,
       col=c("darkgreen","red"),
       text.width = 1.3
)

##

plot(x = c(1:6), y = DU[,11], 
     sub = "Relative Difference between Insured and Uninsured", 
     ylab = "Relative Difference",
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(2.95, 4.10), 
     pch=20, col = "darkgreen",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()


segments(c(1:6), DU[,10], c(1:6), DU[,12], 
         lwd = 1.5)

arrows(c(1:6), DU[,10], c(1:6), DU[,12], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)


points(x = c(1:6), y = DUgPD[,11], 
       pch=20, col = "red")

segments(c(1:6), DUgPD[,10], c(1:6), DUgPD[,12], 
         lwd = 1.5)

arrows(c(1:6), DUgPD[,10], c(1:6), DUgPD[,12], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "All Years"), cex.axis = 1)
axis(2, at = seq(from = 2.90, to = 4.10, by = .1), las = 2, cex.axis = .8)

mtext('Difference in Doctor Utilization between Uninsured and Insured:\n Proportion who have not had a Checkup within 5 Years', 
      outer = T, font = 2, cex = 1.2, adj = .5, padj = 1.5)

dev.off()