
#new_path <- file.path("C:", "", "", "", "")
#setwd(new_path)


par(mfrow=c(3,3))

# Load necessary libraries
library(readxl)
library(multispatialCCM)

set.seed(123)

# Read in your data


temden <- read_excel("data_Zhejiang.xlsx")

# Clear workspace and set plotting parameters
#rm(list = ls())
par(mfrow=c(3,3))


results <- data.frame(
  Factor = character(),
  Factor_maps_Cases_pval = numeric(),
  Cases_map_Factor_pval = numeric(),
  stringsAsFactors = FALSE
)


# Prepare data for CCM
Accm <- temden$Zhejiang_Tx
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]


results <- rbind(results, data.frame(
  Factor = "Tx",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))


#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("Tx maps cases" )
legend_text_B <- paste("cases maps Tx" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)


# Prepare data for CCM
Accm <- temden$Zhejiang_T
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "T",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("T maps cases" )
legend_text_B <- paste("cases maps T" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)


# Prepare data for CCM
Accm <- temden$Zhejiang_Tn
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "Tn",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("Tn maps cases" )
legend_text_B <- paste("cases maps Tn" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)



# Prepare data for CCM
Accm <- temden$Zhejiang_Td
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "Td",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("Td maps cases" )
legend_text_B <- paste("cases maps Td" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)

# Prepare data for CCM
Accm <- temden$Zhejiang_P
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "P",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("P maps cases" )
legend_text_B <- paste("cases maps P" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)


# Prepare data for CCM
Accm <- temden$Zhejiang_RRR
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "RRR",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("RRR maps cases" )
legend_text_B <- paste("cases maps RRR" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)

# Prepare data for CCM
Accm <- temden$Zhejiang_VV
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "VV",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("VV maps cases" )
legend_text_B <- paste("cases maps VV" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)




# Prepare data for CCM
Accm <- temden$Zhejiang_U
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "U",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("U maps cases" )
legend_text_B <- paste("cases maps U" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)



# Prepare data for CCM
Accm <- temden$Zhejiang_Ff
Bccm <- temden$Zhejiang_cases
maxE <- 10  # Maximum E to test
Emat <- matrix(nrow=maxE-1, ncol=2)  # Matrix for storing output
colnames(Emat) <- c("A", "B")

# Loop over potential E values
for(E in 2:maxE) {
  Emat[E-1, "A"] <- SSR_pred_boot(A=Accm, E=E, predstep=1, tau=1)$rho
  Emat[E-1, "B"] <- SSR_pred_boot(A=Bccm, E=E, predstep=1, tau=1)$rho
}

# Determine optimal E values
E_A <- which(Emat[,1] == max(Emat[,1])) + 1
E_B <- which(Emat[,2] == max(Emat[,2])) + 1

# Check signals
signal_A_out <- SSR_check_signal(A=Accm, E=E_A, tau=1, predsteplist=1:10)
signal_B_out <- SSR_check_signal(A=Bccm, E=E_B, tau=1, predsteplist=1:10)

# CCM analysis
CCM_boot_A <- CCM_boot(Accm, Bccm, E_A, tau=1, iterations=200)
CCM_boot_B <- CCM_boot(Bccm, Accm, E_B, tau=1, iterations=200)

# Significance test
CCM_significance_test<-ccmtest(CCM_boot_A,CCM_boot_B)
CCM_significance_test

p_value_A <- CCM_significance_test[1]
p_value_B <- CCM_significance_test[2]

results <- rbind(results, data.frame(
  Factor = "Ff",
  Factor_maps_Cases_pval = p_value_A,
  Cases_map_Factor_pval = p_value_B
))

#Plot results
plotxlimits<-range(c(CCM_boot_A$Lobs, CCM_boot_B$Lobs))
plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=1, lwd=2,
     xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
     xlab="L", ylab="rho")
#Add +/- 1 standard error
matlines(CCM_boot_A$Lobs,
         cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
               CCM_boot_A$rho+CCM_boot_A$sdevrho),
         lty=3, col=1)
#Plot "B causes A"
lines(CCM_boot_B$Lobs, CCM_boot_B$rho, type="l", col=2, lty=2, lwd=2)
#Add +/- 1 standard error
matlines(CCM_boot_B$Lobs,
         cbind(CCM_boot_B$rho-CCM_boot_B$sdevrho,
               CCM_boot_B$rho+CCM_boot_B$sdevrho),
         lty=3, col=2) 


legend_text_A <- paste("Ff maps cases" )
legend_text_B <- paste("cases maps Ff" )

legend("topleft",
       c(legend_text_A, legend_text_B),
       lty=c(1,2), col=c(1,2), lwd=2, bty="n",cex=1.5)

