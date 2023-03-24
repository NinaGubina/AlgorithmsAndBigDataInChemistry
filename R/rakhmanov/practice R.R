data_path <- "/Users/trrak/OneDrive/Рабочий стол/correlation_causation_practice/data.rds"
data <- readRDS(data_path)

attach(data)


plot(A, B, main="Scatterplot A, B", xlab="A", ylab="B")

#In this code, the "alternative" parameter means that there is a significant correlation between 
#two variables in either direction (positive or negative) and not.

correlation.results <- cor.test(A, B, method="pearson", alternative="two.sided")

correlation.results$estimate
correlation.results$p.value

# works like help
?cor.test

?lm
ac.regression <- lm(as.formula("A ~ C"), data = data)
ac.residuals <- residuals(ac.regression)

bc.regression <- lm(as.formula("B ~ C"), data = data)
bc.residuals <- residuals(bc.regression)

plot(ac.residuals, bc.residuals, main="Residuals A ~ C, B ~ C", xlab="A ~ C", ylab="B ~ C")

correlation.results <- cor.test(ac.residuals, bc.residuals)


# allows you to install the library pcalg
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install(version = "3.16")

#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("RBGL")

#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Rgraphviz")

library(pcalg)
?pc
?cor

suffStat_list <- list(C = cor(data), n = nrow(data))

pc.results.1 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 1, labels = colnames(data), verbose = TRUE)
pc.results.2 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.9, labels = colnames(data), verbose = TRUE)
pc.results.3 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.8, labels = colnames(data), verbose = TRUE)
pc.results.4 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.7, labels = colnames(data), verbose = TRUE)
pc.results.5 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.6, labels = colnames(data), verbose = TRUE)
pc.results.6 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.5, labels = colnames(data), verbose = TRUE)
pc.results.7 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.4, labels = colnames(data), verbose = TRUE)
pc.results.8 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.3, labels = colnames(data), verbose = TRUE)
pc.results.9 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.2, labels = colnames(data), verbose = TRUE)
pc.results.10 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.1, labels = colnames(data), verbose = TRUE)
pc.results.11 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data), verbose = TRUE)
pc.results.12 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.04, labels = colnames(data), verbose = TRUE)
pc.results.13 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.03, labels = colnames(data), verbose = TRUE)
pc.results.14 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.02, labels = colnames(data), verbose = TRUE)
pc.results.15 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.01, labels = colnames(data), verbose = TRUE)
pc.results.16 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.005, labels = colnames(data), verbose = TRUE)
pc.results.17 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.004, labels = colnames(data), verbose = TRUE)
pc.results.18 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.003, labels = colnames(data), verbose = TRUE)
pc.results.19 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.002, labels = colnames(data), verbose = TRUE)
pc.results.20 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.001, labels = colnames(data), verbose = TRUE)
pc.results.21 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.0001, labels = colnames(data), verbose = TRUE)
pc.results.21 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0, labels = colnames(data), verbose = TRUE)

# this pc.results to check value limits
pc.results.22 <- pc(suffStat = suffStat_list, indepTest = gaussCItest, alpha = 0.18, labels = colnames(data), verbose = TRUE)

?pc

?par
par(mfrow = c(5,5))

plot(pc.results.1, main = "Alpha = 1")
plot(pc.results.2, main = "Alpha = 0.9")
plot(pc.results.3, main = "Alpha = 0.8")
plot(pc.results.4, main = "Alpha = 0.7")
plot(pc.results.5, main = "Alpha = 0.6")
plot(pc.results.6, main = "Alpha = 0.5")
plot(pc.results.7, main = "Alpha = 0.4")
plot(pc.results.8, main = "Alpha = 0.3")
plot(pc.results.9, main = "Alpha = 0.2")
plot(pc.results.10, main = "Alpha = 0.1")
plot(pc.results.11, main = "Alpha = 0.05")
plot(pc.results.12, main = "Alpha = 0.04")
plot(pc.results.13, main = "Alpha = 0.03")
plot(pc.results.14, main = "Alpha = 0.02")
plot(pc.results.15, main = "Alpha = 0.01")
plot(pc.results.16, main = "Alpha = 0.005")
plot(pc.results.17, main = "Alpha = 0.004")
plot(pc.results.18, main = "Alpha = 0.003")
plot(pc.results.19, main = "Alpha = 0.002")
plot(pc.results.20, main = "Alpha = 0.001")
plot(pc.results.21, main = "Alpha = 0.0001")
plot(pc.results.21, main = "Alpha = 0")
plot(pc.results.22, main = "Alpha = 0.18")
