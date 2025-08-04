library(data.table)
library(ggplot2)
library(readxl)
library(ggrepel)
library(readr)
library(dplyr)

# Read data
d <- read_xlsx("C:/Users/kuo/Desktop/vaers/1.PNEUMO/SOC.xlsx")

# Convert data box d into a data.table object
setDT(d)

# Perform row by row operations on columns a, b, c, and d in the dataset to calculate the p-value of the chi square test
pvalues <- apply(d[, .(a, b, c, d)], 1, function(x) {
  
  inter <- matrix(x, nrow = 2)
  
  chisq.test(inter)$p.value
})

# Add a new column 'p-value' to the dataset
d[, pvalue := pvalues]

# Add a new column 'AdjustedP' to the dataset
d[, adjustedP := p.adjust(pvalue, method = 'fdr')]

# Add a new column log2rr to the dataset
d[, log2ror := log2(ROR)]

# Add a new column 'direction' to the dataset and determine the direction based on the positive or negative sign of log2ror
d[, direction := as.character(sign(log2ror))]

data <- d

# Add a logpadj column to dataset data computing -log10(adjusted p-value).
data$logpadj <- -log10(data$adjustedP)


data <- data %>% rename(case = a)

# Create a volcano map using the ggplot2 package
pp <- ggplot(data, aes(x = log2ror, y = logpadj)) +
  
  geom_point(aes(color = case), size = 3) +
  
  geom_label_repel(aes(label = soc_name_en, color = case), max.overlaps = 100) +
 
  scale_color_gradient(low = "#45BDD8", high = "#E24E3A") +
 
  labs(x = "log ROR", y = "-log(p-adjust)", color = "Number of Cases") +
  
  geom_vline(xintercept = 0, lty = 2, col = "black", lwd = 0.8) +
  
  geom_hline(yintercept = -log10(0.05), lty = 2, col = "black", lwd = 0.8) +
  
  theme_bw()

print(pp)

ggsave('pp.png', plot = pp, dpi = 600, width = 10, height = 10)






















