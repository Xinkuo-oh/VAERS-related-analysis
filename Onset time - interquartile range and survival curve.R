library(survival)
library(survminer)
library(ggplot2)
library(readxl)

# Read data
df <- read_xlsx("C:/Users/kuo/Desktop/vaers/1.PNEUMO/onset time.xlsx")

# Fit survival curve
fit <- survfit(Surv(NUMDAYS, STATUS) ~ 1 , data = df)

# Calculate median survival time
median_time <- median(df$NUMDAYS, na.rm = TRUE)
# Calculate the survival time corresponding to the 25th percentile of time data
q1 <- summary(fit, times = quantile(df$NUMDAYS, 0.25))$time
# Calculate the survival time corresponding to the 75th percentile of time data
q3 <- summary(fit, times = quantile(df$NUMDAYS, 0.75))$time
# Combine the 25th percentile and 75th percentile into a string to represent the interquartile range
iqr <- paste0(q1, "-", q3)

# Use ggsurvplot function to draw survival curve
ggsurvplot(  fit,  
             fun = "event",  
             conf.int = TRUE, 
             xlim = c(0, 360),  
             break.time.by = 30,  
             xlab = "Time to Event (days)",  
             ylab = "Cumulative Percent (%)",  
             legend = "none", 
             ggtheme = theme_minimal(base_size = 14), 
             risk.table = FALSE,  
             risk.table.col = "black", 
             risk.table.y.text = FALSE,  
             tables.theme = theme_void(), 
             tables.height = 0.2,  
             cumevents = FALSE, 
             cumcensor = FALSE  
             ) -> p  

# Add percentage format
p$plot <- p$plot +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# Add median survival time annotation
median_info <- paste0("Median Time to Event (days): ",
                      round(median_time, 2), " [IQR ", iqr, "]")

# Add text annotations on graphics
p$plot <- p$plot +
  annotate("text",
           x = 150, y = 0.2,
           label = median_info,
           hjust = 0, size = 6)

# Risk table
# ylab("No. at risk") 
# theme(axis.title.x = element_blank()) 
#p$table <- p$table + 
#  ylab("No. at risk") + 
#  theme(axis.title.x = element_blank())



# Generate graphic object p
print(p)



