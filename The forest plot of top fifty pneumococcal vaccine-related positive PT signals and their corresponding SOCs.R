library(tidyverse)
library(readxl)
library(forestploter)
library(grid)

# Read data
d <- read_xlsx('C:/Users/kuo/Desktop/vaers/1.PNEUMO/PT.xlsx')


# Create a column named " " (space) containing a 100-space string for layout.
d$` ` <- paste(rep(" ", 100), collapse = " ")

# Set the theme style for the forest map
tm <- forest_theme(
  base_size = 8,   
  ci_pch = 15,  
  ci_col = "#2F4F96",  
  ci_lty = 1,     
  ci_lwd = 1.5,  
  ci_Theight = 0.2,  
  
  # Reference line
  refline_lwd = 1.5,  
  refline_lty = "dashed",  
  refline_col = "red",  
  
  # Footnote
  footnote_cex = 1.1,  
  footnote_fontface = "italic",  
  footnote_col = "blue"  
  )

# Read relevant data
dt <- d[, c(18, 1,  2, 20, 28, 6, 7, 8)]

# Sort by the a value of PT
dt <- dt %>%
  arrange(-a) 
# Take the top 50
dt <- dt[1:50, ]

# Define a boundary value of 5 to limit the display range of the confidence interval
rightmost <- 5
# Copy the data from the RORU (upper limit of confidence interval) column into the uu and uug vectors
uug <- uu <- dt$RORU
# Copy the data from the RORL (lower confidence interval) column into the ll and llg vectors
llg <- ll <- dt$RORL

# Adjust the values greater than or equal to rightmost in the upper limit of the confidence interval to rightmost * 1.001
uug[uu >= rightmost] <- rightmost * 1.001
# Adjust the values greater than or equal to rightmost in the lower limit of the confidence interval to rightmost * 0.999
llg[ll >= rightmost] <- rightmost * 0.999

colnames(dt)[1] <- "SOC"


# Using the forest function to draw a forest map
p <- forest(  dt[,1:5],  
              est = dt$ROR,
              lower = llg,  
              upper = uug,  
              sizes = 0.6,  
              ci_column = 5,  
              ref_line = 1,  
              xlim = c(0, rightmost), 
              theme = tm  
              )

# Create an arrow object  - arr
arr <- segmentsGrob(
  x0 = unit(1, 'npc') - unit(1, 'mm'),  
  x1 = 1,  
  y0 = 0.5,  
  y1 = 0.5,  
  gp = gpar(fill = '#2F4F96', col = 'transparent'),  
  arrow = arrow(length = unit(0.15, "inches"), ends = "last", type = "closed")  
  )


library(gtable)

# Determine whether each element in p$gros is of type 'makeci' and store the result in a pan vector
sapply(p$grobs, function(x) {
  inherits(x, 'makeci')
}) -> pan

# Find the index of the element in the pan vector that is TRUE and store it in the mod vector
which(pan) -> modi


p$layout -> la

# Loop through each eligible element
for (ii in 1:length(modi)) {
  ind <- modi[[ii]]  
  
  if (ll[[ii]] > rightmost) p$grobs[[ind]] <- nullGrob()
 
  if (uu[[ii]] > rightmost) {
    p <- gtable_add_grob(
      p,
      grobs = arr,
      t = la[ind, 't'],  
      l = la[ind, 'l'],  
      b = la[ind, 'b'],  
      r = la[ind, 'r']  
    )
  }
}


# Obtain the width and height of forest map p
p_wh <- get_wh(plot = p)


cairo_pdf("forest.pdf", width = p_wh[1], height = p_wh[2])


plot(p)


dev.off()

