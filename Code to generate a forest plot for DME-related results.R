library(tidyverse)
library(readxl)
library(forestploter)
library(grid)

# Read data
d <- read_xlsx('C:/Users/kuo/Desktop/vaers/1.PNEUMO/DME.xlsx')

d$` ` <- paste(rep(" ", 100), collapse = " ")

tm <- forest_theme(
  base_size = 8,   
  ci_pch = 15, 
  ci_col = "#2F4F96", 
  ci_lty = 1,    
  ci_lwd = 1.5,  
  ci_Theight = 0.2, 
  
  
  refline_lwd = 1.5,  
  refline_lty = "dashed", 
  refline_col = "red",
  
  
  footnote_cex = 1.1,  
  footnote_fontface = "italic",  
  footnote_col = "blue"  
)


dt <- d[, c(18, 1,  2, 20, 28, 6, 7, 8)]


dt <- dt %>%
  arrange(-a) 



rightmost <- 5

uug <- uu <- dt$RORU

llg <- ll <- dt$RORL

uug[uu >= rightmost] <- rightmost * 1.001

llg[ll >= rightmost] <- rightmost * 0.999

colnames(dt)[1] <- "SOC"


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


arr <- segmentsGrob(
  x0 = unit(1, 'npc') - unit(1, 'mm'),  
  x1 = 1,  
  y0 = 0.5, 
  y1 = 0.5, 
  gp = gpar(fill = '#2F4F96', col = 'transparent'), 
  arrow = arrow(length = unit(0.15, "inches"), ends = "last", type = "closed")  
)


library(gtable)


sapply(p$grobs, function(x) {
  inherits(x, 'makeci')
}) -> pan


which(pan) -> modi


p$layout -> la


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



p_wh <- get_wh(plot = p)


cairo_pdf("DME.pdf", width = p_wh[1], height = p_wh[2])


plot(p)


dev.off()

