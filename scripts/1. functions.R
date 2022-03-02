theme_special <- function(){
  font <- "Tahoma"
  theme_classic() %+replace%
    
    theme(panel.background = element_rect(color = "grey20", fill = NA),
          axis.title.y = element_text(angle = 90, size = 20, margin = margin(t = 0, r = 25, b = 0, l = 0)),
          axis.title.x = element_text( size = 20,margin = margin(t = 25, r = 0, b = 0, l = 0)),
          plot.title = element_text(face="bold",hjust = 0,  vjust = 0,size = 25),          
          plot.subtitle = element_text(face="bold", hjust = 1,  vjust = 1,size = 20),
          text=element_text(size = 16, family= font),
          axis.text=element_text(size = 16),
          legend.title.align = 0.5)
}


################################################################################################
###################################### PLOTTING - LEGEND #############################################
######################################################################################################

# We use this function to extract the legend from figures
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}