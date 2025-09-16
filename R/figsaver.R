figsaver <- function(fig, extension = "pdf", width = 9, height = 7, figunits = "in", section = "main"){
  
  if (!exists("counter",envir = .GlobalEnv)){
    counter <<-  1
  } else {
    counter <<- counter + 1
  }
  
  if (section == "main") {
  figname <- paste0("fig_",counter,".",extension)
  } else if (section == "si"){
  
    figname <- paste0("fig_s",counter,".",extension)
    
  }
  
  figpath <- file.path(results_dir, "figs",figname)
  
  ggsave(filename = figpath, fig,width = width, height = height, units = figunits, dpi = "print", device = cairo_pdf)
  fig
}

