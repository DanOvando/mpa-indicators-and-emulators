figsaver <- function(fig, extension = "pdf", width = 9, height = 7, figunits = "in"){
  
  if (!exists("counter",envir = .GlobalEnv)){
    counter <<-  1
  } else {
    counter <<- counter + 1
  }
  
  figname <- paste0("fig_",counter,".",extension)
  
  figpath <- file.path(results_dir, "figs",figname)
  
  ggsave(filename = figpath, fig,width = width, height = height, units = figunits, dpi = "print", device = cairo_pdf)
  fig
}

