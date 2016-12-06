generate_report <- function(number, folder=NULL){
  if(is.null(folder)){
    variants <- c(
      '~/tmp/newEyelinesOnline/',
      'd:/YandexDisk/eyelinesOnlineNew/',
      '~/Yandex.Disk/eyelinesOnlineNew/',
      '~/YandexDisk/eyelinesOnlineNew/',
      '~/sources/eyelinesOnline/files/'
    )
    
    for(path in variants)
      if(suppressWarnings(dir.exists(normalizePath(path)))){
        folder = path
        break
      } 
  }
  
  if(as.numeric(number) %in% 22:27){
    rmarkdown::render(
      system.file("templates", 'eyelinesReport_23-27.Rmd', package="eyelinesOnline"), 
      params = list(path = file.path(folder, 'data', sprintf('%02i', as.numeric(number)), 'experiment.RData')), 
      output_file = file.path(folder, sprintf('report%02i.pdf', as.numeric(number)))
    )
  } else {
    rmarkdown::render(
      system.file("templates", 'eyelinesReport.Rmd', package="eyelinesOnline"), 
      params = list(path = file.path(folder, 'data', sprintf('%02i', as.numeric(number)), 'experiment.RData')), 
      output_file = file.path(folder, sprintf('report%02i.pdf', as.numeric(number)))
    )
  }
 
}