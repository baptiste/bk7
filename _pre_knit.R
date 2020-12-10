library(rmarkdown)
pre_knit <- function(input, ...){
  rd <- readLines(input)
  id <- grep('◊', rd)
  
  rd[id] <- stringr::str_replace_all(rd[id],
                                     "◊([:alnum:]+)\\((.*?)\\)",
                                     replacement = '`r \\1("\\2")`')
  file.copy(input, paste0(input, 'original.Rmd'))
  writeLines(text = rd, con = input)
}


custom_format <- rmarkdown::output_format(knitr = rmarkdown::knitr_options(opts_chunk = list(dev = 'png')),
                               pandoc = rmarkdown::pandoc_options(to = "markdown"),
                               pre_knit = pre_knit)

pdf_format <- rmarkdown::output_format(knitr = rmarkdown::knitr_options(opts_chunk = list(dev = 'png')),
                                          pandoc = rmarkdown::pandoc_options(to = "latex"))
