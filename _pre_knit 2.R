  
  pre_knit <- function(input, ...){
    ## tmp copy
    rd <- readLines(input)
    id <- grep('~', rd)
    
    rd[id] <- stringr::str_replace_all(rd[id],
                    "~([:alnum:]+)\\((.*?)\\)~",
                    replacement = '`r \\1("\\2")`')
    
    writeLines(text = rd, con = paste0(input,"_tmp.Rmd"))
  }
  
  
  custom_format <- output_format(knitr = knitr_options(opts_chunk = list(dev = 'png')),
                                 pandoc = pandoc_options(to = "html"),
                                 pre_knit = pre_knit)

rmarkdown::render('pre.Rmd', output_format = custom_format)
rmarkdown::render('pre.Rmd_tmp.Rmd', output_format = pdf_document())
# 
# library(stringr)
# str_extract(fruit, regex("nana"))
# str_extract("This is an R Markdown ~cap(document). ",regex("Markdown"))
# 
# 
# str_match_all("This is an R Markdown ~cap(document with formatting)~. ",
#             "~([:alnum:]+)\\((.*?)\\)~")
# 
# str_replace_all("This is an R Markdown ~cap(document with formatting)~.",
#               "~([:alnum:]+)\\((.*?)\\)~",
#               replacement = '`r \\1("\\2")`')
# 
