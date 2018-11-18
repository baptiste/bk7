library(commonmark)
library(tinkr)
library(rmarkdown)
library(pandocfilters)
library(knitr)
library(xml2)
library(jsonlite)
library(yaml)

pandoc_to_json <- function(file, from="markdown") {
  args <- sprintf("-f %s -t json %s", from, file)
  jsonlite::fromJSON(system2("pandoc", args, stdout=TRUE, stderr=TRUE))
}

f <- 'test.Rmd'

a <- tinkr::to_xml(f)
b <- pandoc_to_json(f)
str(b)

idc <- lapply(b$blocks$c, "[[", "t") == "Code"
lapply(b$blocks$c[idc], "[[", "t")

b$blocks$c[[idc[1]]][['c']][[1]][[2]]
