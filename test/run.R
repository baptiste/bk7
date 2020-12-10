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
  system2("pandoc", args, stdout=TRUE, stderr=TRUE)
}

pandoc_from_json <- function(json, to) {
  args <- sprintf("%s | pandoc -f json -t %s", shQuote(json), to)
  system2("echo", args, stdout=TRUE, stderr=TRUE)
}

test_filter <- function(x, to="html") {
  d <- list(list(unMeta=setNames(list(), character())), x)
  pandoc_from_json(as.character(jsonlite::toJSON(d, auto_unbox=TRUE)), to=to)
}

# 
# pandoc_to_json <- function(file, from="markdown") {
#   args <- sprintf("-f %s -t json %s", from, file)
#   # cat(system2("pandoc", args, stdout=TRUE, stderr=TRUE), file="tmp.json")
#   jsonlite::fromJSON(system2("pandoc", args, stdout=TRUE, stderr=TRUE), simplifyVector=FALSE)
# }

f <- 'test.Rmd'

b <- pandoc_to_json(f)
str(b)

caps <- function(key, value, ...) {
  if (key == "Code") {print(Code(list(), value)); return( Code(list(), value) )}
  return(NULL)
}

# read connection
input_connection <- textConnection(pandoc_to_json(f, from="markdown"))
# write connection
output_connection <- textConnection("modified_ast", open="w")

# apply filter
filter(caps, input=input_connection, output=output_connection)

# convert altered ast to markdown
pandoc_from_json(modified_ast, to="markdown")
close(input_connection)
close(output_connection)

library(listviewer)
jsonedit(b)

lapply(b$blocks$c[b$blocks$t=="Para"], function(n) if("Code" %in% n$t) n$c)

# library(data.tree)
# cc <- as.Node.list(b$blocks)
# print(cc,"c", "t", limit = 20)
# cc$Get('c', traversal = "post-order")
# cc$Get('c')

# plot(cc)

cc <- as.Node(b$meta)
print(cc,"c", "t", limit = 20)
# plot(cc)




# 
# as.dendrogram(b)

# 
# idc <- lapply(b$blocks$c, "[[", "t") == "Code"
# lapply(b$blocks$c[idc], "[[", "t")
# 
# b$blocks$c[[idc[1]]][['c']][[1]][[2]]
