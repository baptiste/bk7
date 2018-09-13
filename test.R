setwd("~/Documents/github/bk7")
require("pandocfilters", quietly = TRUE, warn.conflicts = FALSE)

pandoc_to_json <- function(file, from="markdown") {
  args <- sprintf("-f %s -t json %s", from, file)
  tmp <- jsonlite::fromJSON(system2("pandoc", args, stdout=TRUE, stderr=TRUE))
  jsonlite::toJSON(tmp, pretty = TRUE)
}



pandoc_from_json <- function(json, to) {
  args <- sprintf("%s | pandoc -f json -t %s", shQuote(json), to)
  system2("echo", args, stdout=TRUE, stderr=TRUE)
}

test_filter <- function(x, to="html") {
  d <- list(list(unMeta=setNames(list(), character())), x)
  pandoc_from_json(as.character(jsonlite::toJSON(d, auto_unbox=TRUE, pretty=TRUE)), to=to)
}


Emph(list(Str("some text")))
Emph(Str("some text"))
Emph("some text")


rmarkdown::render('dummy.Rmd', run_pandoc = FALSE)
test <- pandoc_to_json('dummy.md')

# ?jsonlite::toJSON
cat(test, file='ast.json')
caps <- function(key, value, ...) {
  # if (key == "Str") return( Str( tolower(value) ) )
  # print(key)
  if (key == "BulletList")  print(value)
  
  return(NULL)
}

tmp <- jsonlite::fromJSON(pandoc_to_json('dummy.md'))


# read connection
input_connection <- textConnection(pandoc_to_json('dummy.md'))
# write connection
output_connection <- textConnection("modified_ast", open="w")

# apply filter
filter(caps, input=input_connection, output=output_connection)
close.connection(output_connection)

# convert altered ast to markdown
pandoc_from_json(modified_ast, to="markdown")
