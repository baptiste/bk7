library(pandocfilters)

replace_code <- function(key, value, ...) {
    if (key == "Code") return( Strong(Str("SomeCode")) )
    return(NULL)
}

pandoc_to_json <- function(file, from = "markdown") {
    args <- sprintf("-f %s -t json %s", from, file)
    system2("pandoc", args, stdout=TRUE, stderr=TRUE)
}

infile <- "knitr-minimal.Rmd"
json_ast <- pandoc_to_json(infile, from="markdown")
ast <- jsonlite::fromJSON(json_ast, simplifyVector = FALSE, flatten = TRUE)

if (FALSE) {
    ## This helps to understand the ast!
    writeLines(capture.output(str(ast)), con = "tmpfile.txt")
}

modified_ast <- astrapply(ast, replace_code)
modified_json_ast <- jsonlite::toJSON(modified_ast, auto_unbox = TRUE)
outfile <- "modified_ast.json"
writeLines(as.character(modified_json_ast), con = outfile)
system2("pandoc", sprintf("-f json -o knitr-minimal_modified.html %s", outfile))

clean_up <- function() {
    sapply(c("knitr-minimal_modified.html", "modified_ast.json", "tmpfile.txt"), unlink)
}

if (FALSE) {
    clean_up()
}
