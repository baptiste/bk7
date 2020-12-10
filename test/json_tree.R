as.Node.list <- function (x, mode = c("simple", "explicit"), nameName = "name", 
          childrenName = "children", nodeName = NULL, check = c("check", 
                                                                "no-warn", "no-check"), ...) 
{
  mode <- mode[1]
  check <- check[1]
  if (is.null(nameName) || !(nameName %in% names(x))) {
    if (length(nodeName) == 0) 
      myName <- "Root"
    else myName <- nodeName
  }
  else {
    myName <- x[[nameName]]
  }
  n <- Node$new(as.character(myName), check = check)
  fields <- names(x)
  if (is.null(fields) && length(x) != 0) {
    fields <- as.character(seq_along(x))
  }
  field_nums <- seq_along(x)
  unnamed_fields <- fields == "" & !vapply(x, is.list, logical(1))
  if (!is.null(nameName)) {
    field_nums <- field_nums[fields != nameName]
    unnamed_fields <- unnamed_fields[fields != nameName]
    fields <- fields[fields != nameName]
  }
  if (mode == "explicit") {
    field_nums <- field_nums[fields != childrenName]
    unnamed_fields <- unnamed_fields[fields != childrenName]
    fields <- fields[fields != childrenName]
  }
  fields[unnamed_fields] <- seq_along(which(unnamed_fields))
  if (check != "no-check") {
    fieldNameIsReserved <- (fields %in% NODE_RESERVED_NAMES_CONST) & 
      !(fields %in% c(nameName, childrenName))
    if (any(fieldNameIsReserved) && (check != "no-warn")) 
      warning(paste0("The following names are data.tree reserved words and will be appended with 2: ", 
                     paste(fields[fieldNameIsReserved], sep = ", "), 
                     "."))
  }
  for (i in seq_along(field_nums)) {
    v <- x[[field_nums[i]]]
    if (mode == "simple" && inherits(v, "list")) {
    }
    else {
      fieldNm <- fields[i]
      if (fieldNm %in% NODE_RESERVED_NAMES_CONST) 
        fieldNm <- paste0(fieldNm, "2")
      n[[fieldNm]] <- v
    }
  }
  if (is.character(x)) 
    return(n)
  if (mode == "simple") 
    children <- x[vapply(x, is.list, logical(1))]
  else if (mode == "explicit") 
    children <- x[[childrenName]]
  if (length(children) == 0) 
    return(n)
  for (i in 1:length(children)) {
    if (any(duplicated(names(children)))) {
      childName <- ""
    }
    else if (is.character(children)) {
      childName <- children[i]
    }
    else if (!is.null(names(children))) {
      childName <- names(children)[i]
    }
    else {
      childName <- ""
    }
    if (nchar(childName) == 0) 
      childName <- i
    child <- children[[i]]
    childNode <- as.Node.list(child, mode, nameName, childrenName, 
                              nodeName = childName, check = check, ...)
    n$AddChildNode(childNode)
  }
  return(n)
}