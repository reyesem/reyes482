# Copy functions needed, but not exported from other packages.

# knitr:::split_lines
split_lines <- function(x) {
  if (length(grep("\n", x)) == 0L)
    return(x)
  x = gsub("\n$", "\n\n", x)
  x[x == ""] = "\n"
  unlist(strsplit(x, "\n"))
}
