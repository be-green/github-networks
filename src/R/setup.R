pkgs <- c(
  "tidyr",
  "purrr",
  "data.table",
  "aws.s3",
  "jsonlite",
  "rlang",
  "magrittr",
  "stringr"
)

getAndAttachPkg <- function(pkgs) {
  sapply(pkgs, function(pkg) {
    if(!require(pkg, character.only = T, quietly = T)) {
      install.packages(pkg)
      library(pkg, character.only = T)
    } else {
      library(pkg, character.only = T)
    }
  })
}

getAndAttachPkg(pkgs)
