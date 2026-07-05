#!/usr/bin/env Rscript
# Script to generate README.md from README.Rmd

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown")
  library(rmarkdown)
}

# Render README.Rmd to README.md
rmarkdown::render(
  "README.Rmd",
  output_file = "README.md",
  output_format = "github_document"
)

message("README.md generated from README.Rmd")
