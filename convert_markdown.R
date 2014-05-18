library(knitr)
library(markdown)
knit("movement.Rmd") # convert to md
markdownToHTML("movement.md", "movement.html") # convert to HTML