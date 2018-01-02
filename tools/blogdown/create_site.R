library(purrr)
blogdown_build <- function() {
  
  if (file.exists("content")) unlink("content", recursive = TRUE)
  if (file.exists("static"))  unlink("static", recursive = TRUE)
  if (file.exists("layouts"))  unlink("layouts", recursive = TRUE)
  if (file.exists("public"))  unlink("public", recursive = TRUE)
  
  if(!file.exists("content")) dir.create("content")
  if(!file.exists("content/reference")) dir.create("content/reference")
  if(!file.exists("content/articles")) dir.create("content/articles")
  if(!file.exists("static")) dir.create("static")
  if(!file.exists("layouts")) dir.create("layouts")

  contents <- list.files("vignettes")
  three_last <- substr(contents, nchar(contents) - 3, nchar(contents))
  contents <- contents[three_last == ".Rmd"]
  
  contents %>%
    map(~file.copy(file.path("vignettes", .x),
                   "content/articles/"))
  
  file.copy("README.Rmd", "content/index.Rmd")

  file.copy(
    paste0("tools/blogdown/layouts/"), 
    rprojroot::find_rstudio_root_file(), 
    recursive = TRUE
  )
  
  file.copy(
    paste0("tools/blogdown/static/"), 
    rprojroot::find_rstudio_root_file(), 
    recursive = TRUE
  )
  
  pkgdown::build_reference(path = "content/reference")
  blogdown::serve_site()
}


