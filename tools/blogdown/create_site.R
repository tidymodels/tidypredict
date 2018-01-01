blogdown_build <- function(skip_theme = TRUE) {
  
  if (file.exists("content")) unlink("content", recursive = TRUE)
  if (file.exists("static"))  unlink("static", recursive = TRUE)
  if (file.exists("public"))  unlink("public", recursive = TRUE)
  
  if(!file.exists("content")) dir.create("content")
  if(!file.exists("content/reference")) dir.create("content/reference")
  if(!file.exists("static")) dir.create("static")
  if(!file.exists("themes")) dir.create("themes")

  file.copy("vignettes/", "content/", recursive = TRUE)
  file.copy("README.Rmd", "content/index.Rmd")

  current_theme <- list.files("tools/blogdown/themes/")[1]
  theme_target <- paste0("themes/", current_theme )
  if (!skip_theme &&  file.exists(theme_target)) {
    if (file.exists(theme_target)) unlink(theme_target, recursive = TRUE)
    
    file.copy(
      paste0("tools/blogdown/themes/", current_theme), 
      "themes/", 
      recursive = TRUE
      )
    
  }
  pkgdown::build_reference(path = "content/reference")
  blogdown::serve_site()
}
