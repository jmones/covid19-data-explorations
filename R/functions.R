# Functions used in the drake workflow

RenderRmd <- function(rmd_path, html_path) {
  result = rmarkdown::render(
    rmd_path,
    output_file = path_file(html_path),
    output_dir = path_dir(html_path),
    quiet = TRUE
  )
  return(result)
}
