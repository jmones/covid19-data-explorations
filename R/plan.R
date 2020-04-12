# Plans used in the drake workflow

vignettes_plan <- drake_plan(
  pollution_to_human_transmission_catalonia = RenderRmd(
    knitr_in("vignettes/pollution_to_human_transmission_catalonia.Rmd"),
    file_out("vignettes/pollution_to_human_transmission_catalonia.html")
  )
)
