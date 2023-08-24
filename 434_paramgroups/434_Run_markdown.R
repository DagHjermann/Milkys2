
fn <- "434_paramgroups/Milkys_2021_metals.Rmd"

rmarkdown::render(
  input = fn,          
  output_file = sub("Rmd", "html", fn)
)
  
fn <- "434_paramgroups/Milkys_2021_PFAS.Rmd"

rmarkdown::render(
  input = fn,          
  output_file = sub("Rmd", "html", fn)
)



fn <- "434_paramgroups/Milkys_2021_pesticides.Rmd"

rmarkdown::render(
  input = fn,          
  output_file = sub("Rmd", "html", fn)
)

