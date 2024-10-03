
#
# after copying files here from last years' folder, rename files
# - use code below
# then, commit them  with commit message "copied from---"
# thereafter, make changes in the files (update years)
#

fns <- dir(
  "434_paramgroups_2023",   # remember update
  pattern = "Rmd$", 
  full.names = T)
fns

fns_new <- sub(
  "2022",       # remember update
  "2023",       # remember update
  fns
)
fns_new

# rename files
file.rename(fns, fns_new)


