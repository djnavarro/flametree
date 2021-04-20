
# script to run pkgdown build *after* installing the pkgdowngirl template
remotes::install_github("djnavarro/pkgdowngirl")
pkgdown::deploy_to_branch(new_process = FALSE)
