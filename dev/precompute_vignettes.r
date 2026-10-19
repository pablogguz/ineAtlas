# Precompute vignettes ----
# The vignettes download data (ineAtlas.data on GitHub, mapSpain, Comunidad de
# Madrid), so CRAN cannot build them reliably. They are knitted here from
# vignettes/*.Rmd.orig into static vignettes/*.Rmd (figures in vignettes/figures/).
# Re-run after editing a .Rmd.orig or after a data release. Needs internet access
# and the current version of ineAtlas installed.

devtools::install(upgrade = "never", quiet = TRUE)

old_wd <- setwd("vignettes")
for (f in list.files(pattern = "\\.Rmd\\.orig$")) {
  knitr::knit(f, output = sub("\\.orig$", "", f))
}
setwd(old_wd)
