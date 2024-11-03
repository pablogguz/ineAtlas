
# # Add lifecycle badge to README
# usethis::use_lifecycle_badge("experimental")

# # Add R CMD check GitHub Actions workflow
# usethis::use_github_action_check_standard()

# # Add license
# usethis::use_mit_license()

# Load functions ----
devtools::load_all()

# Documentation ----
devtools::document()

# Tests ----
devtools::test()

# Checks ----
devtools::check()

# Vignettes
devtools::build_vignettes()

# Build website
pkgdown::build_site()

# Package version ----
usethis::use_version()
