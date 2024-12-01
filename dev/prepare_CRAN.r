# Prepare for CRAN ----

# Update dependencies in DESCRIPTION
attachment::att_amend_desc()

# Run tests
devtools::test()
testthat::test_dir("tests/testthat/")

# Run examples
devtools::run_examples()

# Check package as CRAN using the correct CRAN repo
devtools::check(args = c("--no-manual", "--as-cran"))

# Check content
# install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
# All functions must have either `@noRd` or an `@export`.
checkhelper::find_missing_tags()

# Check spelling - No typo
usethis::use_spell_check()

# Check URL are correct
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
# _rhub v2
# rhub::rhub_setup() # Commit, push, merge
# rhub::rhub_doctor()
# rhub::rhub_platforms()
# rhub::rhub_check() # launch manually

# _win devel CRAN
devtools::check_win_devel()
# _win release CRAN
devtools::check_win_release()
# _macos CRAN
# Need to follow the URL proposed to see the results
devtools::check_mac_release()

# Reverse dependencies 
# tools::package_dependencies("ineAtlas", reverse = TRUE)

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Upgrade version number
# usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# Verify you're ready for release, and release
#devtools::release()
devtools::submit_cran()
