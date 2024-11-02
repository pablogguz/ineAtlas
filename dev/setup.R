# dev/setup.R

setup_package <- function(pkg_name = "ebrdify") {
  # Get system username
  username <- Sys.getenv("USERNAME")
  pkg_path <- file.path("C:/Users", username, "Documents/GitHub", pkg_name)
  
  message("🚀 Creating minimal R package...")
  
  # Create and activate package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    usethis::create_package(pkg_path)
  }
  usethis::proj_activate(pkg_path)
  
  # Basic structure
  dir.create("data-raw", showWarnings = FALSE)
  dir.create("dev", showWarnings = FALSE)
  
  # Basic documentation
  usethis::use_package_doc()
  usethis::use_readme_md()
  usethis::use_news_md()
  
  # Add to .Rbuildignore
  usethis::use_build_ignore("dev")
  usethis::use_build_ignore("data-raw")
  
  message("\n✨ Basic package structure created!")
  message("\nNext steps:")
  message("1. Edit DESCRIPTION file")
  message("2. Add functions in R/")
  message("3. Add documentation")
  
  invisible(TRUE)
}

setup_package(
  pkg_name = "ineAtlas"
)
