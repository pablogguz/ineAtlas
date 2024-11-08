## Resubmission
This is a resubmission of version 0.1.1. In this version I have:
* Added single quotes around software names in DESCRIPTION
* Removed unnecessary `options()` settings that were altering the user's options in `get_atlas.r` and `get_tract_geom.r` 

## Test environments 
* local Windows 10 install, R 4.3.1
* win-builder (devel and release)
* mac-builder (release)
* GitHub Actions:
  * windows-latest (devel, release, oldrel-1)
  * macos-latest (devel, release, oldrel-1)
  * ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

### local, mac-builder and GitHub Actions:

0 errors | 0 warnings | 0 notes

### win-builder:

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE Maintainer: 'Pablo García Guzmán <garciagp@ebrd.com>' New submission

## Downstream dependencies
New submission - there are no downstream dependencies to check.
