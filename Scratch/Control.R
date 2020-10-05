

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# usethis ----
# install.packages('usethis')
# library(usethis)
# sessionInfo()
# usethis::use_cran_comments()
# usethis::use_news_md()
# usethis::use_travis()
# usethis::use_testthat()
# usethis::use_vignette(name = 'DoseSelectorInterface',
#                       title = 'Working with dose selectors')
# usethis::use_vignette(name = 'Simulation',
#                       title = 'Simulating dose-escalation trials')
# usethis::use_vignette(name = 'DosePaths',
#                       title = 'Working with dose-paths')
# usethis::use_vignette(name = 'TPI', title = 'TPI Design')
# usethis::use_logo('logo.png')
# usethis::use_pkgdown()
# usethis::use_coverage()


# Something else ----
library(escalation)
getwd()

# Remake man files
.rs.restartR()
roxygen2::roxygenise(clean = TRUE)
# To preview a man file
rstudioapi::previewRd('man/tox_at_dose.Rd')
rstudioapi::previewRd('man/eff_at_dose.Rd')

# To reinstall
# devtools::install(build = TRUE)
# devtools::install(build = TRUE, args = "--preclean")
devtools::install(build = FALSE) # Skip recompilation

# .rs.restartR()
library(escalation)

# Run tests
devtools::test()
# devtools::test(filter = 'careful_escalation')

# Build vignettes
devtools::build_vignettes()

# pkgdown site
pkgdown::build_site()

# Check
devtools::check()

# Manual
devtools::build_manual()

# Build
devtools::build()
