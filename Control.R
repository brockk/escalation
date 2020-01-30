

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



# Something else ----
library(dosefinding)
getwd()
# setwd('/Users/k/Dropbox/Code/trialr/')
# setwd('C:/Users/Kristian Brock/Dropbox/Code/trialr/')
# setwd('/Users/k/ownCloud/Code/trialr/')

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
library(dosefinding)

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

