
# Version 0.1.4

*Resubmission*
Updated to avoid ERROR in dependency package called precautionary.
precautionary still warns - the package author needs to document a new parameter.
I have informed the author to do that.

## Test environments
* local OS X install, R 4.0.3, using --as-cran
* Travis-CI, Ubuntu 16.04.6 LTS on travis-ci, R 4.0.2 using --as-cran
* WinBuilder R-release
* WinBuilder R-devel

### local OS X check results

0 errors ✓ | 0 warnings ✓ | 1 note

* checking CRAN incoming feasibility ... NOTE

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2531628
    From: inst/doc/A205-CRM.html
          inst/doc/A600-DosePaths.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2531628
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

The possibly invalid URL and DOI both refer to the same old paper from 1990 now
on J-Stor. I have checked the URL and the DOI and these are correct.


### Travis-CI check results

0 errors ✓ | 0 warnings ✓ | 1 note

checking installed package size ... NOTE
  installed size is 11.5Mb
  sub-directories of 1Mb or more:
    doc  10.9Mb
    
There are quite a few vignettes, most use graphics to demonstrate usage.


###  WinBuilder R-release

0 errors ✓ | 0 warnings ✓ | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kristian Brock <kristian.brock@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  EffTox (15:75)
  Ji (12:78, 14:8)
  TPI (12:63, 13:64)
  Tait (17:13)
  Thall (16:8)
  mTPI (13:69)
  trialist (25:62)

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2531628
    From: inst/doc/A205-CRM.html
          inst/doc/A600-DosePaths.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2531628
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

The possible misspellings are in fact all correct; mostly names.
The URL and DOI are addressed above.

###  WinBuilder R-devel

0 errors ✓ | 0 warnings ✓ | 1 note

As above.



# Version 0.1.3

* This is an updated version of a current CRAN package, provided in response to
notification that dplyr will soon change.

## Test environments
* local OS X install, R 3.6.2, using --as-cran
* Travis-CI, Ubuntu 16.04.6 LTS, R 3.6.2, using --as-cran
* WinBuilder R-devel


### local OS X check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


### Travis-CI check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


###  WinBuilder R-devel

0 errors ✓ | 0 warnings ✓ | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kristian Brock <kristian.brock@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2531628
    From: inst/doc/DosePaths.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2531628
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking package namespace information ... OK

Author response: This URL and associated DOI are fine.



# Version 0.1.2

* This is an updated version of a current CRAN package.

## Test environments
* local OS X install, R 3.6.2, using --as-cran
* Travis-CI, Ubuntu 16.04.6 LTS, R 3.6.2, using --as-cran
* WinBuilder R-release
* WinBuilder R-devel


### local OS X check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


### Travis-CI check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


###  WinBuilder R-release

0 errors ✓ | 0 warnings ✓ | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kristian Brock <kristian.brock@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2531628
    From: inst/doc/DosePaths.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2531628
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking package namespace information ... OK

Author response: This URL and associated DOI are fine.


###  WinBuilder R-devel

0 errors ✓ | 0 warnings ✓ | 1 note

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kristian Brock <kristian.brock@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/2531628
    From: inst/doc/DosePaths.html
          README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/2531628
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking package namespace information ... OK

Author response: as above



# Version 0.1.1

* This is the first release of a new package.

Addressed issues raised by Jelena Saf.


## Test environments
* local OS X install, R 3.6.2, using --as-cran
* Travis-CI, Ubuntu 16.04.6 LTS, R 3.6.2, using --as-cran
* local Win10 install, R3.6.0, using --as-cran


### local OS X check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


### Travis-CI check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓


### local Win10 check results

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
