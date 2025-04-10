## CRAN comments

#### 2025-03-20

We have been told that a chrome update means using chromote is not reliable.

Note that the manual says that external commands (here 'chrome') must be
used conditionally, so changes should not have resulted in a check
failure.  But it did in shiny.destroy, shinytest2 and shinyscholar
(already attempted a resubmisison) now fail their checks -- see the
M1mac additional issues.

So it seems we need

- urgently a chromote update
- packages using it to ensure that they use chrome in a way that will
never give a check failure.  Before 2025-04-03 to safely retain the
packages on CRAN.

And it would have been basic good manners for the chromote maintainer to
keep CRAN and its users informed.  You agreed to the policy which says

"The time of the volunteers is CRAN's most precious resource"

Note that end users (including me who never knowingly uses chrome) get
chrome updates silently behind their backs and will have no idea what
version is currently installed nor what changed.  Seems to have been
Version 134.0.6998.89.e

Brian D. Ripley

#### 2025-04-09

Tests are now clearly skipped when {chromote} is utilized.

The release also includes routine features and bug fixes.

- Barret


## R CMD check results

0 errors | 0 warnings | 1 note

─  checking CRAN incoming feasibility ... [4s/20s] NOTE (20.2s)
   Maintainer: ‘Barret Schloerke <barret@posit.co>’

   Size of tarball: 5069497 bytes

## revdepcheck results

We checked 58 reverse dependencies (46 from CRAN + 12 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 1 packages

Issues with CRAN packages are summarised below.

### Failed to check

* shiny.destroy (NA)
