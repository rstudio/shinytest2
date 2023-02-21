## Comments

#### 2023-02-07

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_shinytest2.html>.

Please correct before 2023-02-21 to safely retain your package on CRAN.

The CRAN Team

#### 2023-02-19

Check Details from https://cran.r-project.org/web/checks/check_results_shinytest2.html

```
Version: 0.2.0
Check: C++ specification
Result: NOTE
     Specified C++11: please drop specification unless essential
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-devel-windows-x86_64

Version: 0.2.0
Check: whether package can be installed
Result: ERROR
    Installation failed.
Flavor: r-devel-linux-x86_64-debian-gcc

Version: 0.2.0
Check: for detritus in the temp directory
Result: NOTE
    Found the following files/directories:
     ‘Crashpad’
Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc
```

I believe the installation error is a false-positve as it is a permission issue:

```
# File: /home/hornik/tmp/R.check/r-devel-gcc/Work/PKGS/shinytest2.Rcheck/00install.out
# Link: https://www.r-project.org/nosvn/R.check/r-devel-linux-x86_64-debian-gcc/shinytest2-00check.html

* installing to library ‘/home/hornik/tmp/R.check/r-devel-gcc/Work/build/Packages’
Error: ERROR: no permission to install to directory ‘/home/hornik/tmp/R.check/r-devel-gcc/Work/build/Packages’
```

#### 2023-02-19

* Addressed the NOTE about C++11 by dropping the System Requirements field from the DESCRIPTION file.
  * https://github.com/rstudio/shinytest2/pull/326
* I have tried to address the NOTE about the extra Crashpad file/directory, but I can not reproduce this locally or on my test machines.
  * https://github.com/rstudio/shinytest2/pull/327/commits/badef8c
* I believe the installation error is a false-positve as it is a permission issue on the machine.

I have included other small bug fixes.

Please let me know if I can provide any more information.

Thank you,
Barret


#### 2023-02-20

CRAN teams' auto-check service
Package check result: OK

Changes to worse in reverse depends:

```
Package: disaggR
Check: tests
New result: ERROR
```

#### 2023-02-20

Thanks, we see:

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
   ‘Crashpad’

on fedora-clang

Please fix and resubmit.

Best,
Uwe Ligges

#### 2023-02-21

Dear maintainers,

This concerns the CRAN packages

  shiny.benchmark shinytest2

maintained by one of you:

  Barret Schloerke <barret@rstudio.com>: shinytest2
  Douglas Azevedo <opensource+douglas@appsilon.com>: shiny.benchmark

We have asked for an update fixing the check problems shown at
  <https://cran.r-project.org/web/checks/check_results_shinytest2.html>
with no update from the maintainer thus far.

Thus, package shinytest2 is now scheduled for archival on 2023-03-07,
and archiving this will necessitate also archiving its CRAN strong
reverse dependencies.

Please negotiate the necessary actions.

The CRAN Team


#### 2023-02-21

Check Details
Version: 0.2.0
Check: C++ specification
Result: NOTE
     Specified C++11: please drop specification unless essential
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-devel-windows-x86_64

Version: 0.2.0
Check: for detritus in the temp directory
Result: NOTE
    Found the following files/directories:
     ‘Crashpad’
Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc


#### 2023-02-21

Update since last submission:
* I have tried to address the NOTE about the extra Crashpad file/directory (again), but I can not reproduce this locally or on my test machines.
  * Changes:
    * Disable the crash reporting via `"--disable-crash-reporter"` flag
    * Remove the Crashpad directory when done testing
  * Commit: https://github.com/rstudio/shinytest2/pull/327/commits/03307b4
* The installation error on `r-devel-linux-x86_64-debian-gcc` has been fixed by the CRAN team.

Previously addressed:
* Addressed the NOTE about C++11 by dropping the System Requirements field from the DESCRIPTION file.
  * https://github.com/rstudio/shinytest2/pull/326
* ~~I believe the installation error is a false-positve as it is a permission issue on the machine.~~
  * The installation error on `r-devel-linux-x86_64-debian-gcc` has been fixed by the CRAN team.
* I have included other small bug fixes.

Please let me know if I can provide any more information.

Thank you,
Barret



## Test environments

* local macOS, R 4.2
* GitHub Actions
  * macOS
    * 4.2
  * windows
    * devel, 4.2, 4.1, 3.6
  * ubuntu20
    * devel, 4.2, 4.1, 4.0, 3.6, 3.5
* devtools::
  * check_win_devel()

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## revdepcheck results

We checked 12 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
