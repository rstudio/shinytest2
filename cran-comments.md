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
