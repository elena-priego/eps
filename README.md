---
title: "README"  
author: "Elena Priego Saiz"  
date: "26/07/2021"  
output: HTML
---

Working on R (>= 3.5.0)  

eps is an R package with miscellaneous R functions that are useful to me.

License
This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License, version 3, as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose. See the GNU General Public License for more details.

A copy of the GNU General Public License, version 3, is available at https://www.r-project.org/Licenses/GPL-3

## Useful functions to create the package
- `devtools::document()` to generate the Rd and update NAMESPACE  
- `devtools::build()` to generate the package  
- `devtools::check()` to look for errors and warnings  
- `use_data(object, eps, overwrite = TRUE)` to save an object in data. There's need to create an R file with Roxygen documentation.

## check output
> devtools::check()
i Updating eps documentation
i Loading eps
Writing NAMESPACE
Writing NAMESPACE
-- Building ------------------------------------------------------------------------------------------------------------------------------------- eps --
Setting env vars:
* CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
* CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
* CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
--------------------------------------------------------------------------------------------------------------------------------------------------------
√  checking for file 'C:\Users\elena\Documents\R\github\eps/DESCRIPTION' (1.1s)
-  preparing 'eps':
√  checking DESCRIPTION meta-information ...
   Warning: bad markup (extra space?) at micecode.Rd:11:35
-  checking for LF line-endings in source and make files and shell scripts
-  checking for empty or unneeded directories
-  looking to see if a 'data/datalist' file should be added
     NB: this package now depends on R (>= 3.5.0)
     WARNING: Added dependency on R >= 3.5.0 because serialized objects in
     serialize/load version 3 cannot be read in older versions of R.
     File(s) containing such objects:
       'eps/data/micecode.Rdata'
-  building 'eps_0.1.1.tar.gz'
   
-- Checking ------------------------------------------------------------------------------------------------------------------------------------- eps --
Setting env vars:
* _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
* _R_CHECK_CRAN_INCOMING_       : FALSE
* _R_CHECK_FORCE_SUGGESTS_      : FALSE
* NOT_CRAN                      : true
-- R CMD check -----------------------------------------------------------------------------------------------------------------------------------------
-  using log directory 'C:/Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck' (1.3s)
-  using R version 4.1.0 (2021-05-18)
-  using platform: x86_64-w64-mingw32 (64-bit)
-  using session charset: ISO8859-1
-  using options '--no-manual --as-cran'
E  checking for file 'eps/DESCRIPTION' ... 
   Required field missing or empty:
     'Description'
   
   See
     'C:/Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck/00check.log'
   for details.
   
-- R CMD check results ------------------------------------------------------------------------------------------------------------------ eps 0.1.1 ----
Duration: 1.8s

> checking for file 'eps/DESCRIPTION' ... ERROR
  Required field missing or empty:
    'Description'

1 error x | 0 warnings √ | 0 notes √
> devtools::check()
i Updating eps documentation
i Loading eps
Writing NAMESPACE
Writing NAMESPACE
-- Building ------------------------------------------------------------------------------------------------------------------------------------- eps --
Setting env vars:
* CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
* CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
* CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
--------------------------------------------------------------------------------------------------------------------------------------------------------
√  checking for file 'C:\Users\elena\Documents\R\github\eps/DESCRIPTION' (1.2s)
-  preparing 'eps':
√  checking DESCRIPTION meta-information ... 
   Warning: bad markup (extra space?) at micecode.Rd:11:35
-  checking for LF line-endings in source and make files and shell scripts
-  checking for empty or unneeded directories
-  looking to see if a 'data/datalist' file should be added
     NB: this package now depends on R (>= 3.5.0)
     WARNING: Added dependency on R >= 3.5.0 because serialized objects in
     serialize/load version 3 cannot be read in older versions of R.
     File(s) containing such objects:
       'eps/data/micecode.Rdata'
-  building 'eps_0.1.1.tar.gz'
   
-- Checking ------------------------------------------------------------------------------------------------------------------------------------- eps --
Setting env vars:
* _R_CHECK_CRAN_INCOMING_REMOTE_: FALSE
* _R_CHECK_CRAN_INCOMING_       : FALSE
* _R_CHECK_FORCE_SUGGESTS_      : FALSE
* NOT_CRAN                      : true
-- R CMD check -----------------------------------------------------------------------------------------------------------------------------------------
-  using log directory 'C:/Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck' (1.3s)
-  using R version 4.1.0 (2021-05-18)
-  using platform: x86_64-w64-mingw32 (64-bit)
-  using session charset: ISO8859-1
-  using options '--no-manual --as-cran'
√  checking for file 'eps/DESCRIPTION'
-  this is package 'eps' version '0.1.1'
-  package encoding: UTF-8
√  checking package namespace information
√  checking package dependencies (5.4s)
√  checking if this is a source package ...
√  checking if there is a namespace
√  checking for executable files (981ms)
√  checking for hidden files and directories ...
√  checking for portable file names ...
W  checking whether package 'eps' can be installed (8s)
   Found the following significant warnings:
     Warning: bad markup (extra space?) at micecode.Rd:11:35
   See 'C:/Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck/00install.out' for details.
√  checking installed package size ... 
√  checking package directory (366ms)
√  checking for future file timestamps (2.6s)
√  checking DESCRIPTION meta-information (1.1s)
√  checking top-level files ...
√  checking for left-over files
√  checking index information
W  checking package subdirectories (460ms)
   Subdirectory 'data' contains no data sets.
W  checking R files for non-ASCII characters (346ms)
   Found the following file with non-ASCII characters:
     functions.R
   Portable packages must use only ASCII characters in their R code,
   except perhaps in comments.
   Use \uxxxx escapes for other characters.
E  checking R files for syntax errors ... 
   Error in file 'R/functions.R':
     /Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck/00_pkg_src/eps/R/functions.R:75:17: unexpected input
     74:       mutate(
     75:         mice = CÃ
                         ^
√  checking whether the package can be loaded (935ms)
√  checking whether the package can be loaded with stated dependencies (345ms)
√  checking whether the package can be unloaded cleanly (474ms)
√  checking whether the namespace can be loaded with stated dependencies (378ms)
√  checking whether the namespace can be unloaded cleanly (930ms)
√  checking loading without being on the library search path (929ms)
√  checking dependencies in R code (584ms)
√  checking S3 generic/method consistency (2s)
√  checking replacement functions (701ms)
√  checking foreign function calls (680ms)
N  checking R code for possible problems (10.4s)
   facs_tidytable: no visible global function definition for 'read_excel'
   facs_tidytable: no visible global function definition for 'as_tibble'
   facs_tidytable: no visible global function definition for
     'str_replace_all'
   facs_tidytable: no visible global function definition for '%>%'
   facs_tidytable: no visible global function definition for
     'pivot_longer'
   facs_tidytable: no visible global function definition for 'separate'
   facs_tidytable: no visible global function definition for 'mutate'
   facs_tidytable: no visible binding for global variable 'cell'
   facs_tidytable: no visible binding for global variable 'marker'
   facs_tidytable: no visible global function definition for 'replace_na'
   facs_tidytable: no visible global function definition for 'mutate_all'
   facs_tidytable: no visible binding for global variable 'genotype'
   facs_tidytable: no visible binding for global variable 'value'
   facs_tidytable: no visible binding for global variable 'organ'
   facs_tidytable: no visible binding for global variable 'mice'
   facs_tidytable: no visible binding for global variable 'stat'
   get_genotype : <anonymous>: no visible global function definition for
     'read.csv'
   get_genotype: no visible global function definition for '%>%'
   get_genotype: no visible global function definition for 'mutate'
   get_genotype: no visible binding for global variable 'Código'
   get_genotype: no visible binding for global variable 'Mote'
   get_genotype: no visible binding for global variable 'Genotipado'
   get_genotype: no visible global function definition for
     'str_replace_all'
   get_genotype: no visible binding for global variable 'full_genotype'
   get_genotype: no visible global function definition for 'select'
   get_genotype: no visible binding for global variable 'mice'
   get_genotype: no visible binding for global variable 'genotype'
   Undefined global functions or variables:
     %>% Código Genotipado Mote as_tibble cell full_genotype genotype
     marker mice mutate mutate_all organ pivot_longer read.csv read_excel
     replace_na select separate stat str_replace_all value
   Consider adding
     importFrom("utils", "read.csv")
   to your NAMESPACE file.
W  checking Rd files (1.1s)
   prepare_Rd: facs_tidytable.Rd:23-25: Dropping empty section \value
   prepare_Rd: bad markup (extra space?) at micecode.Rd:11:35
   checkRd: (3) micecode.Rd:10-12: Empty section \source
√  checking Rd metadata (470ms)
√  checking Rd line widths (460ms)
√  checking Rd cross-references (1s)
√  checking for missing documentation entries (702ms)
W  checking for code/documentation mismatches (795ms)
   Data with usage in documentation object 'micecode' but not in code:
     'micecode'
   
√  checking Rd \usage sections (4s)
W  checking Rd contents (579ms)
   Argument items with no description in Rd object 'facs_tidytable':
     'marker_pattern'
   
W  checking for unstated dependencies in examples (651ms)
   Warning: parse error in file 'lines':
   1: unexpected symbol
   32: 
   33: cleanEx
       ^
W  checking contents of 'data' directory ...
   Files not of a type allowed in a 'data' directory:
     'micecode.Rdata'
   Please use e.g. 'inst/extdata' for non-R data files
   
√  checking data for non-ASCII characters (966ms)
√  checking data for ASCII and uncompressed saves (917ms)
E  checking examples (2.5s)
   Running examples in 'eps-Ex.R' failed
   The error most likely occurred in:
   
   > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
   > ### Name: facs_tidytable
   > ### Title: Prepare the data in a tidy format from the data obtained in
   > ###   Flowjo
   > ### Aliases: facs_tidytable
   > 
   > ### ** Examples
   > 
   > facs_tidytable("table.xls", gate_pattern = 
   + c("Lymphocytes/Single Cells/Single Cells/CD452/" = "", 
   + "Freq. of Parent" = "Freq.", "Freq. of Grandparent" = "Freq.",
   + "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "", 
   + "Ly6G, Ly6C subset/AMs" = "AMs", "Ly6G, Ly6C subset/EOs" = "EOs",
   + "Monocytes" = "Mos", "Neutrophils" = "NOs")
   + 
   +  
   + 
   + 
   + 
   + base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
   Error: unexpected symbol in:
   "
   base"
   Execution halted
√  checking for non-standard things in the check directory
√  checking for detritus in the temp directory
   
   See
     'C:/Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck/00check.log'
   for details.
   
-- R CMD check results ------------------------------------------------------------------------------------------------------------------ eps 0.1.1 ----
Duration: 55s

> checking R files for syntax errors ... ERROR
  Error in file 'R/functions.R':
    /Users/elena/AppData/Local/Temp/RtmpgRhuJN/eps.Rcheck/00_pkg_src/eps/R/functions.R:75:17: unexpected input
    74:       mutate(
    75:         mice = CÃ
                        ^

> checking examples ... ERROR
  Running examples in 'eps-Ex.R' failed
  The error most likely occurred in:
  
  > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
  > ### Name: facs_tidytable
  > ### Title: Prepare the data in a tidy format from the data obtained in
  > ###   Flowjo
  > ### Aliases: facs_tidytable
  > 
  > ### ** Examples
  > 
  > facs_tidytable("table.xls", gate_pattern = 
  + c("Lymphocytes/Single Cells/Single Cells/CD452/" = "", 
  + "Freq. of Parent" = "Freq.", "Freq. of Grandparent" = "Freq.",
  + "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "", 
  + "Ly6G, Ly6C subset/AMs" = "AMs", "Ly6G, Ly6C subset/EOs" = "EOs",
  + "Monocytes" = "Mos", "Neutrophils" = "NOs")
  + 
  +  
  + 
  + 
  + 
  + base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
  Error: unexpected symbol in:
  "
  base"
  Execution halted

> checking whether package 'eps' can be installed ... WARNING
  See below...

> checking package subdirectories ... WARNING
  Subdirectory 'data' contains no data sets.

> checking R files for non-ASCII characters ... WARNING
  Found the following file with non-ASCII characters:
    functions.R
  Portable packages must use only ASCII characters in their R code,
  except perhaps in comments.
  Use \uxxxx escapes for other characters.

> checking Rd files ... WARNING
  prepare_Rd: facs_tidytable.Rd:23-25: Dropping empty section \value
  prepare_Rd: bad markup (extra space?) at micecode.Rd:11:35
  checkRd: (3) micecode.Rd:10-12: Empty section \source

> checking for code/documentation mismatches ... WARNING
  Data with usage in documentation object 'micecode' but not in code:
    'micecode'

> checking Rd contents ... WARNING
  Argument items with no description in Rd object 'facs_tidytable':
    'marker_pattern'

> checking for unstated dependencies in examples ... WARNING
  Warning: parse error in file 'lines':
  1: unexpected symbol
  32: 
  33: cleanEx
      ^

> checking contents of 'data' directory ... WARNING
  Files not of a type allowed in a 'data' directory:
    'micecode.Rdata'
  Please use e.g. 'inst/extdata' for non-R data files

> checking R code for possible problems ... NOTE
  facs_tidytable: no visible global function definition for 'read_excel'
  facs_tidytable: no visible global function definition for 'as_tibble'
  facs_tidytable: no visible global function definition for
    'str_replace_all'
  facs_tidytable: no visible global function definition for '%>%'
  facs_tidytable: no visible global function definition for
    'pivot_longer'
  facs_tidytable: no visible global function definition for 'separate'
  facs_tidytable: no visible global function definition for 'mutate'
  facs_tidytable: no visible binding for global variable 'cell'
  facs_tidytable: no visible binding for global variable 'marker'
  facs_tidytable: no visible global function definition for 'replace_na'
  facs_tidytable: no visible global function definition for 'mutate_all'
  facs_tidytable: no visible binding for global variable 'genotype'
  facs_tidytable: no visible binding for global variable 'value'
  facs_tidytable: no visible binding for global variable 'organ'
  facs_tidytable: no visible binding for global variable 'mice'
  facs_tidytable: no visible binding for global variable 'stat'
  get_genotype : <anonymous>: no visible global function definition for
    'read.csv'
  get_genotype: no visible global function definition for '%>%'
  get_genotype: no visible global function definition for 'mutate'
  get_genotype: no visible binding for global variable 'Código'
  get_genotype: no visible binding for global variable 'Mote'
  get_genotype: no visible binding for global variable 'Genotipado'
  get_genotype: no visible global function definition for
    'str_replace_all'
  get_genotype: no visible binding for global variable 'full_genotype'
  get_genotype: no visible global function definition for 'select'
  get_genotype: no visible binding for global variable 'mice'
  get_genotype: no visible binding for global variable 'genotype'
  Undefined global functions or variables:
    %>% Código Genotipado Mote as_tibble cell full_genotype genotype
    marker mice mutate mutate_all organ pivot_longer read.csv read_excel
    replace_na select separate stat str_replace_all value
  Consider adding
    importFrom("utils", "read.csv")
  to your NAMESPACE file.

2 errors x | 8 warnings x | 1 note x

