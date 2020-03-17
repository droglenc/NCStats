NCStats
=======

[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![Travis-CI Build Status](https://travis-ci.org/droglenc/NCStats.svg?branch=master)](https://travis-ci.org/droglenc/NCStats)


## Introduction
The **NCStats** package contains helper functions for statistics courses taught by [Dr. Derek H. Ogle](http://derekogle.com) at [Northland College](http://www.northland.edu). Recent changes to the package are described [here](https://github.com/droglenc/NCStats/blob/master/NEWS.md).


## Installation
The latest **NCStats** may be installed by running these lines in R/RStudio:

```r
if (!require('manipulate')) utils::install.packages('manipulate')
if (!require('remotes')) install.packages('remotes'); require('remotes')
remotes::install_github('droglenc/NCStats')
```

If the following code runs without error, then **NCStats** has properly installed on your computer.

```r
source(system.file("InstallTester.R",package="NCStats"),echo=TRUE)
```

This installation may fail if other packages do not install properly (primarily the **Rcpp** and **curl** packages). These failures may be ameliorated by manually installing packages responsible for the errors (see [these directions](http://derekogle.com/IFAR/supplements/installations/InstallPackagesRStudio.html)).

[E-mail me](mailto:derek@derekogle.com?Subject=NCStats%20Installation%20Question) if you continue to experience difficulties installing **NCStats**.


## Note About Using Macs
**NCStats** uses **TCL/TK** for some interactive plots. Some Mac users report problems with using **TCL/TK**. I do not have access to a Mac to test these problems. However, the CRAN page suggests that for recent versions of R (>3.0.0), [XQuartz](https://www.xquartz.org/) must be installed. In the past, some students have reported success installing the **TCL/TK** universal build [located here](http://cran.r-project.org/bin/macosx/tools/) (or [direct link to the file](http://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.dmg)).
