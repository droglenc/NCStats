NCStats
=======

[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![Travis-CI Build Status](https://travis-ci.org/droglenc/NCStats.svg?branch=master)](https://travis-ci.org/droglenc/NCStats)


## Introduction
The **NCStats** contains helper functions for statistics courses taught by [Dr. Derek H. Ogle](http://derekogle.com) at [Northland College](http://www.northland.edu).

You can [browse function help pages here](http://rforge.net/doc/packages/NCStats/00Index.html).  Recent changes are described in the [News file](https://github.com/droglenc/NCStats/blob/master/NEWS.md)

## Installation
**NCStats** has not yet been released on CRAN.  The latest version can be installed from GitHub (requires the **devtools** package) with ...

```r
if (!require('devtools')) install.packages('devtools'); require('devtools')
devtools::install_github(c('droglenc/FSA','droglenc/FSAdata','droglenc/NCStats'))
```

Alternatively, **NCStats** can be installed from **RForge.net** with ...

```r
source("http://www.rforge.net/NCStats/InstallNCStats.R")
```

These installations fail in a few situations.  [Send me an e-mail](mailto:derek@derekogle.com?Subject=NCStats%20Installation%20Question) if you experience difficulties installing **NCStats**.

## Note About Using Macs
**NCStats** uses **TCL/TK** for some interactive plots.  Some Mac users report problems with using **TCL/TK**.  I do not have access to a Mac to test these problems, some students have reported success installing the **TCL/TK** universal build [located here](http://cran.r-project.org/bin/macosx/tools/) (or [direct link to the file](http://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.dmg)).  You may have to reinstall **NCStats** after installing this file. 