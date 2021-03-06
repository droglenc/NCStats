# 0.4.9 ongoing
* Removed suggestion of `relax` package (see below).
* Removed Travis-CI.
* No longer using coveralls for coverage statistics. Changed to codecov.io.
* Added GitHub Action for CI/CD (used `usethis::use_github_action_check_standard()`).
* Added GitHub Action for code coverage with codecov.io (used `usethis::use_github_action("test-coverage")`).

* `assumptionCheck()`: Modified. Removed option to use `relax()` and Tcl/Tk version of the dynamic graphic.
* `ciSim()`: Modified. Removed option to use `relax()` and Tcl/Tk version of the dynamic graphic.
* `cltSim()`: Modified. Removed option to use `relax()` and Tcl/Tk version of the dynamic graphic.
* `filterD()`: Added (from `FSA`).
* `meanMedian()`: Modified. Removed option to use `relax()` and Tcl/Tk version of the dynamic graphic.
* `powerSim()`: Modified. Removed option to use `relax()` and Tcl/Tk version of the dynamic graphic.
* `residPlot()`: Added (from `FSA`).

# 0.4.8 15-Jan-21
* Changed to depending on `R >=3.5.0`, because that is the latest version required by packages (i.e., `car`, `FSA`, `plotrix`) that `NCStats` imports or suggests. Used [this shiny app](https://ateucher.shinyapps.io/check_r_versions_of_package_dependencies/) to help determine this.
* Added suggests for `ggplot2` and `ggpubr`.
* Fixed bad links to other documentation files in documentation files.
* Updated installTester.R file.
* `assumptionCheck()`: Added.
* `chisqPostHoc()`: Modified documentation to eliminate check errors.
* `outlierTest()`: Removed. Now just imports and then exports `outlierTest()` from `car`.
* `snorm()`: Modified. Changed sigma range, controlled y-axis to be between 0 and 1, and defaulted to not showing the CDF.
* `stat_SLR_equation()`: Added.
* `t.test()`: Added an import and then export from `mosaic` package.
* `theme_NCStats()`: Added.
* `transChooser()`: Modified documentation to eliminate check errors.

# 0.4.7 27-Aug-19
* Changed to depending on `R >=3.2.0`, because that is the latest version required by a package (i.e., `car`) that `NCStats` imports or suggests. Used [this shiny app](https://ateucher.shinyapps.io/check_r_versions_of_package_dependencies/) to help determine this.
* Added `Encoding: UTF-8` to DESCRIPTION.
* `AccuracyPrecision()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `ciSim()`: Modified. Changed plot title label to show more decimals and defaults to 95% confidence level (addresses [#2](https://github.com/droglenc/NCStats/issues/2)). Changed all uses of `par()` to `withr::local_par()`.
* `cltSim()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `corrSim()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `distrib()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `meanMedian()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `powerSim()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `rSquared()`: Removed. Moved to `FSA` (with minor new functionality).
* `sXXX()`: Modified. Changed all uses of `par()` to `withr::local_par()`.
* `transChooser()`: Modified. Changed all uses of `par()` to `withr::local_par()`.

# 0.4.6 1-Sep-17
* Added `InstallTester.R` to `inst` directory. In theory, this should run without error and provides a way to test that the installation was completed properly.
* `addSigLetters()`: Modified. Changed a `if()` with `class()` to `inherits()`. Put `\dontrun{}`s in documentation related to `multcomp` to remove CRAN warning.
* `diagPlot()`: Removed. Did not use and too difficult to maintain.
* `distrib()`: Modified. Fixed bug when `distrib="f"` and `plot=TRUE`. Basic problem was that for some distributions x was set to zero and the density returned was infinity, which led to a `ylim=` bug. Now started x at 0.001.
* `gofCI()`: Modified. Changed a `if()` with `class()` to `inherits()`.
* `meanMedian()`: Modified. Added `breaks=`, which defaults to `seq(0,1,0.1)`. Increased number of decimals in printed mean and median results.
* `percTable()`: Modified. Changed `addMargins=` default so that margins are NOT added for 1-D tables. Also changed `digits=` default to 1.
* `rSquared()`: Modified. Changed a `if()` with `class()` to `inherits()`.
* `transChooser()`: Modified. Changed a `if()` with `class()` to `inherits()`. Removed hard-coded variable positions and used positions returned from `iTypeoflm()`, which fixed a bug in IVRs where the factor variable was in the model first. Changed default colors for residual plots.

# 0.4.5 8-Jan-17
* Modified description file.
* `.onAttach()`: Modified. Streamlined package startup message.
* `ABDLakes`: Modified. Fixed errors for when there were two or more `local.names`.
* `chisqPostHoc()`: Modified. Fixed bug when a pair had a column with all zeroes. Added `verbose=`. Fixed comparison names if the original matrix did not have row names.
* `corr()`: Modified. Modified to allow more than two variables. Added `digits=` argument. Added a message if missing values existed and they were handled with use of `use=`. Modified tests.
* `c.region()`,`d.region()`: Modified. Changed output of areas and probabilities so that very small numbers showed scientific notation. This primarily affects `distrib()`.
* `distrib()`: Modified. Fixed bug when `distrib="chisq"` and `plot=TRUE` (related to choosing a chi-square value of 0 for making the plot). Modified plot so that continuous distributions touch x-axis. Also see note for `c.region()` and `d.region()`. Added check for `lower.tail=` not being a logical (addresses [#1](https://github.com/droglenc/NCStats/issues/1)).
* `srsdf()`: Modified. Added `removeRowNames=` argument and defaulted to `TRUE` so that resultant data.frame does not retain original rownames.

# 0.4.4 Feb16
* Added Travis-CI support.
* Converted all data files from .txt to .rdata.
* Now using Roxygen2 v5.0.0.
* Added a suggests for `testthat`.
* `iCheckRStudio()`: Added.
* `c.region()`: Added the `add=` argument to allow adding a shaded polygon to an existing plot created with `c.region()`.
* `ciSim()`: Modified. Added support for using `manipulate()` from `manipulate` if RStudio is being used. Modified the graphic. Streamlined the code. Changed default `reps=` to 200.
* `cltSim()`: Modified. Added support for using `manipulate()` from `manipulate` if RStudio is being used. Modified the graphic. Streamlined the code. Changed default `reps=` to 5000.
* `corr()`: Added.
* `corrSim()`: Added.
* `geomean()`: Deleted. Moved to `FSA`.
* `geosd()`: Deleted. Moved to `FSA`.
* `gofCI()`: Modified. Suppressed warnings on call to `binCI()` in `FSA` which results in warnings from `Hmisc` about reimporting some functions that were imported by `ggplot2`.
* `iqrCalc()`: Added.
* `meanMedian()`: Modified. Added support for using `manipulate()` from `manipulate` if RStudio is being used. Modified the graphic. Streamlined the code.
* `outlierTest()`: Added as a pass-through to `outlierTest()` in `car`.
* `powerSim()`: Modified. Added support for using `manipulate()` from `manipulate` if RStudio is being used. Modified the graphic. Streamlined the code.
* `predictionPlot()`: Modified. Fixed a bug related to `legend=` when a SLR is used.
* `sXXX()`: Modified. Changed all to use `manipulate()` from `manipulate` rather than `slider()` from `relax`.
* `transChooser()`: Modified. Added support for using `manipulate()` from `manipulate` if RStudio is being used. Streamlined the code.

# 0.4.3 Oct15
* Added `ImportFrom` for `nortest` and the "base" packages that are needed. Moved `car` from `Depends` to `ImportFrom`. Removed `FSAdata` from `Depends`.
* Updated `README.md` and `DESCRIPTION` to reflect the new websites.
* `accuracyPrecision()`: Modified. Changed default for `pts.trans=`. Streamlined code.
* `adTest()`: Modified. Send `x` directly to `ad.test()` from `nortest` package. Added a check to see if the namespace for `nortest` was available.
* `chisqPostHoc()`: Modified. Changed default `control=` to match that in `p.adjust.methods` (i.e., defaults to "Holm" method).
* `ci.t()`: Modified. Streamlined code.
* `ciSim()`: Modified. Changed to using `se()` from `FSA` for `method="t"`. Streamlined code.
* `compIntercepts()`: Deleted. Moved to `FSA`.
* `compSlopes()`: Deleted. Moved to `FSA`.
* `discharge()`: Deleted. Moved to `FSAmisc`.
* `distrib()`: Modified. Changed to use `col2rgbt()` from `FSA`.
* `faqNC()`: Deleted. Woefully out-of-data.
* `gofCI()`: Modified. Replaced use of `binconf()` from `Hmisc` with `binCI()` from `FSA`. Streamlined code.
* `iChk4Namespace()`: Added. Allowed `requireNamespaces()`s to be replaced throughout.
* `kAnova()`: Added from `FSA`.
* `kGLHT()`: Added from `FSA`.
* `kHtest()`: Added from `FSA`.
* `kREG()`: Added from `FSA`.
* `limnoProfilePlot()`: Deleted. Moved to `FSAmisc`.
* `print.anova()`: Deleted. Too problematic.
* `wetPerim()`: Deleted. Moved to `FSAmisc`.
* `z.test()`: Modified. Imported and then exported `z.test()` from `TeachingDemos`.

# 0.4.2 Aug14
* Moved `relax` package to a import.
* `addSigLetters()`:  Modified. Changed `typeoflm()` to `iTypeoflm()` due to changes in `FSA`.
* `compIntercept()`:  Modified. Changed `typeoflm()` to `iTypeoflm()` due to changes in `FSA`.
* `compSlope()`:  Modified. Changed `typeoflm()` to `iTypeoflm()` due to changes in `FSA`.
* `diagPlot()`:  Modified. Changed `typeoflm()` to `iTypeoflm()` due to changes in `FSA`.
* `predictionPlot()`:  Modified. Changed `typeoflm()` to `iTypeoflm()` due to changes in `FSA`.
* `transChooser()`:  Modified. Changed `typeoflm()` to `iTypeoflm()` due to changes in `FSA`.

# 0.4.1 Jul14
* Moved to github, compiling under R 3.1.0, and using roxygen2 4.0.0.
* Changed `@S3method` and `@Method` commands to `@export` as described [here](http://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods/22598266#22598266)  This required changes to the following files: `compIntercepts`, `compSlopes`, `discharge`, `glhtSig`, `highlight`, `identify.formula`, `plot.htest`, `print.anova`, `print.glht`, `print.htest`, `print.summary.lm`, `sdCalc`, and `wetPerim`.
* `discharge()`: Modified. Removed `newwin=` argument which will force the user to put the graphic in a new window and size the windows as they see fit, size the windows as they see fit, and set the graphing parameters as they see fit.
* `print.anova()`: Modified. Had to use `:::` as `print.anova()` was made an internal file in the `stats` package.
* `transChooser()`: Modified. Added `inclHist=FALSE` to two `residPlot()` calls to adjust for changes in `residPlot()` in FSA.
* `view()`:  Added (from the FSA package).
* `wetPerim()`: Modified. Removed `newwin=` argument which will force the user to put the graphic in a new window, size the windows as they see fit, and set the graphing parameters as they see fit.


# 0.4.0 Apr14
* Moved relax dependency to a suggest. Modified all functions that used `slider()` or `gslider()` (see below).
* `accuracyPrecision()`:  Modified. Corrected bug that plotted a 0 on the y-axis.
* `addSigLetters()`: Modified. Made explicit that `typeoflm()` is in the FSA package as it is no longer exported. Modified examples in help file with require(FSA).
* `ciSim()`: Modified. Added a require(relax) with a catch if not installed.
* `cltSim()`: Modified. Added a require(relax) with a catch if not installed.
* `compIntercepts()`: Modified. Made explicit that `typeoflm()` is in the FSA package as it is no longer exported. Modified examples in help file with require(FSA).
* `compSlopes()`: Modified. Made explicit that `typeoflm()` is in the FSA package as it is no longer exported. Modified examples in help file with require(FSA).
* `diagPlot()`: Modified. Made explicit that `typeoflm()` is in the FSA package as it is no longer exported. Modified examples in help file with require(FSA).
* `distrib()`: modified examples in help file with require(FSA).
* `meanMedian()`: Modified. Added a require(relax) with a catch if not installed.
* `mrNorm()`: modified examples in help file with require(FSA).
* `powerSim()`: Modified. Added a require(relax) with a catch if not installed.
* `predictionPlot()`: Modified. Made explicit that `typeoflm()` is in the FSA package as it is no longer exported. Modified examples in help file with require(FSA).
* `rSquared()`: modified examples in help file with require(FSA).
* `sumTable()`: Deleted. Move to FSA package.
* `transChooser()`: Modified. Added a require(relax) with a catch if not installed.


# 0.3.4 Nov13
* Changed R dependency to >3.0.0 (because FSA package has that dependency).
* Added importsFrom for `color.scale()` from plotrix for `discharge()` and `wetPerim()`.
* `.onAttach(): Added, was `.onLoad()`.
* `.onLoad(): Deleted, now `.onAttach()`.
* `addMargins(): Added, back from FSA.
* `addSigLetters()`: Added, back from FSA.
* `c.region()`: changed formatting of value and area answers for distributional plots to use `formatC()` and 3 digits for the value and 4 digits for the area. This primarily effects the plots from `distrib()` and `plot.htest()`.
* `discharge()`: Added from FSA.
* `limnoProfilePlot()`: Added from FSA.
* `print.anova()`: Added (actually resurrected with modifications to work properly).
* `wetPerim()`: Added from FSA.


# 0.3.3 21Dec12

* Replaced ImportFrom for TeachingDemos with relax, as that is the original `slider()`.
* General: added `call.=FALSE` to several `stop()`s.
* General: replaced `paste()` inside of several `cat()`s.

* `ciSim()`: Modified in a variety of ways. First, I changed to using `gslider()` rather than `slider()`. Second, I streamlined the main plotting function (creating an internal function called `ciSimPlot()`). Third, I removed the y-axis from the plot, changed the "summary statistics" printed at the top so that they fit better, and darkened the parameter line. Fourth, I used `match.arg()` to catch problems with method=.
* `cltSim()`: Modified. I changed to using `gslider()` rather than `slider()` and streamlined the main plotting function (creating an internal function called `cltSimPlot()`).
* `compIntercepts()`: Modified. Moved the handling of digits to the main function rather than the print method.
* `compSlopes()`: Modified. Moved the handling of digits to the main function rather than the print method.
* `distribSim` family of functions:  Modified. I changed to using `gslider()` rather than `slider()`.
* `meanMedian()`: Modified in a variety of ways. First, I changed to using `gslider()` rather than `slider()`. Second, I changed the plots so that the histogram is stacked on top of the scatterplot which now contains all of the color coding for examining both the mean and the median. This change makes the plotting window much smaller and easier to manipulate. Third, I add the outlier= argument so that outliers can be easily modeled. Fourth, I made several aesthertic changes (shading of histogram, color of points relative to the median, color of the residuals relative to the mean, default values for sliders, maximum values for alpha and beta sliders).
* `NCStatsSims()`: Deleted. Only works in non-Rstudio versions of Windows.
* `powerSim()`: Modified by I changed to using `gslider()` rather than `slider()` and streamlined the code (creating an internal function called `powerSimPlot()`).
* `print.confint.glht()`: Added.
* `print.summary.glht()`: Added.
* `print.summary.glm()`: Added.
* `print.htest()`: Added.
* `print.lm()`: Added.
* `print.summary.lm()`: Added.
* `sdCalc()`: Modified by replacing `numdigs=` with `digits=`.
* `sumTable()`: Modified by adding the `digits=` argument.
* `transChooser()`: Modified in a variety of ways. First, I changed to using `gslider()` rather than `slider()`. Third, streamlined the code so that the one-way and two-way share more code and the SLR and IVR do as well. Fourth, I eliminated the `fitPlot()` and the possible call to `ncvTest()` for the regression code. Fifth, I made show.stats=TRUE the default. Sixth, changed the starty= and startx= argument names to shifty= and shiftx=, respectively. Seventh, I  removed the dontrun{}s from the examples. Eight, I added an argument to control the color of the bars in the histogram and made the default a light gray.  Ninth, I updated the help file.
* `transChooser_ANOVA()` and `transChooser_REGRESS()`: added these internal files.  See transChooser() above.
* `transChooser.lm()`, `transChooser.ONEWAY()`, `transChooser.TWOWAY()`, `transChooser.SLR()`, `transChooser.IVR()`: deleted these files. See `transChooser()` above.


# 0.3.2 1Dec12

* Changed R dependency to >2.14.0

* .onLoad(): slight modification to avoid possible warning on RCMD Check.
* addMargins(): moved to FSA.
* addSigLetters(): moved to FSA.
* bootCase methods: moved to FSA.
* hist.formula(): moved to FSA.
* print.anova(): deleted.
* view(): deleted, moved to FSA.


# 0.3.1 25Nov12

* Switched to using Project development in RStudio.
* Switched to using semantic versioning for version numbers (which means that the hyphen before the last number in the version is replaced with a period).
* Deleted all references to deprecated functions (these are now very old).
* Added depends on FSA (so functions will be available to NCStats users).
* Deleted depends on sciplot.
* Deleted importsFrom gdata and gplots.
* Moved car from importsFrom to depends (so functions will be available to NCStats users).
* Moved plotrix from depends to importsFrom (only need thigmophobe() in highlight()).

* attached(): deleted.
* bootCoef(): deleted. no longer needed as bootCase() from car was modified to meet the same needs.
* chooseColors(): deleted, moved to FSA.
* compIntercepts(): changed class name to "compIntercepts" from "CompInts".
* compSlopes(): changed class name to "compSlopes" from "compSlopes".
* confint.bootCase(), htest.bootCase(), hist.bootCase(), plot.bootCase():  Added, but were all originally *.bootCoef.
* compIntercepts: changed a cat()-based warning to warning(). Set the digits for printing the common covariate to be the same as digits=.
* detachAll(): deleted.
* Ecoli: deleted, moved to FSA.
* fitPlot(): deleted, moved to FSA.
* hoCoef(): deleted, moved to FSA.
* htest(): deleted, moved to FSA.
* legendHelp(): deleted, moved to FSA.
* Mirex: deleted, moved to FSAdata.
* plotBinResp()(): deleted, moved to FSA.
* residPlot(): deleted, moved to FSA.
* sdCalc(): modified class name to "sdCalc" from "sdcalc".
* Subset(): deleted, moved to FSA.
* Summarize(): slightly modified two warnings about factors on the RHS. Then deleted and moved to FSA.
* sumTable(): modified to use tapply() in place of aggregate.table(). This reduces a need to importFrom gdata.
* transChooser(): removed require(tcltk) from code (seems to work without it).


# 0.3-0 8-Nov-12

* Moved several functions to FSA that are used quite often for fisheries analyses. Ultimately, I want to remove the dependency on NCStats.

* binCI(): moved to FSA.
* cltSim(): changed color of bars in both the population and sampling distribution.
* confint.nlsBoot(): moved to FSA.
* fact2num(): moved to FSA.
* gofCI(): removed dependence on binCI() (which is now in FSA) by using binconf() from Hmisc directly. Added a type="goodman" that will perform the method from Goodman (1965) (thanks to Paul Rabie for the discussion and code!).
* htest.nlsBoot(): moved to FSA.
* hist.formula(): modified. Slightly changed mgp() in par().
* hyperCI(): moved to FSA.
* ks2d(): moved to FSA.
* ks2dp(): moved to FSA.
* ksTest(): moved to FSA.
* lagratio(): moved to FSA.
* lsmean(), and related internals: moved to FSA.
* plotH(): moved to FSA.
* poiCI(): moved to FSA.
* popSizesPlot(): moved to FSA.
* pos2adj(): moved to FSA.
* rcumsum(): moved to FSA.


# 0.2-8 21Jun12

* Switched to testing under R version 2.15.0 (32-bit).
* Changed license specification from "GPL version 2 or newer" to "GPL (>= 2)" to avoid warning on check.


* bootCoef(): Added. Basically a wrapper to bootCase() in the car package with the ability to call some S3 methods (see below).
* confint.bootCoef(): Added. This is essentially the (very) old ci.bc().
* faqNC(): Added.
* fitPlot.glm(): Added.
* fitPlot.logreg(): Added.
* hist.bootCoef(): Added.
* htest.bootCoef(): Added. This is essentially the (very) old ho.bc().
* levenesTest(): Added. A convenience wrapper to leveneTest().
* logregPlot(): Removed. Functionality is now in fitPlot.logreg().
* plot.bootCoef(): Added.
* plotBinResp(): Added. Used in fitPlot.logreg() and as stand-alone.
* poiCI.rd: Updated details and references in the help page based on information provided by Jerry Lewis about the historical use of the method described in Ulm.


# 0.2-7 2Mar12

* added an importFrom for multcomp for glhtSig() below.

* .onLoad(): Modified. Moved the startup message into packageStartupMessage() in hopes of eliminating the warning when checking the package.
* addMargins(): Modified. Corrected error when dealing with 1-dimenstional tables and handling call from percTable() correctly with no margin=.
* addSigLetters(): Modified. Added code to deal with a bug that appeared if the x-axis variable when adding to a two-way ANOVA interaction plot was an ordered, rather than regular, factor.
* chisqPostHoc(): Added.
* distrib(): Modified. Added a digits= argument for printed answers.
* glhtSig(): Added.
* percTable(): Modified. Added addMargins= argument. Corrected problem with 2-dimensional table and call to addMargins with margin=NULL argument.
* sumTable(): Added.
* transChooser(): Modified. Added alpha= argument and set outlier test p-values that are greater than 1 to 1.
* typeoflm(): Modified. Fixed a call to the deprecated lgrep().


# 0.2-6 1Oct11

* Switched to compiling under R version 2.13.1 (32-bit).
* Removed utils from importFrom because it is a base package and due to the removal of functions below.
* Removed tools from importFrom due to the removal of functions below.
* Removed suggested packages from description.

* findUpdates(): Removed. Moved to miscOgle package. 
* ksTest(): Added.
* swvCode(): Removed. Moved to miscOgle package.
* swvCounts(): Removed. Moved to miscOgle package.
* swvPvalue(): Removed. Moved to miscOgle package.
* swvANOVA(): Removed. Moved to miscOgle package.
* swvGLHT(): Removed. Moved to miscOgle package.
* swvHtest(): Removed. Moved to miscOgle package.
* swvREG(): Removed. Moved to miscOgle package.
* swv2ad(): Removed. Moved to miscOgle package.
* swv2PDF(): Removed. Moved to miscOgle package.
* updateNCStats(): Removed.


# 0.2-5 19Aug11

* Modified description file to show my e-mail address.
* Added an ImportFrom from tools for texi2dvi used in swv2pdf().

* ABCens90: Added this data file.
* binCI(): Removed reference to an unused type in the .Rd file.
* c.region(): Modified. Changed all arguments (except cex.ans=) so that they did not have defaults. Also added an argument, lbl.col, to allow the axis label to be a different color then the shaded area. Changed the show.axis= argument to lbl.axis=. Re-ordered the arguments.
* distrib(): Modified. Changed label for normal distribution so that the sd= value is rounded to three decimal places. This makes for a "cleaner" plot when the provided sd value is really a computed standard error. Added a  shade.trans= argument to allow the shaded area to have a transparency value; this will allow the plots to be a bit softer to the eye. Added a lbl.col= argument for the axis label so that it is not printed in the transparent color if transparency is used. Modified the call to c.region() per the  changes above.
* expandTable(): Modified. Added methodology to deal with a "contingency table" with only one row or one column. Also fixed a bug that implied that three dimensional tables could be used.
* geomean(): Added.
* geosd(): Added.
* gofCI(): Added.
* meanMedian(): Modified. Added an argument that allows one to plot user-derived data rather than just the data simulated from a beta distribution. Also streamlined the code by adding two internal functions -- meanMedianPlots() and meanMedianBeta().
* percTable(): Added.
* plot.htest(): Modified. Modified the call to c.region() per the changes above.
* plotH(): Modified. Modified to plot variable names on axes by default. Fixed error in how x- and y-axes were labeled if a formula was not used.
* powerSim(): Modified. Modified the call to c.region() per the changes above.
* rSquared(): Added.
* swvCode():  Modified. Modified to allow file= argument to include the .rnw extension and still output a file with a .r extension.
* swv2PDF(): Added.


# 0.2-4 15Jun11

* Switched to compiling under R version 2.13.0.

* accuracyPrecision():  Modified. Added several arguments that allowed more control over the plottings, changed the default number of points plotted, changed the default pch used for plotting the mean, renamed the internal functions (to camel code), removed the make.labels() internal function (incorporated into the makeTargets() internal function), and changed the default coloring for the rings on the targets. Functionality is unchanged. Hopefully it is a bit more general.
* ABDLakes: Added this data file.
* Frogs: Added this data file.
* hist.formula(): Modified. Added the ylab= argument with a default to "Frequency". Added the right= argument with a default to FALSE; this is opposite of the default used in base hist(). Added warn.unused=FALSE arguments to the two calls to hist() with plot=FALSE so that the warnings would be suppressed.
* lsmean(): Added. This is the exact lsmean.default() and lsmean.lme() functions from Yandell's pda package, including three dependent functions which were kept as internal functions in NCStats. The pda package does not seem to work with R >2.13.0. to make this function available to those that use it I put it in NCStats.
* SquareLakePopn: Added this data file (from FSAdata).
* srsdf(): Added.
* updateNCStats(): Modified. Added the tools package to the packages that should not be detached when updating. Also made sure that the package.dependencies function is from the tools package. This eliminated a warning when checking the packages.


# 0.2-3 18Apr11

* Added importFrom utils for installed.packages() and available.packages().

* compIntercepts(): Modified. Changed catches at front of function to note if a model with an interaction was sent and, if so, that the interaction term would be ignored. Also added an if that will allow the RHS of the model formula to have the covariate and factor in either order.
* findUpdates(): Added.
* lgrep(): Modified. Changed to be a pass-through to grepl() and added a message telling the user to use grepl() instead. Basically I just found grepl() and no longer need lgrep().
* pos2adj(): Added. Allows ability to place point labels according to directional coordinates (e.g., "NE").
* residPlot(): Modified. Corrected an error related to labeling points if they were an outlier. Basically needed to realize that the studentized residuals are a named vactor.
* Subset(): Modified. Added a catch to send a warning message if the resultant data frame has no rows in it.
* swvCode(): Modified. Added the moreItems= argument to allow the user to more easily add more items to be removed shen Stangling. Basically the items in moreItems get appended to the items in itemsToRemove. In addition, changed the ItemsToRemove= argument to itemsToRemove (note capitalization).
* swvHtest(): Added.
* updateNCStats(): Added.
* z.test():  Added. Basically same as z.test() from TeachingDemos package. Modified for my use at Northland. 

# 0.2-2 3Mar11

* moved to compiling under 2.12.1 (32 bit)
* changed dependency to >2.11.1
* added a dependency to tcltk package (see transChoooser below).

* addMargins(): Added. Largely the same as addmargins() except that I changed the margin= argument such that margin=1 gives a marginal value at the end of the rows and margin=2 gives a marginal value at the end of the columns. This change makes addMargins() fit more closely (in my opinion) with the use of margin= in prop.table().
* fact2num(): Added. Changes a factor to numeric values. Useful for example if age or year is a factor but you need it to be numeric.
* fitPlot.nls(): Modified. Deleted the col= argument and added col.pt= and  col.mdl= arguments to allow for different colors for the points and the fitted line.
* plotH(): Modified. Added catches for determining if Y variable is not quantitative and if more than one X (or RHS) variable is sent (i.e., function only works for one explanatory variable). Created separate internal functions for whether the X variable is quantitative (the old plotH()) or categorical (simply a  pass through to barplot).
* predictPlot(): Added. Simply a pass-through to (i.e., does the exact same thing as) predictionPlot(). Simply for convenience.
* residualPlot(): Added. Simply a pass through to (i.e., does the exact same thing as) residPlot(). Sipmly for convenience.
* Summarize(): Modified. Removed dimnames attribute on the level labels in the resulting dataframe when a quantitative variable is summarized by a categorical variable. Mostly for aesthetic reasons. Added a digits= argument to be more compatible with other R functions. Retained the numdigs= argument for backward compatability.
* swvCode(): Modified. Added two more items to ItemsToRemove= argument to handle changes to Stangle() output. Added a blanks= argument that can be used to remove all or just "extra" blank lines in the Stangle() output. Also added the annotate=FALSE argument to set as default for sending to Stangle(). This forces Stangle() to not include code block comments.
* transChooser(): Modified. Added require(tcltk) to attempt to ensure that this package is loaded. Corrected an error which caused the Levene's test results not to be printed above the residual plot when show.stats=TRUE for the ONEWAY and TWOWAY functions. Corrected an error which caused overprinting of the outlier test p-value if outlierTest() returned more than one value  (only the minimum will be now be printed). Created internal functions for placing the p-value labels above each graph (reduced redundancy). Removed several calls to round() inside of a formatC() call. 

# 0.2-1 31Jan11

* removed dependency on nortest package (see adTest() below).

* adTest(): brought, with name change, ad.test() from nortest package into NCStats. I changed to 'camel-case' naming convention and then removed dependency on  the nortest package.
* Summarize(): moved a bunch of code into internal functions. Modified the code for a quantitative variable by a factor variable so that the result prints as a data.frame (rather than a matrix) that includes one or two columns that contain the levels of the factor variables. This provides a nicer looking output and an output that could be subsetted if needed. Also modified the code so that multiple "values" of a factor variable can be excluded. The help code examples were modified to illustrate the changes.
* transChooser(): updated ad.test() references to adTest(). 

# 0.2-0 1-Dec-10

* added a work-around .Rd files for the three functions imported from the car package namespace and exported as part of the NCStats namespace. This seems to have corrected the warning produced during Rcmd check.

* distrib(): modified. Added the se= argument in the hopes that this might be somewhat easier for students that are learning to distinguish between sampling and population distributions.
* hist.formula(): modified. Added the ability to create a histogram of a  variable not separated by groups with code such as ~x, x~1, or x~0. This allows one to use the data= argument for a single histogram. Added a main= argument for the situation where a single histogram is plotted and set the default to "" to avoid the annoying main plot label. Added a byrow= argument so that the user can choose the order in which the plots are populated. Changed ylmts= to ymax= argument so as not to confuse ylmts= with ylim=. Changed main.pre= to pre.main= to avoid confusion with partial matching with main= argument. Changed cols= argument to ncol= argument (and rows= to nrow=) to avoid confusion with partial matching of col= that may be sent in the "dots" argument (and to make consistent with notation in matrix()). Modified code such that if pre.main=NULL then no main title will be printed on the histograms. Removed the breaks= argument (will work as part of dots argument). Removed the code that "caught" if the formula= argument was not a formula -- this was redundant because this is an S3 generic for formula types.
* .onLoad(): modified. Changed to include version number of loaded version.
* plotH(): added.
* residPlot(): modified. Added code to plot multiple points returned by  outlierTest(). Add the use of thigmophobe.labels to place outlier text marks with a minimum of overlapping. Added three internal functions --  addOutlierTestResults(), addLoessLine(), and getMainTitle() to reduce and streamline code among the different generics.
* spois(): modified. Fixed small bug (extra comma).
* summarize.formula(): modified. Added the ability to create summaries of a  variable not separated by groups with code such as ~x, x~1, or x~0. This allows one to use the data= argument for a single variable. Modified default exclude= value to better handle these situations.
