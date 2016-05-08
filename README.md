rcusum - An Implementation of Cusum Control Charts as an R package
============
Author: Alexander Meyer [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.51159.svg)](https://zenodo.org/record/51159#.Vy-OCvmLRhE)

This R Package implements the charts as described by _Rogers et al. (2004, doi:10.1016/j.jtcvs.2004.03.011)_. Cusum control charts are devices for monitoring arbitrary processes for a prespecified event. This package and manual uses the term failure instead of event, although any other type of event such as successes could be monitored as well. The processes of our interest are typically surgical procedures.

To create a basic control chart 4 parameters have to be specified:

* $p0$: risk of failure when the process is in control (acceptable failure rate for the procedure)
* $p1$, where $p1>p0$: failure rate considered unacceptable
* $\alpha$: Type I error (the probability of concluding that the failure rate has increased, when in fact it has not)
* $\beta$: Type II error (the probability of concluding that the failure rate has not increased, when in fact it has)
 
Anatomy of a Cusum Control Chart
--------------------------------
A cusum chart shows the cumulative sum of failures for a given process. There are two boundaries shown on the graph.

If the cusum curve...

* ... lies __within the boundaries__, there is not enough evidence for a significant conclusion, thus there is __still need for monitoring__!
* ... __crossed the upper boundary__, we can conclude that the __failure rate has increased to the unacceptable__ event rate $p1$ (at the specified significance level).
* ... __crossed the lower boundary__, we can conclude that the __failure rate is less than or equal to the acceptable__ event rate $p0$ (at the specified significance level)
 
_"The natural progression of the graph for an individual or institution with acceptable performance is toward the lower boundary for this method of constructing control limits."_  (Rogers et. al, 2004)


rcusum Shiny App
-----------------
You can try some functions of the package interactivly in a Shiny App:
```
require(shiny)
shiny::runGitHub(repo="rcusum",username="meyera", subdir = "inst/rcusum-ShinyApp")
```
