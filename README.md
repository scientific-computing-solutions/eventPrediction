# eventPrediction

[![Build Status](https://travis-ci.org/scientific-computing-solutions/eventPrediction.svg?branch=master)](https://travis-ci.org/scientific-computing-solutions/eventPrediction)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/eventPrediction)](https://cran.r-project.org/package=eventPrediction)
[![Coverage Status](https://coveralls.io/repos/scientific-computing-solutions/eventPrediction/badge.svg?branch=forCRAN&service=github)](https://coveralls.io/github/scientific-computing-solutions/eventPrediction?branch=forCRAN)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/scientific-computing-solutions/eventPrediction?branch=forCRAN&svg=true)](https://ci.appveyor.com/project/scientific-computing-solutions/eventPrediction)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/eventPrediction)](https://cran.r-project.org/package=eventPrediction)

Event Prediction in Clinical Trials with Time-to-Event Outcomes

This R package implements methods to predict either the required number to achieve 
a target or the expected time at which you will reach the required number of events.
You can use this package in the design phase of clinical trials or in the reporting
phase. Which means you can use simulate a trial based on a set of assumption and 
run prediction and calculate uncertainties when your trial will finish. Alternatively 
you can upload your trial data, simulate additional patient recruitment based on 
observed one and run prediction on when the target number of event will be reached
or the expected number of events at a given time.

## Contributors
Dalevi, Daniel (maintainer); Burkoff, Nikolas; Hollis, Sally; Mann, Helen; Metcalfe,
 Paul; Ruau, David;

## Installation

To install the development version from GitHub:
```R
install.packages("devtools")
# We spent a lot of time developing the vignettes. We recommend the read but 
# building them from source takes some time
devtools::install_github("scientific-computing-solutions/eventPrediction", 
                         build_vignettes = TRUE)
```
