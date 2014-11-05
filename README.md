## EWStools

[![Build Status](https://travis-ci.org/jknowles/EWStools.png?branch=master)](https://travis-ci.org/jknowles/EWStools)

### Overview

`EWStools` is the source-code behind many of the modules within the [Wisconsin 
Dropout Early Warning System](http://wise.dpi.wi.gov/wisedash_dews) created 
by the [Wisconsin Department of Public Instruction](http://www.dpi.wi.gov). While 
the framework was designed particularly to the development of early-warning 
predictive models on education data, these tools represent a more generalized 
framework for building, testing, and exploring models built through the `train` 
function in the R package `caret`. As such, this package extends the features 
of `caret` to make it more efficient to search across model types, explore 
model performance on test and training data, and to draw ROC comparisons of 
classification models specifically. 

`EWStools` is currently in beta and many of the functions are changing regularly.

### Features

`EWStools` provides two distinct sets of features for model builders. The first 
is tools to automate the **search** for the best fitting model **across model 
types**. The second set of features is the creation of a new object class, `ROCit` 
objects, which allow for the **easy comparison** of ROC performance of 
classification models on both test and training data. 

### Model Search

`EWStools` features wrapper code for `caret`'s `train` function which makes it 
easy to build a sequential test of many model types available to `train` and 
store the results of the test efficiently. 