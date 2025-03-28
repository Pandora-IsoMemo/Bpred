# Bpred 25.03.0

## Updates
- reduce package size by optimizing test data and adding test-model files to the `.Rbuildignore`

# Bpred 25.02.0

## New Features
- option to change the layout of points or to hide points in "Formulas" plot (#57)

# Bpred 25.01.2

## New Features
- integrate new module from `shinyTools` to add points with custom layout to plots (#55)

# Bpred 25.01.1

## Updates
- prevents a warning during imputation of example data (#25)

# Bpred 25.01.0

## Updates
- update links in ReadMe and in app header

# Bpred 24.08.0

## Bug Fixes
- prevent crash of app when having only NA samples (#48)

# Bpred 24.05.0

## New Features
- Renaming of R-Package
- pkgdown documentation

# Bpred 24.04.0

## New Features
- option to show a credible interval in "Formulas" - "Display formulas" (#33)

## Updates
- update table under tab "Measures" formatted such as table under tab "Data" (#33)

# Bpred 24.03.0

## Updates
- integration of modules from shinyTools: dataExport, plotExport (#36)

# Bpred 23.12.1.1

- add R-cmd-check workflow
- add pkgdown documentation
- Ensure compliance of CRAN Standard in NEWS File

# Bpred 23.12.1

## Bug Fixes
- _Import of models from Pandora_: 
  - an error message occurred when trying to load a model from pandora.
  - fix: adding the missing download of the zip file from the url before unpacking the zip

# Bpred 23.12.0

## New Features
- _Import of models_: display of "About" information that is associated to a selected Pandora 
  Repository

# Bpred 23.09.0

## New Features
- _Import of models_:
  - option to import models from Pandora platform

# Bpred 23.08.0

## Bug Fixes
- updates error message (#27)

# Bpred 23.07.1

## New Features
- show Bayesian R-squared in formula overview (following: https://avehtari.github.io/bayes_R2/bayes_R2.html)

# Bpred 23.07.0

## Bug Fixes
- fix bug with the header of matrix and the update of the matrix (#22)
- use most recent version of shinyMatrix

# Bpred 23.03.1

## Bug fixes
- add remote package to enable the _Import Data_ module

# Bpred 23.02.2

## Updates
- catch errors and(!) warnings within a pop up

# Bpred 23.02.1

## New Features
- the _Import Data_ module is now imported from the new package DataTools 
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_

# Bpred 23.01.2

## New Features
- fixed sidebars with auto scroll in all tabs (iso-app #4)

## Updates
- path to remote models was added to the ReadMe 

# Bpred 23.01.1

## Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository (#1)
  - if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

# Bpred 22.11.2

## New Features
- _Impute missings_ as option in "Estimates" tab.

# Bpred 22.11.1

## New Features
- save inputs when downloading a model (#1)
- update all input fields after uploading a model (#1)

## Updates
- also update table of measures after upload of a model (#13)
