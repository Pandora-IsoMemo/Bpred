# MpiBpred app development version

# BMSCApp app 

## Version 23.01.1

### New Features
- the _Import Data_ module is now imported from the new package DataTools 
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_

## MpiBpred 23.01.2

### New Features
- fixed sidebars with auto scroll in all tabs (iso-app #4)

### Updates
- path to remote models was added to the ReadMe 

## MpiBpred 23.01.1

### Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository (#1)
  - if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

## MpiBpred 22.11.2

### New Features
- _Impute missings_ as option in "Estimates" tab.

## MpiBpred 22.11.1

### New Features
- save inputs when downloading a model (#1)
- update all input fields after uploading a model (#1)

### Updates
- also update table of measures after upload of a model (#13)
