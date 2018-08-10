# NSAPH Air Pollution Prediction Code

This project is an implementation of Qian Di's air pollution prediction model.

## Git Development Structure

All new branches should be branched off of the `test` branch. Each branch off of that should be focused on implementing
one specific feature for the overall program. Features should be developed and tested in their individual branches, then
merged into `test` for integrated testing. If all merged features in `test` are functioning together, then `test` can be
merged into `master`.

## Development guide

All new functions added to the package should utilize the same configuration file paradigm
wherein there are no required parameters for any user facing functions. To add a new field to
the configuration file (often required when new functionality is being added) please do the 
following:

1. Add the field and a reasonable default value to the appropriate default configuration file
in `inst/yaml_files`. This will most likely be the 'Config_Default.yml' file unless changes
to training parameters are being made.  

2. Please create a getter function in `R/config.R`. This function have the name `get_<field_name>` with no inputs. It should return an error if there is no configuration file (see other getter functions for the standard check). Otherwise it should return the value from the configuration file as an object ready to be used without any further processing (such as converting from a string to another type).
