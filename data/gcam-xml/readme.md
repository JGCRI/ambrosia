# Running the New Food Demand in GCAM

This directory contains the XML add-on files needed to run the new
food demand system in GCAM, along with python programs to generate the
add-on files and, where applicable, the original GCAM inputs that were
used by the generator programs.  

## Directory Contents

**GCAM config file**

* `config-new-food-demand.xml`: A GCAM configuration file that includes the
  necessary add-on files to run the new demand system.
  
**GCAM add-on files**

* `delete-old-food-demand.xml`: Deletes the old final demand sectors
  for "FoodDemand_Crops" and "FoodDemand_Meat" in all regions.
  Other final demand sectors related to agricultural products (e.g.,
  "NonFoodDemand_Crops") are not changed.  
* `food-demand-input.xml`: Creates the new food final demands and
  gives the food supply sectors nonzero logit exponents.
  
**Original GCAM input files**

* `demand_input.xml`: Original input for food demands.  This file is
  used to generate `food-demand-input.xml`.  The version included here
  is from the GCAM-v4.3 release.
  
**Generator Programs**

* `gen-delete.py`: Generates `delete-old-food-demand.xml`.  This
  program is self-contained.  Because of that, it has a hard-wired
  list of regions that is current as of GCAM-v4.3.
  
* `gen-food-sectors.py`:  Generates `food-demand-input.xml`.  This
  program reads `demand_input.xml` to generate the basic structure
  for its output.  The program also includes a table of parameters
  derived from the Monte Carlo calculation implemented elsewhere in
  this repository, and the regional bias correction factors, also
  calculated elsewhere in this repository.
  
## Setup for GCAM

The GCAM configuration file for the new demand system is set up to
look for the food demand repository under the `exe` directory in the
GCAM workspace.  The easiest way to arrange this is to symlink your
copy of this repository to the appropriate location in the GCAM
workspace.

In the examples that follow, we'll pretend that your food demand
workspace (i.e., the clone of this repository) is under your home
directory in `~/wrk/food-demand`, and your GCAM workspace is in
`~/wrk/gcam-workspaces/gcam-food-demand`.  Replace these as necessary
with the actual locations where you have installed these workspaces.

1. Copy the configuration file to the GCAM workspace.  
```
cp ~/wrk/food-demand/data/gcam-xml/config-new-food-demand.xml
~/wrk/gcam-workspaces/gcam-food-demand/exe/
```

2. Change to the GCAM workspace and create a symbolic link to the food
   demand workspace.  
```
cd ~/wrk/gcam-workspaces/gcam-food-demand/exe
ln -s ~/wrk/food-demand ./
```

At this point you will be able run GCAM using the new configuration
file.  
  
