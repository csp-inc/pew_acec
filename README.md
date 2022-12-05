## Ecological values of candidate Areas of Critical Environmental Concern
#### Conservation Science Partners
#### Caitlin Littlefield (caitlin@csp-inc.org)

The scripts within this repository generate metrics and figures that express the relative ecological values (e.g., species richness, ecological connectivity, geophysical diversity, extractive industry threats, etc.) of a candidate BLM Area of Critical Environmental Concern under consideration. 

The workflow is as follows, and scripts must be run in this order: 

`00_setup.v2.R` - sets working directory<sup>1</sup> and loads requisite packages and sets several parameters for R environment</br>
`01_dataLoad.v2.R` - loads shapefiles for candidate ACEC and relevant geographies and rasters of ecological indicators<sup>2</sup></br>
`03_extractVals.v2.R` - extracts ecological indicator values for both the candidate ACEC and random samples across multiple spatial domains</br>
`04_indicatorFigs.v2.R` - selects extracted values<sup>3</sup> and generates summary figures<sup>4</sup></br>
`05_bonusPts.R` - generates summary statistics that are unique to specific candidate ACECs (e.g., contribution to Important Bird Areas)</br>

<sup>1</sup> User must modify both working directory and input/outputs in lines 7-11. temporarily change working directory to data folder in line 119 then back in line 230.</br>
<sup>2</sup> User must temporarily change working directory to data folder in line 119 then back in line 230.</br>
<sup>3</sup> User must modify csv name in line 13 to reflect date and version generated in script 03.
<sup>4</sup> User must also turn on/off ACEC and its respective state in lines 106-118.</br>
_Nb there is no script 02, as it was discontinued early in project._</br>
