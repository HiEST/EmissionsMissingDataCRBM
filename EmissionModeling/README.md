# Emission modeling

This code is based on the paper of [Jalkanen et al.(2009)](https://www.atmos-chem-phys.net/9/9209/2009/acp-9-9209-2009.pdf).
Using AIS data and ship characteristics for each ship (e.g. installed engine) it
can evaluate how much pollutants (NOx, SOx and CO2) are emitted. As AIS is a GPS
enabled system, the emisions are precisely located.

This code requires the following datasets:
- AIS data containing the ships to evaluate
- Ship charasteristics, mainly regarding the engines and maximum speed (Named
  IHSData in the code).
- Main engine predictions from the *Main Engine Regression* code.

The main code file is *CRBMEmissions.R*. Dataset paths and other parameters can 
be changed at the start of the code. In order to run the code just use 
`Rscript CRBMEmissions.R` after you have modified the previously mentioned
parameters.
