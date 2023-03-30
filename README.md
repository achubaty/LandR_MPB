## LandR_MPB

### Mountain Pine Beetle Red Top Model

Evaluate short-run potential for MPB establishment, eruption, and spread in Canadian boreal forests.

**Authors:**

- Alex M. Chubaty (<achubaty@for-cast.ca>)
- Barry Cooke (<barry.cooke@nrcan-rncan.gc.ca>)
- Eliot McIntire (<eliot.mcintire@nrcan-rncan.gc.ca>)

**Copyright:**

His Majesty the King in Right of Canada, as represented by the Minister of Natural Resources, 2023.

**Date:** March 29, 2023

### Default Study Area

![](images/mpb_studyArea.png)

### Modules

#### Study area and data prep

- [canClimateData](https://github.com/PredictiveEcology/canClimateData)
- [LandR_MPB_studyArea](https://github.com/achubaty/LandR_MPB_studyArea)

#### MPB population dynamics and spread

- [mpbClimateData](https://github.com/achubaty/mpbClimateData)
- [mpbMassAttacksData](https://github.com/achubaty/mpbMassAttacksData)
- [mpbPine](https://github.com/achubaty/mpbPine)
- [mpbRedTopSpread](https://github.com/achubaty/mpbRedTopSpread)

#### LandR vegetation dynamics

- [Biomass_borealDataPrep](https://github.com/PredictiveEcology/Biomass_borealDataPrep)
- [Biomass_core](https://github.com/PredictiveEcology/Biomass_core)
- [Biomass_regeneration](https://github.com/PredictiveEcology/Biomass_regeneration)
- [Biomass_speciesData](https://github.com/PredictiveEcology/Biomass_speciesData)
- [Biomass_speciesFactorial](https://github.com/PredictiveEcology/Biomass_speciesFactorial)
- [Biomass_speciesParameters](https://github.com/PredictiveEcology/Biomass_speciesParameters)
- [Biomass_summary](https://github.com/PredictiveEcology/Biomass_summary)
- [gmcsDataPrep](https://github.com/ianmseddy/gmcsDataPrep)

#### fireSense wildfire simulation

- [fireSense](https://github.com/PredictiveEcology/fireSense)
- [fireSense_dataPrepFit](https://github.com/PredictiveEcology/fireSense_dataPrepFit)
- [fireSense_dataPrepPredict](https://github.com/PredictiveEcology/fireSense_dataPrepPredict)
- [fireSense_EscapeFit](https://github.com/PredictiveEcology/fireSense_EscapeFit)
- [fireSense_EscapePredict](https://github.com/PredictiveEcology/fireSense_EscapePredict)
- [fireSense_IgnitionFit](https://github.com/PredictiveEcology/fireSense_IgnitionFit)
- [fireSense_IgnitionPredict](https://github.com/PredictiveEcology/fireSense_IgnitionPredict)
- [fireSense_SpreadFit](https://github.com/PredictiveEcology/fireSense_SpreadFit)
- [fireSense_SpreadPredict](https://github.com/PredictiveEcology/fireSense_SpreadPredict)
- [fireSense_summary](https://github.com/PredictiveEcology/fireSense_summary)

### Getting the code

All modules are written in R and all model code was developed collaboratively using GitHub (<https://github.com>), with each module contained in it's own (currently private) git repository (see below).
Code that is shared among modules was bundled into R packages, and hosted in open git repositories.
All package code is automatically and regularly tested using cross-platform continuous integration frameworks to ensure the code is reliable and free of errors.

```bash
## main branch
git clone --recurse-submodules -j8 https://github.com/achubaty/LandR_MPB

## development branch
git clone --single-branch -b development --recurse-submodules -j8 https://github.com/achubaty/LandR_MPB
```

### Prerequisites

- R version 4.0 or higher
- GDAL, PROJ, GEOS system libraries

The code is mostly self-sufficient: additional packages than those below are needed, but will be installed automatically.
See `03-packages.R` to see which additional packages will be used.
