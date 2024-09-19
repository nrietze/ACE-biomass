# Code and documentation
This repository contains the code and output for Zemlianskii et al. (in prep.): Finding northernmost baselines: high variability of landscape-level biomass on Eurasian polar desert islands.

> [!WARNING]
> Update when officially done.
 > The necessary data is publicly available under [![DOI](https://img.shields.io/badge/DOI_dryaddoi-blue)](https://datadryad.org/stash/share/SqYWvX6K7085r4-3NtiBl10JwYfS3AdWRtX3jOTb5Uw).

## Repository structure:
Here is the structure of this repo, files have been excluded from this tree for readability.

```bash
├───code
├───data
│   ├───feature_layers
│   ├───raster
│   │   ├───binary
│   │   ├───binary_cut
│   │   ├───fcover
│   │   ├───prediction
│   │   └───uncertainty
│   └───tables
└───figures
```

- All R scripts used in this study are in the folder `code`.
  - `predict_classes.R` runs the drone imagery classification.
  - `Regressions_cover_biomass.R` contains the code for the statistical analysis of the relationships between biomass, species richness, in-situ cover and remotely sensed cover.
  - `mean_subplot_biomass_to_species_richness.Rmd` is used to study and plot the observed biomass and species richness of plant functional types .
  - `get_site_fcover.R` is used to compute the fractional vegetation cover from the drone imagery for the focal window analysis and extrapolation to landscape-scale.
  - The scripts used to produce Figures 3, A1, and A3 are named accordingly.
- The folder `data` and its subdirectories are empty and should contain the data that can be downloaded from Dryad (see link on top).
- The folder `figures` contains the figures that are produced in the correspoinding scripts.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Software requirements
The data pre-processing and data analysis was using R 4.2.2 (2022-10-31 ucrt). Newer versions of these software packages will likely work, but have not been tested.

Code development and processing were carried out in Windows 10 (64 bit), but execution should (in theory) be platform independent.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Contact
Code development and maintenance: Vitalii Zemlianskii ([vitalii.zemlianskii@uzh.ch](vitalii.zemlianskii@uzh.ch)) and Nils Rietze ([nils.rietze@uzh.ch](nils.rietze@uzh.ch))

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Acknowledgements
This research used data collected during the Arctic Century Expedition, a joint initiative of the Swiss Polar Institute (SPI), Antarctic and Arctic Research Institute (AARI) and Helmholtz Centre for Ocean Research Kiel (GEOMAR) and supported by the Swiss Polar Foundation. We are grateful to Heidemarie Kassens, Mikhail Makhotin, Vasiliy Povazhnyi for their leadership and all expedition members for their company and support. This study was supported by a Swiss Government Excellence Scholarship (2019.0075). We thank Tatiana Koroleva, Vladislav Petrovsky, Irina Urbanavichene, Alexey Potemkin (Komarov Botanical Institute RAS) and Anastasia Kurka for their help with species identification. We thank Tatiana Nosova, Artem Fedorov, Dmitrii Botev and Arina Goluzina for their support with biomass measurements.

## Citation
When citing elements in this repository, please cite as:

V. Zemlianskii, K. Ermokhina, N. Rietze, R. Heim, J. Assmann, J. Rüthi, N. Loginova, G. Schaepman-Strub (in prep.). 
Finding northernmost baselines: high variability of landscape-level biomass on Eurasian polar desert islands. 

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## License
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)
