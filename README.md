# Code and documentation
This repository contains the code and output for Zemlianskii et al. (in prep.): Finding northernmost baselines: high variability of above-ground biomass on Eurasian polar desert islands.

## Data availability:
Full geobotanical plots as well as all biomass, cover and species richness data are available through Dryad [https://datadryad.org/stash/share/SqYWvX6K7085r4-3NtiBl10JwYfS3AdWRtX3jOTb5Uw](https://datadryad.org/stash/share/SqYWvX6K7085r4-3NtiBl10JwYfS3AdWRtX3jOTb5Uw). Species data table is additionally provided in the Appendix (Appendix Table 5). The full plots will be also made available upon publication through the Arctic Vegetation Archive (Zemlianskii et al., 2023; [https://avarus.space/](https://avarus.space/)). Sentinel-2 imagery can be freely downloaded from the Copernicus Dataspace Browser at [https://browser.dataspace.copernicus.eu/](https://browser.dataspace.copernicus.eu/).


## Repository structure:
Here is the structure of this repo, files have been excluded from this tree for readability.

```bash
├───code
├───data
│   ├───feature_layers
│   ├───raster
│   │   ├───sentinel2
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
  - `get_Sentinel2_metrics.R` preprocesses and calculates the spectral index metrics for Sentinel-2 imagery.
  - `Regressions_cover_biomass.R` contains the code for the statistical analysis of the relationships between biomass, species richness, in-situ cover and remotely sensed cover.
  - `mean_subplot_biomass_to_species_richness.Rmd` is used to study and plot the observed biomass and species richness of plant functional types .
  - `get_site_fcover.R` is used to compute the fractional vegetation cover from the drone imagery for the focal window analysis and extrapolation to landscape-scale.
  - The scripts used to produce Figures 3, A1, and A3 are named accordingly.
- The folder `data` and its subdirectories are empty and should contain the data that can be downloaded from Dryad (see link on top) and the Copernicus Dataspace browser.
- The folder `figures` contains the figures that are produced in the correspoinding scripts.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Sentinel-2 data preparation
When Sentinel-2 data is downloaded from the Copernicus Dataspace Browser, it'll be in JPEG2000 format (.jp2). We converted the 10 m surface reflectance data using GDAL. Below is an example for the cmd command for one scene:

```
gdal_translate SENTINEL2_L2A:MTD_MSIL2A.xml:10m:EPSG_32646 output_10m.tif
```

This step needs to be repeated for the four Sentinel-2 scenes used in this study.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Software requirements
The data pre-processing and data analysis was using R 4.2.2 (2022-10-31 ucrt). Newer versions of these software packages will likely work, but have not been tested.

Code development and processing were carried out in Windows 10 (64 bit), but execution should (in theory) be platform independent.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Contact
Code development and maintenance: Vitalii Zemlianskii (vitalii.zemlianskii [at] uzh.ch) and Nils Rietze (nils.rietze [at] uzh.ch)

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## Acknowledgements
This research used data collected during the Arctic Century Expedition, a joint initiative of the Swiss Polar Institute (SPI), Antarctic and Arctic Research Institute (AARI) and Helmholtz Centre for Ocean Research Kiel (GEOMAR) and supported by the Swiss Polar Foundation. We are grateful to Heidemarie Kassens, Mikhail Makhotin, Vasiliy Povazhnyi for their leadership and all expedition members for their company and support. This study was supported by a Swiss Government Excellence Scholarship (2019.0075). Joel Rüthi was supported by two WSL internal grants (5231.00900.002.01, Metagenomics and 5233.00388.001.01, Bioactive permafrost). We thank Tatiana Koroleva, Vladislav Petrovsky, Irina Urbanavichene, Alexey Potemkin (Komarov Botanical Institute RAS) and Anastasia Kurka (Polar-Alpine Botanical garden) for their help with species identification. We thank Tatiana Nosova, Artem Fedorov, Dmitrii Botev and Arina Goluzina for their support with biomass measurements.

## Citation
When citing elements in this repository, please cite as:

V. Zemlianskii, K. Ermokhina, N. Rietze, R. Heim, J. Assmann, J. Rüthi, N. Loginova, G. Schaepman-Strub (in prep.). 
Finding northernmost baselines: high variability of above-ground biomass on Eurasian polar desert islands. 

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)

## License
<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

[to top](https://github.com/nrietze/ACE-biomass/main/README.md)
