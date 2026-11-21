# Willapa

`Willapa_Habitat_Areas.R` is the existing watershed analysis for Willapa-area chum, coho, and Chinook. Saved figures are in `outputs/maps/`; `outputs/tables/willapa_hab_lengths.csv` reports habitat lengths by use type in **kilometers** and watershed areas in square kilometers. The saved results were moved without recalculating them.

Run here with `Rscript Willapa_Habitat_Areas.R`, or from the repository root with `source("Willapa/Willapa_Habitat_Areas.R")` in R. Output paths resolve to this regional directory; the script does not change the caller's working directory.

No local source geodatabase is required by this script. It requests HUC10/HUC12 watershed boundaries through `nhdplusTools::get_huc()`, [WDFW SWIFD distribution](https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0), and Natural Earth state boundaries through `rnaturalearth::ne_states()`. A full run makes many network requests and requires those services. Their current availability was not checked during the folder reorganization.

R needs `pacman` and the packages listed in the script's `pacman::p_load()` call, including `sf`, `tidyverse`, `httr`, `jsonlite`, `units`, `rnaturalearth`, and `nhdplusTools`. `sf` requires GDAL/GEOS/PROJ support. Watershed IDs, species filters, and use-type calculations retain their existing values and behavior.
