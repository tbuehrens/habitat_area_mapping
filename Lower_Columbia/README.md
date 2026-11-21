# Lower Columbia

`LCR_Habitat_Areas.R` is the existing Lower Columbia ESU/DPS analysis. Existing figures are in `outputs/maps/`; habitat-length tables are in `outputs/tables/`. Lengths are in **kilometers**, with watershed areas in square kilometers. The saved results were moved without recalculating them.

Run from this directory with `Rscript LCR_Habitat_Areas.R`, or from the repository root with `source("Lower_Columbia/LCR_Habitat_Areas.R")` in R. Input and output paths resolve to this regional directory; the script does not change the caller's working directory.

Place these prerequisite data in `Lower_Columbia/spatial_data/`:

- `Columbia_River_Estuary_Ecosystem_Classification.gdb`, containing `CREEC_Hydrogeomorphic_Reach`. Obtain it from the [USGS Columbia estuary classification release](https://www.sciencebase.gov/catalog/item/631405b7d34e36012efa2f91). This geodatabase is required and is not included in the repository.
- `WCR_Salmon_Steelhead_gdb_2015.gdb`, containing `fish`. The script retains its [NOAA download fallback](https://www.webapps.nwfsc.noaa.gov/portal/sharing/rest/content/items/097239ff29b44a8b87acc048f0363229/data) when this geodatabase is absent.

The script also requests WDFW SWIFD distribution, HUC boundaries through `nhdplusTools::get_huc()`, the dated [StreamNet population workbook](https://cax.streamnet.org/download/ca-data-all%2011-27-2023%2010%2015.xls), and Natural Earth state boundaries through `rnaturalearth::ne_states()`. A full run requires those services to be available and makes many network requests. Their current availability was not checked during the folder reorganization.

R needs `pacman` and the packages listed in the script's `pacman::p_load()` call, plus `readxl` for the StreamNet workbook. In particular, `sf` needs its GDAL/GEOS/PROJ support and a driver capable of reading File Geodatabases. The script retains its original species/run filters, population edits, and manual length substitutions.
