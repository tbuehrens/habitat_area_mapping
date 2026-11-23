# Run from North_Puget_Sound after the explicit public-source fetch:
# python scripts/fetch_phase3_nwi_tidal.py
source("R/phase3_tidal_sources.R")
source("R/phase3_nwi_tidal.R")
source("R/phase3_freshwater_tidal.R")
result <- build_phase3_freshwater_tidal(rebuild_nwi=TRUE)
print(result$audit)
print(result$conflict_audit)
cat("Freshwater-tidal supplement prepared; CFS and scenario outputs unchanged.\n")
