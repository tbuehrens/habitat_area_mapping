# Phase 2: reviewed barrier corrections

Start with `maps/` for the 16 NOAA populations plus the separate custom Stillaguamish watershed.
`North_Puget_Sound_Steelhead_population_maps.pdf` contains all 17 maps.

- Deer Creek #663: user-reviewed full passage barrier above Higgins Creek; the source report's Partial classification remains visible.
- West Cady Creek #344: WDFW-reported Total waterfall near the mouth; exact own-stream alias correction.
- Culmback Dam #912: new numbered cutoff for the Spada Lake / Sultan River watershed.
- South Fork Nooksack: #797 and #798 disabled; #810 (Wanlick Creek) and #833 (Bell Creek) retained.
- North Fork Tolt: lower mainstem cutoffs disabled; #222 retained.
- South Fork Tolt: #913 is the natural falls below the reservoir, distinct from the dam and inventory site #170.
- Custom area #1001: North Fork Stillaguamish watershed above the Deer Creek confluence; excludes Deer Creek itself. Original NOAA polygons remain unchanged.

Blue lines are the original occupancy model; orange lines are qualifying SWIFD gap patches.
Red points and numbers identify the barriers actually used. Alignment links do not count toward mileage.

`population_phase2_miles.csv` gives before, remaining, removed, and change from Phase 1 for each area.
The custom watershed overlaps official populations; do not add its mileage to population totals.
`phase2_changes.csv` lists reviewed changes; `phase2_barrier_decisions.csv` retains every original barrier number and source evidence.
`targeted_review_audit.csv` and `new_barrier_basin_validation.csv` document the added sites and watershed provenance.
`upstream_basin_audit.csv` records every applied basin's final residual and any independently proven local downstream outlet correction.
`basin_completeness_audit.csv` consolidates current Phase 2 coverage and pending cases. Phase 1 fields retained in the decision table are historical.
Unresolved Phase 1 basin requests remain pending; remaining linework is not certified accessible habitat.

The GeoPackage supplies original NOAA polygons, the custom watershed, full extent status, remaining and removed geometry, barrier attributes, applied points, direction anchors, and upstream masks.
Both earlier output folders are preserved byte-for-byte; see `preserved_source_manifest.csv`.

Reproduce from the repository root with `Rscript Phase2_Reviewed_Barriers.R` after preparing the cached, validated targeted source bundles.
Validate read-only with `Rscript tests/validate_phase2_reviewed_barriers.R`.
