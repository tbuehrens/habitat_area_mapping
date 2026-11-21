# Steelhead distribution scenarios

This region is self-contained. Open `North_Puget_Sound.Rproj`, or run the
commands below with `North_Puget_Sound/` as the working directory. From the
repository root, regional entry points can also be sourced by their full
relative path, such as `source("North_Puget_Sound/Phase2_Reviewed_Barriers.R")`;
they restore the caller's working directory afterward.

The branch includes saved scientific outputs, cached public source reports
and geometries, and validation evidence. Installed Python packages and
bytecode remain local. `Check_Results.R` checks all saved scenarios without
regenerating maps.

The current products are kept in separate scenario folders:

| Folder | Purpose |
| --- | --- |
| `outputs/review_swifd_connections/` | Preserved untruncated review: the full occupancy model plus qualifying SWIFD connections, with no cuts. |
| `outputs/phase1_total_barriers/` | Phase 1: remove upstream distribution at explicitly Total natural barriers, verified or unverified, plus the authorized Gorge Dam override. |
| `outputs/phase2_reviewed_barriers/` | Phase 2: user-reviewed passage corrections, additional named barriers, and a separate custom Stillaguamish watershed map. |

## Phase 2 review

Run `Rscript Phase2_Reviewed_Barriers.R` after preparing the cached, validated
targeted source bundles. Phase 2 starts from the complete original model plus
qualifying SWIFD patches, reapplies approved Phase 1 masks, and applies the
reviewed corrections. Both earlier output folders remain unchanged.

The changes add the Deer Creek barrier above Higgins Creek (#663), the
reported Total waterfall near the West Cady Creek mouth (#344), Culmback Dam
at Spada Lake (#912), and the natural South Fork Tolt falls below the reservoir
(#913). The Deer report's Partial classification is retained separately from
the user's passage override. South Fork Nooksack #797/#798 no longer truncate
the scenario; #810/#833 remain. Lower North Fork Tolt mainstem cuts are disabled,
retaining #222.

The 17-map set includes the original 16 NOAA populations and a separate custom
North Fork Stillaguamish watershed above the Deer Creek confluence. It excludes
Deer Creek itself and does not replace the original Stillaguamish polygon.
Custom mileage overlaps official populations and must not be added to their
totals. Review `phase2_changes.csv`, `population_phase2_miles.csv`, and the
basin audits in the Phase 2 folder. Validate the real products with
`Rscript tests/validate_phase2_reviewed_barriers.R`.

To prepare the targeted caches from their public sources:

```text
python scripts/prepare_phase2_targeted_barriers.py
Rscript scripts/build_phase2_dam_tolt.R --prepare-sources --fetch-basins
```

The custom watershed can be prepared in R with
`source("R/phase2_stillaguamish.R")`,
`fetch_phase2_stillaguamish_sources()`, and
`build_phase2_stillaguamish()`. Separate read-only checks in
`tests/validate_phase2_custom_stillaguamish.R` and
`tests/validate_phase2_culmback_terminal.R` verify the custom drainage,
South Fork Nooksack restoration, and terminal dam case independently.

Phase 1 reads the existing review and cached source geometry. It leaves every
file in the untruncated review unchanged. **Green stream matching confirms
stream identity, not fish passability.** It does not authorize a barrier cut.

## Run Phase 1

Open `North_Puget_Sound.Rproj`, set the working directory to this folder,
and run:

```r
source("Phase1_Total_Barriers.R")
```

Or, with R on PATH:

```text
Rscript Phase1_Total_Barriers.R
```

This writes only `outputs/phase1_total_barriers/`. Do not regenerate the
untruncated review or run legacy barrier-truncating workflows as part of this
phase. The existing regional and Nooksack review wrappers regenerate the
untruncated review; they are separate from the Phase 1 entry point.

Required packages are `sf`, `dplyr`, `ggplot2`, `ggrepel`, `httr`, `jsonlite`,
`igraph`, and `gridExtra`.
Public-source downloads and intermediate caches are stored in `spatial_data/`.
The scientific snapshots are included in this branch; later runs reuse them.

## Phase 1 policy and products

A natural barrier must lie within the NOAA population scope,
have explicit Total evidence without a contradictory passage attribute, and
pass the strict own-stream and junction checks. Total evidence can come from
the zero-passability field code or a site-matched natural-barrier assessment
that explicitly reports `Blockage: Total`. A waterfall, gradient, height, or
green stream-match symbol alone does not establish total blockage. Gorge Dam
retains its explicitly authorized override, review number 903. **Verified and
unverified Total records are included; Partial records are excluded.**

The current run applies 128 cuts: 122 unverified Totals, five field-verified
Totals, and Gorge Dam. Every applied basin has zero unexplained upstream
linework remaining. Of 263 selected barriers, 133 have complete upstream
coverage (128 own-basin cuts and five covered by an approved downstream basin);
130 remain pending. `basin_completeness_audit.csv` lists every selected barrier,
its coverage, residual lengths, and any reason for deferral.

Upstream drainage-basin masks identify contributing areas independently of gaps
in the model. Spatial clipping removes model and SWIFD geometry inside an
approved upstream basin, including disconnected tributaries, headwaters, and
reservoir reaches. A graph connection to the barrier is not required. Adjacent
basins and downstream portions remain. The basin outlet must correspond to the
barrier; any local precision correction must be explicit and bounded. Unknown,
conflicting, unmatched, or hydrologically unvalidated records are deferred.

Gorge's downstream anchor uses [USGS station 12178000 at Newhalem](https://waterdata.usgs.gov/monitoring-location/USGS-12178000/).
[Seattle City Light's study plan, section 2.5](https://www.seattle.gov/Documents/Departments/CityLight/CurrentProjects/P-553_04_Skagit_RSP_20210407.pdf)
places that gage downstream of Gorge Dam. Other direction decisions and their
named-reference support are recorded in the direction audit.
The [USGS Gorge Reservoir station](https://waterdata.usgs.gov/nwis/uv?legacy=1&site_no=12177700)
reports a 1,159-square-mile drainage area including 400 square miles in Canada;
the upstream mask must include the transboundary watershed.

Phase 1 maps use blue for remaining model linework, orange for remaining SWIFD
patches, and dotted taupe for alignment links. **Red numbered points identify
applied cuts.** Direction-unresolved linework remains visible, so visibility
does not certify that habitat is accessible.

`outputs/phase1_total_barriers/` contains:

- Regional and Nooksack overview PNGs, `maps/` for all 16 populations, and the
  population-map PDF.
- `Skagit_before_after.png`: a direct comparison with the untruncated geometry.
- `population_phase1_miles.csv`: the population mileage partition below.
- `phase1_barrier_decisions.csv` and `barrier_characteristics.csv`: all numbered
  review records with original fields, decoded values, report evidence,
  dimensions, selection, deferral, and applied-cut status.
- `phase1_cut_barriers.csv`: only barriers where a cut was applied.
- `phase1_graph_cut_audit.csv` and `downstream_direction_audit.csv`: selected
  barriers, upstream-removal results, direction evidence, and unresolved cases.
- `upstream_basin_audit.csv`, `upstream_basin_validation.csv`, and
  `nldi_delineation_audit.csv`: final mask residuals and local protections,
  basin-approval decisions, and service request/name/course evidence.
- `upstream_coverage_audit.csv` and `upstream_navigation_audit.csv`: distinguish
  an applied own-basin cut, proven coverage by a downstream basin, and pending
  requests; retain the upstream-network evidence.
- `basin_completeness_audit.csv`: one consolidated check for each selected
  barrier, including remaining upstream geometry and any explicitly proven
  local downstream protection at the basin outlet.
- `barrier_field_inventory.csv`, `barrier_code_inventory.csv`,
  `barrier_metadata_summary.csv`, and `report_fetch_audit.csv`: source metadata,
  coded meanings, report availability, and evidence provenance.
- `baseline_file_manifest.csv`, `run_metadata.json`, and `sessionInfo.txt`:
  baseline checksums, scenario assumptions, and software versions.
- `North_Puget_Sound_Steelhead_phase1.gpkg`: `noaa_populations`,
  `phase1_extent_status`, `phase1_remaining`, `phase1_removed_upstream`,
  `phase1_unresolved_direction`, `barrier_characteristics`, `applied_barriers`,
  `downstream_anchors`, and `upstream_basin_masks`, in EPSG:26910.

| Mileage column | Meaning |
| --- | --- |
| `baseline_untruncated_miles` | Original model plus SWIFD patches, excluding alignment links. |
| `remaining_direction_resolved_miles` | Remaining physical linework outside the direction-unresolved subset. |
| `remaining_direction_unresolved_miles` | Remaining linework whose upstream direction is unresolved for a selected barrier. |
| `removed_upstream_miles` | Physical linework proven upstream of applied Phase 1 cuts. |
| `phase1_remaining_visible_miles` | Resolved plus unresolved remaining linework. |
| `phase1_remaining_model_miles` / `phase1_remaining_swifd_miles` | Remaining mileage by source. |

For each population, baseline miles equal resolved remaining + unresolved
remaining + removed miles. Alignment links are excluded. These are scenario
geometry lengths, not a complete estimate of accessible habitat. Population
polygons overlap, and multiple barrier-removal footprints can overlap; do not
sum population or individual-barrier removal totals to estimate unique mileage.

In the decision table, `phase1_selected` means that the evidence and matching
policy permit an upstream-cut assessment. `upstream_basin_approved` requires a
validated complete drainage mask, including its local outlet precision.
`phase1_applied` requires both selection and basin approval. Graph-only cuts
remain diagnostics when a complete basin cannot be approved. Read
`phase1_cut_status` for holds and unresolved cases; approval coverage is reported
separately from the completeness of each approved cut.
Some selected Totals can already be covered by an approved downstream Total's
basin. This is recorded separately from applying their own cut: it requires
positive USGS upstream-network membership on a distinct reach, or explicitly
published flow direction and upstream ordering on the same reach. Both the
modeled and indexed locations must be more than 100 m inside the approved basin.
Geographic overlap alone, a shared reach ID without ordering, or a nearby basin
boundary does not establish this coverage.
Report URLs and assessment-page numbers preserve the Total evidence.
Missing or sentinel dimensions remain unknown, and printed species/run labels
are not treated as checked passage evidence. Reported Total is a qualitative
adult-salmonid classification, not independently verified steelhead-run passage.

## Read the preserved untruncated review maps

- **Blue:** the original occupancy-model linework, without barrier cuts.
- **Orange:** accepted SWIFD routes that connect separate model components.
- **Dotted taupe:** short endpoint alignment links. These support the geometry
  connection and are excluded from distribution mileage.
- **Numbered gray barrier points:** locations whose stream identity or junction
  placement needs review.
- **Numbered green barrier points:** confirmed own-stream matches that pass the
  junction checks, without a requested total-barrier/Gorge cut.
- **Numbered red barrier points:** requested total-barrier/Gorge candidates that
  also pass the strict stream-identity and junction checks.
- **Numbered amber barrier points:** requested cut candidates whose stream
  identity or junction placement still needs review.

NOAA polygons outline all 16 North Cascades MPG steelhead populations. A map's
barrier review numbers correspond to `barrier_review.csv` and the GeoPackage.
The original barrier positions are also retained for comparison. Existing
review numbers are preserved; newly placed records receive numbers after the
previous maximum. An unconfirmed location keeps its number so review comments
remain attached to the same source record.

SWIFD routes follow eligible winter and summer steelhead stream geometry.
Presence, spawning, and rearing records include artificial, transported, and
presumed distribution; gradient-accessible categories are excluded. Both model
attachment locations must be within **100 meters** of eligible SWIFD linework.
Where no qualifying path exists, the gap remains. This adds selected connecting
routes rather than overlaying the full SWIFD distribution.

## Review products

Everything listed below is in `outputs/review_swifd_connections/`:

- `North_Puget_Sound_Steelhead_map.png`: regional overview.
- `maps/`: individual maps for all 16 NOAA populations.
- `North_Puget_Sound_Steelhead_population_maps.pdf`: the population maps in one PDF.
- `Nooksack_Steelhead_map.png`: Nooksack overview.
- `population_distribution_miles.csv`: untruncated model and SWIFD route lengths.
- `barrier_review.csv`: numbered location-review points within 50 m, including
  locations whose strict stream identity remains unconfirmed. It records source
  attributes, cut requests, matching evidence, and future-cut eligibility.
- `barrier_snap_rejections.csv`: records without a display location within the
  50 m tolerance.
- `barrier_match_candidates.csv`: every physical candidate edge within 50 m of
  each original barrier, with line-level identity and junction evidence.
- `barrier_field_definitions.json`: source field definitions and coded domains
  for interpreting the barrier attributes.
- `swifd_connector_audit.csv` and `gap_endpoint_audit.csv`: route and endpoint
  decisions under the 100 m connection rule.
- `North_Puget_Sound_Steelhead.gpkg`: the geometry used by the maps and review.
- `run_metadata.json` and `sessionInfo.txt`: sources, assumptions, tolerances,
  generation details, and software versions.

The GeoPackage uses NAD83 / UTM zone 10N (EPSG:26910) and contains these layers:
`noaa_populations`, `raw_occupancy_model`, `steelhead_patched_untruncated`,
`swifd_gap_routes`, `endpoint_alignment_links`, `barriers_snapped`,
`barriers_original`, `gap_endpoint_audit`, `barrier_match_candidates`, and
`stream_matching_edges`. The matching edges retain graph-edge and channel-chain
identifiers. A `stream_identity_samples` layer also records sampled identity
evidence when available.

### Mileage

| Column | Meaning |
| --- | --- |
| `raw_model_miles` | Original occupancy-model linework within the population, before any barrier cuts. |
| `swifd_added_miles` | Qualifying SWIFD routes added within the population; excludes alignment links. |
| `patched_untruncated_miles` | `raw_model_miles + swifd_added_miles`. |

These are **untruncated distribution geometry lengths**, not estimates of
accessible habitat. Population polygons can overlap, including summer and
winter ranges, so their totals should not be summed to estimate unique MPG
mileage. The same occupancy model is used for all run timings because its
published attributes do not identify a steelhead run. Canadian portions of NOAA
polygons do not have complete coverage in the Washington prediction.

### Untruncated review barrier fields

Every barrier record in the preserved review has `truncation_applied = FALSE`.
The Phase 1 decision table separately adds report evidence and applied status.
Review the numbered
snapped locations against the original points, stream names, source attributes,
and field definitions before selecting any future cuts.

Location review and a confirmed stream match are separate decisions. The
workflow evaluates **all physical model/SWIFD candidate edges within 50 m** and
compares the candidate line's pathway with a named-stream reference. A nearby
reference point alone cannot establish identity. A future cut requires a unique
positive match to the barrier's own stream and safe placement away from an
ambiguous junction. Unnamed, mismatched, unsupported, or competing matches do
not fall back to the nearest line for future-cut eligibility. Their numbered
display locations remain available for review.

| Field | Meaning |
| --- | --- |
| `display_location_available` | A physical stream location is available within 50 m; this alone does not confirm stream identity. |
| `stream_identity_confirmed` | The candidate has a unique positive own-stream identity. |
| `junction_ambiguous` | Placement is near an ambiguous physical junction and blocks future-cut eligibility. |
| `future_cut_requested` | Explicit total-passage evidence or the Gorge Dam override requests a later cut. |
| `eligible_for_future_cut` | The requested candidate also passes strict identity and junction checks. |
| `identity_review_status` | Explanation of the identity decision or remaining review need. |

`snap_accepted` refers to the strict identity/junction decision; a numbered
display point can have `snap_accepted = FALSE`. `future_cut_candidate` matches
`eligible_for_future_cut`. Neither field applies a cut during this checkpoint.
The total-barrier/Gorge request does not establish steelhead-specific
passability. Unknown or missing passability values remain unknown.

## Sources

- [NOAA steelhead population polygons](https://maps.fisheries.noaa.gov/server/rest/services/WCR/ESA_Species_Ranges/MapServer/57):
  select `MIDLEVEL_GROUP = 713` and dissolve by `NWFSC_POP_ID`. The loader checks
  the expected 16 populations.
- [Steelhead Modeled Statewide, layer 0](https://services.arcgis.com/rcya3vExsaVBGUDp/arcgis/rest/services/Statewide_Dist_Models_1b66a3ddb471/FeatureServer/0):
  include the complete published steelhead prediction. Coho is not included.
- [WDFW/NWIFC SWIFD, layer 0](https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0):
  winter and summer steelhead records selected as described above. Exact
  categories and source snapshots are recorded in the metadata.
- [WDFW named-stream NHD reference, layer 4](https://geodataservices.wdfw.wa.gov/arcgis/rest/services/FP_Projects/NHDwithLLID/MapServer/4):
  line geometry and names used for positive stream-identity evidence, without a
  fish-distribution filter. Its LLID identifies a watercourse, not an individual
  NHD segment. The occupancy model and barrier records do not publish compatible
  reach IDs, so unnamed or unsupported matches remain unconfirmed.
- [USGS NLDI upstream basin and split-catchment services](https://api.water.usgs.gov/docs/nldi/basin/):
  precise drainage polygons independent of the occupancy model's discontinuities.
  Service responses require stream-name/course and local outlet validation;
  successful download alone does not approve a mask.
- WDFW FPDSI [Natural Barrier - Verified, layer 5](https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/5)
  and [Natural Barrier - Not Field Verified, layer 6](https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/6):
  barrier locations and original attributes. Phase 1 separately reads the linked
  site-assessment reports; the preserved review remains untruncated.

## Checks

```text
Rscript tests/test_strict_barrier_review.R
Rscript tests/test_strict_barrier_candidates.R
Rscript tests/validate_untruncated_review.R
Rscript tests/test_barrier_report_evidence_independent.R
Rscript tests/test_phase1_local_cuts_independent.R
Rscript tests/test_upstream_basin_completeness_independent.R
Rscript tests/test_nldi_cached_loader_independent.R
Rscript tests/validate_phase1_total_barriers.R
```

The fixture checks cover line-level identity, competing candidates, junctions,
stable numbering, and location tolerance. The export validator compares the
current review with cached source geometry, enumerates candidate pairs
independently, and checks that no barrier truncation occurred. It does not
download data or rewrite outputs. The Phase 1 checks additionally cover explicit
report evidence, local upstream cuts, alternative routes, disconnected
components, complete upstream-basin clipping, adjacent-basin retention,
source-length partitions, and unchanged baseline files.
