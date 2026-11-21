# Phase 1: Total barriers

Run `Rscript Phase1_Total_Barriers.R` from the repository root. This folder is
separate from the preserved `outputs/review_swifd_connections/` baseline.
Phase 1 does not rewrite that baseline.

**Green means stream identity is confirmed, not that a barrier is total.**
Phase 1 selects natural barriers with explicit Total
field/report evidence, strict stream matches, and safe junction placement,
plus the authorized Gorge Dam override (#903). Verified and unverified Totals
are included. Partial records are excluded; Unknown, conflicting, or unmatched
records are deferred.

The current run applies 128 cuts: 122 unverified Totals, five field-verified
Totals, and Gorge Dam. All 128 applied basins pass the check for zero unexplained
upstream linework. Of 263 selected barriers, 133 have complete coverage: the
128 own-basin cuts plus five covered by an approved downstream basin. The other
130 remain pending. Start with `basin_completeness_audit.csv` for a consolidated
barrier-by-barrier review of coverage, residual lengths, and deferral reasons.

Approved upstream drainage masks clip the distribution spatially, including
isolated headwaters, tributaries, and reservoir reaches disconnected from the
barrier in the model graph. Adjacent basins and downstream portions remain.
Mask outlet precision must be validated; a graph-only cut cannot establish
complete upstream removal. No global coast-connection filter is applied.

- Blue: remaining occupancy model; orange: remaining SWIFD patches.
- Dotted taupe: alignment links, excluded from mileage.
- Red numbered points: applied cuts, matching `phase1_cut_barriers.csv`.
- `Skagit_before_after.png`: comparison with the full untruncated geometry.

`phase1_barrier_decisions.csv` and `barrier_characteristics.csv` retain all review
numbers and source/report fields. `phase1_selected` permits a direction test;
`upstream_basin_approved` requires a complete validated drainage mask;
`phase1_applied` requires both selection and approval. `phase1_cut_status`
explains holds. Graph-only results do not substitute for complete-basin approval.
Coverage by an already approved downstream basin is a separate result and does
not claim a new cut at the upper barrier. It requires positive upstream-network
evidence and locations safely inside that basin; overlap alone is insufficient.
Report URLs, page numbers, blockage classifications, dimensions, and quality
flags make the evidence reviewable. Height/type alone never establish Total,
and printed species/run checkbox labels are not interpreted as checked.

`population_phase1_miles.csv` balances baseline mileage as:

`remaining_direction_resolved_miles + remaining_direction_unresolved_miles + removed_upstream_miles`.

`phase1_remaining_visible_miles` includes both remaining groups. Alignment links
are excluded. Remaining geometry is not certified accessible habitat, and
overlapping population or per-barrier totals should not be added together.

The folder also includes all 16 population maps, overviews, a multipage PDF,
GeoPackage, graph/direction audits, decoded field/code inventories, report-fetch
audit, baseline checksums, and run metadata. See the repository README for
the layer list, source links, and validation commands.

The GeoPackage's `upstream_basin_masks` layer supplies the approved boundaries.
`upstream_basin_audit.csv` records remaining-inside and bounded downstream
protection lengths; `upstream_basin_validation.csv` and
`nldi_delineation_audit.csv` explain basin approval and request/identity status.
`upstream_coverage_audit.csv` and `upstream_navigation_audit.csv` document proven
coverage by another downstream basin separately from applying a new own cut.
Remaining upstream linework must be zero except explicitly proven local
downstream geometry within the documented outlet-precision correction.
