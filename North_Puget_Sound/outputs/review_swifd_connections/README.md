# Untruncated steelhead distribution review

This is the single current output folder. Regenerate it with
`Rscript Review_SWIFD_Connections.R` from the repository root.
**No barrier truncation is applied.** Use the current review entry point rather
than legacy barrier-cutting workflows.

- Blue lines show the complete occupancy model within NOAA population polygons.
- Orange lines show qualifying SWIFD connections across gaps; both attachment
  locations must meet the 100 m rule. Remaining gaps stay disconnected.
- Dotted taupe alignment links are excluded from mileage.
- Numbered gray barrier points need stream-identity or junction review; green
  points pass both checks without a cut request. Red points are eligible requested
  total-barrier/Gorge candidates. Amber points are requested candidates still
  blocked by identity or junction uncertainty. No point color indicates a cut.

`barrier_review.csv` matches the barrier numbers on the maps and in
`North_Puget_Sound_Steelhead.gpkg`. Display points lie on physical streams within
50 m; their original positions remain in the `barriers_original` layer. Existing
review numbers stay attached to their source records, including unconfirmed
locations; new records receive numbers after the previous maximum.
`barrier_snap_rejections.csv` lists records without an in-tolerance display
location. `barrier_field_definitions.json` supplies source definitions and domains.

All barriers have `truncation_applied = FALSE`. A numbered location is not a
confirmed stream match. All physical candidates within 50 m are evaluated
against named-stream **line geometry**, rather than the nearest reference point.
`barrier_match_candidates.csv` and the GeoPackage's `barrier_match_candidates`,
`stream_matching_edges`, and optional `stream_identity_samples` layers preserve
this evidence.

Future-cut eligibility requires a unique positive own-stream match, no ambiguous
junction, and an explicit total-barrier/Gorge request. Unnamed, mismatched,
unsupported, and competing identities cannot fall back to a nearest-line cut.
Inspect `stream_identity_confirmed`, `junction_ambiguous`,
`future_cut_requested`, `eligible_for_future_cut`, and `identity_review_status`.
`snap_accepted` is the strict match decision; display points can remain numbered
when it is false. `future_cut_candidate` equals `eligible_for_future_cut`.
Missing passability values remain unknown. Review these records before deciding
whether any later upstream cuts should be made.

`population_distribution_miles.csv` reports
`raw_model_miles + swifd_added_miles = patched_untruncated_miles`.
These are untruncated geometry lengths, not accessible-habitat estimates.
Alignment links are excluded, and overlapping population totals should not be
added together.

The folder includes the regional and Nooksack overviews, `maps/` for all 16
populations, the population-map PDF, GeoPackage, connector/endpoint audits,
and run metadata. See the repository README for source links and methods.
