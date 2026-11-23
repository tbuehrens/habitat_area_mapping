# Habitat area mapping

Code and saved results are organized by region:

| Region | Code and documentation | Maps and tables |
| --- | --- | --- |
| Lower Columbia | [Lower_Columbia](Lower_Columbia/README.md) | [Outputs](Lower_Columbia/outputs/) |
| Willapa | [Willapa](Willapa/README.md) | [Outputs](Willapa/outputs/) |
| North Puget Sound | [North_Puget_Sound](North_Puget_Sound/README.md) | [Phase 3 results](North_Puget_Sound/outputs/phase3_tidal_barriers/) |

Open `habitat_area_mapping.Rproj` for the whole repository, or a regional
`.Rproj` to work within one region. For example, from the repository root:

```r
source("North_Puget_Sound/Phase3_Tidal_Barriers.R")
```

Regional entry points resolve their own input and output folders and restore
the caller's working directory. The Lower Columbia and Willapa READMEs list
their original data prerequisites; the Lower Columbia estuary geodatabase must
be supplied separately.

North Puget Sound contains the full occupancy-model/SWIFD workflow, numbered
barrier evidence, cached public source snapshots, and the preserved
untruncated, Phase 1, Phase 2, and Phase 3 scenarios. Phase 3 is the latest
review set: 16 NOAA populations plus the custom North Fork Stillaguamish
watershed above Deer Creek, with reviewed Woods/Olney and French/Pilchuck Creek barriers and mapped
tidal exclusions. Original standalone Nooksack outputs are retained in
`North_Puget_Sound/outputs/legacy_nooksack/`.

To validate saved North Puget Sound results without regenerating maps:

```text
Rscript North_Puget_Sound/Check_Results.R
```

Scientific source snapshots and output files retain their original bytes so
their recorded checksums remain usable. Historical provenance paths may name
the former working directory; the workflow resolves those paths against the
current region's `spatial_data` folder. Installed Python packages, bytecode,
and temporary R session files are excluded from Git.
