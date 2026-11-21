# Explicit report evidence is separate from green stream identity and does not
# apply a cut. Fixtures deliberately include printed species checkbox labels.
source(file.path("R", "barrier_metadata.R"))
check <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
assessment <- function(id, blockage = "Total", barrier = "Yes", height = "12.5") {
  paste("Natural Barrier Assessment Report", paste0("Site ID: ", id),
    "Stream: Test Creek", "Survey Date: 01/02/2020", "Type: Waterfall",
    paste0("Barrier: ", barrier), paste0("Blockage: ", blockage),
    "Barrier Criteria: Survey assessment", paste0("Waterfall Height (m): ", height),
    "Plunge Pool Depth (m): -999.99", "Stream Length (m): 40",
    "Gradient (%): 8.5", "Channel Width (m): 0", "Species Blocked:",
    "Steelhead (Summer)", "Steelhead (Winter)",
    "Classification is based on the swimming abilities of adult salmonids.", sep = "\n")
}
parse <- function(id, blockage = "Total", ...) parse_natural_barrier_report(
  assessment(id, blockage, ...), id, report_url = paste0("https://example.invalid/", id, ".pdf"),
  retrieved_utc = "2026-11-21 00:00:00 UTC", source_md5 = "fixture")

total <- parse("100")
check(total$report_identity_verified && total$report_explicit_total && total$report_blockage == "Total",
  "A site-matched natural-barrier assessment with explicit Total should be retained as evidence")
check(total$report_waterfall_height_m == 12.5 && is.na(total$report_plunge_pool_depth_m) &&
  total$report_plunge_pool_depth_quality == "missing_negative_sentinel" &&
  total$report_stream_length_m == 40 && total$report_gradient_percent == 8.5 &&
  total$report_channel_width_m == 0,
  "Report dimensions must decode correctly while negative missing sentinels remain unknown")
check(!total$report_steelhead_run_blockage_verified &&
  grepl("Not inferred", total$report_species_run_evidence, fixed = TRUE),
  "Printed species/run checkbox labels must not be treated as checked evidence")

partial <- parse("101", "Partial")
height_only <- parse("102", "Unknown", height = "25")
no_barrier <- parse("103", "Total", barrier = "No")
narrative <- parse("104", "not a total barrier")
wrong_id <- parse_natural_barrier_report(assessment("1234"), "123")
generic <- parse_natural_barrier_report("Site ID: 100\nConstruction Report\nBlockage: Total", "100")
for (item in list(partial, height_only, no_barrier, narrative, wrong_id, generic)) {
  check(!item$report_explicit_total,
    "Partial, height-only, negative, unrelated, or non-assessment evidence was promoted to Total")
}
check(!wrong_id$report_identity_verified, "A SiteId prefix collision must not validate report identity")
mixed <- parse_natural_barrier_report(c(assessment("105", "Total"), assessment("105", "Partial")), "105")
check(mixed$report_blockage_conflict && !mixed$report_explicit_total,
  "Conflicting report classifications must block a confirmed Total")

# Field domains distinguish a yes-barrier code from total passability. A report
# can add explicit Total, but cannot overrule conflicting source attributes.
fields <- list(
  list(name = "PercentFishPassableCode", alias = "Percent passable", type = "esriFieldTypeInteger",
    domain = list(codedValues = list(list(code = 0, name = "N/A"), list(code = 10, name = "0"),
      list(code = 40, name = "100"), list(code = 99, name = "Unknown")))),
  list(name = "FishPassageBarrierStatusCode", type = "esriFieldTypeInteger",
    domain = list(codedValues = list(list(code = 10, name = "Yes"), list(code = 20, name = "No")))),
  list(name = "StreamName", type = "esriFieldTypeString"))
metadata <- list(list(id = 5, fields = fields), list(id = 6, fields = fields))
barriers <- data.frame(SiteId = paste0("X", 1:9), source_layer = c(rep("5", 8), "6"),
  StreamName = "Test Creek", stream_identity_confirmed = TRUE,
  PercentFishPassableCode = c(10, 0, 99, NA, 40, 10, NA, NA, NA),
  FishPassageBarrierStatusCode = c(rep(10, 7), 20, 10))
reports <- rbind(parse("X4"), parse("X5"), parse("X6", "Partial"),
  parse_natural_barrier_report(assessment("other"), "X7"), parse("X8"), parse("X9"))
decoded <- audit_barrier_metadata(barriers, metadata, reports)$decoded
check(identical(decoded$confirmed_total_for_scenario,
  c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)),
  "Total evidence must depend on explicit field/report values, never green identity alone")
check(all(decoded$total_evidence_conflict[c(5, 6, 8)]),
  "Conflicting total/non-total field/report attributes require manual review")
check(decoded$PercentFishPassableCode_description[2] == "N/A" &&
  decoded$FishPassageBarrierStatusCode_description[2] == "Yes" &&
  !decoded$confirmed_total_for_scenario[2],
  "Barrier=Yes with passability=N/A does not establish total blockage")
check(decoded$source_layer[9] == "6" && decoded$report_explicit_total[9],
  "Unverified report Total must remain visible as evidence for a later phase, not be discarded")

# Download/extraction envelopes can repeat location-review fields. Joining
# evidence must never replace source identity, numbering, or authorization.
protected <- barriers
protected$review_number <- 101:109
protected$review_record_key <- paste0("source:", protected$SiteId)
protected$override_future_cut <- FALSE
collisions <- reports
collisions$review_number <- 999L
collisions$review_record_key <- "wrong report-envelope key"
collisions$StreamName <- "wrong report-envelope stream"
collisions$source_layer <- "user_override"
collisions$override_future_cut <- TRUE
collisions$stream_identity_confirmed <- FALSE
joined <- audit_barrier_metadata(protected, metadata, collisions)$decoded
for (field in c("review_number", "review_record_key", "StreamName", "source_layer",
    "override_future_cut", "stream_identity_confirmed")) {
  check(identical(joined[[field]], protected[[field]]),
    paste("A report join overwrote protected source/review field:", field))
}

cat("PASS: exact site-matched Total evidence, domain meanings, missing dimensions, no species-checkbox inference, conflicting evidence blocked, green identity distinct from passability.\n")
