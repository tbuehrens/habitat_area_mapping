# Independent integration fixtures: retain location-review rows, search every
# physical candidate, preserve prior numbers, and never apply barrier cuts.
source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
source(file.path("R", "barrier_review.R"))
suppressPackageStartupMessages(library(sf))
require_true <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)
offset <- c(500000, 5400000)
line <- function(y) st_linestring(rbind(c(0, y), c(1000, y)) +
  matrix(offset, nrow = 2, ncol = 2, byrow = TRUE))
point <- function(x, y) st_point(c(x, y) + offset)
network <- st_sf(source_class = c("prediction", "prediction", "alignment_link"),
  measure_habitat = c(TRUE, TRUE, FALSE),
  geometry = st_sfc(line(0), line(20), line(69.999), crs = 26910))
reference <- st_sf(LLID = c("main", "little"),
  LLID_STRM_NAME = c("Main River", "Little Creek"),
  geometry = st_geometry(network[1:2, ]))
barriers <- st_sf(SiteId = LETTERS[1:5], barrier_key = paste0("5:", LETTERS[1:5]),
  source_layer = "5", StreamName = c("Little Cr", "unnamed", "Main R", "Little Cr", "Little Cr"),
  TributaryToName = c("Other River", "Main R", "Other R", "Other R", "Other R"),
  PercentFishPassableCode = c(10L, 10L, NA_integer_, 10L, 10L),
  FishPassageBarrierStatusCode = 10L,
  geometry = st_sfc(point(500, 2), point(700, 1), point(300, 0),
    point(600, 70.001), point(800, 70), crs = 26910))
prior <- data.frame(review_record_key = c("5:A", "5:B"), review_number = c(15L, 903L))
before_network <- st_as_binary(st_geometry(network))
before_barriers <- st_as_binary(st_geometry(barriers))
result <- prepare_barrier_review(barriers, network, reference,
  previous_review_numbers = prior)
audit <- result$audit[match(LETTERS[1:5], result$audit$SiteId), ]

# A nearest non-own stream must not prevent finding the correct own stream.
# TributaryToName deliberately does not name that nearest Main River.
require_true(audit$stream_identity_confirmed[1] && audit$snap_accepted[1] &&
  audit$eligible_for_future_cut[1] && audit$candidate_count[1] == 2L,
  "The farther own stream within 50 m was not selected from all candidates")
placed_a <- result$points[result$points$SiteId == "A", ]
require_true(abs(st_coordinates(placed_a)[1, 2] - 5400020) < 1e-6,
  "Confirmed barrier A was left snapped to the nearer wrong stream")

# A location can remain numbered while its requested cut is blocked.
require_true(audit$display_location_available[2] && !audit$stream_identity_confirmed[2] &&
  !audit$snap_accepted[2] && audit$future_cut_requested[2] && !audit$eligible_for_future_cut[2],
  "An unnamed location was dropped or incorrectly accepted for a future cut")
require_true(all(c("A", "B", "C", "E") %in% result$points$SiteId) && nrow(result$points) == 4L,
  "Location-review output must retain every in-radius source record")
require_true(identical(as.integer(audit$review_number[1:3]), c(15L, 903L, 904L)) &&
  audit$review_number[5] == 905L,
  "Existing numbers changed or new records were inserted before the previous maximum")

# Alignment links are never physical candidates; the 50 m threshold is exact.
require_true(!audit$display_location_available[4] && is.na(audit$review_number[4]) &&
  audit$candidate_count[4] == 0L && audit$display_location_available[5] &&
  audit$candidate_count[5] == 1L && abs(audit$snap_distance_m[5] - 50) < 1e-6,
  "Alignment geometry or an out-of-tolerance physical stream attracted a review location")
require_true(nrow(result$candidate_matches) == 7L,
  "Candidate audit did not retain every original-point/physical-edge pair within 50 m")
require_true(all(!audit$applied) && all(!audit$truncation_applied) &&
  identical(audit$future_cut_candidate, audit$eligible_for_future_cut),
  "Strict location review applied a cut or disagreed about future-cut eligibility")
require_true(identical(before_network, st_as_binary(st_geometry(network))) &&
  identical(before_barriers, st_as_binary(st_geometry(barriers))),
  "Strict candidate matching altered source geometry")

# Input order must not change source-record numbers or the identity decisions.
reordered <- prepare_barrier_review(barriers[5:1, ], network[c(3, 2, 1), ], reference[2:1, ],
  previous_review_numbers = prior)
again <- reordered$audit[match(audit$review_record_key, reordered$audit$review_record_key), ]
require_true(identical(audit$review_number, again$review_number) &&
  identical(audit$stream_identity_confirmed, again$stream_identity_confirmed) &&
  identical(audit$eligible_for_future_cut, again$eligible_for_future_cut),
  "Input order changed stable review numbers or strict identity decisions")

# A named mainstem candidate near a physical junction is retained for review,
# with a cut request, but cannot become eligible through nearest-line fallback.
tee <- st_sf(source_class = "prediction", measure_habitat = TRUE,
  geometry = st_sfc(line(0), st_linestring(rbind(c(500, 0), c(500, 200)) +
    matrix(offset, nrow = 2, ncol = 2, byrow = TRUE)), crs = 26910))
junction_barrier <- barriers[1, ]
junction_barrier$StreamName <- "Main River"
st_geometry(junction_barrier) <- st_sfc(point(490, 0), crs = 26910)
junction_result <- prepare_barrier_review(junction_barrier, tee, reference[1, ])
require_true(nrow(junction_result$points) == 1L && junction_result$audit$junction_ambiguous &&
  junction_result$audit$future_cut_requested && !junction_result$audit$eligible_for_future_cut,
  "An ambiguous-junction cut request was not kept visible and blocked")

cat("PASS: all physical candidates searched, own-stream correction, in-radius unknown rows retained, stable numbers, exact tolerance, alignment exclusion, junction blocking, zero cuts.\n")
