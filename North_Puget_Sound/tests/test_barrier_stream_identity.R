source(file.path("R", "barrier_stream_identity.R"))
library(sf)
point <- function(y) st_point(c(500000, 5400000 + y))
reference <- st_sf(
  LLID = c("south", "north", "bear"),
  LLID_STRM_NAME = c("South Fork Stillaguamish River", "North Fork Stillaguamish River", "Bear Creek"),
  geometry = st_sfc(lapply(c(0, 1000, 2000), function(y)
    st_linestring(rbind(c(499000,5400000+y),c(501000,5400000+y)))), crs=26910))
barriers <- data.frame(
  StreamName = c("unnamed", "Small Cr", "SF Stillaguamish R", "Bear River",
    NA, "Mystery Cr", "Unnamed Cr", "Unnamed tributary"),
  TributaryToName = c("SF Stillaguamish R", "South Fork Stillaguamish River",
    "SF Stilly R", "Bear Creek", "NF Stillaguamish R", "Other R", "SFStilly", "SF Stillaguamish R"))
points <- st_sf(geometry=st_sfc(lapply(c(2,29,1,2001,1001,2,0,301),point),crs=26910))
result <- validate_barrier_stream_matches(barriers,points,reference)
stopifnot(identical(result$allow_cut,c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE)),
  all(result$stream_identity_status[c(1,2,4,5,7)]=="receiving_stream_conflict"),
  result$stream_identity_status[3]=="own_stream_name_match",
  result$stream_identity_status[6]=="different_stream_name_unvalidated",
  result$stream_identity_status[8]=="no_nearby_swifd_reference",
  all(abs(result$reference_distance_m[c(1,2)]-c(2,29))<1e-8),
  all(result$matched_llid[c(1,2,3,6,7,8)]=="south"))

# Canonical abbreviations do not turn differently named rivers/creeks into a
# match, or apply fuzzy string similarity to unrelated waterways.
stopifnot(barrier_stream_names_match("SF Stilly R","South Fork Stillaguamish River"),
  barrier_stream_names_match("NF Nooksack R","North Fork Nooksack River"),
  barrier_stream_names_match("MF Nooksack","Middle Fork Nooksack River"),
  barrier_stream_names_match("Canyon Cr","Canyon Creek"),
  !barrier_stream_names_match("Bear River","Bear Creek"),
  !barrier_stream_names_match("Stillaguamish River","South Fork Stillaguamish River"),
  !barrier_stream_names_match("Canyon Creek","Cannon Creek"))

# Unknown reference identities and absence of a nearby reference preserve the
# existing cut decision, but explicitly flag that identity was not validated.
unnamed <- reference[1,]
unnamed$LLID_STRM_NAME <- "Unnamed Creek"
unknown <- validate_barrier_stream_matches(barriers[1,,drop=FALSE],points[1,],unnamed)
stopifnot(unknown$allow_cut,unknown$stream_identity_status=="swifd_stream_name_unavailable")
missing <- validate_barrier_stream_matches(barriers[1,,drop=FALSE],points[1,],reference[0,])
stopifnot(missing$allow_cut,missing$stream_identity_status=="no_swifd_reference")
empty <- validate_barrier_stream_matches(barriers[0,],points[0,],reference)
stopifnot(nrow(empty)==0L,is.logical(empty$allow_cut),is.character(empty$matched_llid))
sfc_result <- validate_barrier_stream_matches(barriers[1,,drop=FALSE],st_geometry(points[1,]),reference)
stopifnot(!sfc_result$allow_cut)
empty_point <- st_sf(geometry=st_sfc(st_point(),crs=26910))
empty_point_result <- validate_barrier_stream_matches(barriers[1,,drop=FALSE],empty_point,reference)
stopifnot(empty_point_result$allow_cut,
  empty_point_result$stream_identity_status=="empty_candidate_point_unvalidated")

# Regression through the full barrier-cut workflow: a tributary barrier only
# two meters from the receiving mainstem must not cut that mainstem's habitat.
source(file.path("R", "network.R"))
main <- st_sf(geometry=st_sfc(st_linestring(rbind(
  c(500000,5400000),c(500100,5400000))),crs=26910))
main_reference <- main
main_reference$LLID_STRM_NAME <- "Main River"
main_reference$LLID <- "main_llid"
root <- st_sfc(st_point(c(500000,5400000)),crs=26910)
tributary_barrier <- st_sf(StreamName="unnamed",TributaryToName="Main R",
  geometry=st_sfc(st_point(c(500050,5400002)),crs=26910))
trace_fixture <- function(barrier,reference=main_reference) {
  truncate_network_at_barriers(main,barrier,root,
    snap_distance=5,root_distance=.1,stream_reference=reference)
}
wrong_mainstem <- trace_fixture(tributary_barrier)
stopifnot(!wrong_mainstem$barrier_audit$applied,
  wrong_mainstem$barrier_audit$match_status=="rejected_receiving_stream",
  wrong_mainstem$barrier_audit$stream_identity_status=="receiving_stream_conflict",
  wrong_mainstem$summary$barriers_rejected_receiving_stream==1L,
  abs(wrong_mainstem$summary$retained_m-100)<1e-8,
  wrong_mainstem$summary$removed_m==0,
  all(wrong_mainstem$lines$retained))

# A barrier explicitly recorded on the mainstem still cuts its upstream half,
# even when the receiving-name field happens to repeat the same stream name.
own_mainstem_barrier <- tributary_barrier
own_mainstem_barrier$StreamName <- "Main River"
correct_mainstem <- trace_fixture(own_mainstem_barrier)
stopifnot(correct_mainstem$barrier_audit$applied,
  correct_mainstem$barrier_audit$stream_identity_status=="own_stream_name_match",
  abs(correct_mainstem$summary$retained_m-50)<1e-8,
  abs(correct_mainstem$summary$removed_m-50)<1e-8)

# No positive receiving-stream conflict means no new scientific conclusion:
# the pre-existing snap decision remains possible, with the uncertainty named.
unvalidated_barrier <- tributary_barrier
unvalidated_barrier$TributaryToName <- "Other R"
unvalidated_trace <- trace_fixture(unvalidated_barrier)
stopifnot(unvalidated_trace$barrier_audit$applied,
  unvalidated_trace$barrier_audit$stream_identity_status=="barrier_stream_unknown_unvalidated",
  abs(unvalidated_trace$summary$removed_m-50)<1e-8)
far_reference <- main_reference
st_geometry(far_reference) <- st_geometry(far_reference)+c(0,1000)
st_crs(far_reference) <- 26910
far_trace <- trace_fixture(tributary_barrier,far_reference)
stopifnot(far_trace$barrier_audit$applied,
  far_trace$barrier_audit$stream_identity_status=="no_nearby_swifd_reference")
cat("Barrier stream-identity checks and full truncation regression pass: tributary/mainstem conflicts rejected; valid and unvalidated cuts distinguished.\n")
