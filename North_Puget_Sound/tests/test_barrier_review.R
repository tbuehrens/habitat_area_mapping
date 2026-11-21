source(file.path("R", "barrier_stream_identity.R"))
source(file.path("R", "stream_edge_identity.R"))
source(file.path("R", "barrier_review.R"))
suppressPackageStartupMessages(library(sf))

# 'Barrier: Yes' and 'percent passable: N/A/Unknown/missing' do not establish
# complete blockage. The field-specific percent code 10 means 0% passable.
attributes <- data.frame(
  SiteId = LETTERS[1:8], FishPassageBarrierStatusCode = c(10,10,10,10,10,10,10,20),
  PercentFishPassableCode = c(NA,0,99,20,30,40,10,10))
classification <- classify_barrier_passability(attributes)
stopifnot(identical(classification$explicit_total_passage,
  c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)),
  all(is.na(classification$passability_percent[1:3])),
  identical(classification$passability_percent[4:7],c(33,67,100,0)),
  classification$passability_evidence_conflict[8])
descriptions <- data.frame(PercentFishPassableCode=c(NA,NA,NA,20),
  PassabilityDescription=c("Total Fish Passage Blockage", "not a total barrier", "unknown", "total barrier"))
descriptive <- classify_barrier_passability(descriptions)
stopifnot(identical(descriptive$explicit_total_passage,c(TRUE,FALSE,FALSE,FALSE)),
  descriptive$passability_evidence_conflict[4])

line <- function(y, from=500000, to=501000) st_linestring(rbind(
  c(from,5400000+y),c(to,5400000+y)))
point <- function(x,y) st_point(c(500000+x,5400000+y))
main <- st_sf(source_class="prediction",measure_habitat=TRUE,
  geometry=st_sfc(line(0),crs=26910))
main_reference <- st_sf(LLID="main",LLID_STRM_NAME="Main River",
  geometry=st_geometry(main))
barriers <- st_sf(SiteId=c("B","A","C","D"),
  barrier_key=c("5:B","5:A","5:C","5:D"),
  StreamName="Main R",TributaryToName="Other River",
  FishPassageBarrierStatusCode=10L, PercentFishPassableCode=c(NA,10L,10L,NA),
  OriginalMetadata=c("preserve B","preserve A","preserve C","preserve D"),
  geometry=st_sfc(point(100,49.999),point(200,50),point(300,50.001),point(400,6),crs=26910))
alignment <- main
alignment$source_class <- "alignment_link"
alignment$measure_habitat <- FALSE
st_geometry(alignment) <- st_sfc(line(5),crs=26910)
network <- rbind(main,alignment)
result <- prepare_barrier_review(barriers,network,main_reference)
stopifnot(identical(result$audit$snap_accepted,c(TRUE,TRUE,FALSE,TRUE)),
  identical(result$audit$future_cut_candidate,c(FALSE,TRUE,FALSE,FALSE)),
  identical(result$audit$review_number,c(2L,1L,NA_integer_,3L)),
  identical(result$points$SiteId,c("A","B","D")),
  all(result$audit$nearest_physical_line==1L),
  abs(result$audit$snap_distance_m[4]-6)<1e-8,
  all(!result$audit$applied),all(!result$audit$truncation_applied),
  identical(result$audit$OriginalMetadata,barriers$OriginalMetadata),
  identical(st_geometry(result$audit),st_geometry(barriers)),
  identical(st_geometry(result$source_points),st_geometry(barriers)),
  all(abs(st_coordinates(result$points)[,2]-5400000)<1e-8),
  abs(result$audit$original_y[4]-5400006)<1e-8)

# Numbering is stable under input order, and beyond-radius points have no number.
reordered <- prepare_barrier_review(barriers[c(4,3,2,1),],network,main_reference)
stopifnot(identical(reordered$points$SiteId,result$points$SiteId),
  identical(reordered$points$review_number,result$points$review_number),
  nrow(result$candidate_points)==nrow(barriers))

# A receiving-river conflict with no named tributary support stays invalid,
# even when it has a zero-passability code. No mainstem cut can be proposed.
unnamed <- barriers[2,]
unnamed$StreamName <- "unnamed"
unnamed$TributaryToName <- "Main R"
st_geometry(unnamed) <- st_sfc(point(200,2),crs=26910)
rejected <- prepare_barrier_review(unnamed,main,main_reference)
stopifnot(nrow(rejected$points)==1L,
  rejected$audit$identity_conflict_flag,
  rejected$audit$stream_identity_status=="receiving_stream_conflict",
  rejected$audit$snap_status=="manual_review_barrier_stream_unknown",
  !rejected$audit$snap_accepted,!rejected$audit$allow_cut,
  !rejected$audit$future_cut_candidate,
  rejected$audit$review_number==1L)

# Correct the positive conflict only when a nearby physical candidate is
# positively named as the barrier's OWN stream. Original location is preserved.
tributary <- main
st_geometry(tributary) <- st_sfc(line(20),crs=26910)
two_streams <- rbind(main,tributary)
two_reference <- st_sf(LLID=c("main","tributary"),
  LLID_STRM_NAME=c("Main River","Little Creek"),geometry=st_geometry(two_streams))
named <- unnamed
named$StreamName <- "Little Cr"
corrected <- prepare_barrier_review(named,two_streams,two_reference)
stopifnot(nrow(corrected$points)==1L,
  corrected$audit$initial_stream_identity_status=="receiving_stream_conflict",
  corrected$audit$stream_identity_status=="own_stream_confirmed",
  corrected$audit$identity_correction_applied,corrected$audit$identity_conflict_flag,
  corrected$audit$snap_status=="confirmed_own_stream",
  corrected$audit$graph_edge_id==2L,
  abs(corrected$audit$initial_snap_distance_m-2)<1e-8,
  abs(corrected$audit$snap_distance_m-18)<1e-8,
  abs(st_coordinates(corrected$points)[1,2]-5400020)<1e-8,
  corrected$audit$future_cut_candidate)

# Unknown passability can be explicitly overridden, but the override still
# does not apply a cut or bypass the location-tolerance/identity requirements.
override <- prepare_barrier_review(barriers,main,main_reference,override_site_ids=c("B","C"))
stopifnot(override$audit$future_cut_candidate[1],!override$audit$future_cut_candidate[3],
  override$audit$override_future_cut[3],all(!override$audit$truncation_applied))

# Source CRS coordinates and all original fields survive reprojection; output
# points lie on the projected physical network and audit geometry stays original.
geographic <- st_transform(barriers,4326)
geographic_result <- prepare_barrier_review(geographic,main,main_reference)
stopifnot(st_crs(geographic_result$audit)==st_crs(geographic),
  st_crs(geographic_result$points)==st_crs(main),
  max(abs(geographic_result$audit$original_source_x-st_coordinates(geographic)[,1]))<1e-10,
  identical(geographic_result$audit$OriginalMetadata,barriers$OriginalMetadata))

empty <- prepare_barrier_review(barriers[0,],main,main_reference)
stopifnot(nrow(empty$points)==0L,nrow(empty$audit)==0L,
  is.logical(empty$audit$future_cut_candidate),is.integer(empty$audit$review_number))
gorge <- gorge_dam_review_override()
stopifnot(nrow(gorge)==1L,is.na(gorge$SiteId),is.na(gorge$PercentFishPassableCode),
  gorge$override_future_cut,gorge$source_layer=="user_override",
  gorge$StreamName=="Skagit River",grepl("173-201A-602",gorge$source_url,fixed=TRUE))
definitions <- barrier_review_field_definitions(list(test_source=list(id=5)))
stopifnot(definitions$PercentFishPassableCode$coded_values$`10`=="0",
  definitions$FishPassageBarrierStatusCode$coded_values$`10`=="Yes",
  definitions$source_metadata$test_source$id==5)
cat("Barrier review tests passed\n")
