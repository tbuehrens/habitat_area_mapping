source("R/network.R")
source("R/source_classification.R")
source("R/swifd_connectors.R")
library(sf)
lines <- function(...) st_sf(geometry = st_sfc(lapply(list(...), st_linestring), crs = 26910))
near <- function(a,b) stopifnot(isTRUE(all.equal(a,b,tolerance=1e-7)))

# A bent SWIFD route connects the two model ends; no straight gap chord is used.
model <- lines(rbind(c(0,0),c(0,10)), rbind(c(20,10),c(20,0)))
guide <- lines(rbind(c(0,10),c(0,15),c(20,15),c(20,10)))
b <- bridge_prediction_with_swifd(model, guide, snap_distance=1, interior_anchors=FALSE)
stopifnot(b$summary$augmented_components==1, nrow(b$bridge_audit)==1)
near(sum(b$connectors$length_m),30)
near(sum(b$prediction_network$length_m),20)
stopifnot(!nrow(b$alignment_links), all(b$network$measure_habitat))
stopifnot(max(st_coordinates(b$connectors)[,2])==15)

# Tolerance includes exactly100m at each endpoint; short tie-ins are separate
# from physical habitat and one endpoint beyond tolerance cannot be connected.
model2 <- lines(rbind(c(0,-10),c(0,0)),rbind(c(1000,0),c(1000,-10)))
guide2 <- lines(rbind(c(0,100),c(1000,100)))
tolerance <- bridge_prediction_with_swifd(model2,guide2,snap_distance=100,interior_anchors=FALSE)
stopifnot(tolerance$summary$augmented_components==1,
          all(tolerance$bridge_audit$from_snap_m<=100),
          all(tolerance$bridge_audit$to_snap_m<=100),
          !any(tolerance$alignment_links$measure_habitat))
near(sum(tolerance$alignment_links$length_m),200)
near(sum(tolerance$connectors$length_m),1000)
model3 <- lines(rbind(c(0,-10),c(0,0)),rbind(c(1000,-.01),c(1000,-10)))
outside <- bridge_prediction_with_swifd(model3,guide2,snap_distance=100,interior_anchors=FALSE)
stopifnot(outside$summary$augmented_components==2,nrow(outside$connectors)==0)

# SWIFD support must itself supply a continuous route.
separate <- lines(rbind(c(0,10),c(0,15)),rbind(c(20,15),c(20,10)))
none <- bridge_prediction_with_swifd(model,separate,snap_distance=1,interior_anchors=FALSE)
stopifnot(none$summary$augmented_components==2,nrow(none$connectors)==0)
same <- bridge_prediction_with_swifd(lines(rbind(c(0,0),c(0,10))),guide,snap_distance=1)
stopifnot(nrow(same$connectors)==0)
duplicates <- bridge_prediction_with_swifd(model,rbind(guide,guide),snap_distance=1,interior_anchors=FALSE)
near(sum(duplicates$connectors$length_m),30)

# A tributary terminus attaches to the nearby mainstem interior at a SWIFD
# junction, rather than routing along a duplicate parallel mainstem to its end.
main_model <- lines(rbind(c(0,0),c(0,100)),rbind(c(10,50),c(30,50)))
main_guide <- lines(rbind(c(1,0),c(1,100)),rbind(c(1,50),c(30,50)))
interior <- bridge_prediction_with_swifd(main_model,main_guide,snap_distance=2)
stopifnot(interior$summary$augmented_components==1,
          any(interior$bridge_audit$to_role=="interior_target"),
          sum(interior$connectors$length_m)<20,
          interior$summary$augmented_cycle_rank==interior$summary$original_cycle_rank)
near(sum(interior$prediction_network$length_m),120)

# A natural barrier on a newly added SWIFD route blocks the upstream model.
barrier <- st_sf(geometry=st_sfc(st_point(c(10,15)),crs=26910))
root <- st_sfc(st_point(c(0,0)),crs=26910)
cut <- truncate_network_at_barriers(b$network,barrier,root,
  snap_distance=.1,root_distance=.1)
stopifnot(any(cut$lines$removed),sum(cut$lines$length_m[cut$lines$retained])<50)

# Two endpoint tie-ins meeting at one SWIFD point are not a routed stream.
# Rejecting this V avoids an alignment-only bypass around a natural barrier.
v_model <- lines(rbind(c(-80,-60),c(-60,-60)),rbind(c(60,-60),c(80,-60)))
v_guide <- lines(rbind(c(0,0),c(0,200)))
v <- bridge_prediction_with_swifd(v_model,v_guide,snap_distance=100)
stopifnot(v$summary$augmented_components==2,nrow(v$connectors)==0,
          nrow(v$alignment_links)==0)

# A candidate route crossing a third model component twice must not add a
# parallel channel around a future barrier on that existing modeled stream.
cross_model <- rbind(model,lines(rbind(c(-10,12),c(30,12))))
crossed <- bridge_prediction_with_swifd(cross_model,guide,snap_distance=1,interior_anchors=FALSE)
stopifnot(crossed$summary$augmented_components==1,
          crossed$summary$augmented_cycle_rank==crossed$summary$original_cycle_rank)
near(sum(crossed$connectors$length_m),4)
near(sum(crossed$prediction_network$length_m),60)
cat("SWIFD curved routing, endpoint tolerance, continuity, duplicate removal, interior joins, and barrier cuts passed.\n")
