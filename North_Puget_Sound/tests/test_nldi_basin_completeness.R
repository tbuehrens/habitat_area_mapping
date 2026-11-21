# A local split is not a complete upstream basin unless positive published
# headwater attributes AND independent upstream navigation prove it is.
source(file.path("R","nldi_delineation.R"))
suppressPackageStartupMessages(library(sf))
check<-function(x,message) if(!isTRUE(x)) stop(message,call.=FALSE)
feature<-function(id,width) {
  ring<-rbind(c(0,0),c(width,0),c(width,1000),c(0,1000),c(0,0))
  ring<-sweep(ring,2,c(500000,5400000),"+")
  geographic<-st_coordinates(st_transform(st_sfc(st_polygon(list(ring)),crs=26910),4326))[,1:2,drop=FALSE]
  list(type="Feature",id=id,properties=list(),geometry=list(type="Polygon",
    coordinates=list(unname(lapply(seq_len(nrow(geographic)),function(i)unname(geographic[i,]))))))
}
full<-list(type="FeatureCollection",features=list(feature("catchment",1000),feature("drainageBasin",2500)))
split<-list(type="FeatureCollection",features=list(feature("catchment",1000),feature("splitCatchment",500)))
non_headwater<-list(startflag=0,streamorde=2,uphydroseq=100,areasqkm=1,totdasqkm=3)
headwater<-list(startflag=1,streamorde=1,uphydroseq=0,areasqkm=1,totdasqkm=1)
record<-list(comid="123",approved=TRUE)
directory<-tempfile("headwater_proof_");dir.create(directory)
good<-nldi_validate_basin_completeness(full,non_headwater,record,st_crs(26910),directory)
check(good$valid && good$diagnostics$completeness_method=="full_drainageBasin",
  "A full drainage basin with consistent cumulative area was not accepted")
check(good$diagnostics$published_area_check=="passes_published_plausible_range" &&
  abs(good$diagnostics$full_basin_area_km2-2.5)<1e-7,
  "Full-basin cumulative-area diagnostics are absent or incorrect")

# Manifest approved=TRUE never lets a small local polygon claim completeness.
bad<-nldi_validate_basin_completeness(split,non_headwater,record,st_crs(26910),directory)
check(!bad$valid && bad$reason=="incomplete_upstream_drainage_split_only_non_headwater",
  "A local split on a non-headwater reach was certified complete")
missing<-nldi_validate_basin_completeness(split,headwater,record,st_crs(26910),directory)
check(!missing$valid && missing$reason=="split_only_headwater_navigation_proof_missing",
  "Published headwater attributes alone substituted for navigation corroboration")
nav_path<-file.path(directory,"comid_123_UT_1000.json")
write_nav<-function(ids) jsonlite::write_json(list(type="FeatureCollection",features=lapply(ids,function(id)
  list(type="Feature",properties=list(nhdplus_comid=id),geometry=NULL))),nav_path,auto_unbox=TRUE)
write_nav(character())
check(!nldi_validate_basin_completeness(split,headwater,record,st_crs(26910),directory)$valid,
  "An empty navigation response was mistaken for positive headwater evidence")
write_nav(c("123","456"))
check(!nldi_validate_basin_completeness(split,headwater,record,st_crs(26910),directory)$valid,
  "An upstream reach outside the local catchment was ignored")
write_nav("123")
proved<-nldi_validate_basin_completeness(split,headwater,record,st_crs(26910),directory)
check(proved$valid && proved$diagnostics$completeness_method=="published_headwater_single_catchment_split" &&
  proved$diagnostics$headwater_upstream_reach_count==1L &&
  proved$diagnostics$headwater_navigation_md5==unname(tools::md5sum(nav_path)),
  "Positive single-catchment headwater proof or source provenance was lost")
unknown<-headwater;unknown$startflag<-NULL
check(!nldi_validate_basin_completeness(split,unknown,record,st_crs(26910),directory)$valid,
  "Missing published headwater attributes were treated as positive proof")

# A named full response must still have a plausible cumulative drainage area.
too_small<-full;too_small$features[[2]]<-feature("drainageBasin",100)
too_big<-full;too_big$features[[2]]<-feature("drainageBasin",4000)
check(!nldi_validate_basin_completeness(too_small,non_headwater,record,st_crs(26910),directory)$valid,
  "A truncated full-labeled basin escaped the cumulative-area check")
check(!nldi_validate_basin_completeness(too_big,non_headwater,record,st_crs(26910),directory)$valid,
  "A grossly oversized full-labeled basin escaped the cumulative-area check")
check(nldi_validate_basin_completeness(full,list(),record,st_crs(26910),directory)$diagnostics$published_area_check==
  "published_area_attributes_unavailable","Unavailable published area diagnostics were fabricated")
cat("PASS: full drainage basins distinguished from local splits; positive published/headwater-navigation proof required; source and area diagnostics retained.\n")
