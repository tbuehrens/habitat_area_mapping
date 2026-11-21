# Read-only geographic scope checks for the additional custom analysis unit.
source("R/phase2_stillaguamish.R")
library(sf)
b<-load_phase2_stillaguamish()
p<-b$population
stopifnot(nrow(p)==1L,p$pop_id==1001L,p$is_custom,p$coverage_complete,
  st_crs(p)$epsg==26910L,all(st_is_valid(p)),!any(st_is_empty(p)),
  p$outlet_comid=="24274669",p$excluded_deer_creek_comid=="24274775",all(b$audit$passed))
deer<-st_transform(st_read("spatial_data/phase2/deer_creek_basin.json",quiet=TRUE),st_crs(p))
overlap<-suppressWarnings(st_intersection(st_geometry(p),st_union(st_geometry(deer))))
stopifnot(sum(as.numeric(st_area(overlap)))<1)
upstream<-st_transform(st_read("spatial_data/phase2/nf_stillaguamish_above_deer_UT.json",quiet=TRUE),st_crs(p))
ids<-as.character(upstream$nhdplus_comid)
stopifnot(nrow(upstream)==108L,"24274669" %in% ids,
  !any(c("24274775","24274777","24274667") %in% ids),
  sum(as.numeric(st_length(suppressWarnings(st_difference(st_geometry(upstream),st_geometry(p))))))<.01)
f<-st_read("spatial_data/phase2/deer_confluence_fabric.json",quiet=TRUE)
upper<-f[as.character(f$comid)=="24274669",]
tributary<-f[as.character(f$comid)=="24274775",]
lower<-f[as.character(f$comid)=="24274667",]
stopifnot(as.character(upper$tonode)==as.character(tributary$tonode),
  as.character(upper$tonode)==as.character(lower$fromnode),
  abs(upper$totdasqkm+tributary$totdasqkm+lower$areasqkm-lower$totdasqkm)<.001,
  abs(sum(as.numeric(st_area(p)))/1e6/upper$totdasqkm-1)<.002)
site<-st_transform(st_read("spatial_data/phase2/nf_stillaguamish_oso_site.json",quiet=TRUE),st_crs(p))
stopifnot(lengths(st_within(site,p))==1L)
network<-readRDS("spatial_data/north_puget_swifd_bridges.rds")$bridges$network
before<-suppressWarnings(st_intersection(network,st_union(st_geometry(p))))
before<-before[!st_is_empty(before) & st_dimension(before)==1L & before$measure_habitat,]
length_miles<-sum(as.numeric(st_length(st_union(st_geometry(before)))))/1609.344
stopifnot(abs(length_miles-114.646456051)<1e-6)
cat("PASS: custom1001 contains the complete108-reach NF Stillaguamish watershed above Deer Creek; Deer/downstream drainage excluded; raw source114.646456mi available.\n")
