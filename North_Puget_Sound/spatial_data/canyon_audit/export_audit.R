library(sf); library(ggplot2)
source("R/arcgis.R"); source("R/noaa_populations.R"); source("R/habitat_workflow.R")
output <- "outputs/canyon_creek_audit"
dir.create(output,recursive=TRUE,showWarnings=FALSE)
pops <- get_north_puget_populations()
canyon <- pops[pops$pop_id==269,]
raw <- st_transform(readRDS("spatial_data/north_puget_steelhead.rds"),26910)
raw_canyon <- habitat_clip(raw,canyon)
summarize_stage <- function(g,stage,scenario,generated=NA_character_) {
  g <- habitat_clip(g,canyon)
  if (!"measure_habitat" %in% names(g)) g$measure_habitat <- TRUE
  if (!"source_class" %in% names(g)) g$source_class <- "prediction"
  g <- g[g$measure_habitat,]
  miles <- function(selected) if(nrow(selected)) sum(as.numeric(st_length(st_union(st_geometry(selected)))))/1609.344 else 0
  if (!"retained" %in% names(g)) {g$retained<-FALSE;g$unresolved<-FALSE;g$removed<-FALSE}
  data.frame(stage=stage,scenario=scenario,generated_utc=generated,
    original_prediction_miles=miles(g[g$source_class=="prediction",]),
    all_physical_miles=miles(g),retained_miles=miles(g[g$retained,]),
    unresolved_miles=miles(g[g$unresolved,]),removed_above_barrier_miles=miles(g[g$removed,]),
    drawn_on_map_miles=miles(g[g$retained|g$unresolved,]))
}
original <- readRDS("spatial_data/north_puget_traces.rds")
stages <- list()
for (scenario in c("trace","verified")) stages[[length(stages)+1]] <- summarize_stage(
  original[[scenario]]$lines,"Original occupancy-model-only graph",scenario)
folders <- c("outputs/north_puget_sound","outputs/review_swifd_connections")
saved <- list()
for (directory in folders) {
  meta <- jsonlite::fromJSON(file.path(directory,"run_metadata.json"))
  g <- st_read(file.path(directory,"North_Puget_Sound_Steelhead.gpkg"),
               layer="predicted_extent_status",quiet=TRUE)
  g$length_m <- as.numeric(st_length(g))
  saved[[directory]] <- g
  stages[[length(stages)+1]] <- summarize_stage(g,directory,"both natural-barrier layers",meta$generated_utc)
}
stage_table <- do.call(rbind,stages)
utils::write.csv(stage_table,file.path(output,"canyon_stage_lengths.csv"),row.names=FALSE)
cause <- readRDS("spatial_data/canyon_audit/barrier_cause.rds")
b <- cause$barriers
cols <- intersect(c("SiteId","OBJECTID","SiteLatitude","SiteLongitude","StreamName","TributaryToName",
  "verification","DataSource","SurveyDate","source_layer","snap_distance_m","matched_edge",
  "measure_m","match_status","PercentFishPassableCode","FormLinkURL"),names(b))
barrier_table <- st_drop_geometry(b)[,cols]
barrier_table$assigned_stream <- "South Fork Stillaguamish River"
barrier_table$original_Canyon_miles <- 11.927758921744
barrier_table$Canyon_miles_retained_with_this_barrier_alone <- 0
utils::write.csv(barrier_table,file.path(output,"incorrect_mainstem_barrier_assignments.csv"),row.names=FALSE)

# Directly visible reach-loss audit for every population in both saved products.
pop_rows <- list()
for (directory in names(saved)) {
  g <- saved[[directory]]
  for (i in seq_len(nrow(pops))) {
    z <- g[g$pop_id==pops$pop_id[i] & g$source_class=="prediction",]
    measure <- function(index) if(any(index)) sum(as.numeric(st_length(st_union(st_geometry(z[index,])))))/1609.344 else 0
    total<-measure(rep(TRUE,nrow(z))); removed<-measure(z$removed)
    pop_rows[[length(pop_rows)+1]] <- data.frame(folder=directory,pop_id=pops$pop_id[i],
      population=pops$population[i],original_prediction_miles=total,retained_miles=measure(z$retained),
      unresolved_miles=measure(z$unresolved),removed_above_barrier_miles=removed,
      whole_population_model_removed=(total>.001 && abs(total-removed)<.001))
  }
}
all_pops <- do.call(rbind,pop_rows)
utils::write.csv(all_pops,file.path(output,"all_population_filter_audit.csv"),row.names=FALSE)
print(stage_table)
print(all_pops[all_pops$whole_population_model_removed,])

# Two views: model mileage inside Canyon, and misplaced cuts far downstream.
canyon_saved <- saved[["outputs/review_swifd_connections"]]
canyon_saved <- canyon_saved[canyon_saved$pop_id==269 & canyon_saved$source_class=="prediction",]
view1 <- ggplot()+geom_sf(data=canyon,fill="grey97",color="grey35")+
  geom_sf(data=raw_canyon,color="#126a91",linewidth=1.1)+
  geom_sf(data=canyon_saved[canyon_saved$removed,],color="#c62c2c",linewidth=.5)+
  coord_sf(datum=st_crs(4326))+
  labs(title="Canyon Creek summer steelhead: original model still exists",
    subtitle="11.93 miles in the raw model; all were incorrectly marked above-barrier",
    caption="Blue underlay: raw occupancy model. Red: same geometry classified as removed in the review.",x=NULL,y=NULL)+theme_bw()
window <- st_bbox(st_buffer(st_union(b),1000))
near_model <- suppressWarnings(st_crop(cause$model,window))
near_swifd <- suppressWarnings(st_crop(cause$swifd,window))
snaps<-st_as_sf(st_drop_geometry(b),coords=c("snap_x","snap_y"),crs=26910,remove=FALSE)
view2 <- ggplot()+geom_sf(data=near_model,color="#126a91",linewidth=.5)+
  geom_sf(data=near_swifd,color="#e66b18",linewidth=.3,linetype="dashed")+
  geom_sf(data=cause$edge,color="#8b28a3",linewidth=.8)+
  geom_sf(data=b,color="red",shape=24,fill="yellow",size=3)+
  geom_sf(data=snaps,color="black",shape=4,size=3)+
  geom_sf_label(data=b,aes(label=paste0("Site ",SiteId,"\nunnamed tributary\nsnap ",round(snap_distance_m,1)," m")),
    nudge_y=c(150,-150),size=2.7)+
  coord_sf(xlim=window[c("xmin","xmax")],ylim=window[c("ymin","ymax")],datum=st_crs(4326))+
  labs(title="The two offending cuts are downstream on the receiving mainstem",
    subtitle="Both records name an unnamed tributary of SF Stillaguamish River",
    caption="Purple: assigned SF Stillaguamish mainstem edge. Triangles: barrier records; crosses: snapped locations.",x=NULL,y=NULL)+theme_bw()
ggsave(file.path(output,"canyon_raw_and_removed.png"),view1,width=9,height=8,dpi=200)
ggsave(file.path(output,"incorrect_barrier_assignment.png"),view2,width=10,height=8,dpi=200)
if (requireNamespace("gridExtra",quietly=TRUE)) {
  combined <- gridExtra::arrangeGrob(view1,view2,ncol=2)
  ggsave(file.path(output,"canyon_creek_filter_audit.png"),combined,width=18,height=9,dpi=180)
  ggsave(file.path(output,"canyon_creek_filter_audit.pdf"),combined,width=18,height=9)
}
writeLines(c(
  "Canyon Creek filtering audit (before barrier-assignment correction)",
  "Raw occupancy-model mileage: 11.927758921744 miles.",
  "Model-only graph: all 11.93 miles unresolved and therefore visible in gray.",
  "SWIFD-augmented saved and review graphs: all 11.93 miles classified removed_above_barrier and omitted from maps.",
  "Either verified record 600836 or 600837 alone disconnects all Canyon model reaches from the coastal root.",
  "Both records describe an unnamed tributary of South Fork Stillaguamish River, but the nearest-line rule placed them on the receiving mainstem.",
  "This is a barrier-assignment error exposed by connecting upstream reaches, not missing raw prediction geometry.",
  "Stages and exact generation times are recorded in canyon_stage_lengths.csv; causes in incorrect_mainstem_barrier_assignments.csv.",
  "No production outputs were changed by this audit."),file.path(output,"AUDIT.txt"))
