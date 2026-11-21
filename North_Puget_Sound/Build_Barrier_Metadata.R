# Locate this region when invoked from the repository root or this folder.
local({
  source_files <- unlist(lapply(sys.frames(), function(frame) frame$ofile), use.names = FALSE)
  cli_files <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  starts <- unique(c(dirname(c(source_files, cli_files)), getwd()))
  bootstrap <- NULL
  for (start in starts) {
    current <- normalizePath(start, winslash = "/", mustWork = FALSE)
    repeat {
      candidates <- c(file.path(current, "workflow_bootstrap.R"),
        file.path(current, "North_Puget_Sound", "workflow_bootstrap.R"))
      existing <- candidates[file.exists(candidates)]
      if (length(existing)) { bootstrap <- existing[1L]; break }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
    if (!is.null(bootstrap)) break
  }
  if (is.null(bootstrap)) stop("Cannot locate North_Puget_Sound/workflow_bootstrap.R")
  source(bootstrap, local = FALSE)
})
north_puget_workflow({
# Explicit parsing step after scripts/fetch_barrier_reports.py. No downloads occur.
source(file.path("R","barrier_metadata.R"))
suppressPackageStartupMessages(library(sf))
cache_dir<-file.path("spatial_data","barrier_reports")
records<-jsonlite::fromJSON(file.path(cache_dir,"report_text_index.json"),simplifyVector=FALSE)
evidence<-lapply(records,function(record) {
  parsed<-parse_natural_barrier_report(unlist(record$pages),record$SiteId,
    record$report_url,record$report_retrieved_utc,
    if(is.null(record$report_source_md5)) NA_character_ else record$report_source_md5)
  parsed$review_number<-record$review_number
  parsed$report_download_status<-record$report_download_status
  parsed$report_download_error<-if(is.null(record$report_error)) NA_character_ else record$report_error
  parsed$report_pdf<-record$report_pdf
  if(record$report_download_status!="downloaded_and_text_extracted") parsed$report_parse_status<-
    paste0("unavailable:",record$report_download_status)
  parsed
})
evidence<-do.call(rbind,evidence)
stopifnot(nrow(evidence)==691L,!anyDuplicated(evidence$SiteId))
fetch_audit<-evidence[c("SiteId","review_number","report_url","report_pdf","report_download_status",
  "report_download_error","report_retrieved_utc","report_source_md5","report_page_count","report_parse_status")]
saveRDS(evidence,file.path(cache_dir,"report_evidence.rds"))
utils::write.csv(evidence,file.path(cache_dir,"report_evidence.csv"),row.names=FALSE,na="")
utils::write.csv(fetch_audit,file.path(cache_dir,"report_fetch_audit.csv"),row.names=FALSE,na="")
metadata<-lapply(c(5,6),function(layer) jsonlite::fromJSON(
  file.path("spatial_data",paste0("barrier_layer_",layer,".json")),simplifyVector=FALSE))
review<-sf::st_read(file.path("outputs","review_swifd_connections","North_Puget_Sound_Steelhead.gpkg"),
  layer="barriers_snapped",quiet=TRUE)
green<-review[review$stream_identity_confirmed %in% TRUE,]
green_audit<-audit_barrier_metadata(green,metadata,evidence)
utils::write.csv(green_audit$decoded,file.path(cache_dir,"green_barrier_metadata_inventory.csv"),row.names=FALSE,na="")
saveRDS(green_audit,file.path(cache_dir,"green_metadata_audit.rds"))
natural<-do.call(rbind,lapply(c(5,6),function(layer) {
  x<-readRDS(file.path("spatial_data",paste0("north_puget_barriers_",layer,".rds")))
  x$source_layer<-as.character(layer)
  x$verification<-if(layer==5) "Verified" else "Not field verified"
  x
}))
natural_audit<-audit_barrier_metadata(natural,metadata,evidence)
utils::write.csv(natural_audit$field_inventory,file.path(cache_dir,"all_natural_barrier_field_inventory.csv"),row.names=FALSE,na="")
utils::write.csv(natural_audit$code_inventory,file.path(cache_dir,"all_natural_barrier_code_inventory.csv"),row.names=FALSE,na="")
utils::write.csv(green_audit$field_inventory,file.path(cache_dir,"green_barrier_field_inventory.csv"),row.names=FALSE,na="")
utils::write.csv(green_audit$summary,file.path(cache_dir,"green_barrier_metadata_summary.csv"),row.names=FALSE,na="")
sources<-list(
  natural_layers=paste0("https://geodataservices.wdfw.wa.gov/arcgis/rest/services/ApplicationServices/FP_Sites/MapServer/",c(5,6)),
  manual_publication="https://wdfw.wa.gov/publications/02061",
  manual_pdf="https://wdfw.wa.gov/sites/default/files/publications/02061/Fish%20Passage%20Inventory%2C%20Assessment%2C%20and%20Prioritization%20Manual.pdf",
  manual_local="spatial_data/barrier_reports/wdfw02061.pdf",
  manual_md5=if(file.exists(file.path(cache_dir,"wdfw02061.pdf"))) unname(tools::md5sum(file.path(cache_dir,"wdfw02061.pdf"))) else NA_character_,
  manual_pages=list(passability="PDF page12 / printed1-4",natural_barriers="PDF page102 / printed7-2"),
  interpretation=c("Public Barrier Yes is distinct from explicit Total blockage",
    "Printed species labels/list are not independently validated per-run blockage evidence",
    "Dimension sentinels are missing, and height/type alone do not establish total steelhead blockage",
    "Report Total is a source classification, not an independently verified universal steelhead-run passage conclusion"),
  generated_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
jsonlite::write_json(sources,file.path(cache_dir,"metadata_sources.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
counts<-function(x) as.list(table(x,useNA="ifany"))
decoded<-green_audit$decoded
decoded_summary<-list(green_stream_identity_records=nrow(decoded),natural_report_records=nrow(evidence),
  retrieval_status=counts(evidence$report_download_status),blockage=counts(evidence$report_blockage),
  verified_blockage=counts(decoded$report_blockage[decoded$verification=="Verified"]),
  unverified_blockage=counts(decoded$report_blockage[decoded$verification=="Not field verified"]),
  natural_type=counts(evidence$report_natural_type),
  waterfall_height=list(positive=sum(evidence$report_waterfall_height_m>0,na.rm=TRUE),
    zero_reported_review=sum(evidence$report_waterfall_height_m==0,na.rm=TRUE),
    missing=sum(is.na(evidence$report_waterfall_height_m))),
  passage_evidence_conflicts=sum(decoded$total_evidence_conflict),
  measured_height_never_used_to_infer_total=TRUE,
  species_run_specific_blockage_independently_verified=FALSE)
jsonlite::write_json(decoded_summary,file.path(cache_dir,"decoded_metadata_summary.json"),
  pretty=TRUE,auto_unbox=TRUE,null="null")
cat("REPORT EVIDENCE CACHE READY:",nrow(evidence),"records\n")
print(green_audit$summary,row.names=FALSE)
print(with(green_audit$decoded,table(verification,report_blockage,useNA="ifany")))
print(table(evidence$report_natural_type,useNA="ifany"))
cat("Reported positive waterfall heights:",sum(evidence$report_waterfall_height_m>0,na.rm=TRUE),
  "; zero values:",sum(evidence$report_waterfall_height_m==0,na.rm=TRUE),
  "; missing:",sum(is.na(evidence$report_waterfall_height_m)),"\n")
})
