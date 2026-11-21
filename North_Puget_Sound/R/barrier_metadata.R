# Decode inventory attributes and retain explicit, sourced natural-barrier
# report evidence. This module never snaps, cuts, or infers blockage from height.
if (!exists("nps_read_cache_rds", mode = "function")) source(file.path("R", "project_paths.R"))
parse_natural_barrier_report <- function(pages, site_id, report_url=NA_character_,
    retrieved_utc=NA_character_, source_md5=NA_character_) {
  pages<-as.character(pages)
  pages[is.na(pages)]<-""
  pages<-gsub("\r","",pages,fixed=TRUE)
  assessment<-which(grepl("Natural\\s+Barrier\\s+Assessment\\s+Report",pages,
    ignore.case=TRUE,perl=TRUE))
  result<-data.frame(SiteId=as.character(site_id),report_url=as.character(report_url),
    report_retrieved_utc=as.character(retrieved_utc),report_source_md5=as.character(source_md5),
    report_page_count=length(pages),report_assessment_pages=paste(assessment,collapse=";"),
    report_evidence_pages="",
    report_parse_status="no_natural_barrier_assessment",report_site_id=NA_character_,
    report_identity_verified=FALSE,report_stream_name=NA_character_,
    report_natural_type=NA_character_,report_blockage=NA_character_,report_barrier=NA_character_,
    report_barrier_criteria=NA_character_,report_survey_date=NA_character_,report_data_source=NA_character_,
    report_comments=NA_character_,report_plunge_pool_depth_comment_m=NA_real_,
    report_dimension_comment_conflict=FALSE,
    report_explicit_total=FALSE,report_blockage_conflict=FALSE,
    report_general_scope=NA_character_,report_species_run_evidence="Not inferred from printed species labels/list without independent run-specific validation",
    report_steelhead_run_blockage_verified=FALSE,stringsAsFactors=FALSE)
  measurements<-c(waterfall_height="Waterfall Height (m)",plunge_pool_depth="Plunge Pool Depth (m)",
    stream_length="Stream Length (m)",gradient="Gradient (%)",channel_width="Channel Width (m)")
  for(name in names(measurements)) {
    suffix<-if(name=="gradient") "_percent" else "_m"
    result[[paste0("report_",name,suffix)]]<-NA_real_
    result[[paste0("report_",name,"_raw")]]<-NA_character_
    result[[paste0("report_",name,"_quality")]]<-"not_reported"
  }
  if(any(grepl("based on the swimming abilities of adult\\s+salmonids",pages,ignore.case=TRUE,perl=TRUE))) {
    result$report_general_scope<-"Adult salmonids; qualitative passability; no steelhead-run checkbox inference"
  }
  if(!length(assessment)) return(result)
  # Repeated assessment-image headings have no labeled status fields and do
  # not supply evidence. Retain all pages containing actual labeled blockage.
  main<-assessment[grepl("Blockage\\s*:",pages[assessment],ignore.case=TRUE,perl=TRUE)]
  if(!length(main)) { result$report_parse_status<-"assessment_has_no_blockage_field"; return(result) }
  result$report_evidence_pages<-paste(main,collapse=";")
  text<-pages[main[1L]]
  capture<-function(pattern,source_text=text) {
    matched<-regexec(pattern,source_text,ignore.case=TRUE,perl=TRUE)
    values<-regmatches(source_text,matched)[[1L]]
    if(length(values)<2L) return(NA_character_)
    value<-trimws(gsub("[ \\t]+"," ",values[2L],perl=TRUE))
    if(nzchar(value)) value else NA_character_
  }
  ids<-unique(na.omit(vapply(pages[main],function(page)
    capture("Site[ \\t]+ID[ \\t]*:?[ \\t]*([^\\s]+)",page),character(1))))
  result$report_site_id<-paste(ids,collapse=";")
  result$report_identity_verified<-length(ids)==1L && identical(ids,as.character(site_id))
  result$report_parse_status<-if(result$report_identity_verified) "parsed" else "site_identity_mismatch"
  result$report_stream_name<-capture("Stream[ \\t]*:[ \\t]*([^\\r\\n]*)")
  result$report_natural_type<-capture("(?:^|\\n)Type[ \\t]*:[ \\t]*([^\\r\\n]*)")
  result$report_barrier<-capture("(?:^|\\n)Barrier[ \\t]*:[ \\t]*(Yes|No|Unknown)(?=[^A-Za-z]|$)")
  blockage<-vapply(pages[main],function(page) {
    value<-capture("Blockage[ \\t]*:[ \\t]*([^\\r\\n]*)",page)
    if(is.na(value)) return(NA_character_)
    trimws(sub("Barrier[ \\t]+Criteria[ \\t]*:.*$","",value,ignore.case=TRUE,perl=TRUE))
  },character(1))
  blockage<-unique(na.omit(blockage[nzchar(blockage)]))
  result$report_blockage<-if(length(blockage)) paste(blockage,collapse=";") else NA_character_
  normalized<-tolower(trimws(blockage))
  total<-normalized %in% c("total","total blockage","total barrier")
  result$report_blockage_conflict<-length(unique(normalized))>1L
  result$report_explicit_total<-result$report_identity_verified && length(total)==1L &&
    isTRUE(total) && !result$report_blockage_conflict &&
    !identical(tolower(result$report_barrier),"no")
  result$report_barrier_criteria<-capture("Barrier[ \\t]+Criteria[ \\t]*:[ \\t]*([^\\r\\n]*)")
  result$report_survey_date<-capture("Survey[ \\t]+Date[ \\t]*:[ \\t]*([0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4})")
  result$report_data_source<-capture("Data[ \\t]+Source[ \\t]*:[ \\t]*([^\\r\\n]*)")
  result$report_comments<-capture(paste0("(?s)(?:^|\\n)Comments[ \\t]*\\n(.*?)",
    "(?:\\n(?:No Image Available|Dimensions|Status|Fishway Present|Species Blocked|These data represent)|$)"))
  for(name in names(measurements)) {
    label<-measurements[[name]]
    escaped<-gsub("([()])","\\\\\\1",label,perl=TRUE)
    raw<-capture(paste0(escaped,"[ \\t]*:[ \\t]*(-?[0-9]+(?:\\.[0-9]+)?)"))
    value<-suppressWarnings(as.numeric(raw))
    suffix<-if(name=="gradient") "_percent" else "_m"
    result[[paste0("report_",name,"_raw")]]<-raw
    quality<-if(is.na(value)) "not_reported" else if(value<0) "missing_negative_sentinel" else
      if(value==0) "reported_zero_review" else "reported_positive_value"
    result[[paste0("report_",name,"_quality")]]<-quality
    if(!is.na(value) && value>=0) result[[paste0("report_",name,suffix)]]<-value
  }
  if(!is.na(result$report_comments)) {
    note_depth<-capture("Plunge[ \\t]+pool[ \\t]+Depth[ \\t]*(?:is[ \\t]+)?([0-9]+(?:\\.[0-9]+)?)[ \\t]*m",result$report_comments)
    result$report_plunge_pool_depth_comment_m<-suppressWarnings(as.numeric(note_depth))
    result$report_dimension_comment_conflict<-!is.na(result$report_plunge_pool_depth_m) &&
      !is.na(result$report_plunge_pool_depth_comment_m) &&
      abs(result$report_plunge_pool_depth_m-result$report_plunge_pool_depth_comment_m)>1e-6
  }
  result
}

barrier_metadata_layers <- function(metadata) {
  if(!is.null(metadata$fields)) return(list(metadata))
  if(!is.list(metadata)) return(list())
  unlist(lapply(metadata,barrier_metadata_layers),recursive=FALSE)
}

load_barrier_report_evidence <- function(cache_dir="spatial_data/barrier_reports") {
  path<-file.path(cache_dir,"report_evidence.rds")
  fallback<-file.path(cache_dir,"report_evidence.csv")
  if(file.exists(path)) evidence<-nps_read_cache_rds(path) else if(file.exists(fallback)) {
    evidence<-nps_resolve_record_paths(utils::read.csv(fallback,stringsAsFactors=FALSE,check.names=FALSE))
  } else stop("Missing report evidence cache; run scripts/fetch_barrier_reports.py and Build_Barrier_Metadata.R explicitly")
  if(!is.data.frame(evidence) || !"SiteId" %in% names(evidence) || anyDuplicated(evidence$SiteId)) {
    stop("Invalid report evidence cache")
  }
  audit_path<-file.path(cache_dir,"report_fetch_audit.csv")
  if(!file.exists(audit_path)) stop("Missing report-fetch audit")
  fetch_audit<-nps_resolve_record_paths(utils::read.csv(audit_path,stringsAsFactors=FALSE,check.names=FALSE,
    colClasses=c(SiteId="character")))
  if(!setequal(as.character(evidence$SiteId),as.character(fetch_audit$SiteId))) stop("Report evidence/fetch inventory differs")
  attr(evidence,"fetch_audit")<-fetch_audit
  attr(evidence,"cache_path")<-normalizePath(if(file.exists(path)) path else fallback,winslash="/")
  evidence
}

audit_barrier_metadata <- function(barriers,layer_metadata,report_evidence=NULL) {
  decoded<-if(inherits(barriers,"sf")) sf::st_drop_geometry(barriers) else as.data.frame(barriers)
  layers<-barrier_metadata_layers(layer_metadata)
  if(!length(layers)) stop("Supply the cached layer metadata including fields/domains")
  present<-function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  source_value<-function(field) if(field %in% names(decoded)) as.character(decoded[[field]]) else rep(NA_character_,nrow(decoded))
  code_rows<-list(); inventory<-list()
  for(layer in layers) {
    layer_id<-if(is.null(layer$id)) NA_character_ else as.character(layer$id)
    row_scope<-if("source_layer" %in% names(decoded)) which(as.character(decoded$source_layer)==layer_id) else seq_len(nrow(decoded))
    for(field in layer$fields) {
      if(is.null(field$name)) next
      name<-field$name
      geometry_field<-identical(field$type,"esriFieldTypeGeometry") && inherits(barriers,"sf")
      available<-name %in% names(decoded) || geometry_field
      values<-if(geometry_field) sf::st_as_text(sf::st_geometry(barriers)[row_scope]) else
        if(available) as.character(decoded[[name]][row_scope]) else rep(NA_character_,length(row_scope))
      populated<-present(values)
      numeric_values<-suppressWarnings(as.numeric(values))
      sentinel<-!is.na(numeric_values) & numeric_values<0 &
        (abs(numeric_values+99.99)<0.02 | abs(numeric_values+999.99)<0.02 | numeric_values %in% c(-999,-9999,-99))
      inventory[[length(inventory)+1L]]<-data.frame(source_layer=layer_id,field=name,
        alias=if(is.null(field$alias)) name else field$alias,
        field_type=if(is.null(field$type)) NA_character_ else field$type,
        field_available=available,records=length(row_scope),populated=sum(populated),
        missing=sum(!populated),negative_missing_sentinels=sum(sentinel),
        distinct_populated_values=length(unique(values[populated])),
        examples=paste(head(unique(values[populated]),5L),collapse=" | "),stringsAsFactors=FALSE)
      coded<-field$domain$codedValues
      if(is.null(coded) || !length(coded)) next
      descriptions<-rep(NA_character_,nrow(decoded))
      for(item in coded) {
        key<-as.character(item$code); label<-as.character(item$name)
        descriptions[source_value(name)==key & !is.na(source_value(name))]<-label
        code_rows[[length(code_rows)+1L]]<-data.frame(source_layer=layer_id,field=name,
          code=key,description=label,count=sum(values==key,na.rm=TRUE),stringsAsFactors=FALSE)
      }
      output_name<-paste0(name,"_description")
      if(!output_name %in% names(decoded)) decoded[[output_name]]<-rep(NA_character_,nrow(decoded))
      decoded[[output_name]][row_scope]<-descriptions[row_scope]
    }
  }
  count<-nrow(decoded)
  percent_code<-suppressWarnings(as.numeric(source_value("PercentFishPassableCode")))
  barrier_code<-suppressWarnings(as.numeric(source_value("FishPassageBarrierStatusCode")))
  decoded$source_explicit_total<-!is.na(percent_code) & percent_code==10
  decoded$source_percent_passable<-unname(c(`10`=0,`20`=33,`30`=67,`40`=100)[as.character(percent_code)])
  if(!is.null(report_evidence) && nrow(report_evidence)) {
    if(!"SiteId" %in% names(report_evidence) || anyDuplicated(report_evidence$SiteId)) stop("Report evidence requires unique SiteId rows")
    location<-match(source_value("SiteId"),as.character(report_evidence$SiteId))
    # The report cache carries lookup/display helpers such as review_number.
    # Only report evidence/provenance may join back: source identity, numbering,
    # stream assignments, verification and override records remain untouched.
    evidence_fields<-grep("^report_",names(report_evidence),value=TRUE)
    for(name in evidence_fields) decoded[[name]]<-report_evidence[[name]][location]
  }
  if(!"report_explicit_total" %in% names(decoded)) decoded$report_explicit_total<-rep(FALSE,count)
  if(!"report_blockage" %in% names(decoded)) decoded$report_blockage<-rep(NA_character_,count)
  if(!"report_blockage_conflict" %in% names(decoded)) decoded$report_blockage_conflict<-rep(FALSE,count)
  if(!"report_identity_verified" %in% names(decoded)) decoded$report_identity_verified<-rep(FALSE,count)
  report_total<-decoded$report_explicit_total %in% TRUE
  report_non_total<-!is.na(decoded$report_blockage) &
    tolower(trimws(decoded$report_blockage)) %in% c("partial","partial blockage","none","no blockage","not a barrier")
  source_non_total<-!is.na(percent_code) & percent_code %in% c(20,30,40)
  decoded$total_evidence_conflict<-decoded$report_blockage_conflict %in% TRUE |
    (decoded$source_explicit_total & report_non_total) | (report_total & source_non_total) |
    ((!is.na(barrier_code) & barrier_code==20) & (decoded$source_explicit_total | report_total))
  if("report_barrier" %in% names(decoded)) decoded$total_evidence_conflict<-
    decoded$total_evidence_conflict | ((!is.na(decoded$report_barrier) &
      tolower(decoded$report_barrier)=="no") &
      (decoded$source_explicit_total | tolower(decoded$report_blockage) %in% c("total","total blockage","total barrier")))
  decoded$confirmed_total_for_scenario<-(decoded$source_explicit_total | report_total) & !decoded$total_evidence_conflict
  decoded$total_evidence_basis<-ifelse(decoded$total_evidence_conflict,"Conflicting source/report attributes; manual review",
    ifelse(decoded$source_explicit_total,"Public PercentFishPassableCode=10 (0% passable)",
      ifelse(report_total,"Site-matched natural-barrier assessment explicitly reports Blockage: Total",
        "No explicit total-blockage evidence")))
  decoded$metadata_height_policy<-"Height, type and gradient alone never establish total steelhead blockage"
  decoded$metadata_run_scope<-"Generic adult-salmonid classification unless species/run-specific evidence separately verified"
  scopes<-list(all_input=seq_len(count))
  if("stream_identity_confirmed" %in% names(decoded)) scopes$green_stream_identity_confirmed<-
    which(tolower(as.character(decoded$stream_identity_confirmed)) %in% c("true","t","1"))
  summary<-do.call(rbind,lapply(names(scopes),function(scope) {
    rows<-scopes[[scope]]
    data.frame(scope=scope,records=length(rows),public_explicit_total=sum(decoded$source_explicit_total[rows]),
      site_reports_available=if("report_parse_status" %in% names(decoded)) sum(!is.na(decoded$report_parse_status[rows])) else 0L,
      report_explicit_total=sum(report_total[rows]),
      report_partial=sum(tolower(trimws(decoded$report_blockage[rows])) %in% c("partial","partial blockage")),
      report_non_total=sum(report_non_total[rows]),
      conflicting_evidence=sum(decoded$total_evidence_conflict[rows]),
      confirmed_total_for_scenario=sum(decoded$confirmed_total_for_scenario[rows]),stringsAsFactors=FALSE)
  }))
  list(decoded=decoded,field_inventory=do.call(rbind,inventory),
    code_inventory=if(length(code_rows)) do.call(rbind,code_rows) else data.frame(),summary=summary)
}
