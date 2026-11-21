source(file.path("R","barrier_metadata.R"))

# Report-cache row numbers and source-like columns must never overwrite the
# immutable review identities, including an unmatched Gorge override without ID.
baseline<-data.frame(SiteId=c("123456",NA_character_),review_number=c(22L,903L),
  barrier_key=c("5:source_record","user_override:Gorge_Dam"),
  StreamName=c("Own Creek","Skagit River"),verification=c("Verified","User-requested override"),
  source_layer=c("5","user_override"),FishPassageBarrierStatusCode=c(10L,NA_integer_),
  PercentFishPassableCode=c(NA_integer_,NA_integer_),override_future_cut=c(FALSE,TRUE),
  stream_identity_confirmed=TRUE,stringsAsFactors=FALSE)
evidence<-data.frame(SiteId="123456",review_number=999L,
  barrier_key="wrong_report_key",StreamName="Wrong stream",verification="Wrong verification",
  override_future_cut=TRUE,report_explicit_total=TRUE,report_blockage="Total",
  report_identity_verified=TRUE,report_blockage_conflict=FALSE,
  report_url="https://example.test/public_report.pdf",stringsAsFactors=FALSE)
metadata<-list(id=5,fields=list(list(name="FishPassageBarrierStatusCode",alias="Barrier?",
  type="esriFieldTypeSmallInteger",domain=list(codedValues=list(list(code=10,name="Yes"))))))
result<-audit_barrier_metadata(baseline,metadata,evidence)$decoded
stopifnot(identical(result[names(baseline)],baseline),
  identical(result$review_number,c(22L,903L)),
  result$report_explicit_total[1L],is.na(result$report_explicit_total[2L]),
  result$confirmed_total_for_scenario[1L],!result$confirmed_total_for_scenario[2L],
  identical(result$override_future_cut,c(FALSE,TRUE)))
cat("Metadata joins preserve source identities, numbering, and unmatched Gorge override\n")
