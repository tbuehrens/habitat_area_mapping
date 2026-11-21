# This repair changes only barrier assignment in network.R, not its three
# projection/slicing/endpoint helpers used to compute the accepted SWIFD routes.
# Verify all actual routing inputs are identical before retaining that work.
path <- "spatial_data/north_puget_swifd_bridges.rds"
x <- readRDS(path)
inputs <- c("spatial_data/north_puget_steelhead.rds", "spatial_data/north_puget_swifd_steelhead.rds",
  file.path("R",c("swifd_connectors.R","source_classification.R","network.R","swifd_sources.R")))
current <- unname(tools::md5sum(inputs))
stopifnot(length(x$signature$md5)==6L,
  identical(x$signature$md5[-5],current[-5]),x$signature$endpoint_distance_m==100)
x$signature$md5[5] <- current[5]
saveRDS(x,path)
cat("Preserved identical SWIFD routing inputs; only barrier assignments will be recomputed.\n")
