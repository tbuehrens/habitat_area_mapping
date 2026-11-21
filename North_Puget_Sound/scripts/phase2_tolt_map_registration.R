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
library(sf)
source('R/stream_edge_identity.R')
# Pixel positions are manually traced on City of Seattle2024 PAD Figure4-86,
# printed page318/PDF page340, rendered at150% (1188x918px). They register the
# source's mapped SF Tolt channel to the independent fine WDFW named channel.
# The red natural-waterfall symbol is at949,434. Residuals describe river
# tracing/reference disagreement and do not assert surveyed point accuracy.
# Source: https://fercrelicensing.seattle.gov/library/assets/docs/SFT_PAD_FINAL.pdf
ref<-st_transform(st_read('spatial_data/phase2/sf_tolt_reference.geojson',quiet=TRUE),26910)
ref<-suppressWarnings(st_cast(ref,'LINESTRING'))
pixel<-rbind(c(195,395),c(206,399),c(215,408),c(235,420),c(257,433),c(274,450),c(290,454),c(310,452),c(324,443),c(335,434),c(353,428),c(376,415),c(389,400),c(402,383),c(411,379),c(431,370),c(456,367),c(482,361),c(517,353),c(536,354),c(559,360),c(590,367),c(620,381),c(649,401),c(662,416),c(677,427),c(699,425),c(725,427),c(754,430),c(784,432),c(813,442),c(839,455),c(853,457),c(873,449),c(896,442),c(914,420),c(923,428),c(932,430),c(938,435),c(949,434),c(963,428),c(989,426))
refxy<-st_coordinates(ref)[,1:2]
refgeom<-st_union(st_geometry(ref))
mouth<-refxy[which.min(refxy[,1]),]
transform<-function(par,xy) {
  angle<-par[4];rot<-matrix(c(cos(angle),sin(angle),-sin(angle),cos(angle)),2)
  sweep(sweep(xy,2,c(195,395))%*%diag(c(1,-1))%*%rot*par[3],2,par[1:2],'+')
}
objective<-function(par) {
  xy<-transform(par,pixel)
  distances<-as.numeric(st_distance(st_sfc(lapply(seq_len(nrow(xy)),function(i)st_point(xy[i,])),crs=26910),refgeom))
  sum(pmin(distances,100)^2)+sum((xy[1,]-mouth)^2)*2
}
fit<-optim(c(mouth,12.2,0),objective,method='Nelder-Mead',control=list(maxit=4000,parscale=c(1000,1000,10,.01),reltol=1e-12))
print(fit)
xy<-transform(fit$par,pixel)
distances<-as.numeric(st_distance(st_sfc(lapply(seq_len(nrow(xy)),function(i)st_point(xy[i,])),crs=26910),refgeom))
print(summary(distances))
fall<-transform(fit$par,matrix(c(949,434),nrow=1))
fall<-st_sfc(st_point(unname(fall[1,])),crs=26910)
print(st_coordinates(st_transform(fall,4326)))
write.csv(data.frame(pixel_x=pixel[,1],pixel_y=pixel[,2],x=xy[,1],y=xy[,2],distance_to_reference_m=distances),'spatial_data/phase2/tolt_map_registration.csv',row.names=FALSE)
saveRDS(list(fit=fit,fall=fall,distances=distances,pixel=pixel,source_page=340,source_figure='4-86',source_falls_pixel=c(949,434)),'spatial_data/phase2/tolt_map_registration.rds')
if(file.exists('spatial_data/phase2/tolt_page340.png')) {
png('spatial_data/phase2/tolt_map_registration.png',width=1188,height=918)
par(mar=c(0,0,0,0));plot(NA,xlim=c(0,1188),ylim=c(918,0),asp=1,xaxs='i',yaxs='i',axes=FALSE,xlab='',ylab='')
rasterImage(png::readPNG('spatial_data/phase2/tolt_page340.png'),0,918,1188,0)
rot<-matrix(c(cos(fit$par[4]),sin(fit$par[4]),-sin(fit$par[4]),cos(fit$par[4])),2)
for(line in st_geometry(ref)) {
  back<-sweep(((sweep(unclass(line)[,1:2],2,fit$par[1:2]))/fit$par[3])%*%t(rot)%*%diag(c(1,-1)),2,c(195,395),'+')
  lines(back,col='magenta',lwd=1.3)
}
points(pixel,col='green',pch=20,cex=.4);points(949,434,col='red',pch=1,cex=1.3)
dev.off()
}
})
