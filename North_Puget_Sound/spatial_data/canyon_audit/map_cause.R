library(sf);library(ggplot2)
x<-readRDS("spatial_data/canyon_audit/barrier_cause.rds")
b<-x$barriers
window<-st_bbox(st_buffer(st_union(b),1000))
model<-suppressWarnings(st_crop(x$model,window))
swifd<-suppressWarnings(st_crop(x$swifd,window))
snaps<-st_as_sf(st_drop_geometry(b),coords=c("snap_x","snap_y"),crs=26910,remove=FALSE)
links<-st_sf(SiteId=b$SiteId,geometry=st_sfc(lapply(seq_len(nrow(b)),function(i){
  st_linestring(rbind(st_coordinates(b)[i,],st_coordinates(snaps)[i,]))
}),crs=26910))
p<-ggplot()+geom_sf(data=model,aes(color="Occupancy model"),linewidth=.6)+
  geom_sf(data=swifd,aes(color="SWIFD distribution"),linewidth=.35,linetype="dashed")+
  geom_sf(data=x$edge,aes(color="Assigned mainstem edge"),linewidth=1)+
  geom_sf(data=links,color="red",linetype="dotted")+
  geom_sf(data=b,color="red",shape=24,fill="yellow",size=4)+
  geom_sf(data=snaps,color="black",shape=4,size=3)+
  geom_sf_label(data=b,aes(label=paste0("Site ",SiteId,"\nunnamed tributary\nsnap ",round(snap_distance_m,1)," m")),
    nudge_y=c(130,-150),size=3)+
  scale_color_manual(values=c("Occupancy model"="#126a91","SWIFD distribution"="#e66b18","Assigned mainstem edge"="#8b28a3"))+
  coord_sf(xlim=window[c("xmin","xmax")],ylim=window[c("ymin","ymax")],datum=st_crs(4326))+
  labs(title="Canyon Creek audit: tributary barriers assigned to SF Stillaguamish mainstem",
       subtitle="Either of these two verified records alone removes all modeled Canyon Creek miles",
       caption="Triangles: original barrier points; black crosses: snapped locations. The receiving mainstem is the SWIFD South Fork Stillaguamish River.",color=NULL,x=NULL,y=NULL)+
  theme_bw()+theme(legend.position="bottom")
ggsave("spatial_data/canyon_audit/barrier_assignment.png",p,width=11,height=8,dpi=200)
