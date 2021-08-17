source("R/core.R")

# Load the shapefiles
ca <- readOGR("Files/lfed000b16a_e.shp")
ca_map <- fortify(ca, region="FEDUID")

# Fetch Latest Projections from 338 Canada
projections<-read.delim("https://338canada.com/338canadafederalproj.txt",
                        skip=5,header=T,sep=",")

# Form the base map
source("R/map_core.R")

# Main Projection Map
plotdata<-ridingmap %>%
  mutate(riding_code=as.numeric(substr(id,1,5))) %>%
  left_join(projections,by="riding_code") %>%
  mutate(party=ifelse(grepl("BQ",current_projection),"BQ",NA),
         party=ifelse(grepl("CPC",current_projection),"CPC",party),
         party=ifelse(grepl("LPC",current_projection),"LPC",party),
         party=ifelse(grepl("NDP",current_projection),"NDP",party),
         party=ifelse(grepl("GPC",current_projection),"GPC",party),
         party=ifelse(grepl("PPC",current_projection),"PPC",party),
         party=ifelse(grepl("Toss up",current_projection),"Toss Up",party),
         projection=gsub(" leaning","",current_projection),
         projection=gsub(" likely","",projection),
         projection=gsub(" safe","",projection),
         change=(projection!=current_party),
         strength=1,
         strength=ifelse(grepl("Safe",current_projection),1,strength),
         strength=ifelse(grepl("Leaning",current_projection),0.9,strength),
         strength=ifelse(grepl("Likely",current_projection),0.95,strength))
count<-plotdata %>%
  select(id,party) %>%
  filter(!grepl("_",id)) %>%
  distinct() %>%
  group_by(party) %>%
  mutate(seats=1) %>%
  summarise(count=sum(seats)) %>%
  arrange(-count)
plotdata<-plotdata %>%
  left_join(count,by="party") %>%
  mutate(party=paste0(party," (",count,") "))
ggplot(plotdata,aes(x=long,y=lat,group=group,fill=party))+
  geom_polygon(color="gray90",size=0)+
  geom_polygon(data=filter(plotdata,id=='35032_KIT'),color="gray90",size=0)+ # needed b/c Guelph is an enclave
  geom_polygon(data=filter(plotdata,id=='24073'),color="gray90",size=0)+ # needed b/c Sherbrooke is an enclave
  mythememap+
  coord_cartesian(xlim=c(2500000,9420000))+ # funny thing with halifax
  scale_fill_manual(name = "Party",
                    values = c("#33B2CC","#1A4782","#3D9B35","#D71920",
                               "#F37021","yellow"))+
  maplabels+
  labs(x="",y="",
       title=paste0("#elxn44 Projection from 338Canada.com (",
                    gsub(" 0"," ",format(Sys.Date(),"%B %d, %Y")),")"),
       subtitle="Seat projections from P.J. Fourier @338Canada. Likely/Leaning/Safe are grouped by party. Total seat counts in parentheses.",
       caption="Graph by @trevortombe")
filename<-paste0("Plots/proj_",Sys.Date(),".png")
ggsave(filename,width=7,height=8)
ggsave("Plots/proj_latest.png",width=7,height=8)

# Toss Ups and Seat Changes
ggplot(plotdata,aes(x=long,y=lat,group=group,fill=change))+
  geom_polygon(color="gray90",size=0)+
  geom_polygon(data=filter(plotdata,id=='35032_KIT'),color="gray90",size=0)+ # needed b/c Guelph is an enclave
  geom_polygon(data=filter(plotdata,id=='24073'),color="gray90",size=0)+ # needed b/c Sherbrooke is an enclave
  mythememap+
  coord_cartesian(xlim=c(2500000,9420000))+ # funny thing with halifax
  scale_fill_manual(name = "Battlegrounds in Canada",
                    values = c("gray","yellow"))+
  theme(legend.position = 'none')+
  maplabels+
  labs(x="",y="",
       title=paste0("#elxn44 Battlegrounds (",
                    gsub(" 0"," ",format(Sys.Date(),"%B %d, %Y")),")"),
       subtitle="Based on Projections from P.J. Fourier @338Canada. Displays toss up and projected party switches.",
       caption="Graph by @trevortombe")
filename<-paste0("Plots/battle_",Sys.Date(),".png")
ggsave(filename,width=7,height=8)
ggsave("Plots/battle_latest.png",width=7,height=8)
