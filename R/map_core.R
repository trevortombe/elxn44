regions<-read.csv("Files/riding_list.csv")
test<-ca
for (r in c("VAN","CGY","EDM","WPG","TOR","QC","REG","SK","LON","WIN","HAM")){
  list<-(regions %>% 
           filter(Region==r) %>% 
           select(FEDUID=idnum) %>% 
           mutate(FEDUID=as.factor(FEDUID)))
  temp <- ca[ca$FEDUID %in% list$FEDUID & !is.na(ca$FEDUID),]
  temp <- elide(temp, scale=max(apply(bbox(temp), 1, diff)) * 15)
  proj4string(temp) <- proj4string(ca)
  temp$FEDUID<-paste0(temp$FEDUID,"_",r)
  test<-rbind(test,temp)
}
for (r in c("VIC","OTT","HAL","KIT","STJ","MTR","NIA")){
  list<-(regions %>% 
           filter(Region==r) %>% 
           select(FEDUID=idnum) %>% 
           mutate(FEDUID=as.factor(FEDUID)))
  temp <- ca[ca$FEDUID %in% list$FEDUID & !is.na(ca$FEDUID),]
  temp <- elide(temp, scale=max(apply(bbox(temp), 1, diff)) * 10)
  proj4string(temp) <- proj4string(ca)
  temp$FEDUID<-paste0(temp$FEDUID,"_",r)
  test<-rbind(test,temp)
}
ridingmap<-fortify(test,region="FEDUID") %>%
  mutate(long=ifelse(grepl("VAN",id),long+2500000,long),
         lat=ifelse(grepl("VAN",id),lat-500000,lat),
         long=ifelse(grepl("VIC",id),long+2500000,long),
         lat=ifelse(grepl("VIC",id),lat+1700000,lat),
         long=ifelse(grepl("CGY",id),long+4200000,long),
         lat=ifelse(grepl("CGY",id),lat-500000,lat),
         long=ifelse(grepl("REG",id),long+5000000,long),
         lat=ifelse(grepl("REG",id),lat-0,lat),
         long=ifelse(grepl("SK",id),long+5000000,long),
         lat=ifelse(grepl("SK",id),lat+800000,lat),
         long=ifelse(grepl("EDM",id),long+4250000,long),
         lat=ifelse(grepl("EDM",id),lat+800000,lat),
         long=ifelse(grepl("WPG",id),long+5750000,long),
         lat=ifelse(grepl("WPG",id),lat+400000,lat),
         long=ifelse(grepl("LON",id),long+5500000,long),
         lat=ifelse(grepl("LON",id),lat-400000,lat),
         long=ifelse(grepl("WIN",id),long+5000000,long),
         lat=ifelse(grepl("WIN",id),lat-1250000,lat),
         long=ifelse(grepl("NIA",id),long+8350000,long),
         lat=ifelse(grepl("NIA",id),lat-1700000,lat),
         long=ifelse(grepl("TOR",id),long+6000000,long),
         lat=ifelse(grepl("TOR",id),lat-1250000,lat),
         long=ifelse(grepl("HAM",id),long+6750000,long),
         lat=ifelse(grepl("HAM",id),lat-2000000,lat),
         long=ifelse(grepl("OTT",id),long+8750000,long),
         lat=ifelse(grepl("OTT",id),lat-1400000,lat),
         long=ifelse(grepl("KIT",id),long+7500000,long),
         lat=ifelse(grepl("KIT",id),lat-1000000,lat),
         long=ifelse(grepl("MTR",id),long+8000000,long),
         lat=ifelse(grepl("MTR",id),lat-0,lat),
         long=ifelse(grepl("HAL",id),long+8000000,long),
         lat=ifelse(grepl("HAL",id),lat+3000000,lat),
         long=ifelse(grepl("STJ",id),long+9000000,long),
         lat=ifelse(grepl("STJ",id),lat+3000000,lat),
         long=ifelse(grepl("QC",id),long+9000000,long),
         lat=ifelse(grepl("QC",id),lat+1500000,lat))

# Useful to map labels
maplabels<-list(
  annotate('text',x=mean((ridingmap %>% filter(grepl("TOR",id)))$long),
           y=min((ridingmap %>% filter(grepl("TOR",id)))$lat),
           label="Greater\nToronto",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("VIC",id)))$long),
           y=min((ridingmap %>% filter(grepl("VIC",id)))$lat),
           label="Victoria",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("CGY",id)))$long),
           y=min((ridingmap %>% filter(grepl("CGY",id)))$lat),
           label="Calgary",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("EDM",id)))$long),
           y=min((ridingmap %>% filter(grepl("EDM",id)))$lat),
           label="Edmonton",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("WPG",id)))$long),
           y=min((ridingmap %>% filter(grepl("WPG",id)))$lat),
           label="Winnipeg",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("VAN",id)))$long),
           y=min((ridingmap %>% filter(grepl("VAN",id)))$lat),
           label="Greater Vancouver",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("MTR",id)))$long),
           y=min((ridingmap %>% filter(grepl("MTR",id)))$lat),
           label="Greater Montreal",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("OTT",id)))$long),
           y=min((ridingmap %>% filter(grepl("OTT",id)))$lat),
           label="Ottawa-Hull",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("QC",id)))$long),
           y=min((ridingmap %>% filter(grepl("QC",id)))$lat),
           label="Quebec City",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("KIT",id)))$long),
           y=min((ridingmap %>% filter(grepl("KIT",id)))$lat),
           label="Kitchener-\nGuelph",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("REG",id)))$long),
           y=min((ridingmap %>% filter(grepl("REG",id)))$lat),
           label="Regina",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("SK",id)))$long),
           y=min((ridingmap %>% filter(grepl("SK",id)))$lat),
           label="Saskatoon",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("LON",id)))$long),
           y=min((ridingmap %>% filter(grepl("LON",id)))$lat),
           label="London",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("WIN",id)))$long),
           y=min((ridingmap %>% filter(grepl("WIN",id)))$lat),
           label="Windsor",vjust=1.1,size=2),
  annotate('text',x=max((ridingmap %>% filter(grepl("NIA",id)))$long),
           y=min((ridingmap %>% filter(grepl("NIA",id)))$lat)-100000,
           label="Niagara",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("HAL",id)))$long),
           y=min((ridingmap %>% filter(grepl("HAL",id)))$lat),
           label="Halifax",vjust=1.1,size=2),
  annotate('text',x=max((ridingmap %>% filter(grepl("HAM",id)))$long),
           y=min((ridingmap %>% filter(grepl("HAM",id)))$lat),
           label="Hamilton",vjust=1.1,size=2),
  annotate('text',x=mean((ridingmap %>% filter(grepl("STJ",id)))$long),
           y=min((ridingmap %>% filter(grepl("STJ",id)))$lat),
           label="St. John's",vjust=1.1,size=2)
)
