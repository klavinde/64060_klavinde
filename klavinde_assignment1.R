DataSource:https://data.ohio.gov/wps/portal/gov/data/view/swimming-pools-by-county
mydata<-read.csv("C:/Users/krist/Desktop/KENT/MAchine Learning/swimming_pools_by_county.csv")
summary(mydata)
table(mydata$County)
hist(mydata$Pool.Volume_gal,n=20)
colnames(mydata)=c("County","Facility.Name","Facility.Address","City","Indoor.Outdoor","Pool.Size_sf","Pool.Volume_gal")
table(mydata$Indoor.Outdoor)
mydata$newcol=mydata$Pool.Volume_gal/mydata$Pool.Size_sf
colnames(mydata)=c("County","Facility.Name","Facility.Address","City","Indoor.Outdoor","Pool.Size_sf","Pool.Volume_gal","Gal_per_sf")
plot(mydata$Pool.Size_sf,mydata$Pool.Volume_gal)
