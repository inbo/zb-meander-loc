source("./code/not_functions/libraries.R")
source("./code/functions/f.read_excel_allsheets.R")

list.file<-list.files(path='./data/locaties/extern/2016/', pattern=".csv", all.files=TRUE,full.names=TRUE)
for (j in 1:length(list.file)){
  file.temp<-read.csv(list.file[j],header = FALSE)[,1:4]
  colnames(file.temp)=c("trimblecode","X","Y","Z")
  file.temp$loc<-gsub(".csv","",str_split(list.file[j],"/")[[1]][6])
  file.temp$jaar<-gsub(".csv","",str_split(list.file[j],"/")[[1]][5])
  if (j==1){file=file.temp} 
  if (j>1){file=smartbind(file,file.temp)}
}
remove(file.temp)
file$traject<-substr(file$loc, nchar(file$loc), nchar(file$loc))
file$sectie<-word(file$loc,1)
file$sectie[which(file$sectie=="schurf")]="schurfert"
file$loc<-paste0(file$sectie,file$traject,"-",file$trimblecode)
file$sectie<-NULL
file$traject<-NULL
file$trimblecode<-NULL

file.temp<-read.csv('./data/locaties/extern/2019/Trimble_Zwarte beek_2019.csv',header = FALSE)[,c(1,3:5)]
colnames(file.temp)=c("loc","X","Y","Z")
file.temp$jaar<-"2019"
file.temp<-file.temp[!grepl("test", file.temp$loc),]
file.temp<-file.temp[!grepl("pijke", file.temp$loc),]
file.temp<-file.temp[!grepl("raf", file.temp$loc),]
file=smartbind(file,file.temp)
remove(file.temp)

file.temp<-read.csv('./data/locaties/extern/2024/Stijn ZB 2024.csv',sep=";",skip=1)[,c(1,3:5)]
colnames(file.temp)=c("loc","X","Y","Z")
file.temp<-file.temp[ !duplicated(file.temp$loc, fromLast=T),]
file.temp$jaar<-"2024"
file=smartbind(file,file.temp)
remove(file.temp)

file <- file %>%
  filter(!loc %in% c("test-thuis1","test-thuis2","test-thuis3","schurfert1","schurfert6-basis","schurfert6-ori1","schurfert6-ori2","schurfert5-ori1","schurfert5-ori2","schurfert5-ori3","schurfert5-ori10","schurfert5-ori20","schurfert5-basis1","laren3-test"))

write.csv(file,"./data/locaties/intern/coordinaten_overzicht.csv")

source('./code/functions/f.map.R')
map<-f.map(file,"X","Y",crs="+init=epsg:31370","jaar","loc")
f.map(file,"X","Y",crs="+init=epsg:31370","jaar","loc",plot=TRUE)
