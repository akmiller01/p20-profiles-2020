list.of.packages <- c("data.table","varhandle")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/p20-profiles-2018")

countries <- read.csv("data/countries.csv",na.strings="")

incomedata <- read.csv("data/IncomeData1.csv")
if(!"longname" %in% names(incomedata)){setnames(incomedata,"CountryName","longname")}
if(is.factor(incomedata$longname)){incomedata$longname=unfactor(incomedata$longname)}
incomedata$longname[which(incomedata$longname=="Cabo Verde")] = "Cape Verde"
incomedata$longname[which(incomedata$longname=="Congo")] = "Republic of Congo"
incomedata$longname[which(incomedata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
incomedata$longname[which(incomedata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
missing_from_income = setdiff(countries$longname,incomedata$longname)
missing_from_countries = setdiff(incomedata$longname,countries$longname)

countries <- merge(countries,incomedata,by="longname",all.x=T)

names(countries) = gsub(".","",names(countries),fixed=T)
write.csv(countries,"data/countries_merged.csv",na="",row.names=F)
