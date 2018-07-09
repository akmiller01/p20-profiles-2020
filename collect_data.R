list.of.packages <- c("data.table","varhandle","reshape2")
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
income_missing_from_countries = setdiff(incomedata$longname,countries$longname)

gapdata <- read.csv("data/IncomeGapGraph.csv")
if(!"longname" %in% names(gapdata)){setnames(gapdata,"Country","longname")}
if(is.factor(gapdata$longname)){gapdata$longname=unfactor(gapdata$longname)}
gapdata$longname[which(gapdata$longname=="Cabo Verde")] = "Cape Verde"
gapdata$longname[which(gapdata$longname=="Congo")] = "Republic of Congo"
gapdata$longname[which(gapdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
gapdata$longname[which(gapdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
missing_from_gap = setdiff(countries$longname,gapdata$longname)
gap_missing_from_countries = setdiff(gapdata$longname,countries$longname)
gapdata <- subset(gapdata,Year %in% c(1999,2013))
gapdata$gap = gapdata$rest.of.population.average - gapdata$national.P20.average
keep <- c("longname","Year","gap")
gapdata <- gapdata[keep]
gapdata.m = melt(gapdata,id.vars=c("longname","Year"))
gapdata.w = dcast(gapdata.m,longname~variable+Year)

stuntingdata <- read.csv("data/stunting.csv")
if(!"longname" %in% names(stuntingdata)){setnames(stuntingdata,"CountryName","longname")}
if(is.factor(stuntingdata$longname)){stuntingdata$longname=unfactor(stuntingdata$longname)}
stuntingdata$longname[which(stuntingdata$longname=="Cabo Verde")] = "Cape Verde"
stuntingdata$longname[which(stuntingdata$longname=="Congo")] = "Republic of Congo"
stuntingdata$longname[which(stuntingdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
stuntingdata$longname[which(stuntingdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
stuntingdata$longname[which(stuntingdata$longname=="Democratic People's Republic of Korea")] = "Democratic People’s Republic of Korea"
missing_from_stunting = setdiff(countries$longname,stuntingdata$longname)
stunting_missing_from_countries = setdiff(stuntingdata$longname,countries$longname)

regdata <- read.csv("data/birthregP20.csv")

countries <- merge(countries,incomedata,by="longname",all.x=T)
countries <- merge(countries,gapdata.w,by="longname",all.x=T)
countries <- merge(countries,stuntingdata,by="longname",all.x=T)

names(countries) = gsub(".","",names(countries),fixed=T)
write.csv(countries,"data/countries_merged.csv",na="",row.names=F)
