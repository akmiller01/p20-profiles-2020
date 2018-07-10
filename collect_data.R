list.of.packages <- c("data.table","varhandle","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/p20-profiles-2018")

countries <- read.csv("data/countries.csv",na.strings="")
countries$original.order = c(1:nrow(countries))

incomedata <- read.csv("data/IncomeData1.csv")
if(!"longname" %in% names(incomedata)){setnames(incomedata,"CountryName","longname")}
if(is.factor(incomedata$longname)){incomedata$longname=unfactor(incomedata$longname)}
incomedata$longname[which(incomedata$longname=="Cabo Verde")] = "Cape Verde"
incomedata$longname[which(incomedata$longname=="Congo")] = "Republic of Congo"
incomedata$longname[which(incomedata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
incomedata$longname[which(incomedata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
if(is.factor(incomedata$birthregtrends)){incomedata$birthregtrends=unfactor(incomedata$birthregtrends)}
incomedata$birthregtrends[which(incomedata$birthregtrends=="")] = NA
incomedata$birthregtrends[which(incomedata$birthregtrends=="increasing")] = "increased"
incomedata$birthregtrends[which(incomedata$birthregtrends=="decreasing")] = "decreased"
incomedata$birthregtrends[which(incomedata$birthregtrends=="about.the.same")] = "stayed constant"
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

regdata <- read.csv("data/birthregP20.csv",fileEncoding="latin1")
if(!"longname" %in% names(regdata)){setnames(regdata,"CountryName","longname")}
if(is.factor(regdata$longname)){regdata$longname=unfactor(regdata$longname)}
if(is.factor(regdata$NP20.Reg)){regdata$NP20.Reg=unfactor(regdata$NP20.Reg)}
regdata$NP20.Reg = gsub("\u0096","",regdata$NP20.Reg)
regdata$NP20.Reg[which(regdata$longname=="Solomon Islands")] <- "87"
regdata$NP20.Reg = as.numeric(regdata$NP20.Reg)
regdata$longname[which(regdata$longname=="Cabo Verde")] = "Cape Verde"
regdata$longname[which(regdata$longname=="Congo")] = "Republic of Congo"
regdata$longname[which(regdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
regdata$longname[which(regdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
regdata$longname[which(regdata$longname=="Democratic People's Republic of Korea")] = "Democratic People’s Republic of Korea"
regdata$longname[which(regdata$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
missing_from_reg = setdiff(countries$longname,regdata$longname)
reg_missing_from_countries = setdiff(regdata$longname,countries$longname)

nateducdata <- read.csv("data/ntl_secondarycompletion.csv")
if(!"longname" %in% names(nateducdata)){setnames(nateducdata,"CountryName","longname")}
if(is.factor(nateducdata$longname)){nateducdata$longname=unfactor(nateducdata$longname)}
nateducdata$longname[which(nateducdata$longname=="Cabo Verde")] = "Cape Verde"
nateducdata$longname[which(nateducdata$longname=="Congo")] = "Republic of Congo"
nateducdata$longname[which(nateducdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
nateducdata$longname[which(nateducdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
nateducdata$longname[which(nateducdata$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
nateducdata$longname[which(nateducdata$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
nateducdata$longname[which(nateducdata$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
nateducdata$longname[which(nateducdata$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
nateducdata$longname[which(nateducdata$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_nateduc = setdiff(countries$longname,nateducdata$longname)
nateduc_missing_from_countries = setdiff(nateducdata$longname,countries$longname)
nateducdata = subset(nateducdata,!is.na(Secondary.Completion))
nateducdata = nateducdata[order(nateducdata$longname,-nateducdata$Year),]
nateducdata$dup = duplicated(nateducdata$longname)
nateducdata = subset(nateducdata,!dup)
nateducdata$dup = NULL
setnames(nateducdata,"Year","nateducYear")

p20educdata <- read.csv("data/P20_secondarycompletion.csv")
if(!"longname" %in% names(p20educdata)){setnames(p20educdata,"CountryName","longname")}
if(is.factor(p20educdata$longname)){p20educdata$longname=unfactor(p20educdata$longname)}
p20educdata$longname[which(p20educdata$longname=="Cabo Verde")] = "Cape Verde"
p20educdata$longname[which(p20educdata$longname=="Congo")] = "Republic of Congo"
p20educdata$longname[which(p20educdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
p20educdata$longname[which(p20educdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
p20educdata$longname[which(p20educdata$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
p20educdata$longname[which(p20educdata$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
p20educdata$longname[which(p20educdata$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
p20educdata$longname[which(p20educdata$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
p20educdata$longname[which(p20educdata$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_p20educ = setdiff(countries$longname,p20educdata$longname)
p20educ_missing_from_countries = setdiff(p20educdata$longname,countries$longname)
setnames(p20educdata,"Year","p20educYear")

natmortdata <- read.csv("data/ntl_u5Mortality.csv")
if(!"longname" %in% names(natmortdata)){setnames(natmortdata,"CountryName","longname")}
if(is.factor(natmortdata$longname)){natmortdata$longname=unfactor(natmortdata$longname)}
natmortdata$longname[which(natmortdata$longname=="Cabo Verde")] = "Cape Verde"
natmortdata$longname[which(natmortdata$longname=="Congo")] = "Republic of Congo"
natmortdata$longname[which(natmortdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
natmortdata$longname[which(natmortdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
natmortdata$longname[which(natmortdata$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
natmortdata$longname[which(natmortdata$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
natmortdata$longname[which(natmortdata$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
natmortdata$longname[which(natmortdata$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
natmortdata$longname[which(natmortdata$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_natmort = setdiff(countries$longname,natmortdata$longname)
natmort_missing_from_countries = setdiff(natmortdata$longname,countries$longname)
natmortdata = subset(natmortdata,!is.na(NtlUnder5Mortality))
natmortdata = natmortdata[order(natmortdata$longname,-natmortdata$Year),]
natmortdata$dup = duplicated(natmortdata$longname)
natmortdata = subset(natmortdata,!dup)
natmortdata$dup = NULL
setnames(natmortdata,"Year","natmortYear")

p20mortdata <- read.csv("data/P20_u5mortality.csv")
if(!"longname" %in% names(p20mortdata)){setnames(p20mortdata,"CountryName","longname")}
if(is.factor(p20mortdata$longname)){p20mortdata$longname=unfactor(p20mortdata$longname)}
p20mortdata$longname[which(p20mortdata$longname=="Cabo Verde")] = "Cape Verde"
p20mortdata$longname[which(p20mortdata$longname=="Congo")] = "Republic of Congo"
p20mortdata$longname[which(p20mortdata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
p20mortdata$longname[which(p20mortdata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
p20mortdata$longname[which(p20mortdata$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
p20mortdata$longname[which(p20mortdata$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
p20mortdata$longname[which(p20mortdata$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
p20mortdata$longname[which(p20mortdata$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
p20mortdata$longname[which(p20mortdata$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_p20mort = setdiff(countries$longname,p20mortdata$longname)
p20mort_missing_from_countries = setdiff(p20mortdata$longname,countries$longname)
p20mortdata = subset(p20mortdata,!is.na(U5Mortality.Rest))
p20mortdata = p20mortdata[order(p20mortdata$longname,-p20mortdata$Year),]
p20mortdata$dup = duplicated(p20mortdata$longname)
p20mortdata = subset(p20mortdata,!dup)
p20mortdata$dup = NULL
setnames(p20mortdata,"Year","p20mortYear")

obesitydata <- read.csv("data/obesityrates.csv")
if(!"longname" %in% names(obesitydata)){setnames(obesitydata,"CountryName","longname")}
if(is.factor(obesitydata$longname)){obesitydata$longname=unfactor(obesitydata$longname)}
obesitydata$longname[which(obesitydata$longname=="Cabo Verde")] = "Cape Verde"
obesitydata$longname[which(obesitydata$longname=="Congo")] = "Republic of Congo"
obesitydata$longname[which(obesitydata$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
obesitydata$longname[which(obesitydata$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
obesitydata$longname[which(obesitydata$longname=="Democratic People's Republic of Korea")] = "Democratic People’s Republic of Korea"
obesitydata$longname[which(obesitydata$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
obesitydata$longname[which(obesitydata$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
obesitydata$longname[which(obesitydata$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
obesitydata$longname[which(obesitydata$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_obesity = setdiff(countries$longname,obesitydata$longname)
obesity_missing_from_countries = setdiff(obesitydata$longname,countries$longname)
obesitydata = subset(obesitydata,!is.na(obesity.rate))
obesitydata = obesitydata[order(obesitydata$longname,-obesitydata$Year),]
obesitydata$dup = duplicated(obesitydata$longname)
obesitydata = subset(obesitydata,!dup)
obesitydata$dup = NULL
setnames(obesitydata,"Year","obesityYear")
obesitydata$obesity.rate = obesitydata$obesity.rate*100

countries <- merge(countries,incomedata,by="longname",all.x=T)
countries <- merge(countries,gapdata.w,by="longname",all.x=T)
countries <- merge(countries,stuntingdata,by="longname",all.x=T)
countries <- merge(countries,regdata,by="longname",all.x=T)
countries <- merge(countries,nateducdata,by="longname",all.x=T)
countries <- merge(countries,p20educdata,by="longname",all.x=T)
countries <- merge(countries,natmortdata,by="longname",all.x=T)
countries <- merge(countries,p20mortdata,by="longname",all.x=T)
countries <- merge(countries,obesitydata,by="longname",all.x=T)

countries = countries[order(countries$original.order),]
names(countries) = gsub(".","",names(countries),fixed=T)
write.csv(countries,"data/countries_merged.csv",na="",row.names=F)

# for(theslug in countries$slug){
#   country = subset(countries,slug==theslug)[1,]
#   lowincome = country$lowincome
#   if(lowincome){
#     for(i in c(1:6)){
#       chart_name = paste0("charts/",theslug,"_c",i,".png")
#       file.copy("final_template/no_data.png",chart_name)
#     }
#   }else{
#     for(i in c(1:3)){
#       chart_name = paste0("charts/",theslug,"_c",i,".png")
#       file.copy("final_template/no_data.png",chart_name)
#     }
#   }
# }
