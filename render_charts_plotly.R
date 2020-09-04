list.of.packages <- c("data.table","varhandle","reshape2","ggplot2","scales", "plotly","htmlwidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# C1: Income trends line
# C2: Stunting bar (LI only)
# C3: Birth reg bar (LI only)
# C4: Secondary educ donut
# C5: Secondary educ bar (LI only)
# C6: Under 5 mort bar (LI only)
# C7: Obesity line (HI only)

wd = "~/git/p20-profiles-2020"
setwd(wd)

unlink("charts_interactive/*.html")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

light.grey = "#DFDAE0"
mid.grey = "#AAA6AB"
dark.grey = "#5E585C"
red = "#E9443A"
pink = "#F8C1B2"
font.size = 15
line.size = 1.1
dpi = 300
two_tone = scale_color_manual(values=c(red,pink))
two_tone_fill = scale_fill_manual(values=c(red,pink))

simple_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.major.y = element_line(color=light.grey )
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.line.x = element_line(colour = dark.grey)
    ,axis.line.y = element_blank()
    ,axis.text.x = element_text(colour=dark.grey, size=font.size)
    ,axis.text.y = element_text(colour=mid.grey, size=font.size)
    ,axis.title = element_text(size=font.size)
    ,axis.ticks = element_blank()
    ,legend.text = element_text(size=font.size))


countries <- read.csv("data/countries.csv",na.strings="")
countries$original.order = c(1:nrow(countries))

c1_data = read.csv("data/IncomeGapGraph.csv")
if(!"longname" %in% names(c1_data)){setnames(c1_data,"Country","longname")}
if(is.factor(c1_data$longname)){c1_data$longname=unfactor(c1_data$longname)}
c1_data$longname[which(c1_data$longname=="Cabo Verde")] = "Cape Verde"
c1_data$longname[which(c1_data$longname=="Congo")] = "Republic of Congo"
c1_data$longname[which(c1_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c1_data$longname[which(c1_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
missing_from_c1 = setdiff(countries$longname,c1_data$longname)
c1_missing_from_countries = setdiff(c1_data$longname,countries$longname)

c2_data <- read.csv("data/stunting.csv")
if(!"longname" %in% names(c2_data)){setnames(c2_data,"CountryName","longname")}
if(is.factor(c2_data$longname)){c2_data$longname=unfactor(c2_data$longname)}
c2_data$longname[which(c2_data$longname=="Cabo Verde")] = "Cape Verde"
c2_data$longname[which(c2_data$longname=="Congo")] = "Republic of Congo"
c2_data$longname[which(c2_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c2_data$longname[which(c2_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
c2_data$longname[which(c2_data$longname=="Democratic People's Republic of Korea")] = "Democratic People’s Republic of Korea"
missing_from_c2 = setdiff(countries$longname,c2_data$longname)
c2__missing_from_countries = setdiff(c2_data$longname,countries$longname)

c3_data <- read.csv("data/birthregP20.csv",fileEncoding="latin1")
if(!"longname" %in% names(c3_data)){setnames(c3_data,"CountryName","longname")}
if(is.factor(c3_data$longname)){c3_data$longname=unfactor(c3_data$longname)}
if(is.factor(c3_data$NP20.Reg)){c3_data$NP20.Reg=unfactor(c3_data$NP20.Reg)}
c3_data$NP20.Reg = gsub("\u0096","",c3_data$NP20.Reg)
c3_data$NP20.Reg[which(c3_data$longname=="Solomon Islands")] <- "87"
c3_data$NP20.Reg = as.numeric(c3_data$NP20.Reg)
c3_data$longname[which(c3_data$longname=="Cabo Verde")] = "Cape Verde"
c3_data$longname[which(c3_data$longname=="Congo")] = "Republic of Congo"
c3_data$longname[which(c3_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c3_data$longname[which(c3_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
c3_data$longname[which(c3_data$longname=="Democratic People's Republic of Korea")] = "Democratic People’s Republic of Korea"
c3_data$longname[which(c3_data$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
missing_from_c3 = setdiff(countries$longname,c3_data$longname)
c3__missing_from_countries = setdiff(c3_data$longname,countries$longname)

c4_data <- read.csv("data/ntl_secondarycompletion.csv")
if(!"longname" %in% names(c4_data)){setnames(c4_data,"CountryName","longname")}
if(is.factor(c4_data$longname)){c4_data$longname=unfactor(c4_data$longname)}
c4_data$longname[which(c4_data$longname=="Cabo Verde")] = "Cape Verde"
c4_data$longname[which(c4_data$longname=="Congo")] = "Republic of Congo"
c4_data$longname[which(c4_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c4_data$longname[which(c4_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
c4_data$longname[which(c4_data$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
c4_data$longname[which(c4_data$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
c4_data$longname[which(c4_data$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
c4_data$longname[which(c4_data$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
c4_data$longname[which(c4_data$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_c4 = setdiff(countries$longname,c4_data$longname)
c4__missing_from_countries = setdiff(c4_data$longname,countries$longname)
c4_data = subset(c4_data,!is.na(Secondary.Completion))
c4_data = c4_data[order(c4_data$longname,-c4_data$Year),]
c4_data$dup = duplicated(c4_data$longname)
c4_data = subset(c4_data,!dup)
c4_data$dup = NULL
c4_data$Year = NULL
c4_data$Secondary.non.completion = 100-c4_data$Secondary.Completion

c5_data <- read.csv("data/P20_secondarycompletion.csv")
if(!"longname" %in% names(c5_data)){setnames(c5_data,"CountryName","longname")}
if(is.factor(c5_data$longname)){c5_data$longname=unfactor(c5_data$longname)}
c5_data$longname[which(c5_data$longname=="Cabo Verde")] = "Cape Verde"
c5_data$longname[which(c5_data$longname=="Congo")] = "Republic of Congo"
c5_data$longname[which(c5_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c5_data$longname[which(c5_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
c5_data$longname[which(c5_data$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
c5_data$longname[which(c5_data$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
c5_data$longname[which(c5_data$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
c5_data$longname[which(c5_data$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
c5_data$longname[which(c5_data$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_c5 = setdiff(countries$longname,c5_data$longname)
c5_missing_from_countries = setdiff(c5_data$longname,countries$longname)
c5_data$Year = NULL

c6_data <- read.csv("data/P20_u5mortality.csv")
if(!"longname" %in% names(c6_data)){setnames(c6_data,"CountryName","longname")}
if(is.factor(c6_data$longname)){c6_data$longname=unfactor(c6_data$longname)}
c6_data$longname[which(c6_data$longname=="Cabo Verde")] = "Cape Verde"
c6_data$longname[which(c6_data$longname=="Congo")] = "Republic of Congo"
c6_data$longname[which(c6_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c6_data$longname[which(c6_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
c6_data$longname[which(c6_data$longname=="Korea, Dem. People’s Rep.")] = "Democratic People’s Republic of Korea"
c6_data$longname[which(c6_data$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
c6_data$longname[which(c6_data$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
c6_data$longname[which(c6_data$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
c6_data$longname[which(c6_data$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_c6_ = setdiff(countries$longname,c6_data$longname)
c6__missing_from_countries = setdiff(c6_data$longname,countries$longname)
c6_data = subset(c6_data,!is.na(U5Mortality.Rest))
c6_data = c6_data[order(c6_data$longname,-c6_data$Year),]
c6_data$dup = duplicated(c6_data$longname)
c6_data = subset(c6_data,!dup)
c6_data$dup = NULL
c6_data$Year = NULL

c7_data <- read.csv("data/obesityrates.csv")
if(!"longname" %in% names(c7_data)){setnames(c7_data,"CountryName","longname")}
if(is.factor(c7_data$longname)){c7_data$longname=unfactor(c7_data$longname)}
c7_data$longname[which(c7_data$longname=="Cabo Verde")] = "Cape Verde"
c7_data$longname[which(c7_data$longname=="Congo")] = "Republic of Congo"
c7_data$longname[which(c7_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c7_data$longname[which(c7_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
c7_data$longname[which(c7_data$longname=="Democratic People's Republic of Korea")] = "Democratic People’s Republic of Korea"
c7_data$longname[which(c7_data$longname=="Libyan Arab Jamahiriya")] = "State of Libya"
c7_data$longname[which(c7_data$longname=="Virgin Islands (U.S.)")] = "United States Virgin Islands"
c7_data$longname[which(c7_data$longname=="aint Vincent and the Grenadines")] = "Saint Vincent and the Grenadines"
c7_data$longname[which(c7_data$longname=="Macao SAR, China")] = "China, Macao Special Administrative Region"
missing_from_obesity = setdiff(countries$longname,c7_data$longname)
obesity_missing_from_countries = setdiff(c7_data$longname,countries$longname)

# test_slugs = c("uganda","usa","united-kingdom")
# for(theslug in test_slugs){
for(theslug in countries$slug){
  country = subset(countries,slug==theslug)[1,]
  lowincome = country$lowincome
  thelongname = country$longname
  message(thelongname)
  ###C1####
  c1_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c1.html")
  c1_data_sub = subset(c1_data,longname==thelongname)
  if(nrow(c1_data_sub)>0){
    if(getmode(c1_data_sub$income.consumption)=="Income"){
      y_axis_lab = "average daily income per person, USD 2011 PPP"
    } else{
      y_axis_lab = "average daily consumption per person, USD 2011 PPP"
    }
    keep = c("Year","longname","national.P20.average","rest.of.population.average")
    c1_data_sub = c1_data_sub[keep]
    names(c1_data_sub) = c("Year","longname","national P20","rest of population")
    c1_data_melt = melt(c1_data_sub,id.vars=c("Year","longname"))
    c1_max = max(c1_data_melt$value)
    c1_year_min = min(c1_data_melt$Year,na.rm=T)
    c1_year_max = max(c1_data_melt$Year,na.rm=T)
    if(!is.na(c1_max)){
      c1 = ggplot(c1_data_melt,aes(x=Year,y=value,group=variable,color=variable)) +
        geom_line(size=line.size) +
        two_tone +
        scale_y_continuous(expand=c(0,0), limits=c(0,c1_max*1.1),label=dollar) +
        expand_limits(x=c(c1_year_min-0.5,c1_year_max+0.5)) +
        labs(y = y_axis_lab, x="",title="\n") +
        simple_style +
        theme(
          legend.position="bottom",
          legend.title=element_blank(),
          axis.line.x = element_line(colour = dark.grey)
        )
      htmlwidgets::saveWidget(ggplotly(c1, tooltip=c("x","y","color")), c1_chart_name, selfcontained=F)
    }
  }
  ###C2####
  c2_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c2.html")
  if(lowincome){
    c2_data_sub = subset(c2_data,longname==thelongname)
    if(nrow(c2_data_sub)>0){
      y_axis_lab = "share of children under 5 measured as stunted"
      keep = c("longname","nationalP20stunting","restP20stunting")
      c2_data_sub = c2_data_sub[keep]
      names(c2_data_sub) = c("longname","national P20","rest of population")
      c2_data_melt = melt(c2_data_sub,id.vars=c("longname"))
      c2_data_melt$value = c2_data_melt$value/100
      c2_max = max(c2_data_melt$value)
      if(!is.na(c2_max)){
        c2 = ggplot(c2_data_melt,aes(x=variable,y=value,fill=variable)) +
          geom_bar(stat="identity") +
          two_tone_fill +
          scale_y_continuous(expand=c(0,0), limits=c(0,c2_max*1.2),label=percent) +
          two_tone +
          labs(y = y_axis_lab, x="") +
          simple_style +
          theme(
            legend.position="none",
            legend.title=element_blank(),
            axis.line.x = element_line(colour = dark.grey)
          )
        htmlwidgets::saveWidget(ggplotly(c2, tooltip=c("x","y")), c2_chart_name, selfcontained=F)
      }
    } 
  }
  ###C3####
  c3_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c3.html")
  if(lowincome){
    c3_data_sub = subset(c3_data,longname==thelongname)
    if(nrow(c3_data_sub)>0){
      y_axis_lab = "share of births registered in children under 5"
      names(c3_data_sub) = c("longname","national P20","rest of population")
      c3_data_melt = melt(c3_data_sub,id.vars=c("longname"))
      c3_data_melt$value = c3_data_melt$value/100
      c3_max = max(c3_data_melt$value)
      if(!is.na(c3_max)){
        c3 = ggplot(c3_data_melt,aes(x=variable,y=value,fill=variable)) +
          geom_bar(stat="identity") +
          two_tone_fill +
          scale_y_continuous(expand=c(0,0), limits=c(0,c3_max*1.2),label=percent) +
          two_tone +
          labs(y = y_axis_lab, x="") +
          simple_style +
          theme(
            legend.position="none",
            legend.title=element_blank(),
            axis.line.x = element_line(colour = dark.grey)
          )
        htmlwidgets::saveWidget(ggplotly(c3, tooltip=c("x","y")), c3_chart_name, selfcontained=F)
      }
    } 
  }
  ###C4####
  c4_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c4.html")
  c4_data_sub = subset(c4_data,longname==thelongname)
  if(nrow(c4_data_sub)>0){
    names(c4_data_sub) = c("longname","completed \nsecondary education","not completed \nsecondary education")
    c4_data_melt = melt(c4_data_sub,id.vars=c("longname"))
    if(!is.na(max(c4_data_melt$value,na.rm=T))){
      c4_data_melt$value = c4_data_melt$value/100
      c4_max = max(c4_data_melt$value)
      c4 = ggplot(c4_data_melt,aes(x=variable,y=value,fill=variable)) +
        geom_bar(stat="identity") +
        two_tone_fill +
        scale_y_continuous(expand=c(0,0), limits=c(0,c4_max*1.2),label=percent) +
        two_tone +
        labs(y = y_axis_lab, x="") +
        simple_style +
        theme(
          legend.position="none",
          legend.title=element_blank(),
          axis.line.x = element_line(colour = dark.grey)
        )
      htmlwidgets::saveWidget(ggplotly(c4, tooltip=c("x","y")), c4_chart_name, selfcontained=F)
    }
  }
  ###C5####
  c5_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c5.html")
  if(lowincome){
    c5_data_sub = subset(c5_data,longname==thelongname)
    if(nrow(c5_data_sub)>0){
      y_axis_lab = "% who have completed secondary school"
      names(c5_data_sub) = c("longname","national P20","rest of population")
      c5_data_melt = melt(c5_data_sub,id.vars=c("longname"))
      c5_data_melt$value = c5_data_melt$value/100
      c5_max = max(c5_data_melt$value)
      if(!is.na(c5_max)){
        c5 = ggplot(c5_data_melt,aes(x=variable,y=value,fill=variable)) +
          geom_bar(stat="identity") +
          two_tone_fill +
          scale_y_continuous(expand=c(0,0), limits=c(0,c5_max*1.2),label=percent) +
          two_tone +
          labs(y = y_axis_lab, x="") +
          simple_style +
          theme(
            legend.position="none",
            legend.title=element_blank(),
            axis.line.x = element_line(colour = dark.grey)
          )
        htmlwidgets::saveWidget(ggplotly(c5, tooltip=c("x","y")), c5_chart_name, selfcontained=F)
      }
    } 
  }
  ###C6####
  c6_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c6.html")
  if(lowincome){
    c6_data_sub = subset(c6_data,longname==thelongname)
    if(nrow(c6_data_sub)>0){
      y_axis_lab = "under 5 mortality rate per 1,000 live births"
      names(c6_data_sub) = c("longname","national P20","rest of population")
      c6_data_melt = melt(c6_data_sub,id.vars=c("longname"))
      c6_data_melt$value = c6_data_melt$value/100
      c6_max = max(c6_data_melt$value)
      if(!is.na(c6_max)){
        c6 = ggplot(c6_data_melt,aes(x=variable,y=value,fill=variable)) +
          geom_bar(stat="identity") +
          two_tone_fill +
          scale_y_continuous(expand=c(0,0), limits=c(0,c6_max*1.2),label=percent) +
          two_tone +
          labs(y = y_axis_lab, x="") +
          simple_style +
          theme(
            legend.position="none",
            legend.title=element_blank(),
            axis.line.x = element_line(colour = dark.grey)
          )
        htmlwidgets::saveWidget(ggplotly(c6, tooltip=c("x","y")), c6_chart_name, selfcontained=F)
      }
    } 
  }
  ###C7####
  c7_chart_name = paste0(getwd(),"/charts_interactive/",theslug,"_c7.html")
  if(!lowincome){
    c7_data_sub = subset(c7_data,longname==thelongname)
    if(nrow(c7_data_sub)>0){
      y_axis_lab = "% population who are obese"
      c7_data_melt = melt(c7_data_sub,id.vars=c("Year","longname"))
      c7_max = max(c7_data_melt$value)
      c7_year_min = min(c7_data_melt$Year,na.rm=T)
      c7_year_max = max(c7_data_melt$Year,na.rm=T)
      if(!is.na(c7_max)){
        c7 = ggplot(c7_data_melt,aes(x=Year,y=value,group=variable,color=variable)) +
          geom_line(size=line.size) +
          two_tone +
          scale_y_continuous(expand=c(0,0), limits=c(0,c7_max*1.1),label=percent) +
          expand_limits(x=c(c7_year_min-0.5,c7_year_max+0.5)) +
          labs(y = y_axis_lab, x="") +
          simple_style +
          theme(
            legend.position="none",
            legend.title=element_blank(),
            axis.line.x = element_line(colour = dark.grey)
          )
        htmlwidgets::saveWidget(ggplotly(c7, tooltip=c("x","y","color")), c7_chart_name, selfcontained=F)
      }
    }
  }
}
