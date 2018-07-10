list.of.packages <- c("data.table","varhandle","reshape2","ggplot2","scales")
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

setwd("~/git/p20-profiles-2018")

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

for(theslug in countries$slug){
  country = subset(countries,slug==theslug)[1,]
  lowincome = country$lowincome
  if(lowincome){
    for(i in c(1:6)){
      chart_name = paste0("charts/",theslug,"_c",i,".png")
      file.copy("final_template/no_data_text.png",chart_name,overwrite=T)
    }
  }else{
    for(i in c(1,4,7)){
      chart_name = paste0("charts/",theslug,"_c",i,".png")
      file.copy("final_template/no_data_text.png",chart_name,overwrite=T)
    }
  }
}

c1_data = read.csv("data/IncomeGapGraph.csv")
if(!"longname" %in% names(c1_data)){setnames(c1_data,"Country","longname")}
if(is.factor(c1_data$longname)){c1_data$longname=unfactor(c1_data$longname)}
c1_data$longname[which(c1_data$longname=="Cabo Verde")] = "Cape Verde"
c1_data$longname[which(c1_data$longname=="Congo")] = "Republic of Congo"
c1_data$longname[which(c1_data$longname=="Cote d'Ivoire")] = "Côte d’Ivoire"
c1_data$longname[which(c1_data$longname=="Lao People's Democratic Republic")] = "Lao People’s Democratic Republic"
missing_from_c1 = setdiff(countries$longname,c1_data$longname)
c1_missing_from_countries = setdiff(c1_data$longname,countries$longname)

for(theslug in countries$slug){
  country = subset(countries,slug==theslug)[1,]
  lowincome = country$lowincome
  thelongname = country$longname
  message(thelongname)
  c1_chart_name = paste0("charts/",theslug,"_c1.png")
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
    c1 = ggplot(c1_data_melt,aes(x=Year,y=value,group=variable,color=variable)) +
      geom_line(size=line.size) +
      two_tone +
      scale_y_continuous(expand=c(0,0), limits=c(0,c1_max*1.1),label=dollar) +
      expand_limits(x=c(c1_year_min-0.5,c1_year_max+0.5)) +
      geom_vline(xintercept=c(c1_year_min,1999,c1_year_max),linetype="dotted") +
      geom_text(
        data=subset(c1_data_melt,Year %in% c(c1_year_min,1999,c1_year_max) & variable=="national P20")
        ,aes(label=dollar_format(largest_with_cents=10)(value),y=c1_max)
        ,size=4
        ,hjust=1.1
        ,show.legend=F
        ) +
      geom_text(
        data=subset(c1_data_melt,Year %in% c(c1_year_min,1999,c1_year_max) & variable=="rest of population")
        ,aes(label=dollar_format(largest_with_cents=10)(value),y=c1_max)
        ,size=4
        ,hjust=-0.2
        ,show.legend=F
      ) +
      labs(y = y_axis_lab, x="",title="\n") +
      simple_style +
      theme(
        legend.position="bottom",
        legend.title=element_blank(),
        axis.line.x = element_line(colour = dark.grey),
        )
    ggsave(c1_chart_name,c1,units="in",dpi=dpi,width=(1000/dpi),height=(770/dpi),scale=3)
  }
}
