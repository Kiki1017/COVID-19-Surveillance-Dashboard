library(BreakoutDetection)
library(RCurl)
library(tidyverse)

#### get data
x2 <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data2 <- read.csv(text = x2)
data2$Country.Region<-as.character(data2$Country.Region)
data2$Province.State<-as.character(data2$Province.State)

### get country list
countrylist<-as.character(unique(data2$Country.Region))

breakout<-function(n.break=7, country="US"){
data_break<-data2

out.break<-data_break %>%
  as.tbl() %>%
  group_by(Country.Region) %>%
  summarise_at(names(data_break[,c(5:ncol(data_break))]), sum, na.rm=T)  

#### Get country list
#### subselector - make this a function with shiny
plotme3<-out.break[out.break$Country.Region==country,]

plotme4<-data.frame(diff(t(plotme3[,-1])))
plotme4$ds<-gsub("X", "", row.names(plotme4))
plotme4$ds<-gsub("\\.", "-", plotme4$ds)
plotme4$ds<-as.Date(plotme4$ds, format="%m-%d-%y")
plotme4$ds<-as.POSIXct(plotme4$ds)
plotme4<-plotme4[,c(2,1)]
names(plotme4)<-c("timestamp", "count")
row.names(plotme4)<-seq(1,nrow(plotme4))
plotme4$count<-ifelse(plotme4$count<0,0, plotme4$count)

total.break<-sum(plotme4$count)

first<-min(which(plotme4$count != 0))
last<-nrow(plotme4)

data_break<-data.frame(plotme4[first:last,])
plotme4<-data.frame(plotme4[first:last,])



## run breakout algorightm
res.break = BreakoutDetection::breakout(data_break, min.size=n.break, method='multi', beta=.001, degree=1, plot=TRUE)
plotme4$timestamp<-as.Date(plotme4$timestamp)



## create mean segments
segs<-c(0, res.break$loc, nrow(data_break))
means<-vector("list", length(segs))
group<-NULL
for(i in 1:(length(segs)-1)){
  group<-c(group,rep(i, times=diff(segs)[i]))
}
data_break$group<-factor(group)
groupmean<-tapply(data_break$count, data_break$group, function(x) mean(x))

segment_data = data.frame(
  x = segs[2:length(segs)-1],
  xend = segs[2:length(segs)], 
  y = groupmean,
  yend = groupmean
)
segment_data$x <- ifelse(segment_data$x==0, 1, segment_data$x)
startdate<-as.Date(unlist(lapply(segment_data$x, function(x) plotme4$timestamp[x])), origin="1970-01-01")
enddate<-c(startdate[2:length(startdate)], max(plotme4$timestamp))



#### clean up the plot
plotz.break<-ggplot(plotme4) +
  geom_line(aes(x=timestamp, y=count), color="gray35", size=1) +
  geom_segment(data = segment_data, aes(x = startdate, y = y, xend = enddate, yend = yend), lty=3, color="darkred", lwd=1.25, show.legend = T) + 
  scale_x_date(date_breaks="1 week", expand = c(0.05, 0.05), name="") +
  coord_cartesian(ylim=c(0, (max(plotme4$count) + (10 - max(plotme4$count) %% 10)))) +
  scale_y_continuous(n.breaks = 10, expand = c(0.05, 0.05) ) +
  ylab("Number of Deaths") +
  annotate("text", label=paste0("Total deaths = ", total.break), x=(min(plotme4$timestamp)+20), y=max(plotme4$count)+2) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color="lightgray"),
        panel.background = element_blank(),
        axis.line = element_line(color="black"),
        legend.position="bottom")

  
  
return(plotz.break)
}


