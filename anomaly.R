library(forecast)
library(RCurl)
library(tidyverse)
library(anomalize)


##### read data and fix
x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data <- read.csv(text = x)
data$Country.Region<-as.character(data$Country.Region)
data$Province.State<-as.character(data$Province.State)

#### list for dropdown
countrylist<-sort(as.character(unique(data$Country.Region)))

#### begin function
anomaly.covid.mort<-function(alpha=0.05, max_anoms=0.2, country="US"){

#### create summary stats by day
out<-data %>%
  as.tbl() %>%
  group_by(Country.Region) %>%
  summarise_at(names(data[,c(5:ncol(data))]), sum, na.rm=T)  

#### subselector - make this a function with shiny
plotme<-out[out$Country.Region==country,]

#### create the lag, make actual date column, and organize dataset
plotme2<-data.frame(diff(t(plotme[,-1])))
plotme2$ds<-gsub("X", "", row.names(plotme2))
plotme2$ds<-gsub("\\.", "-", plotme2$ds)
plotme2$ds<-as.Date(plotme2$ds, format="%m-%d-%y")
plotme2<-plotme2[,c(2,1)]
names(plotme2)<-c("ds", "y")
row.names(plotme2)<-seq(1,nrow(plotme2))
plotme2$y<-ifelse(plotme2$y<0,0, plotme2$y)

#### total for plot text
total<-sum(plotme2$y)

#### select from first positive case to currently avaiable date
first<-min(which(plotme2$y != 0))
last<-nrow(plotme2)
plotme2<-plotme2[first:last,]

#### anomaly
plotz<-as_tibble(plotme2)
  x<-time_decompose(data=plotz, target=y,  frequency="1 week", trend="auto", method="stl")
  z<-anomalize(data=x, target =remainder, alpha = alpha, max_anoms = max_anoms, method="gesd")
  a<-time_recompose(z)
plotz<-plot_anomalies(a, time_recomposed=T, alpha_dots=0.5, fill_ribbon = "lightblue", alpha_ribbon = 0.001)

#### make the plot look nice
plotz2<-plotz + geom_line(color="gray35", size=0.7) + geom_point(color=ifelse(plotz$data$anomaly=="Yes", "red", "gray30"), pch=21, fill=ifelse(plotz$data$anomaly=="Yes", NA, "gray"), size=ifelse(plotz$data$anomaly=="Yes", 4, 2), stroke=1) +
  scale_x_date(date_breaks="1 week", expand = c(0.05, 0.05), name="") +
  scale_y_continuous(n.breaks = 10, expand = c(0.05, 0.05) ) +
  ylab("Number of Deaths") +
  coord_cartesian(ylim=c(0, (max(plotz$data$recomposed_l2) + (10 - max(plotz$data$recomposed_l2) %% 10)))) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="bottom") +
  annotate("text", label=paste0("Total deaths = ", total), x=(min(plotme2$ds)+10), y=max(plotme2$y)+2)

return(plotz2)

}


