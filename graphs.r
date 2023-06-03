library(dplyr)
library(ggplot2)
library(reshape2)
gdp_df <-read.csv("GDP_countries_yr.csv")
releases_df <- read.csv("netflix_countries_yr.csv")
new_gdp_df<-gdp_df[,c("GDP_Growth_per_yr","United.States","Canada","South.Korea","Germany","United.Kingdom","India")]
for(i in 1:ncol(new_gdp_df)){
  new_gdp_df[,i]<-as.numeric(new_gdp_df[,i])
}
d <- melt(new_gdp_df,id.vars="GDP_Growth_per_yr")
line <-ggplot(data=d, aes(x=GDP_Growth_per_yr,y=value,col=variable)) +
  geom_line()
print(line)
new_releases_df<-releases_df[,c("release_year","United.States","Canada","South.Korea","Germany","United.Kingdom","India")]
for(i in 1:ncol(new_releases_df)){
  new_releases_df[,i]<-as.numeric(new_releases_df[,i])
}
d_2 <- melt(new_releases_df,id.vars="release_year")
line_2 <-ggplot(data=d_2, aes(x=release_year,y=value,col=variable)) +
  geom_line()
print(line_2)