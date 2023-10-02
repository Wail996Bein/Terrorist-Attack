install.packages('ggplot2') 
install.packages('ggthemes')
install.packages('xlsx')
install.packages('rworldmap')

library(ggplot2)
library(dplyr)
library(ggthemes)
library(xlsx)
library(reshape2)
library(rworldmap)

getwd()
setwd("D:\\Projects\\Terrorist Attack")
terrorism <- read.csv('terrorism.csv', stringsAsFactors = F)

dim(terrorism)
str(terrorism)

terrorism <- terrorism[,c("iyear","imonth", "iday", "country_txt", "region_txt", "provstate", "city", "latitude", "longitude", "attacktype1_txt", "targtype1_txt", "corp1", "target1", "natlty1_txt", "gname", "weaptype1_txt", "weapsubtype1_txt")]

by_year <- terrorism %>% group_by(iyear) %>% 
  summarise(n=n())
ggplot(aes(x = iyear, y = n), data = by_year) +
  geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen") +
  geom_point(size = 0.5) + xlab("Year") + ylab("Number of terrorist Attacks") +
  ggtitle("Terrorist Attacks Worldwide by Year 1970-2015") + theme_fivethirtyeight()

by_region <- terrorism %>% group_by(region_txt, iyear) %>% 
  summarise(n=n())
ggplot(by_region, aes(x = iyear, y = n, colour = region_txt)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~region_txt) + xlab('Year') +
  ggtitle('Terrorist Attacks by Region and Year 1970-2015') + 
  theme(legend.position="none")

by_region_no_year <- terrorism %>% group_by(region_txt) %>% 
  summarise(n=n())
ggplot(aes(x=reorder(region_txt, n), y=n), data=by_region_no_year) +
  geom_bar(stat = 'identity') +
  ggtitle('Terrorist Attacks by Region 1970-2015') + coord_flip() + theme_fivethirtyeight()

by_country <- terrorism %>% group_by(country_txt) %>% 
  summarise(n=n())
by_country <- arrange(by_country, desc(n))
top10 <- head(by_country, 10)
top10
ggplot(aes(x=reorder(country_txt, n), y=n), data=top10) + 
  geom_bar(stat = 'identity') + xlab('Country') + ylab('Number of Terrorist Attacks') + ggtitle('Countries with the most terrorist attacks, 1970-2015') +
  coord_flip() + theme_fivethirtyeight()

by_attacktype <- terrorism %>% group_by(attacktype1_txt) %>% 
  summarise(n=n())
ggplot(aes(x=reorder(attacktype1_txt, n), y=n), data=by_attacktype) + 
  geom_bar(stat = 'identity') + xlab('Attack Type') + ylab('Number of Attacks') + ggtitle('Terrorist Attack Tactics Wordwide, 1970-2015') + coord_flip() +
  theme_fivethirtyeight()

by_weapon <- terrorism %>% group_by(weaptype1_txt) %>% 
  summarise(n=n())
ggplot(aes(x=reorder(weaptype1_txt, n), y=n), data=by_weapon) + 
  geom_bar(stat = 'identity') + xlab('Weapon') + ylab('Number of Attacks') + ggtitle('Terrorist Attack By Weapon Wordwide, 1970-2015') + coord_flip() +
  theme_fivethirtyeight()

casualties <- read.xlsx('casualties.xlsx', sheetIndex = 1, header = TRUE, stringsAsFactors = F)
casualties <- melt(casualties, id="month")
casualties<-casualties[!casualties$month=="Total",]

casualties$month <- ordered(casualties$month, levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
ggplot(aes(x=month, y=value, fill=variable), data=casualties) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  ggtitle('Terrorist attacks and casualties worldwide by month, 2015') +
  theme_fivethirtyeight()

group <- read.xlsx('group.xlsx', sheetIndex = 1, header=T, stringsAsFactors=F)
group <- melt(group, id = 'group_name')
group[5, 1] = "Kurdistan Workers' Party"
group[10, 1] = "Kurdistan Workers' Party"
group[15, 1] = "Kurdistan Workers' Party"
group[20, 1] = "Kurdistan Workers' Party"
ggplot(aes(x=group_name, y=value, fill=variable), data=group) +
  geom_bar(stat = 'identity', position = position_dodge()) + ggtitle('Terrorist Groups with the most Attacks Wordwide, 2015') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

attack2015 <- terrorism[terrorism$iyear==2015, ]
by_target <- attack2015 %>% group_by(targtype1_txt) %>% 
  summarise(n=n())
by_target <- arrange(by_target, desc(n))
by_target
ggplot(aes(x=reorder(targtype1_txt, n), y=n), data=by_target) +
  geom_bar(stat = 'identity') + ggtitle('Terrorist Attack Targets/Victims, 2015') +
  coord_flip() + theme_fivethirtyeight()

attack2015_by_city <- attack2015 %>% group_by(country_txt, city) %>% 
  summarise(n=n())
attack2015_by_city <- arrange(attack2015_by_city, desc(n))
top10_city_2015 <- head(attack2015_by_city, 20)
top10_city_2015


baghdad <- terrorism[terrorism$city=='Baghdad', ]
baghdad_year <- baghdad %>% group_by(iyear) %>% 
  summarise(n=n())
ggplot(aes(x = iyear, y = n), data = baghdad_year) +
  geom_line(size = 2.5, alpha = 0.7, color = "mediumseagreen") +
  geom_point(size = 0.5) + xlab("Year") + ylab("Number of terrorist Attacks") +
  ggtitle("Terrorist Attacks in Baghdad by Year 1970-2015") + theme_fivethirtyeight()


baghdad_type <- baghdad %>% group_by(attacktype1_txt, iyear) %>% 
  summarise(n=n())
ggplot(aes(x=iyear, y=n, fill=attacktype1_txt), data=baghdad_type) + 
  geom_bar(stat = 'identity') +
  ggtitle('Attack Type in Baghdad') + theme_fivethirtyeight()


gtd <- read.csv("terrorism.csv")
gtd2015 <- gtd[gtd$iyear==2015, ]
gtd2015 <- aggregate(nkill~country_txt,gtd2015,sum)

gtdMap <- joinCountryData2Map( gtd2015, 
                               nameJoinColumn="country_txt", 
                               joinCode="NAME" )

mapDevice('x11')
mapCountryData( gtdMap, 
                nameColumnToPlot='nkill', 
                catMethod='fixedWidth', 
                numCats=100 )
