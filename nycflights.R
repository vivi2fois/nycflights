library(RMySQL)
library(dbConnect)
library(tidyverse)
library(DBI)
library(leaflet)
library(geosphere)
library(gridExtra)
library(maps)
library(geosphere)
#con = dbConnect(MySQL(), user='root', password='root', dbname='nycflights', host='127.0.0.1')
#myQuery <- "Select * from planes;"
#df <- dbGetQuery(con, myQuery)
#View(df)

con <- DBI::dbConnect(RMySQL::MySQL(),user='root', password='root', dbname='nycflights', host='127.0.0.1')



# requetes 
#Question 2 

# Relation entre les tables 
vhd <- inner_join(airports,flights, by = c("faa" = "origin"))
cn <- inner_join(airlines,flights, by = "carrier")

# Préparation du DataFrame
vol_hd <- vhd %>% select(faa,name,carrier,dest,tailnum,dst,month)
company_names <- cn %>% select(name)
company_names <- rename(company_names, company_names = name)
vol_company <- dplyr::bind_cols(vol_hd, company_names)

total_company <- vol_company %>% filter(month > 4) %>% 
  group_by() %>% summarise(Count=n())


#Question.3  a) L'Aéroport le plus emprunté 
dep_origin <- vhd %>% group_by(faa,name) %>% summarise(Count=n()) %>% head(1)
dep_origin

#Question.3  b) Les 10 Aéroports les plus empruntés
aero_plus <- vhd %>% group_by(dest) %>% summarise(Count=n())
aero_plus <- aero_plus %>% arrange(desc(Count)) %>%  head(10)
aero_plus

#Qestion.3 b.1) Les 10 avions qui ont le plus décollé
dep_planes <-  vhd %>% group_by(dep_delay,tailnum) %>% summarise(Count=n())
dep_planes <- dep_planes %>% arrange(Count) %>% head(10)
dep_planes

#Question.4 Combien chaque compagnie a desservi de destination 

# DISTINCT
ce <- left_join(airlines,flights, by = "carrier")
ce <- ce %>% select(origin,dest,name,carrier) 
company_origin <- ce %>% group_by(origin,dest,name) %>% distinct() %>% 
  summarise(total_vol = n()) 


co <- left_join(airlines,flights, by = "carrier")
co <- co %>% select(origin,dest,name,carrier) 
company_origin <- co %>% group_by(origin,dest,name) %>% 
  summarise(total_vol = n()) 
company_origin <- company_origin %>% arrange(desc(total_vol)) %>% head(20)

company_o <- co %>% group_by(origin) %>% summarise(nb_dest=n())

graph_company_origin <- ggplot(company_origin)+aes(x=dest,y=total_vol,color=origin)+geom_point()
graph_company_origin 
plotly::ggplotly(graph_company_origin)

#Question.5 Trier par ordre alphabétique, origins, compagnies, destinations
alpha <- left_join(airlines,flights, by = "carrier")
alpha <- alpha %>% select(origin,dest,name) %>% arrange(dest)

#Qestions.6 pas compris ?



#Question.7 ? regrouper 2 jointures 
brt <- inner_join(airports,flights, by = c("faa" = "origin"))
brt1 <- inner_join(airports,flights, by = c("faa" = "dest"))


#Question.9 Filtrer le vol pour trouver ceux exploités par United, American ou Delta
bet <- left_join(airlines,flights, by = "carrier")
r <- filter(bet, carrier %in% c("DL", "UA", "AA")) %>% group_by() %>% summarise(total_ca=n())

#Question.9 Tous les vols ayant atterri à Houston (IAH ou HOU)
fil <- filter(bet, dest %in% c("HOU","IAH")) %>% group_by(dest) %>% summarise(total_dest=n())
#Question.9 Les vols partant de NYC airports vers Seattle
se <- filter(bet, origin %in% c("JFK","EWR","LGA"), dest == "SEA") %>% group_by(dest) %>% summarise(total_dest=n())
#Qestion.9 combien de compagnies desservent cette destination
comp <- filter(bet, origin %in% c("JFK","EWR","LGA"),dest == "SEA") %>% group_by(name) %>% summarise(total_dest=n())
#Question.9 combien d’avions “uniques” 
avions <- filter(bet, origin %in% c("JFK","EWR","LGA"),dest == "SEA") %>% group_by(tailnum) %>% summarise(total_dest=n())



#graph_month
m_flights <- dplyr::bind_cols(m_flights, month_name)
m_flights <- flights %>% group_by(month) %>% summarise(Count=n())
#view(m_flights)
moy_mensuelle <- mean(m_flights$Count, na.rm = TRUE)



graph_month1 <- ggplot(m_flights, aes(x=month, y=Count)) + 
  geom_line(color="red") + geom_point(color="red") + 
  geom_hline(yintercept = moy_mensuelle, color="blue") + 
  scale_x_discrete(limits=month.abb) + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"))
  
  

graph_month1





#graph_month <- ggplot(m_flights, aes(y=Count, x=month)) +
#graph_month + geom_point()+
#facet_wrap( Count ~ month)+
#geom_hline(yintercept = 28064.67) + 
#scale_x_discrete(limits=c("jan","fev","mar","abr","may","jun","jul","aug","sep","oct","nov","dec"))


#cartograhie usa
# cluster airports


data_airports <- airports %>% select(name,lon,lat,tzone)
#View(data_airports)

# Répartition des aéroports sur la carte des USA
usa_map_airports <- leaflet(data_airports) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lng = ~lon, lat = ~lat, popup = ~name)
usa_map_airports



# Top 3 des vols les plus fréquentés origin flights
#gps <- data.frame(lat = c(40.6895314,40.6413111,40.7769271),lon = c(-74.1744624,-73.7781391,-73.8739659))
o_flights <- flights %>% group_by(origin) %>% summarise(Count=n())
airports_top <- cbind(o_flights, gps)

airports_top

num <- as.numeric(c(6000,4000,3000))

airports_ny <- leaflet(airports_top) %>% addTiles() %>%
  addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = ~num, popup = ~c(origin))

airports_ny

# Les vols durant les grandes vacances 

# vacances d'hiver, winter holidays ( novembre + décembre)
# mois de novembre
november <- filter(flights, month == 11)
nov_holydays <- november %>% group_by(month) %>% summarise(volnov=n())

#mois de décembre
december <- filter(flights, month == 12)
dec_holidays <- december %>% group_by(month) %>% summarise(volnov=n())

# Winter holidays ( novembre + décembre)
winter_holidays <- dplyr::bind_rows(nov_holydays,dec_holidays)
 #format(ISOdate(2000, 1:12, 1), "%B")[11]
 #format(ISOdate(2000, 1:12, 1), "%B")[12]
name_month<- data.frame(
  name_month = c("novembre","décembre")
)

winter_holidays <- dplyr::bind_cols(winter_holidays,name_month)

winter <- ggplot(winter_holidays, aes(x=name_month,y=volnov))+
  geom_bar(stat="identity",width = 0.2,color="green",fill="yellow")+
  geom_text(aes(label=volnov), vjust=1.5, color="black", size=3)+
  theme_minimal()

winter

#vacances d'été, summer holidays (juillet + août + septembre)
# mois de juillet
july <- filter(flights, month == 7)
july_holydays <- july %>% group_by(month) %>% summarise(volnov=n())

# mois d'août 
august <- filter(flights, month == 8)
august_holydays <- august %>% group_by(month) %>% summarise(volnov=n())

# mois de septembre 
septembre <- filter(flights, month == 9)
septembre_holydays <- septembre %>% group_by(month) %>% summarise(volnov=n())

#Summer holidays (juillet,aout,septembre)

summer_holidays <- dplyr::bind_rows(july_holydays,august_holydays,septembre_holydays)

name_month<- data.frame(
name_month = c("juillet","août","septembre")
)

summer_holidays <- dplyr::bind_cols(summer_holidays,name_month)

summer <- ggplot(summer_holidays, aes(x=name_month,y=volnov))+
  geom_bar(stat="identity",width = 0.2,color="blue",fill="blue")+
  geom_text(aes(label=volnov), vjust=1.5, color="black", size=3)+
  theme_minimal()

summer

# Regroupement de winter + summer 
# facet_grind

grid.arrange(summer,winter,nrow=2)


# Les jours clés de l'année 

# concaténation year,month,day => date

full_date <- flights %>% select(year, month, day)
# for loop
for (col in 1:ncol(full_date)) {
  full_date[[col]] <- as.character(full_date[[col]])
} 
# Alternative à une boucle
apply(full_date, 2, as.character)

# traitement manuelle
full_date$year <- as.character(full_date$year)
full_date$month <- as.character(full_date$month)
full_date$day <- as.character(full_date$day)
#View(full_date)


#flights <- dplyr::bind_cols(flights,full_date)

# nouvel an 
ny <- filter(flights, month == 1, day == 1)
new_year <- ny %>% group_by(month,day) %>% summarise(volnov=n())

# Chrismas day 
cd <- filter(flights, month == 12, day == 25)
chrismas <- cd %>% group_by(month,day) %>% summarise(volnov=n())

# independance day 
id <- filter(flights, month == 7, day == 4)
independance <- id %>% group_by(month,day) %>% summarise(volnov=n())

# thanksgiving 
tg <- filter(flights, month == 11, day == 29)
thanks <- tg %>% group_by(month,day) %>% summarise(volnov=n())

# Les jours clés


jour_cles <- dplyr::bind_rows(new_year,chrismas,independance,thanks)

evenement <- data.frame(
evenement = c("nouvel_an","noel","fete_nationale","thanksgiving")
)

jour_cles <- dplyr::bind_cols(jour_cles,evenement)

special_day <- ggplot(jour_cles, aes(x=evenement,y=volnov))+
  geom_bar(stat="identity",width = 0.2,color="purple",fill="purple")+
  geom_text(aes(label=volnov), vjust=-1, color="black", size=4)+
  theme_minimal()

special_day

# départ entre minuit et 6h du matin inclus
dv <- filter(flights, hour >= 0 & hour <= 6)
dep_vol <- dv %>% group_by(hour) %>% summarise(volnov=n())

dep_hour <- ggplot(dep_vol, aes(x=hour,y=volnov))+
  geom_bar(stat="identity",width = 0.2,color="purple",fill="purple")+
  geom_text(aes(label=volnov), vjust=-1, color="black", size=4)+
  theme_minimal()

dep_hour





# Les 5 vols les plus retardés au départ 
# data frame inner join de airports et flights
af_dep <- inner_join(airports,flights, by = c("faa" = "origin"))
# récupérer les informations dans le data frame af_dep
origin_delay <- af_dep %>% select(name,lon,lat,faa,dest,dep_delay,year,month,day,sched_dep_time,sched_arr_time)

# récupérer les 5 vols les plus retardés au départ avec dep_delay
dep_delay_flights <- origin_delay %>% arrange(desc(dep_delay)) %>% head(5)

# Graphique des 5 vols plus retardés au décollage

# ajouter une table a_dest : colonne lon,lat,path..dans dep_delay_flights
a_dest <- data.frame(lat = c(21.294932,39.99800,41.948990,37.615413,39.054485),lon = c(-157.979622,-82.89190,-87.966328,-122.391129,-84.674034),path = c("A","B","C","D","E"))

dep_delay_flights <- dplyr::bind_cols(dep_delay_flights,a_dest)

icon_dep <- iconList(avion = makeIcon("/home/olivier/Rprojet/dep_plane.png","/home/olivier/Rprojet/dep_plane@2x.png", 35, 35) )

graph_dep_delay <- leaflet(dep_delay_flights) %>% addTiles() %>% 
  addPolylines(lat=c(40.63975,21.29493), lng = c(-73.77893,-157.97962),color="purple") %>% 
  addPolylines(lat=c(40.63975,39.99800), lng = c(-73.77893,-82.89190),color="purple") %>% 
  addPolylines(lat=c(40.69250,41.94899), lng = c(-74.16867,-87.96633),color="red") %>% 
  addPolylines(lat=c(40.63975,37.61541), lng = c(-73.77893,-122.39113),color="purple") %>% 
  addPolylines(lat=c(40.63975,39.05448), lng = c(-73.77893,-84.67403),color="purple") %>% 

  
  addCircles(lng = ~lon1, lat = ~lat1, weight = 1,
             radius = 6000,color = "red") %>% 
  addMarkers(icon = icon_dep)

 

graph_dep_delay

# Les 5 vols les plus retardés à l'arrivé
# data frame inner join de airports et flights
af_arr <- inner_join(airports,flights, by = c("faa" = "dest"))
# récupérer les informations dans le data frame af_arr
arr_delay <- af_arr %>% select(name,lon,lat,faa,origin,arr_delay,year,month,day,sched_dep_time,sched_arr_time)

# récupérer les 5 vols les plus retardés à l'arrivé avec arr_delay
arr_delay_flights <- arr_delay %>% arrange(desc(arr_delay)) %>% head(5)

# Graphique des 5 vols plus retardés à l'attérissage

# ajouter une table a_arr : colonne lon,lat,path..dans arr_delay_flights
a_arr <- data.frame(lat = c(21.294932,39.99800,41.948990,37.615413,39.054485),lon = c(-157.979622,-82.89190,-87.966328,-122.391129,-84.674034),path = c("A","B","C","D","E"))

arr_delay_flights <- dplyr::bind_cols(arr_delay_flights,a_arr)

icon_arr <- iconList(avion = makeIcon("/home/olivier/Rprojet/arr_plane.png","/home/olivier/Rprojet/arr_plane@2x.png", 35, 35) )

graph_arr_delay <- leaflet(arr_delay_flights) %>% addTiles() %>% 
  addPolylines(lat=c(40.63975,21.29493), lng = c(-73.77893,-157.97962),color="purple") %>% 
  addPolylines(lat=c(40.63975,39.99800), lng = c(-73.77893,-82.89190),color="purple") %>% 
  addPolylines(lat=c(40.69250,41.94899), lng = c(-74.16867,-87.96633),color="red") %>% 
  addPolylines(lat=c(40.63975,37.61541), lng = c(-73.77893,-122.39113),color="purple") %>% 
  addPolylines(lat=c(40.63975,39.05448), lng = c(-73.77893,-84.67403),color="purple") %>% 
  
  addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = 6000,color = "yellow") %>% 
  addCircles(lng = ~lon1, lat = ~lat1, weight = 1,
             radius = 6000,color = "red") %>% 
  addMarkers(icon = icon_arr)

graph_arr_delay

# Calcule du retard moyen au décollage sur l'ensemble des vols en minutes
moy_dep_retard <- mean(flights$dep_delay, na.rm = TRUE)

# Calcule du retard moyen journalier au décollage 
total_dep_delay <- sum(flights$dep_delay, na.rm = TRUE)
day_dep_delay <- sum(flights$day, na.rm = TRUE) 

moy_dep_jour <- total_dep_delay / day_dep_delay * 60
# 0.78 * 60min soit environs 47 minutes

# Les vols qui sont arrivés avec plus de 2h de retard 
delay_flights_arr <- filter(flights, dep_delay == 0 & arr_delay > 120)
delay_flights_arr <- arrange(delay_flights_arr,desc(arr_delay))
#TOTAL
delay_flights_arr <- group_by(delay_flights_arr) %>% summarise(volnov=n())

# les vols qui n'ont pas été retardé de plus de 2h ni au décollage ni à l'attérissage
no_delay <- filter(flights, dep_delay < 120 & arr_delay < 120)
#TOTAL
no_delay <- group_by(no_delay) %>% summarise(volnov=n())


# les vos qui ont décollés plus tôt que prévu 
advanced_flights <- filter(flights, dep_delay < 0)
#TOTAL
advanced_flights <- group_by(advanced_flights) %>% summarise(volnov=n())

# Les vols qui sont partis avec une heure de retard ou plus et qui ont rattrapé plus de 30 minutes
delay_ratt <- filter(flights, dep_delay >= 60 & arr_delay < 30)
#TOTAL
delay_ratt <- group_by(delay_ratt) %>% summarise(volnov=n())

# Calcule gain à faire corriger ?  
dep <- sum(flights$dep_delay, na.rm = TRUE)
arr <- sum(flights$arr_delay, na.rm = TRUE)
hours <- sum(flights$hour, na.rm = TRUE)

gain_c <- dep - arr
total_gain_c <- gain_c / hours

total_gain_c <- total_gain_c * 60

# gain entre le départ et arrivé en minutes
flights <- mutate(flights,
                  gain = arr_delay - dep_delay,
                  gain_per_hour = gain / 60,
                  hours = air_time / 60)
select(flights,gain,gain_per_hour,hours)



#speed et distance 
flights <- mutate(flights, 
                  distance_km = distance / 0.62137,
                  vitesse = distance_km / air_time * 60)
select(flights,distance_km, vitesse)


distance_cal <- select(flights,distance,origin,dest)

# distance la plus grande 
distance_max <- distance_cal %>% arrange(desc(distance)) %>% head(1)
# distance la plus petite
distance_min <- distance_cal %>% arrange(distance) %>% head(1)



# regrouper les vols par destinations dans la table flights
flights <- flights %>%
group_by(dest) %>%
mutate(delay_moy = mean(arr_delay, na.rm=TRUE)) 

# distance moyenne pour chaque groupes
flights <- flights %>%
  group_by(distance) %>%
  mutate(dist_moy = mean(distance, na.rm=TRUE)) 

# vérification de la requête
#hnl <- select(flights,dest) %>% 
  #filter(dest=="HNL")

# Exclure HNL outlier 
dist_flights <- flights %>% 
  group_by(distance) %>% 
  filter(dest != "HNL") %>% 
  mutate(dist_moy = mean(distance, na.rm=TRUE)) 

# vérification de l'exclusion HNL
#select(dist_flights,dest) %>% 
  #filter(dest=="HNL")

#relation entre la distance et le retard moyen 
graph_dist_delay <- ggplot(dist_flights, aes(x=distance, y=delay_moy)) +
  geom_point(shape=23, fill="blue", color="darkred", size=3)

graph_dist_delay

# les compagnies et destination les plus touchées par les retards
company_flights <- inner_join(airlines,flights, by = "carrier")

cf <- select(company_flights,name,dest,delay_moy,distance) %>% arrange(desc(delay_moy)) %>% head(5)
cf


# l’aéroport où on enregistre le retard moyen le plus faible 
retard_min <-  select(company_flights,name,dest,delay_moy,distance) %>% arrange(delay_moy) %>% head(1)
retard_min

#graphique corélation entre l'heure du décollage et le retard à l'arrivé
rda <- select(company_flights,hour,dep_delay)

graph_rda <- ggplot(rda, aes(x=hour,y=dep_delay)) +
   geom_point( color="red", size=3)
graph_rda

#NA

na_flights <- select(company_flights,name,origin,dest,dep_delay,arr_delay,arr_time,dep_time,distance)

na1 <- na_flights %>% filter(is.na(dep_delay)) %>%  arrange(dep_delay)

na1 <- na_flights %>% filter(is.na(dep_delay)) %>%  arrange(dep_delay) %>% 
  group_by(name,dest,dep_time,arr_time) %>% summarise(volnov=n())

#vol annulé par company au départ
vol_dep_annule <- na_flights %>% filter(is.na(dep_time)) %>%  group_by(dest,name) %>% 
  summarise(volnov=n())
#vol annulé par company à l'arrivé
vol_arr_annule <- na_flights %>% filter(is.na(arr_time)) %>%  group_by(dest,name) %>% 
  summarise(volnov=n())

delay_desc <- na_flights %>% 
  arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)))


# calcule de durée pas fait ? 




# gestions des données géospaciales
usairports <- filter(airports, lat < 48.5)
usairports <- filter(usairports, lon > -130)
usairports <- filter(usairports, faa!="JFK") #filter out jfk
jfk <- filter(airports, faa=="JFK") #separate df for jfk
#create basemap
map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
#overlay airports
points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")

for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}

