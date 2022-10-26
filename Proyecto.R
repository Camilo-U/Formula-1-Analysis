# Analisis de Formula 1
# Camilo Ulloa, Maria Davila Escobar, Francesco Petrora

# Año 2020

#Equipo 

raceresults = read.csv("race_results_1950-2020.csv")

p2 = aggregate(Points~Team+Year, data = raceresults, sum)

p3 = p2

anio2020 = p3[p3$Year == 2020,]
anio2020$Year = as.factor(anio2020$Year)

Legend <- ifelse(anio2020$Team == "Mercedes", "Mercedes", "Otras escuderias")

g1 = ggplot(anio2020,aes(x=Team,y=Points)) + 
  ggtitle("Año 2020 resultados") + 
  geom_bar(aes(fill = Legend), stat="identity") + 
  coord_flip() 

# Corredores

p4 = aggregate(Points~Name+Year, data = raceresults, sum)

anio2020_driver = p4[p4$Year == 2020,]
anio2020_driver$Year = as.factor(anio2020_driver$Year)

Legend_c20 <- ifelse(anio2020_driver$Name == "Lewis Hamilton", "Mercedes", "Otras escuderias")

h1 = ggplot(anio2020_driver,aes(x=Name,y=Points)) + 
  ggtitle("Año 2020 resultados por corredor") + 
  geom_bar(aes(fill = Legend_c20), stat="identity") +
  coord_flip()
 
mean(anio2020_driver$Points)

# Fastest Lap 2020

library(rvest)
url.fl2020 = "https://www.formula1.com/en/results.html/2020/fastest-laps.html"
tmp <- read_html(url.fl2020)
tmp <- html_nodes(tmp, "table")
sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
fl2020 <- html_table(tmp[[1]])

fl2020[,1]= NULL
fl2020[,5]= NULL

library(tidyr)

fl2020 = fl2020 %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
colnames(fl2020)[1] = "GP"

moda <- function(x) {            
  xtabla <- table(x)             
  names(xtabla[xtabla == max(xtabla)])          
}

lapply(fl2020, moda)  #Funciona 

ggplot(fl2020,aes(x=GP,y=Time)) + 
  ggtitle("Año 2020 resultados vuelta mas rapida por corredor y pista") + 
  geom_bar(aes(fill = as.factor(Surname)), stat="identity") +
  coord_flip()

# Anio 2019

#Equipo 

anio2019 = p2[p2$Year == 2019,]

Legend19 <- ifelse(anio2019$Team == "Mercedes", "Mercedes", "Otras escuderias")

library(ggplot2)
anio2019$Year = as.factor(anio2019$Year)
g2 = ggplot(anio2019,aes(x=Team,y=Points)) + 
  ggtitle("Año 2019 resultados") + 
  geom_bar(aes(fill = Legend19), stat="identity") + 
  coord_flip() 

# Corredores

anio2019_driver = p4[p4$Year == 2019,]
anio2019_driver$Year = as.factor(anio2019_driver$Year)

Legend_c19 <- ifelse(anio2019_driver$Name == "Lewis Hamilton", "Mercedes", "Otras escuderias")

h2 = ggplot(anio2019_driver,aes(x=Name,y=Points)) + 
  ggtitle("Año 2019 resultados por corredor") + 
  geom_bar(aes(fill = Legend_c19), stat="identity") +
  coord_flip()

# Fastest Lap 2019

library(rvest)
url.fl2019 = "https://www.formula1.com/en/results.html/2019/fastest-laps.html"
tmp19 <- read_html(url.fl2019)
tmp19 <- html_nodes(tmp19, "table")
sapply(tmp19, function(x) dim(html_table(x, fill = TRUE)))
fl2019 <- html_table(tmp19[[1]])

fl2019[,1]= NULL
fl2019[,5]= NULL

library(tidyr)

fl2019 = fl2019 %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
colnames(fl2019)[1] = "GP"

lapply(fl2019, moda)  

ggplot(fl2019,aes(x=GP,y=Time)) + 
  ggtitle("Año 2019 resultados vuelta mas rapida por corredor y pista") + 
  geom_bar(aes(fill = as.factor(Surname)), stat="identity") +
  coord_flip()

# Anio 2018

#Equipo 

anio2018 = p2[p2$Year == 2018,]

Legend18 <- ifelse(anio2019$Team == "Mercedes", "Mercedes", "Otras escuderias")

library(ggplot2)
anio2018$Year = as.factor(anio2018$Year)
g3 = ggplot(anio2018,aes(x=Team,y=Points)) + 
  ggtitle("Año 2018 resultados") + 
  geom_bar(aes(fill = Legend18), stat="identity") + 
  coord_flip() 

# Corredores

anio2018_driver = p4[p4$Year == 2018,]
anio2018_driver$Year = as.factor(anio2018_driver$Year)

Legend_c18 <- ifelse(anio2018_driver$Name == "Lewis Hamilton", "Mercedes", "Otras escuderias")

h3 = ggplot(anio2018_driver,aes(x=Name,y=Points)) + 
  ggtitle("Año 2018 resultados por corredor") + 
  geom_bar(aes(fill = Legend_c18), stat="identity") +
  coord_flip()

# Fastest Lap 2018

library(rvest)
url.fl2018 = "https://www.formula1.com/en/results.html/2018/fastest-laps.html"
tmp18 <- read_html(url.fl2018)
tmp18 <- html_nodes(tmp18, "table")
sapply(tmp18, function(x) dim(html_table(x, fill = TRUE)))
fl2018 <- html_table(tmp18[[1]])

fl2018[,1]= NULL
fl2018[,5]= NULL

library(tidyr)

fl2018 = fl2018 %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))  
fl2018[[3]][9] = "Raikkonen"
fl2018[[4]][9] = "RAI"
colnames(fl2018)[1] = "GP"

lapply(fl2018, moda)  

ggplot(fl2018,aes(x=GP,y=Time)) + 
  ggtitle("Año 2018 resultados vuelta mas rapida por corredor y pista") + 
  geom_bar(aes(fill = as.factor(Surname)), stat="identity") +
  coord_flip()

# Anio 2017

#Equipo 

anio2017 = p2[p2$Year == 2017,]

Legend17 <- ifelse(anio2019$Team == "Mercedes", "Mercedes", "Otras escuderias")

library(ggplot2)
anio2017$Year = as.factor(anio2017$Year)
g4 = ggplot(anio2017,aes(x=Team,y=Points)) + 
  ggtitle("Año 2017 resultados") + 
  geom_bar(aes(fill = Legend17), stat="identity") + 
  coord_flip() 

# Corredores

anio2017_driver = p4[p4$Year == 2017,]
anio2017_driver$Year = as.factor(anio2017_driver$Year)

Legend_c17 <- ifelse(anio2017_driver$Name == "Lewis Hamilton", "Mercedes", "Otras escuderias")

h4 = ggplot(anio2017_driver,aes(x=Name,y=Points)) + 
  ggtitle("Año 2017 resultados por corredor") + 
  geom_bar(aes(fill = Legend_c17), stat="identity") +
  coord_flip()

# Fastest Lap 2017

library(rvest)
url.fl2017 = "https://www.formula1.com/en/results.html/2017/fastest-laps.html"
tmp17 <- read_html(url.fl2017)
tmp17 <- html_nodes(tmp17, "table")
sapply(tmp17, function(x) dim(html_table(x, fill = TRUE)))
fl2017 <- html_table(tmp17[[1]])

fl2017[,1]= NULL
fl2017[,5]= NULL

library(tidyr)

fl2017 = fl2017 %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))  
fl2017[[3]][9] = "Raikkonen"
fl2017[[4]][9] = "RAI"
colnames(fl2017)[1] = "GP"

lapply(fl2017, moda)  

ggplot(fl2017,aes(x=GP,y=Time)) + 
  ggtitle("Año 2017 resultados vuelta mas rapida por corredor y pista") + 
  geom_bar(aes(fill = as.factor(Surname)), stat="identity") +
  coord_flip()

# Anio 2016

#Equipo 

anio2016 = p2[p2$Year == 2016,]

Legend16 <- ifelse(anio2016$Team == "Mercedes", "Mercedes", "Otras escuderias")

library(ggplot2)
anio2016$Year = as.factor(anio2016$Year)
g5 = ggplot(anio2016,aes(x=Team,y=Points)) + 
  ggtitle("Año 2016 resultados") + 
  geom_bar(aes(fill = Legend16), stat="identity") + 
  coord_flip() 

# Corredores

anio2016_driver = p4[p4$Year == 2016,]
anio2016_driver$Year = as.factor(anio2016_driver$Year)

Legend_c16 <- ifelse(anio2016_driver$Name == "Lewis Hamilton", "Mercedes", "Otras escuderias")

h5 = ggplot(anio2016_driver,aes(x=Name,y=Points)) + 
  ggtitle("Año 2016 resultados por corredor") + 
  geom_bar(aes(fill = Legend_c16), stat="identity") +
  coord_flip()

# Fastest Lap 2016

library(rvest)
url.fl2016 = "https://www.formula1.com/en/results.html/2016/fastest-laps.html"
tmp16 <- read_html(url.fl2016)
tmp16 <- html_nodes(tmp16, "table")
sapply(tmp16, function(x) dim(html_table(x, fill = TRUE)))
fl2016 <- html_table(tmp16[[1]])

fl2016[,1]= NULL
fl2016[,5]= NULL

library(tidyr)

fl2016 = fl2016 %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))  
fl2016[[3]][11] = "Raikkonen"
fl2016[[4]][11] = "RAI"
colnames(fl2016)[1] = "GP"

lapply(fl2016, moda)  

ggplot(fl2016,aes(x=GP,y=Time)) + 
  ggtitle("Año 2016 resultados vuelta mas rapida por corredor y pista") + 
  geom_bar(aes(fill = as.factor(Surname)), stat="identity") +
  coord_flip()

# Anio 2015

#Equipo 

anio2015 = p2[p2$Year == 2015,]

Legend15 <- ifelse(anio2015$Team == "Mercedes", "Mercedes", "Otras escuderias")

library(ggplot2)
anio2015$Year = as.factor(anio2015$Year)
g6 = ggplot(anio2015,aes(x=Team,y=Points)) + 
  ggtitle("Año 2015 resultados") + 
  geom_bar(aes(fill = Legend15), stat="identity") + 
  coord_flip() 

# Corredores

anio2015_driver = p4[p4$Year == 2015,]
anio2015_driver$Year = as.factor(anio2015_driver$Year)

Legend_c15 <- ifelse(anio2015_driver$Name == "Lewis Hamilton", "Mercedes", "Otras escuderias")

h6 = ggplot(anio2015_driver,aes(x=Name,y=Points)) + 
  ggtitle("Año 2015 resultados por corredor") + 
  geom_bar(aes(fill = Legend_c15), stat="identity") +
  coord_flip()

# Fastest Lap 2015

library(rvest)
url.fl2015 = "https://www.formula1.com/en/results.html/2015/fastest-laps.html"
tmp15 <- read_html(url.fl2015)
tmp15 <- html_nodes(tmp15, "table")
sapply(tmp15, function(x) dim(html_table(x, fill = TRUE)))
fl2015 <- html_table(tmp15[[1]])

fl2015[,1]= NULL
fl2015[,5]= NULL

library(tidyr)

fl2015 = fl2015 %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))  
fl2015[[3]][4] = "Raikkonen"
fl2015[[4]][4] = "RAI"
fl2015[[3]][7] = "Raikkonen"
fl2015[[4]][7] = "RAI"
colnames(fl2015)[1] = "GP"

lapply(fl2015, moda)  

ggplot(fl2015,aes(x=GP,y=Time)) + 
  ggtitle("Año 2015 resultados vuelta mas rapida por corredor y pista") + 
  geom_bar(aes(fill = as.factor(Surname)), stat="identity") +
  coord_flip()

# Resultados graficos por equipos 2015-2020

require(gridExtra)

C1 = grid.arrange(g6,g5,g4,g3,g2,g1, nrow = 3)
C1

# Resultados graficos por corredor 2015-2020

require(gridExtra)
C2 = grid.arrange(h6,h5,h4,h3,h2,h1, nrow = 3)
C2

# Media por Equipos 2015-2020 y puntos obtenidos mercedes 2015-2020

median_t = data.frame(x = c(median(anio2015$Points), median(anio2016$Points), median(anio2017$Points),
                            median(anio2018$Points), median(anio2019$Points), median(anio2020$Points)), 
                      y = c(anio2015$Points[anio2015$Team == "Mercedes"], anio2016$Points[anio2016$Team == "Mercedes"], anio2017$Points[anio2017$Team == "Mercedes"],
                            anio2018$Points[anio2018$Team == "Mercedes"], anio2019$Points[anio2019$Team == "Mercedes"], anio2020$Points[anio2020$Team == "Mercedes"]))

colnames(median_t)[1:2]=c("Media de puntos","Puntos Mercedes")
median_t$Anio = c(2015,2016,2017,2018,2019,2020)
library(reshape2)
t1 = melt(median_t, id.vars = c("Anio"))
colnames(t1)[2:3]=c("Observaciones", "Valores")
summary(t1)
ggplot(t1, aes(fill=Observaciones, y=Valores, x=Anio)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Media de puntos por anio vs Puntos Mercedes por anio") 

library(reshape2)
t1 = melt(median_t, id.vars = c("Anio"))

# Análisis individual de Hamilton en el año 2020 por circuitos

library(rvest)    
library(tidyr)  
library(dplyr)    

browseURL('https://www.formula1.com/en/results.html/2020/drivers.html') # Observamos los datos de la pagina web

f1 <- read_html('https://www.formula1.com/en/results.html/2020/drivers.html') %>%   # Leemos la tabla en R
  html_node('table') %>% 
  html_table()
f1

# TOP 10 conductores
HAM <- read_html('https://www.formula1.com/en/results.html/2020/drivers/LEWHAM01/lewis-hamilton.html') %>% 
  html_node('table') %>% 
  html_table()
BOT <- read_html('https://www.formula1.com/en/results.html/2020/drivers/VALBOT01/valtteri-bottas.html') %>% 
  html_node('table') %>% 
  html_table()
VER <- read_html('https://www.formula1.com/en/results.html/2020/drivers/MAXVER01/max-verstappen.html') %>% 
  html_node('table') %>% 
  html_table()
PER <- read_html('https://www.formula1.com/en/results.html/2020/drivers/SERPER01/sergio-perez.html') %>% 
  html_node('table') %>% 
  html_table()
RIC <- read_html('https://www.formula1.com/en/results.html/2020/drivers/DANRIC01/daniel-ricciardo.html') %>% 
  html_node('table') %>% 
  html_table()
SAI <- read_html('https://www.formula1.com/en/results.html/2020/drivers/CARSAI01/carlos-sainz.html') %>% 
  html_node('table') %>% 
  html_table()
ALB <- read_html('https://www.formula1.com/en/results.html/2020/drivers/ALEALB01/alexander-albon.html') %>% 
  html_node('table') %>% 
  html_table()
LEC <- read_html('https://www.formula1.com/en/results.html/2020/drivers/CHALEC01/charles-leclerc.html') %>% 
  html_node('table') %>% 
  html_table()
NOR <- read_html('https://www.formula1.com/en/results.html/2020/drivers/LANNOR01/lando-norris.html') %>% 
  html_node('table') %>% 
  html_table()
GAS <- read_html('https://www.formula1.com/en/results.html/2020/drivers/PIEGAS01/pierre-gasly.html') %>% 
  html_node('table') %>% 
  html_table()
class(HAM$`Grand Prix`)
library(ggplot2)
ggplot(HAM, aes(x =`Grand Prix`, y =`PTS`, group = Car, colour = Car)) + 
  geom_line() +
  labs(title = 'Lewis Hamilton Results 2020',
       caption = 'source: www.formula1.com')
ggplot(HAM, aes(x=`Grand Prix`, y=PTS))+ggtitle("Lewis Hamilton 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(BOT, aes(x=`Grand Prix`, y=PTS))+ggtitle("Valtteri Bottas 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(VER, aes(x=`Grand Prix`, y=PTS))+ggtitle("Max Verstappen 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(PER, aes(x=`Grand Prix`, y=PTS))+ggtitle("Sergio Perez 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(RIC, aes(x=`Grand Prix`, y=PTS))+ggtitle("Daniel Ricciardo 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(SAI, aes(x=`Grand Prix`, y=PTS))+ggtitle("Carlos Sainz 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(ALB, aes(x=`Grand Prix`, y=PTS))+ggtitle("Alexander Albon 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(LEC, aes(x=`Grand Prix`, y=PTS))+ggtitle("Charles Leclerc 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(NOR, aes(x=`Grand Prix`, y=PTS))+ggtitle("Lando Norris 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

ggplot(GAS, aes(x=`Grand Prix`, y=PTS))+ggtitle("Pierre Gasly 2020 Results")+
  geom_bar(aes(fill = as.factor(Date)), stat = "identity")+
  coord_flip()

Teams <- read_html('https://www.formula1.com/en/results.html/2020/team.html') %>% 
  html_node('table') %>% 
  html_table()
mean(HAM$PTS)  
mean(f1$PTS)  
mean(BOT$PTS)  
mean(VER$PTS)  
mean(PER$PTS)

# Analisis por circuitos PIT STOP vs Race Result

# Sochi 2020

#PIT STOP
library(rvest)
url.ps2020s = "https://www.formula1.com/en/results.html/2020/races/1054/russia/pit-stop-summary.html"
tbl20 <- read_html(url.ps2020s)
tbl20 <- html_nodes(tbl20, "table")
sapply(tbl20, function(x) dim(html_table(x, fill = TRUE)))
PS2020_S <- html_table(tbl20[[1]])

PS2020_S[,1]= NULL
PS2020_S[,9]= NULL
PS2020_S = PS2020_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2020_S[[4]][20] = "Raikkonen"
PS2020_S[[5]][20] = "RAI"

p5 = aggregate(Time~Surname, data = PS2020_S,sum)
R_PS_2020S = p5[p5$Surname == "Hamilton",]

#RACE RESULT
url.rr2020s = "https://www.formula1.com/en/results.html/2020/races/1054/russia/race-result.html"
tbl20rr <- read_html(url.rr2020s)
tbl20rr <- html_nodes(tbl20rr, "table")
sapply(tbl20rr, function(x) dim(html_table(x, fill = TRUE)))
RR2020_S <- html_table(tbl20rr[[1]])

RR2020_S[,1]= NULL
RR2020_S[,8]= NULL
RR2020_S = RR2020_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

RR2020_S$Pos = as.factor(RR2020_S$Pos)
p5_rr = aggregate(Pos~Surname, data = RR2020_S, sum)

R_RR_2020S = RR2020_S[RR2020_S$Surname == "Hamilton",]
R_RR_2020S[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2020s = merge(R_PS_2020S,R_RR_2020S, by= "Surname")


# Sochi 2019

#PIT STOP
library(rvest)
url.ps2019s = "https://www.formula1.com/en/results.html/2019/races/1015/russia/pit-stop-summary.html"
tbl19 <- read_html(url.ps2019s)
tbl19 <- html_nodes(tbl19, "table")
sapply(tbl19, function(x) dim(html_table(x, fill = TRUE)))
PS2019_S <- html_table(tbl19[[1]])

PS2019_S[,1]= NULL
PS2019_S[,9]= NULL
PS2019_S = PS2019_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2019_S[[4]][5] = "Raikkonen"
PS2019_S[[5]][5] = "RAI"
PS2019_S[[4]][18] = "Raikkonen"
PS2019_S[[5]][18] = "RAI"


p6 = aggregate(Time~Surname, data = PS2019_S,sum)
R_PS_2019S = p6[p6$Surname == "Hamilton",]


#RACE RESULT
url.rr2019s = "https://www.formula1.com/en/results.html/2019/races/1015/russia/race-result.html"
tbl19rr <- read_html(url.rr2019s)
tbl19rr <- html_nodes(tbl19rr, "table")
sapply(tbl19rr, function(x) dim(html_table(x, fill = TRUE)))
RR2019_S <- html_table(tbl19rr[[1]])

RR2019_S[,1]= NULL
RR2019_S[,8]= NULL
RR2019_S = RR2019_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2019S = RR2019_S[RR2019_S$Surname == "Hamilton",]
R_RR_2019S[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2019s = merge(R_PS_2019S,R_RR_2019S, by= "Surname")

# Sochi 2018

#PIT STOP
library(rvest)
url.ps2018s = "https://www.formula1.com/en/results.html/2018/races/994/russia/pit-stop-summary.html"
tbl18 <- read_html(url.ps2018s)
tbl18 <- html_nodes(tbl18, "table")
sapply(tbl18, function(x) dim(html_table(x, fill = TRUE)))
PS2018_S <- html_table(tbl18[[1]])

PS2018_S[,1]= NULL
PS2018_S[,9]= NULL
PS2018_S = PS2018_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2018_S[[4]][15] = "Raikkonen"
PS2018_S[[5]][15] = "RAI"

p7 = aggregate(Time~Surname, data = PS2018_S,sum)
R_PS_2018S = p7[p7$Surname == "Hamilton",]


#RACE RESULT
url.rr2018s = "https://www.formula1.com/en/results.html/2018/races/994/russia/race-result.html"
tbl18rr <- read_html(url.rr2018s)
tbl18rr <- html_nodes(tbl18rr, "table")
sapply(tbl18rr, function(x) dim(html_table(x, fill = TRUE)))
RR2018_S <- html_table(tbl18rr[[1]])

RR2018_S[,1]= NULL
RR2018_S[,8]= NULL
RR2018_S = RR2018_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2018S = RR2018_S[RR2018_S$Surname == "Hamilton",]
R_RR_2018S[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2018s = merge(R_PS_2018S,R_RR_2018S, by= "Surname")

# Sochi 2017

#PIT STOP
library(rvest)
url.ps2017s = "https://www.formula1.com/en/results.html/2017/races/962/russia/pit-stop-summary.html"
tbl17 <- read_html(url.ps2017s)
tbl17 <- html_nodes(tbl17, "table")
sapply(tbl17, function(x) dim(html_table(x, fill = TRUE)))
PS2017_S <- html_table(tbl17[[1]])

PS2017_S[,1]= NULL
PS2017_S[,9]= NULL
PS2017_S = PS2017_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2017_S[[4]][15] = "Raikkonen"
PS2017_S[[5]][15] = "RAI"

p8 = aggregate(Time~Surname, data = PS2017_S,sum)
R_PS_2017S = p8[p8$Surname == "Hamilton",]


#RACE RESULT
url.rr2017s = "https://www.formula1.com/en/results.html/2017/races/962/russia/race-result.html"
tbl17rr <- read_html(url.rr2017s)
tbl17rr <- html_nodes(tbl17rr, "table")
sapply(tbl17rr, function(x) dim(html_table(x, fill = TRUE)))
RR2017_S <- html_table(tbl17rr[[1]])

RR2017_S[,1]= NULL
RR2017_S[,8]= NULL
RR2017_S = RR2017_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2017S = RR2017_S[RR2017_S$Surname == "Hamilton",]
R_RR_2017S[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2017s = merge(R_PS_2017S,R_RR_2017S, by= "Surname")

# Sochi 2016

#PIT STOP
library(rvest)
url.ps2016s = "https://www.formula1.com/en/results.html/2016/races/941/russia/pit-stop-summary.html"
tbl16 <- read_html(url.ps2016s)
tbl16 <- html_nodes(tbl16, "table")
sapply(tbl16, function(x) dim(html_table(x, fill = TRUE)))
PS2016_S <- html_table(tbl16[[1]])

PS2016_S[,1]= NULL
PS2016_S[,9]= NULL
PS2016_S = PS2016_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2016_S[[4]][17] = "Raikkonen"
PS2016_S[[5]][17] = "RAI"

p9 = aggregate(Time~Surname, data = PS2016_S,sum)
R_PS_2016S = p9[p9$Surname == "Hamilton",]


#RACE RESULT
url.rr2016s = "https://www.formula1.com/en/results.html/2016/races/941/russia/race-result.html"
tbl16rr <- read_html(url.rr2016s)
tbl16rr <- html_nodes(tbl16rr, "table")
sapply(tbl16rr, function(x) dim(html_table(x, fill = TRUE)))
RR2016_S <- html_table(tbl16rr[[1]])

RR2016_S[,1]= NULL
RR2016_S[,8]= NULL
RR2016_S = RR2016_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2016S = RR2016_S[RR2016_S$Surname == "Hamilton",]
R_RR_2016S[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2016s = merge(R_PS_2016S,R_RR_2016S, by= "Surname")

# Sochi 2015

#PIT STOP
library(rvest)
url.ps2015s = "https://www.formula1.com/en/results.html/2015/races/932/russia/pit-stop-summary.html"
tbl15 <- read_html(url.ps2015s)
tbl15 <- html_nodes(tbl15, "table")
sapply(tbl15, function(x) dim(html_table(x, fill = TRUE)))
PS2015_S <- html_table(tbl15[[1]])

PS2015_S[,1]= NULL
PS2015_S[,9]= NULL
PS2015_S = PS2015_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2015_S[[4]][16] = "Raikkonen"
PS2015_S[[5]][16] = "RAI"

p10 = aggregate(Time~Surname, data = PS2015_S,sum)
R_PS_2015S = p10[p10$Surname == "Hamilton",]


#RACE RESULT
url.rr2015s = "https://www.formula1.com/en/results.html/2015/races/932/russia/race-result.html"
tbl15rr <- read_html(url.rr2015s)
tbl15rr <- html_nodes(tbl15rr, "table")
sapply(tbl15rr, function(x) dim(html_table(x, fill = TRUE)))
RR2015_S <- html_table(tbl15rr[[1]])

RR2015_S[,1]= NULL
RR2015_S[,8]= NULL
RR2015_S = RR2015_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2015S = RR2015_S[RR2015_S$Surname == "Hamilton",]
R_RR_2015S[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2015s = merge(R_PS_2015S,R_RR_2015S, by= "Surname")

# SOCHI PIT-STOP vs Position results

PSTime_vs_Pos = rbind(Time_Pos_2015s, Time_Pos_2016s, Time_Pos_2017s, Time_Pos_2018s, Time_Pos_2019s, Time_Pos_2020s)  # Funciona pero hay que hacerlo al final

PSTime_vs_Pos$Pos = as.numeric(PSTime_vs_Pos$Pos)
Corr <- cor(PSTime_vs_Pos$Time, PSTime_vs_Pos$Pos)

plot(PSTime_vs_Pos$Time, PSTime_vs_Pos$Pos, pch = 19, col = "black", 
     main= "Tiempo en Pit-Stop vs Posicion final de carrera en Rusia 2015-2020",
     xlab = "Tiempo en Pit-Stop", ylab = "Posicion final de carrera") 
abline(lm( PSTime_vs_Pos$Pos ~ PSTime_vs_Pos$Time), col = "red", lwd = 3)
legend("topleft", legend =c("Lineal"),
       lwd = 3, lty = 1, col = "red")
text(paste("Correlación:", round(Corr, 2)), x = 30, y = 3.0)


# Austria 2020

#PIT STOP
library(rvest)
url.ps2020a = "https://www.formula1.com/en/results.html/2020/races/1045/austria/pit-stop-summary.html"
tbl20a <- read_html(url.ps2020a)
tbl20a <- html_nodes(tbl20a, "table")
sapply(tbl20a, function(x) dim(html_table(x, fill = TRUE)))
PS2020_A <- html_table(tbl20a[[1]])

PS2020_A[,1]= NULL
PS2020_A[,9]= NULL
PS2020_A = PS2020_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2020_A[[4]][2] = "Raikkonen"
PS2020_A[[5]][2] = "RAI"
PS2020_A[[4]][21] = "Raikkonen"
PS2020_A[[5]][21] = "RAI"

p11 = aggregate(Time~Surname, data = PS2020_A,sum)
R_PS_2020A = p11[p11$Surname == "Hamilton",]

#RACE RESULT
url.rr2020A = "https://www.formula1.com/en/results.html/2020/races/1045/austria/race-result.html"
tbl20rra <- read_html(url.rr2020A)
tbl20rra <- html_nodes(tbl20rra, "table")
sapply(tbl20rra, function(x) dim(html_table(x, fill = TRUE)))
RR2020_A <- html_table(tbl20rra[[1]])

RR2020_A[,1]= NULL
RR2020_A[,8]= NULL
RR2020_A = RR2020_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2020A = RR2020_A[RR2020_A$Surname == "Hamilton",]
R_RR_2020A[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2020a = merge(R_PS_2020A,R_RR_2020A, by= "Surname")

# Austria 2019

#PIT STOP
library(rvest)
url.ps2019a = "https://www.formula1.com/en/results.html/2019/races/1008/austria/pit-stop-summary.html"
tbl19a <- read_html(url.ps2019a)
tbl19a <- html_nodes(tbl19a, "table")
sapply(tbl19a, function(x) dim(html_table(x, fill = TRUE)))
PS2019_A <- html_table(tbl19a[[1]])

PS2019_A[,1]= NULL
PS2019_A[,9]= NULL
PS2019_A = PS2019_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2019_A[[4]][7] = "Raikkonen"
PS2019_A[[5]][7] = "RAI"

p12 = aggregate(Time~Surname, data = PS2019_A,sum)
R_PS_2019A = p12[p12$Surname == "Hamilton",]


#RACE RESULT
url.rr2019a = "https://www.formula1.com/en/results.html/2019/races/1008/austria/race-result.html"
tbl19rra <- read_html(url.rr2019a)
tbl19rra <- html_nodes(tbl19rra, "table")
sapply(tbl19rra, function(x) dim(html_table(x, fill = TRUE)))
RR2019_A <- html_table(tbl19rra[[1]])

RR2019_A[,1]= NULL
RR2019_A[,8]= NULL
RR2019_A = RR2019_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2019A = RR2019_A[RR2019_A$Surname == "Hamilton",]
R_RR_2019A[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2019a = merge(R_PS_2019A,R_RR_2019A, by= "Surname")

# Austria 2018

#PIT STOP
library(rvest)
url.ps2018a = "https://www.formula1.com/en/results.html/2018/races/987/austria/pit-stop-summary.html"
tbl18a <- read_html(url.ps2018a)
tbl18a <- html_nodes(tbl18a, "table")
sapply(tbl18a, function(x) dim(html_table(x, fill = TRUE)))
PS2018_A <- html_table(tbl18a[[1]])

PS2018_A[,1]= NULL
PS2018_A[,9]= NULL
PS2018_A = PS2018_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2018_A[[4]][3] = "Raikkonen"
PS2018_A[[5]][3] = "RAI"

p13 = aggregate(Time~Surname, data = PS2018_A,sum)
R_PS_2018A = p13[p13$Surname == "Hamilton",]


#RACE RESULT
url.rr2018a = "https://www.formula1.com/en/results.html/2018/races/987/austria/race-result.html"
tbl18rra <- read_html(url.rr2018a)
tbl18rra <- html_nodes(tbl18rra, "table")
sapply(tbl18rra, function(x) dim(html_table(x, fill = TRUE)))
RR2018_A <- html_table(tbl18rra[[1]])

RR2018_A[,1]= NULL
RR2018_A[,8]= NULL
RR2018_A = RR2018_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2018A = RR2018_A[RR2018_A$Surname == "Hamilton",]
R_RR_2018A[,c(2,3,5,6,7,8,9)]= NULL   # NC = Not clasiffied, meaning that they retire the race before completing the 90% of it
R_RR_2018A[[1]][1] = "0"

Time_Pos_2018A = merge(R_PS_2018A,R_RR_2018A, by= "Surname")

# Austria 2017

#PIT STOP
library(rvest)
url.ps2017a = "https://www.formula1.com/en/results.html/2017/races/967/austria/pit-stop-summary.html"
tbl17a <- read_html(url.ps2017a)
tbl17a <- html_nodes(tbl17a, "table")
sapply(tbl17a, function(x) dim(html_table(x, fill = TRUE)))
PS2017_A <- html_table(tbl17a[[1]])

PS2017_A[,1]= NULL
PS2017_A[,9]= NULL
PS2017_A = PS2017_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2017_A[[4]][17] = "Raikkonen"
PS2017_A[[5]][17] = "RAI"

p14 = aggregate(Time~Surname, data = PS2017_A,sum)
R_PS_2017A = p14[p14$Surname == "Hamilton",]


#RACE RESULT
url.rr2017a = "https://www.formula1.com/en/results.html/2017/races/967/austria/race-result.html"
tbl17rra <- read_html(url.rr2017a)
tbl17rra <- html_nodes(tbl17rra, "table")
sapply(tbl17rra, function(x) dim(html_table(x, fill = TRUE)))
RR2017_A <- html_table(tbl17rra[[1]])

RR2017_A[,1]= NULL
RR2017_A[,8]= NULL
RR2017_A = RR2017_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2017A = RR2017_A[RR2017_A$Surname == "Hamilton",]
R_RR_2017A[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2017A = merge(R_PS_2017A,R_RR_2017A, by= "Surname")

# Austria 2016

#PIT STOP
library(rvest)
url.ps2016a = "https://www.formula1.com/en/results.html/2016/races/945/austria/pit-stop-summary.html"
tbl16a <- read_html(url.ps2016a)
tbl16a <- html_nodes(tbl16a, "table")
sapply(tbl16a, function(x) dim(html_table(x, fill = TRUE)))
PS2016_A <- html_table(tbl16a[[1]])

PS2016_A[,1]= NULL
PS2016_A[,9]= NULL
PS2016_A = PS2016_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PS2016_A[[4]][17] = "Raikkonen"
PS2016_A[[5]][17] = "RAI"
PS2016_A[[4]][30] = "Raikkonen"
PS2016_A[[5]][30] = "RAI"
PS2016_A[[4]][50] = "Raikkonen"
PS2016_A[[5]][50] = "RAI"

p15 = aggregate(Time~Surname, data = PS2016_A,sum)
R_PS_2016A = p15[p15$Surname == "Hamilton",]


#RACE RESULT
url.rr2016a = "https://www.formula1.com/en/results.html/2016/races/945/austria/race-result.html"
tbl16rra <- read_html(url.rr2016a)
tbl16rra <- html_nodes(tbl16rra, "table")
sapply(tbl16rra, function(x) dim(html_table(x, fill = TRUE)))
RR2016_A <- html_table(tbl16rra[[1]])

RR2016_A[,1]= NULL
RR2016_A[,8]= NULL
RR2016_A = RR2016_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2016A = RR2016_A[RR2016_A$Surname == "Hamilton",]
R_RR_2016A[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2016a = merge(R_PS_2016A,R_RR_2016A, by= "Surname")

# Austria 2015

#PIT STOP
library(rvest)
url.ps2015a = "https://www.formula1.com/en/results.html/2015/races/924/austria/pit-stop-summary.html"
tbl15a <- read_html(url.ps2015a)
tbl15a <- html_nodes(tbl15a, "table")
sapply(tbl15a, function(x) dim(html_table(x, fill = TRUE)))
PS2015_A <- html_table(tbl15a[[1]])

PS2015_A[,1]= NULL
PS2015_A[,9]= NULL
PS2015_A = PS2015_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

p16 = aggregate(Time~Surname, data = PS2015_A,sum)
R_PS_2015A = p16[p16$Surname == "Hamilton",]


#RACE RESULT
url.rr2015a = "https://www.formula1.com/en/results.html/2015/races/924/austria/race-result.html"
tbl15rra <- read_html(url.rr2015a)
tbl15rra <- html_nodes(tbl15rra, "table")
sapply(tbl15rra, function(x) dim(html_table(x, fill = TRUE)))
RR2015_A <- html_table(tbl15rra[[1]])

RR2015_A[,1]= NULL
RR2015_A[,8]= NULL
RR2015_A = RR2015_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

R_RR_2015A = RR2015_A[RR2015_A$Surname == "Hamilton",]
R_RR_2015A[,c(2,3,5,6,7,8,9)]= NULL

Time_Pos_2015a = merge(R_PS_2015A,R_RR_2015A, by= "Surname")

# Austria PIT-STOP vs Position results

PSTime_vs_Pos_Austria = rbind(Time_Pos_2015a, Time_Pos_2016a, Time_Pos_2017A, Time_Pos_2018A, Time_Pos_2019a, Time_Pos_2020a)  # Funciona pero hay que hacerlo al final

PSTime_vs_Pos_Austria$Pos = as.numeric(PSTime_vs_Pos_Austria$Pos)
Corr_A <- cor(PSTime_vs_Pos_Austria$Time, PSTime_vs_Pos_Austria$Pos)

plot(PSTime_vs_Pos_Austria$Time, PSTime_vs_Pos_Austria$Pos, pch = 19, col = "black",
     main= "Tiempo en Pit-Stop vs Posicion final de carrera en Austria 2015-2020",
     xlab = "Tiempo en Pit-Stop", ylab = "Posicion final de carrera")
abline(lm( PSTime_vs_Pos$Pos ~ PSTime_vs_Pos$Time), col = "red", lwd = 3)
legend("topleft", legend =c("Lineal"),
       lwd = 3, lty = 1, col = "red")
text(paste("Correlación:", round(Corr_A, 2)), x = 25, y = 3.5)



# Analisis por circuitos Posicion en clasificacion vs Posicion final de carrera 

# Sochi 2020

#Posicion en clasificacion
library(rvest)
url.pc2020s = "https://www.formula1.com/en/results.html/2020/races/1054/russia/qualifying.html"
tb20 <- read_html(url.pc2020s)
tb20 <- html_nodes(tb20, "table")
sapply(tb20, function(x) dim(html_table(x, fill = TRUE)))
PC2020_S <- html_table(tb20[[1]])

PC2020_S[,1]= NULL
PC2020_S[,9]= NULL
PC2020_S = PC2020_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2020_S[[4]][20] = "Raikkonen"
PC2020_S[[5]][20] = "RAI"

q1 = aggregate(Pos~Surname, data = PC2020_S,sum)
R_PC_2020S = q1[q1$Surname == "Hamilton",]

#RACE RESULT

R_RR_2020S = RR2020_S[RR2020_S$Surname == "Hamilton",]
R_RR_2020S[,c(2,3,5,6,7,8,9)]= NULL

PC_Pos_2020s = merge(R_PC_2020S,R_RR_2020S, by= "Surname")
colnames(PC_Pos_2020s)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Sochi 2019

#Posicion en clasificacion
library(rvest)
url.pc2019s = "https://www.formula1.com/en/results.html/2019/races/1015/russia/qualifying.html"
tb19 <- read_html(url.pc2019s)
tb19 <- html_nodes(tb19, "table")
sapply(tb19, function(x) dim(html_table(x, fill = TRUE)))
PC2019_S <- html_table(tb19[[1]])

PC2019_S[,1]= NULL
PC2019_S[,9]= NULL
PC2019_S = PC2019_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2019_S[[4]][16] = "Raikkonen"
PC2019_S[[5]][16] = "RAI"

PC2019_S$Pos = as.numeric(PC2019_S$Pos)
q2 = aggregate(Pos~Surname, data = PC2019_S,sum)
R_PC_2019S = q2[q2$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2019s = merge(R_PC_2019S,R_RR_2019S, by= "Surname")
colnames(PC_Pos_2019s)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Sochi 2018

#Posicion en clasificacion
library(rvest)
url.pc2018s = "https://www.formula1.com/en/results.html/2018/races/994/russia/qualifying.html"
tb18 <- read_html(url.pc2018s)
tb18 <- html_nodes(tb18, "table")
sapply(tb18, function(x) dim(html_table(x, fill = TRUE)))
PC2018_S <- html_table(tb18[[1]])

PC2018_S[,1]= NULL
PC2018_S[,9]= NULL
PC2018_S = PC2018_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2018_S[[4]][4] = "Raikkonen"
PC2018_S[[5]][4] = "RAI"

q3 = aggregate(Pos~Surname, data = PC2018_S,sum)
R_PC_2018S = q3[q3$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2018s = merge(R_PC_2018S,R_RR_2018S, by= "Surname")
colnames(PC_Pos_2018s)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Sochi 2017

#Posicion en clasificacion
library(rvest)
url.pc2017s = "https://www.formula1.com/en/results.html/2017/races/962/russia/qualifying.html"
tb17 <- read_html(url.pc2017s)
tb17 <- html_nodes(tb17, "table")
sapply(tb17, function(x) dim(html_table(x, fill = TRUE)))
PC2017_S <- html_table(tb17[[1]])

PC2017_S[,1]= NULL
PC2017_S[,9]= NULL
PC2017_S = PC2017_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2017_S[[4]][2] = "Raikkonen"
PC2017_S[[5]][2] = "RAI"

q4 = aggregate(Pos~Surname, data = PC2017_S,sum)
R_PC_2017S = q4[q4$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2017s = merge(R_PC_2017S,R_RR_2017S, by= "Surname")
colnames(PC_Pos_2017s)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")


# Sochi 2016

#Posicion en clasificacion
library(rvest)
url.pc2016s = "https://www.formula1.com/en/results.html/2016/races/941/russia/qualifying.html"
tb16 <- read_html(url.pc2016s)
tb16 <- html_nodes(tb16, "table")
sapply(tb16, function(x) dim(html_table(x, fill = TRUE)))
PC2016_S <- html_table(tb16[[1]])

PC2016_S[,1]= NULL
PC2016_S[,9]= NULL
PC2016_S = PC2016_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2016_S[[4]][4] = "Raikkonen"
PC2016_S[[5]][4] = "RAI"

q5 = aggregate(Pos~Surname, data = PC2016_S,sum)
R_PC_2016S = q5[q5$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2016s = merge(R_PC_2016S,R_RR_2016S, by= "Surname")
colnames(PC_Pos_2016s)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")


# Sochi 2015

#Posicion en clasificacion
library(rvest)
url.pc2015s = "https://www.formula1.com/en/results.html/2015/races/932/russia/qualifying.html"
tb15 <- read_html(url.pc2015s)
tb15 <- html_nodes(tb15, "table")
sapply(tb15, function(x) dim(html_table(x, fill = TRUE)))
PC2015_S <- html_table(tb15[[1]])

PC2015_S[,1]= NULL
PC2015_S[,9]= NULL
PC2015_S = PC2015_S %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2015_S[[4]][5] = "Raikkonen"
PC2015_S[[5]][5] = "RAI"

q6 = aggregate(Pos~Surname, data = PC2015_S,sum)
R_PC_2015S = q6[q6$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2015s = merge(R_PC_2015S,R_RR_2015S, by= "Surname")
colnames(PC_Pos_2015s)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# SOCHI Posicion en clasificacion vs Position final carrera resultados

PC_vs_PosF = rbind(PC_Pos_2015s, PC_Pos_2016s, PC_Pos_2017s, PC_Pos_2018s, PC_Pos_2019s, PC_Pos_2020s)  # Funciona pero hay que hacerlo al final

summary(PC_vs_PosF)
PC_vs_PosF$`Posicion Final Carrera` = as.numeric(PC_vs_PosF$`Posicion Final Carrera`)
Corr_1 <- cor(PC_vs_PosF$`Posicion Clasificacion`, PC_vs_PosF$`Posicion Final Carrera`)

plot(PC_vs_PosF$`Posicion Clasificacion`, PC_vs_PosF$`Posicion Final Carrera`, pch = 19, col = "black", 
     main= "Posicion Clasificacion vs Posicion final de carrera en Rusia 2015-2020",
     xlab = "Posicion Clasificacion", ylab = "Posicion final de carrera",
     xlim = c(0, 20), ylim = c(0, 20) ) 
abline(lm(PC_vs_PosF$`Posicion Final Carrera`  ~ PC_vs_PosF$`Posicion Clasificacion`), col = "red", lwd = 3)
legend("topleft", legend =c("Lineal"),
       lwd = 3, lty = 1, col = "red")
text(paste("Correlación:", round(Corr_1, 2)), x = 1.5, y = 14)


# Austria 2020

#Posicion en clasificacion
library(rvest)
url.pc2020a = "https://www.formula1.com/en/results.html/2020/races/1045/austria/qualifying.html"
tb20a <- read_html(url.pc2020a)
tb20a <- html_nodes(tb20a, "table")
sapply(tb20a, function(x) dim(html_table(x, fill = TRUE)))
PC2020_A <- html_table(tb20a[[1]])

PC2020_A[,1]= NULL
PC2020_A[,9]= NULL
PC2020_A = PC2020_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))

q7= aggregate(Pos~Surname, data = PC2020_A,sum)
R_PC_2020A = q7[q7$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2020a = merge(R_PC_2020A,R_RR_2020A, by= "Surname")
colnames(PC_Pos_2020a)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Austria 2019

#Posicion en clasificacion
library(rvest)
url.pc2019a = "https://www.formula1.com/en/results.html/2019/races/1008/austria/qualifying.html"
tb19a <- read_html(url.pc2019a)
tb19a <- html_nodes(tb19a, "table")
sapply(tb19a, function(x) dim(html_table(x, fill = TRUE)))
PC2019_A <- html_table(tb19a[[1]])

PC2019_A[,1]= NULL
PC2019_A[,9]= NULL
PC2019_A = PC2019_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2019_A[[4]][7] = "Raikkonen"
PC2019_A[[5]][7] = "RAI"

q8= aggregate(Pos~Surname, data = PC2019_A,sum)
R_PC_2019A = q8[q8$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2019a = merge(R_PC_2019A,R_RR_2019A, by= "Surname")
colnames(PC_Pos_2019a)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Austria 2018

#Posicion en clasificacion
library(rvest)
url.pc2018a = "https://www.formula1.com/en/results.html/2018/races/987/austria/qualifying.html"
tb18a <- read_html(url.pc2018a)
tb18a <- html_nodes(tb18a, "table")
sapply(tb18a, function(x) dim(html_table(x, fill = TRUE)))
PC2018_A <- html_table(tb18a[[1]])

PC2018_A[,1]= NULL
PC2018_A[,9]= NULL
PC2018_A = PC2018_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2018_A[[4]][4] = "Raikkonen"
PC2018_A[[5]][4] = "RAI"

q9= aggregate(Pos~Surname, data = PC2018_A,sum)
R_PC_2018A = q9[q9$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2018a = merge(R_PC_2018A,R_RR_2018A, by= "Surname")
colnames(PC_Pos_2018a)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Austria 2017

#Posicion en clasificacion
library(rvest)
url.pc2017a = "https://www.formula1.com/en/results.html/2017/races/967/austria/qualifying.html"
tb17a <- read_html(url.pc2017a)
tb17a <- html_nodes(tb17a, "table")
sapply(tb17a, function(x) dim(html_table(x, fill = TRUE)))
PC2017_A <- html_table(tb17a[[1]])

PC2017_A[,1]= NULL
PC2017_A[,9]= NULL
PC2017_A = PC2017_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2017_A[[4]][4] = "Raikkonen"
PC2017_A[[5]][4] = "RAI"

q10= aggregate(Pos~Surname, data = PC2017_A,sum)
R_PC_2017A = q10[q10$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2017a = merge(R_PC_2017A,R_RR_2017A, by= "Surname")
colnames(PC_Pos_2017a)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")


# Austria 2016

#Posicion en clasificacion
library(rvest)
url.pc2016a = "https://www.formula1.com/en/results.html/2016/races/945/austria/qualifying.html"
tb16a <- read_html(url.pc2016a)
tb16a <- html_nodes(tb16a, "table")
sapply(tb16a, function(x) dim(html_table(x, fill = TRUE)))
PC2016_A <- html_table(tb16a[[1]])

PC2016_A[,1]= NULL
PC2016_A[,9]= NULL
PC2016_A = PC2016_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2016_A[[4]][6] = "Raikkonen"
PC2016_A[[5]][6] = "RAI"

q11= aggregate(Pos~Surname, data = PC2016_A,sum)
R_PC_2016A = q11[q11$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2016a = merge(R_PC_2016A,R_RR_2016A, by= "Surname")
colnames(PC_Pos_2016a)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")


# Austria 2015

#Posicion en clasificacion
library(rvest)
url.pc2015a = "https://www.formula1.com/en/results.html/2015/races/924/austria/qualifying.html"
tb15a <- read_html(url.pc2015a)
tb15a <- html_nodes(tb15a, "table")
sapply(tb15a, function(x) dim(html_table(x, fill = TRUE)))
PC2015_A <- html_table(tb15a[[1]])

PC2015_A[,1]= NULL
PC2015_A[,9]= NULL
PC2015_A = PC2015_A %>% separate(Driver, c('Name', 'Surname', 'Driver Tag'))
PC2015_A[[4]][18] = "Raikkonen"
PC2015_A[[5]][18] = "RAI"

q12= aggregate(Pos~Surname, data = PC2015_A,sum)
R_PC_2015A = q12[q12$Surname == "Hamilton",]

#RACE RESULT

PC_Pos_2015a = merge(R_PC_2015A,R_RR_2015A, by= "Surname")
colnames(PC_Pos_2015a)[2:3]=c("Posicion Clasificacion","Posicion Final Carrera")

# Austria Posicion en clasificacion vs Position final carrera resultados

PC_vs_PosF_A = rbind(PC_Pos_2015a, PC_Pos_2016a, PC_Pos_2017a, PC_Pos_2018a, PC_Pos_2019a, PC_Pos_2020a)  # Funciona pero hay que hacerlo al final

summary(PC_vs_PosF_A)
PC_vs_PosF_A$`Posicion Final Carrera` = as.numeric(PC_vs_PosF_A$`Posicion Final Carrera`)
Corr_2 <- cor(PC_vs_PosF_A$`Posicion Clasificacion`, PC_vs_PosF_A$`Posicion Final Carrera`)

plot(PC_vs_PosF_A$`Posicion Clasificacion`, PC_vs_PosF_A$`Posicion Final Carrera`, pch = 19, col = "black", 
     main= "Posicion Clasificacion vs Posicion final de carrera en Austria 2015-2020",
     xlab = "Posicion Clasificacion", ylab = "Posicion final de carrera",
     xlim = c(0, 20), ylim = c(0, 20) ) 
abline(lm(PC_vs_PosF_A$`Posicion Final Carrera`  ~ PC_vs_PosF_A$`Posicion Clasificacion`), col = "red", lwd = 3)
legend("topleft", legend =c("Lineal"),
       lwd = 3, lty = 1, col = "red")
text(paste("Correlación:", round(Corr_2, 2)), x = 1.5, y = 14)


# Analisis por costo de carro de f1

options(scipen = 999)
Costo_carrof1_2018 = data.frame(Componentes = c("Volante", "Motor", "Caja de cambios", "Halo", "Tanque de gasolina", "Hidraulica", "Ala trasera", "Ala delantera", "Neumaticos", "Fibra de carbono"),
                                Costos = as.numeric(c(50000, 10500000, 440000, 17000, 140000, 170000, 85000, 150000, 2700, 650000)))
  
  # Todos estos datos fueron sacadaos de la siguiente pagina web: "https://www.essentiallysports.com/formula-one-car-cost/"

ggplot(Costo_carrof1_2018, aes(x=Componentes, y=Costos, fill = Componentes)) + 
  geom_bar(stat = "identity", width=0.2) + ggtitle("Costos de un carro de F1 decompuesto por sus componentes") 

Costo_carrof1_2018_M = Costo_carrof1_2018[!(Costo_carrof1_2018$Componentes == "Motor"),]

ggplot(Costo_carrof1_2018_M, aes(x=Componentes, y=Costos, fill = Componentes)) + 
  geom_bar(stat = "identity", width=0.2) + ggtitle("Costos de un carro de F1 decompuesto por sus componentes") 

# Presupuesto 2019 

  # Todos los datos fueron obtenidos de la siguiente pagina web: "https://beyondtheflag.com/2021/10/13/2021-fansided-sports-fan-year/"
presupuesto_2019 = data.frame(Escuderia = c("Mercedes", "Ferrari", "Red Bull Racing", "Renault", "McLaren", "Racing Point", "Hass", "Alfa Romeo", "Toro Rosso", "Williams"),
                              Presupuesto_en_millones = c(484, 463, 445, 272, 269, 188, 173, 141, 138, 132))

library(ggplot2)

ggplot(presupuesto_2019, aes(x=Escuderia, y=Presupuesto_en_millones, fill = Escuderia)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values = c("grey", "grey", "grey", "grey", "blue", "grey", "grey", "grey", "grey", "grey") ) +
  ggtitle("Presupuesto de escuderias para año 2019")

