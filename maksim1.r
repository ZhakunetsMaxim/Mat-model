# ПАЭ 123
#Вариант 5
#Задание 1
#Жакунец Максим Русланович — для региона 55 рассчитайте урожайность пшеницы в 2013 году,
#взяв для рассчета средние суммы активных температур за предыдущие 3 года, 
#с метеостанций на расстоянии от 90 до 180 км
setwd("C:/mat"); 
getwd()

# устанавливаем пакеты
#install.packages("tidyverse")
#install.packages("rnoaa")
#install.packages ("lubridate")
# открываем нужные нам пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

# скачиваем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,"station_data.csv")
station_data=read.csv("station_data.csv")

# формируем список метеостанций
# создаем таблицу с именем региона и координатами его столицы
omsk = data.frame(id = "OMSK", latitude = 55.095781,  longitude = 73.736094)
omsk


# выбираем метеостанции в Омске,
# которые имеют необходимые данные за определнный период
omsk_around = meteo_nearby_stations(lat_lon_df = omsk,
                                         station_data = station_data,
                                         limit = 100,
                                         var=c("TAVG"),
                                         year_min = 2010, year_max = 2013)
omsk_around

# получили таблицу где указаны все индетификаторы метеостанций,
# получаем индентификатор метеостанции Омска
omsk_id=omsk_around[["OMSK"]][["id"]][1]
summary(omsk_id)

# для получения таблицы со всеми метеостанциями вокруг Омска
# необходимо выбрать целиком первый объект из списка
omsk_table = omsk_around[[1]]
summary(omsk_table)

#выберем те метеостации, которые располагаются на расстоянии от 90 до 180

omsk_table = filter (omsk_table, distance > 89 & distance < 181 )
#omsk_stations = omsk_table[omsk_table$omsk.distance>70 & omsk_table$omsk.distance<180] 
omsk_stations = omsk_table

#Список необходимых станций
str(omsk_stations)
omsk_stations$id


#Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте след. команду
meteo_tidy_ghcnd
#all_omsk_data = meteo_tidy_ghcnd(stationid =omsk _id)


#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
# создать цикл, в котором бы скачивались нужные данные для всех метеостанций
# cоздадим объект, куда скачаем все данные с метеостанций
all_omsk_meteodata = data.frame()
# создаем цикл для наших метеостанций
stations_names=omsk_stations$id
stations_names=stations_names[1:9] 

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2010-01-01",
                              date_max = "2013-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_omsk_meteodata=rbind(all_omsk_meteodata, one_meteo)}

# записываем полученные результаты
write.csv(all_omsk_meteodata,"all_omsk_meteodata.csv")
# считываем данные all_omsk_meteodata.csv
#all_omsk_meteodata=read.csv("all_omsk_meteodata.csv")
# смотрим что получилось
str(all_omsk_meteodata)

# Отфильтруем данные за 2010 - 2013 годы
years_omsk_meteodata = filter(all_omsk_meteodata, year %in% c(2010:2013))

# добавим год, месяц, день
all_omsk_meteodata = mutate(all_omsk_meteodata, year = year(date), month = month(date), day = day(date))
str(all_omsk_meteodata)

# превратим NA в 0 и где tavg<5
all_omsk_meteodata[is.na(all_omsk_meteodata$tavg),"tavg"] = 0
all_omsk_meteodata[all_omsk_meteodata$tavg<5, "tavg"] = 0
summary(all_omsk_meteodata)

# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам
# для всех метеостанций
group_meteodata =all_omsk_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# отношение числа дней i-го месяца,
#входящих в период вегетации культуры, к общему

#числу дней в месяце,константа по табл. 1.
y = 1.0
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300
# Коэффициент использования ФАР посевом
Qj = 1600
# калорийность урожая культуры
Lj = 2.2
# сумма частей основной и побочной продукции
Ej = 25
# стандартная влажность культуры
# Рассчитаем Fi по месяцаv
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
## Расчитываем урожай
Yield = (sum(sumT_month$Yi))
Yield
# получаем урожай в ц/га т.к если умножать на 10^6 то получим урожай в мг
# получили 16,78 ц/га

