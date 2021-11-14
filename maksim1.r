# ��� 123
#������� 5
#������� 1
#������� ������ ���������� � ��� ������� 55 ����������� ����������� ������� � 2013 ����,
#���� ��� �������� ������� ����� �������� ���������� �� ���������� 3 ����, 
#� ������������ �� ���������� �� 90 �� 180 ��
setwd("C:/mat"); 
getwd()

# ������������� ������
#install.packages("tidyverse")
#install.packages("rnoaa")
#install.packages ("lubridate")
# ��������� ������ ��� ������
library(tidyverse)
library(rnoaa)
library(lubridate)

# ��������� ������ ������������
station_data = ghcnd_stations()
write.csv(station_data,"station_data.csv")
station_data=read.csv("station_data.csv")

# ��������� ������ ������������
# ������� ������� � ������ ������� � ������������ ��� �������
omsk = data.frame(id = "OMSK", latitude = 55.095781,  longitude = 73.736094)
omsk


# �������� ������������ � �����,
# ������� ����� ����������� ������ �� ����������� ������
omsk_around = meteo_nearby_stations(lat_lon_df = omsk,
                                         station_data = station_data,
                                         limit = 100,
                                         var=c("TAVG"),
                                         year_min = 2010, year_max = 2013)
omsk_around

# �������� ������� ��� ������� ��� �������������� ������������,
# �������� �������������� ������������ �����
omsk_id=omsk_around[["OMSK"]][["id"]][1]
summary(omsk_id)

# ��� ��������� ������� �� ����� �������������� ������ �����
# ���������� ������� ������� ������ ������ �� ������
omsk_table = omsk_around[[1]]
summary(omsk_table)

#������� �� �����������, ������� ������������� �� ���������� �� 90 �� 180

omsk_table = filter (omsk_table, distance > 89 & distance < 181 )
#omsk_stations = omsk_table[omsk_table$omsk.distance>70 & omsk_table$omsk.distance<180] 
omsk_stations = omsk_table

#������ ����������� �������
str(omsk_stations)
omsk_stations$id


#���������� �������� ������ ��� ��������� ������������
#��� ��������� ���� ������ � 1 ������������, ���� �� �������������, ����������� ����. �������
meteo_tidy_ghcnd
#all_omsk_data = meteo_tidy_ghcnd(stationid =omsk _id)


#�������� ������������� ������, ���� ����� ��������� ������ � ���������� ������������
all_i = data.frame()
# ������� ����, � ������� �� ����������� ������ ������ ��� ���� ������������
# c������� ������, ���� ������� ��� ������ � ������������
all_omsk_meteodata = data.frame()
# ������� ���� ��� ����� ������������
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

# ���������� ���������� ����������
write.csv(all_omsk_meteodata,"all_omsk_meteodata.csv")
# ��������� ������ all_omsk_meteodata.csv
#all_omsk_meteodata=read.csv("all_omsk_meteodata.csv")
# ������� ��� ����������
str(all_omsk_meteodata)

# ����������� ������ �� 2010 - 2013 ����
years_omsk_meteodata = filter(all_omsk_meteodata, year %in% c(2010:2013))

# ������� ���, �����, ����
all_omsk_meteodata = mutate(all_omsk_meteodata, year = year(date), month = month(date), day = day(date))
str(all_omsk_meteodata)

# ��������� NA � 0 � ��� tavg<5
all_omsk_meteodata[is.na(all_omsk_meteodata$tavg),"tavg"] = 0
all_omsk_meteodata[all_omsk_meteodata$tavg<5, "tavg"] = 0
summary(all_omsk_meteodata)

# ����������� ������������ �� id,������� � ����� � ������������ �������������
# �� ���� �������, ����� ����������� ������ �� ������� � ������ ������� �� �������
# ��� ���� ������������
group_meteodata =all_omsk_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## ���������� � ������� �� ������� ������ ##
### ���� ��������
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# ��������� �� ����.1. ������� ������
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# ��������� �� ����. 1. ������� ������
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# ��������� ����� ���� i-�� ������,
#�������� � ������ ��������� ��������, � ������

#����� ���� � ������,��������� �� ����. 1.
y = 1.0
# ����������� ��� ���������� ������ - �������, ��� ��� ���� �������� ������
Kf = 300
# ����������� ������������� ��� �������
Qj = 1600
# ������������ ������ ��������
Lj = 2.2
# ����� ������ �������� � �������� ���������
Ej = 25
# ����������� ��������� ��������
# ���������� Fi �� ������v
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#���������� Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
## ����������� ������
Yield = (sum(sumT_month$Yi))
Yield
# �������� ������ � �/�� �.� ���� �������� �� 10^6 �� ������� ������ � ��
# �������� 16,78 �/��

