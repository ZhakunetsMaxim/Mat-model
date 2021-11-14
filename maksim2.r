#Жакунец М.Р., ПАЭ 123
#Вариант 5
#Задание 2
# Cоздайте модель множествеенной линейной регрессии дневных
# потоков углекислого газа за весенний период 2013 года по данным измерений
# методом турбулентной пульсации.

#Указание и проверка рабочей директории
setwd("C:/mat");
getwd()
#Работа с библиотеками и установкой пакетов

library(tidyverse)
library(rnoaa)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),
                   comment=c("["))
eddypro = eddypro[-1, ]
# посмотрим на сами переменные и для этого воспользуеся функцией glimpse(),
# которая более наглядно представляет каждую отдельную переменную,
# жертвуя при этом предсталение строчек данных
glimpse(eddypro)
#Удаление первой строки и ненужного пустого столбца "roll"
eddypro = select(eddypro, -(roll))

#Изменение специальных символов в названии стобцов на допустимые для переменных названия
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]", "_emph_") %>%
  str_replace_all("[?]", "_quest_") %>%
  str_replace_all("[*]", "_star_") %>%
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")
# нам необходимо получить данные за весенний период, дневное время
eddypro=filter(eddypro, DOY>243&DOY<335,daytime==TRUE)
#Преобразуем переменные типа char в факторы
eddypro = eddypro %>% mutate_if(is.character, as.factor)
# нам необходимо получить все переменные были типа numeric,
# воспользуемся функциями saplly и is.numeric
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
str(eddypro_numeric)
# посмотрим коэф корреляции
cor_eddy=cor(eddypro_numeric)
str(cor_eddy)

# необходимо избавиться от na
# для начала надо посчитать в таблице сколько NA в каждой переменной
# воспользуемся функцией summarise_all и sum
na_cor_eddy=eddypro_numeric %>% summarise_all(~sum(is.na(.x)))
navect=na_cor_eddy[1,]%>% as.integer()
# посмотрим у каких переменных кол-во NA превышает 30
names(eddypro_numeric)[navect>30]
# исключим все na
eddypro_numeric=na.exclude(eddypro_numeric)
# снова посчитаем коэф корреляции
cor_eddy=cor(eddypro_numeric)
cor_eddy=data.frame(cor_eddy)
#найдем коэф детерминации для нашей зависимой переменной
cor_vars=cor_eddy$co2_flux^2
names(cor_vars)=names(cor_eddy)
# выбрем только значимые коэффициенты, в которых коэф детерминации более 0,16
cor_vars=cor_vars[cor_vars>0.16]
# узнаем имена значимых переменных
names(cor_vars)%>% na.exclude()

#Создадим непересекающиеся выборки
row_numbers = 1:length(eddypro_numeric$co2_flux)
#Обучающая
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
#Тестирующая
test = row_numbers[-teach]
#Обучающая
teaching_edd = eddypro_numeric[teach,]
#Тестирующая
testing_edd = eddypro_numeric[test,]

# МНОЖЕСТВЕННАЯ РЕГРЕССИЯ


#МОДЕЛЬ 1
# Построим модель по известным нам значимым переменным
mod1 = lm(data = teaching_edd, co2_flux~ DOY+h2o_molar_density+h2o_mole_fraction
          +h2o_mixing_ratio+air_density+air_heat_capacity+air_molar_volume
          +water_vapor_density+e+specific_humidity+Tdew+un_co2_flux+w_div_co2_cov
          +h2o+h2o_1+flowrate)
# помострим коэффициенты
coef(mod1)
#остатки
resid(mod1)
# доверительный интервал
confint(mod1)
# посмотрим р-значения по модели
summary(mod1)
# коэффициент детерминации = 0,569
# проведем дисперсионный анализ
anova(mod1)
# согласно ДА мы видим какие переменные у нас не значимые:
# air_molar_volume,water_vapor_density,specific_humidity,w_div_co2_cov,h2o,flowrate
##
#Построим графиик нормального распределения:
  plot(mod1,2)
# в целом данные распределены нормально
# Построим график наблюдаемых значений от предсказанных значений
plot(mod1$fitted.values, teaching_edd$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")

# Построим график зависимости остатков от наблюдаемых значений
plot(teaching_edd$co2_flux,mod1$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo1=lm(mod1$residuals~teaching_edd$co2_flux)
abline(a=mo1$coefficients[1],b=mo1$coefficients[2],col="red")
# наблюдается зависимость остатков от наблюдаемых значений


#МОДЕЛЬ 2
# построим аналогичную модель при этом будем искать зависимость между переменными
# второго порядка
mod2 = lm(data = teaching_edd, co2_flux~ (DOY+h2o_molar_density+h2o_mole_fraction+
                                            h2o_mixing_ratio+air_density+air_heat_capacity+
                                            air_molar_volume+water_vapor_density+e+
                                            specific_humidity+Tdew+un_co2_flux+w_div_co2_cov+h2o+h2o_1+flowrate)^2)
# помострим коэффициенты
coef(mod2)
#Остатки
resid(mod2)
#Доверительный интервал
confint(mod2)
#P-значения по модели
summary(mod2)
# коэффициент детерминации = 0,8452
#Дисперсионный анализ
anova(mod2)
# согласно ДА мы видим какие переменные 1 и 2-го порядка у нас не значимые:
#air_molar_volume,water_vapor_density,specific_humidity,w_div_co2_cov,h2o,flowrate и тд.
# однако если в первом порядке эти переменные не значимы, то во 2-м могут быть зависимости
#Графиик на нормальной веротяностной бумаге :
plot(mod2,2)
# Построим график наблюдаемых значений от предсказанных значений
plot(mod2$fitted.values, teaching_edd$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="blue")
# Построим график остатков от набоюдаемых значений
plot(teaching_edd$co2_flux,mod2$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo2=lm(mod2$residuals~teaching_edd$co2_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="blue")


#МОДЕЛЬ 3
# так как незначимые 1 порядка могут иметь связи с перемнными 2-го порядка, мы их не искоючаем
# исключим только незначиму пременную 1 порядка specific_humidity, так как связь с другими переменными
# полность отсутвует, а также исключим остальные незначимые переменные
mod3 = lm (data = teaching_edd, co2_flux~ (DOY+h2o_molar_density+h2o_mole_fraction+
                                             h2o_mixing_ratio+air_density+air_heat_capacity+
                                             air_molar_volume+water_vapor_density+e+Tdew+un_co2_flux+
                                             w_div_co2_cov+h2o+h2o_1+flowrate)^2-DOY:h2o_molar_density-
             DOY:h2o_mole_fraction-DOY:h2o_mixing_ratio-DOY:air_density-DOY:air_heat_capacity-
             DOY:air_molar_volume-DOY:water_vapor_density-DOY:e-DOY:Tdew-DOY:un_co2_flux-DOY:w_div_co2_cov-DOY:h2o-DOY:h2o_1-
             DOY:flowrate-h2o_molar_density:h2o_mole_fraction-h2o_molar_density:h2o_mixing_ratio-h2o_molar_density:air_density-
             h2o_molar_density:air_heat_capacity-h2o_molar_density:air_molar_volume-h2o_molar_density:water_vapor_density-
             h2o_molar_density:e-h2o_molar_density:Tdew -h2o_molar_density:un_co2_flux-h2o_molar_density:w_div_co2_cov-
             h2o_molar_density:h2o_1 -h2o_molar_density:flowrate-h2o_mole_fraction:air_heat_capacity-
             h2o_mole_fraction:water_vapor_density-h2o_mole_fraction:e-
             h2o_mole_fraction:Tdew-h2o_mole_fraction:un_co2_flux-h2o_mole_fraction:w_div_co2_cov-h2o_mole_fraction:h2o-h2o_mole_fraction:flowrate-
             h2o_mixing_ratio:un_co2_flux-
             h2o_mixing_ratio:w_div_co2_cov-h2o_mixing_ratio:flowrate -air_density:water_vapor_density-air_density:e-air_density:Tdew-
             air_density:un_co2_flux-air_density:h2o-air_density:flowrate-air_heat_capacity:un_co2_flux-air_heat_capacity:w_div_co2_cov-
             air_molar_volume:e-air_molar_volume:flowrate -water_vapor_density:e-water_vapor_density:un_co2_flux-
             water_vapor_density:h2o-water_vapor_density:flowrate-e:Tdew-e:un_co2_flux -e:h2o-e:flowrate-Tdew:un_co2_flux-Tdew:w_div_co2_cov-
             Tdew:flowrate-un_co2_flux:w_div_co2_cov-un_co2_flux:h2o-un_co2_flux:h2o_1-w_div_co2_cov:h2o-w_div_co2_cov:flowrate-h2o:flowrate)
#Коэффициенты
coef(mod3)
#Остатки
resid(mod3)
#Доверительный интервал
confint(mod3)
#P-значения по модели
summary(mod3)
# коеффициент детерминации = 0,6992, меньше чем во 2 модели,
# таким образом, более полная модель 2 лучше отражает данные
#Дисперсионный анализ
anova(mod3)
#Графиик на нормальной веротяностной бумаге :
plot(mod3,2)
# Построим график наблюдаемых значений от предсказанных значений
plot(mod3$fitted.values, teaching_edd$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="green")
# Построим график остатков от набоюдаемых значений
plot(teaching_edd$co2_flux,mod3$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo3=lm(mod3$residuals~teaching_edd$co2_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")


#МОДЕЛЬ 4
# на основании ДА 3 модели уберем незначимые переменные
mod3 = lm (data = teaching_edd, co2_flux~ (DOY+h2o_molar_density+h2o_mole_fraction+
                                             h2o_mixing_ratio+air_density+air_heat_capacity+
                                             air_molar_volume+water_vapor_density+e+Tdew+un_co2_flux+
                                             w_div_co2_cov+h2o)^2-DOY:h2o_molar_density-
             DOY:h2o_mole_fraction-DOY:h2o_mixing_ratio-DOY:air_density-DOY:air_heat_capacity-
             DOY:air_molar_volume-DOY:water_vapor_density-DOY:e-DOY:Tdew-DOY:un_co2_flux-DOY:w_div_co2_cov-DOY:h2o-DOY:h2o_1-
             DOY:flowrate-h2o_molar_density:h2o_mole_fraction-h2o_molar_density:h2o_mixing_ratio-h2o_molar_density:air_density-
             h2o_molar_density:air_heat_capacity-h2o_molar_density:air_molar_volume-h2o_molar_density:water_vapor_density-
             h2o_molar_density:e-h2o_molar_density:Tdew -h2o_molar_density:un_co2_flux-h2o_molar_density:w_div_co2_cov-
             h2o_molar_density:h2o_1 -h2o_molar_density:flowrate-h2o_mole_fraction:air_heat_capacity-
             h2o_mole_fraction:water_vapor_density-h2o_mole_fraction:e-
             h2o_mole_fraction:Tdew-h2o_mole_fraction:un_co2_flux-h2o_mole_fraction:w_div_co2_cov-h2o_mole_fraction:h2o-h2o_mole_fraction:flowrate-
             h2o_mixing_ratio:un_co2_flux-
             h2o_mixing_ratio:w_div_co2_cov-h2o_mixing_ratio:flowrate -air_density:water_vapor_density-air_density:e-air_density:Tdew-
             air_density:un_co2_flux-air_density:h2o-air_density:flowrate-air_heat_capacity:un_co2_flux-air_heat_capacity:w_div_co2_cov-
             air_molar_volume:e-air_molar_volume:flowrate -water_vapor_density:e-water_vapor_density:un_co2_flux-
             water_vapor_density:h2o-water_vapor_density:flowrate-e:Tdew-e:un_co2_flux -e:h2o-e:flowrate-Tdew:un_co2_flux-Tdew:w_div_co2_cov-
             Tdew:flowrate-un_co2_flux:w_div_co2_cov-un_co2_flux:h2o-un_co2_flux:h2o_1-w_div_co2_cov:h2o-w_div_co2_cov:flowrate-h2o:flowrate)
#Коэффициенты
coef(mod3)
#Остатки
resid(mod3)
#Доверительный интервал
confint(mod3)
#P-значения по модели
summary(mod3)
# коеффициент детерминации = 0,6992, меньше чем во 2 модели,
# таким образом, более полная модель 2 лучше отражает данные
#Дисперсионный анализ
anova(mod3)
#Графиик на нормальной веротяностной бумаге :
plot(mod3,2)
# Построим график наблюдаемых значений от предсказанных значений
plot(mod3$fitted.values, teaching_edd$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="green")
# Построим график остатков от набоюдаемых значений
plot(teaching_edd$co2_flux,mod3$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo3=lm(mod3$residuals~teaching_edd$co2_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")




##Проверка моделей
#Изпользуя МОДЕЛЬ 2, наложим её предсказанные значения

#Первый график
qplot(co2_flux , co2_flux, data = teaching_edd) + geom_line(aes(y = predict(mod2, teaching_edd)))

#Второй график
qplot(co2_flux , co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))


#Модель зависит от множества переменных, мы можем вывести много графиков зависимостей co2_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке

#Примеры
qplot(DOY, co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))
qplot(Tau, co2_flux, data =
        testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))

