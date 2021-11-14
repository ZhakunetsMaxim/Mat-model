#������� �.�., ��� 123
#������� 5
#������� 2
# C������� ������ �������������� �������� ��������� �������
# ������� ����������� ���� �� �������� ������ 2013 ���� �� ������ ���������
# ������� ������������ ���������.

#�������� � �������� ������� ����������
setwd("C:/mat");
getwd()
#������ � ������������ � ���������� �������

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
# ��������� �� ���� ���������� � ��� ����� ������������ �������� glimpse(),
# ������� ����� �������� ������������ ������ ��������� ����������,
# ������� ��� ���� ������������ ������� ������
glimpse(eddypro)
#�������� ������ ������ � ��������� ������� ������� "roll"
eddypro = select(eddypro, -(roll))

#��������� ����������� �������� � �������� ������� �� ���������� ��� ���������� ��������
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
# ��� ���������� �������� ������ �� �������� ������, ������� �����
eddypro=filter(eddypro, DOY>243&DOY<335,daytime==TRUE)
#����������� ���������� ���� char � �������
eddypro = eddypro %>% mutate_if(is.character, as.factor)
# ��� ���������� �������� ��� ���������� ���� ���� numeric,
# ������������� ��������� saplly � is.numeric
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
str(eddypro_numeric)
# ��������� ���� ����������
cor_eddy=cor(eddypro_numeric)
str(cor_eddy)

# ���������� ���������� �� na
# ��� ������ ���� ��������� � ������� ������� NA � ������ ����������
# ������������� �������� summarise_all � sum
na_cor_eddy=eddypro_numeric %>% summarise_all(~sum(is.na(.x)))
navect=na_cor_eddy[1,]%>% as.integer()
# ��������� � ����� ���������� ���-�� NA ��������� 30
names(eddypro_numeric)[navect>30]
# �������� ��� na
eddypro_numeric=na.exclude(eddypro_numeric)
# ����� ��������� ���� ����������
cor_eddy=cor(eddypro_numeric)
cor_eddy=data.frame(cor_eddy)
#������ ���� ������������ ��� ����� ��������� ����������
cor_vars=cor_eddy$co2_flux^2
names(cor_vars)=names(cor_eddy)
# ������ ������ �������� ������������, � ������� ���� ������������ ����� 0,16
cor_vars=cor_vars[cor_vars>0.16]
# ������ ����� �������� ����������
names(cor_vars)%>% na.exclude()

#�������� ���������������� �������
row_numbers = 1:length(eddypro_numeric$co2_flux)
#���������
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
#�����������
test = row_numbers[-teach]
#���������
teaching_edd = eddypro_numeric[teach,]
#�����������
testing_edd = eddypro_numeric[test,]

# ������������� ���������


#������ 1
# �������� ������ �� ��������� ��� �������� ����������
mod1 = lm(data = teaching_edd, co2_flux~ DOY+h2o_molar_density+h2o_mole_fraction
          +h2o_mixing_ratio+air_density+air_heat_capacity+air_molar_volume
          +water_vapor_density+e+specific_humidity+Tdew+un_co2_flux+w_div_co2_cov
          +h2o+h2o_1+flowrate)
# ��������� ������������
coef(mod1)
#�������
resid(mod1)
# ������������� ��������
confint(mod1)
# ��������� �-�������� �� ������
summary(mod1)
# ����������� ������������ = 0,569
# �������� ������������� ������
anova(mod1)
# �������� �� �� ����� ����� ���������� � ��� �� ��������:
# air_molar_volume,water_vapor_density,specific_humidity,w_div_co2_cov,h2o,flowrate
##
#�������� ������� ����������� �������������:
  plot(mod1,2)
# � ����� ������ ������������ ���������
# �������� ������ ����������� �������� �� ������������� ��������
plot(mod1$fitted.values, teaching_edd$co2_flux)
# ������� ����� �=�
abline(a=0, b=1, col="red")

# �������� ������ ����������� �������� �� ����������� ��������
plot(teaching_edd$co2_flux,mod1$residuals)
# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � CO2
mo1=lm(mod1$residuals~teaching_edd$co2_flux)
abline(a=mo1$coefficients[1],b=mo1$coefficients[2],col="red")
# ����������� ����������� �������� �� ����������� ��������


#������ 2
# �������� ����������� ������ ��� ���� ����� ������ ����������� ����� �����������
# ������� �������
mod2 = lm(data = teaching_edd, co2_flux~ (DOY+h2o_molar_density+h2o_mole_fraction+
                                            h2o_mixing_ratio+air_density+air_heat_capacity+
                                            air_molar_volume+water_vapor_density+e+
                                            specific_humidity+Tdew+un_co2_flux+w_div_co2_cov+h2o+h2o_1+flowrate)^2)
# ��������� ������������
coef(mod2)
#�������
resid(mod2)
#������������� ��������
confint(mod2)
#P-�������� �� ������
summary(mod2)
# ����������� ������������ = 0,8452
#������������� ������
anova(mod2)
# �������� �� �� ����� ����� ���������� 1 � 2-�� ������� � ��� �� ��������:
#air_molar_volume,water_vapor_density,specific_humidity,w_div_co2_cov,h2o,flowrate � ��.
# ������ ���� � ������ ������� ��� ���������� �� �������, �� �� 2-� ����� ���� �����������
#������� �� ���������� ������������� ������ :
plot(mod2,2)
# �������� ������ ����������� �������� �� ������������� ��������
plot(mod2$fitted.values, teaching_edd$co2_flux)
# ������� ����� �=�
abline(a=0, b=1, col="blue")
# �������� ������ �������� �� ����������� ��������
plot(teaching_edd$co2_flux,mod2$residuals)
# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � CO2
mo2=lm(mod2$residuals~teaching_edd$co2_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="blue")


#������ 3
# ��� ��� ���������� 1 ������� ����� ����� ����� � ���������� 2-�� �������, �� �� �� ���������
# �������� ������ ��������� ��������� 1 ������� specific_humidity, ��� ��� ����� � ������� �����������
# �������� ���������, � ����� �������� ��������� ���������� ����������
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
#������������
coef(mod3)
#�������
resid(mod3)
#������������� ��������
confint(mod3)
#P-�������� �� ������
summary(mod3)
# ����������� ������������ = 0,6992, ������ ��� �� 2 ������,
# ����� �������, ����� ������ ������ 2 ����� �������� ������
#������������� ������
anova(mod3)
#������� �� ���������� ������������� ������ :
plot(mod3,2)
# �������� ������ ����������� �������� �� ������������� ��������
plot(mod3$fitted.values, teaching_edd$co2_flux)
# ������� ����� �=�
abline(a=0, b=1, col="green")
# �������� ������ �������� �� ����������� ��������
plot(teaching_edd$co2_flux,mod3$residuals)
# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � CO2
mo3=lm(mod3$residuals~teaching_edd$co2_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")


#������ 4
# �� ��������� �� 3 ������ ������ ���������� ����������
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
#������������
coef(mod3)
#�������
resid(mod3)
#������������� ��������
confint(mod3)
#P-�������� �� ������
summary(mod3)
# ����������� ������������ = 0,6992, ������ ��� �� 2 ������,
# ����� �������, ����� ������ ������ 2 ����� �������� ������
#������������� ������
anova(mod3)
#������� �� ���������� ������������� ������ :
plot(mod3,2)
# �������� ������ ����������� �������� �� ������������� ��������
plot(mod3$fitted.values, teaching_edd$co2_flux)
# ������� ����� �=�
abline(a=0, b=1, col="green")
# �������� ������ �������� �� ����������� ��������
plot(teaching_edd$co2_flux,mod3$residuals)
# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � CO2
mo3=lm(mod3$residuals~teaching_edd$co2_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")




##�������� �������
#��������� ������ 2, ������� � ������������� ��������

#������ ������
qplot(co2_flux , co2_flux, data = teaching_edd) + geom_line(aes(y = predict(mod2, teaching_edd)))

#������ ������
qplot(co2_flux , co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))


#������ ������� �� ��������� ����������, �� ����� ������� ����� �������� ������������ co2_flux �� ����������� � ������ ����������
#� ������ ������������� ����� ������ ������ ����� ��� �����, ��� ��� ����� ����� � ��� �� ����������� �������

#�������
qplot(DOY, co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))
qplot(Tau, co2_flux, data =
        testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))

