#Kode Analisis Deskriptif
#Aktivasi package dplyr + paket lainnya yg tergabung dalam grup tdyverse
library(tidyverse)

#membaca data dari URL
#menggunakan library readr
library(readr)
#membaca data crime(kejahatan) di kampus2 
c_data <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/c_data.csv")
names(c_data)
summary(c_data)
table(c_data$region)
table(c_data$type)
table(c_data$region,c_data$type)
hist(c_data$nvrate)
hist(c_data$nv)
hist(c_data$region)
mean(c_data$nv)

#membaca data
pef <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")
smoke <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/smoking.csv")
stroke <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/stroke_new.csv")
w5 <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")
datapef <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/datapef.csv")
datapef = datapef %>% select(-1)
pef  = pef %>% select(pidlink, age, height, pef)
smoke = smoke %>% select(pidlink, smoking)
stroke = stroke %>% select(pidlink, event)
w5 = w5 %>% select(pidlink, sex, hyper, agegr)

summary(pef)
pef = pef %>% filter(!is.na(pef)&!is.na(height))
pef %>% 
  count(pidlink) %>% 
  filter(n > 1)

summary(smoke)
table(smoke$smoking)
smoke %>% 
  count(pidlink) %>% 
  filter(n > 1)

summary(stroke)
stroke <- stroke %>%
  mutate(pidlink = as.numeric(pidlink))
table(stroke$event) #1=yes; 3=no
stroke %>% 
  count(pidlink) %>% 
  filter(n > 1)

summary(w5)
w5 <- w5 %>%
  mutate(pidlink = as.numeric(pidlink))
w5 = w5 %>% filter(!is.na(pidlink))
w5 %>% 
  count(pidlink) %>% 
  filter(n > 1)

datapef = left_join(pef, smoke, by = "pidlink")
datapef = left_join(datapef, stroke, by = "pidlink")
datapef = left_join(datapef, w5, by = "pidlink")
summary(datapef)
datapef %>% 
  count(pidlink) %>% 
  filter(n > 1)

write.csv(datapef,"datapef.csv")

#melihat daftar variable dari datapef
names(datapef)

#mengaktifkan library/package gt
library(gt)

#membuat table frekuensi
tab1 = table(datapef$hyper)
variables <- row.names(tab1)

#membuat tabel silang
tab1 = table(datapef$hyper,datapef$sex)
tab1

# 1. Get raw counts
counts <- table(datapef$hyper)
counts1 <- table(datapef$hyper,datapef$sex)
counts1

# 2a. Get proportions (dari total observasi)
proportions <- prop.table(counts)
proportions0 <- prop.table(counts1)
proportions0
percentage0 = round(proportions0,3)*100
percentage0

# 2b. Get proportions (dari total baris /by row)
proportions1 <- prop.table(counts1,1)
proportions1
percentage1 = round(proportions1,3)*100
percentage1

# 2c. Get proportions (dari total kolom /by column)
proportions2 <- prop.table(counts1,2)
proportions2
percentage2 = round(proportions2,3)*100
percentage2

# 3 uji statistik membandingkan proporsi antara kelompok
chisq.test(datapef$sex,datapef$hyper,correct = TRUE)
