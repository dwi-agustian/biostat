# skript untuk kelas data manajemen dan analisis
#skript R
1+1
#Aktivasi package dplyr
library(dplyr)

#membaca data dari URL
library(readr)
c_data <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/c_data.csv")

library(data.table)
pef <- fread("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

w5 <- fread("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")


#mempelajari struktur data
names(pef)
str(pef)
glimpse(pef)
summary(pef)

names(w5)
str(w5)
glimpse(w5)
summary(w5)


#subsetting data set
#subsetting variables
#memilih berdasarkan nama variables
pef1  = select(pef, pidlink, age, pef)

#memilih berdasarkan huruf yg terkandung dalam variables
pef2 = select(pef, contains("us"))

#memilih dengan mendrop/mendelete nama variables
pef3 = select(pef, -pidlink)

#memilih range variable yang berada diantara dua variables
pef4 = select(pef, height:us09c)

#subsetting observations
#with operator >, <, >=, <=, ==, !=, 
#memilih berdasarkan kriteria kuantitatif tertentu
pef5 = filter(pef, pef > 250)

#memilih berdasarkan range urutan observasi/baris
pef6 = slice(pef, 1:10)

#memilih secara random (random sampling) sejumlah yg ditentukan
pef7 = sample_n(pef, 15, replace = TRUE)

##memilih secara random (random sampling) dengan prosentase
pef8 = sample_frac(pef, 0.5, replace = TRUE)

#memilih berdasarkan kriteria pef & heighttidak missing(NA)
pef_c = filter(pef,!is.na(pef)&!is.na(height))

summary(pef_c)
rm(pef_c)
summary(w5)

#combining dataset (menggabungkan data)
# menggabungkan variables dengan common variable
w5_pef_c_lj = left_join(w5, pef, by = "pidlink")
w5_pef_c_rj = right_join(w5, pef, by = "pidlink")
w5_pef_c_ij = inner_join(w5, pef, by = "pidlink")
w5_pef_c_fj = full_join(w5, pef, by = "pidlink")

summary(w5_pef_c_ij)
summary(w5_pef_c_fj)

#menggabungkan observation dengan struktur variable yg sama
#contoh, misalkan data multicenter (lokasi penelitian di berbagai tempat)
# dataDKI
pef_dki <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef_dki.csv")

# data Jabar
pef_jabar <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef_jabar.csv")

#menggabungkan observasi
pef_dki_jabar = rbind(pef_dki,pef_jabar)
                      
                      
#remove objects
rm(w5_pef_c_fj,w5_pef_c_ij,w5_pef_c_lj,w5_pef_c_rj)


#exploring the data
# check jumlah obs
length(pef$pidlink)

# check jumlah obs pidlink unik
n_distinct(pef$pidlink)

length(w5$pidlink)
n_distinct(w5$pidlink)

# Find duplicated pidlink
pef %>% 
  count(pidlink) %>% 
  filter(n > 1)

w5 %>% 
  count(pidlink) %>% 
  filter(n > 1)



# Remove duplicated rows based on pidlink
w5 = w5 %>% distinct(pidlink, .keep_all = TRUE)

pef = pef %>% distinct(pidlink, .keep_all = TRUE)


# Count the number of full duplicates
sum(duplicated(pef))
sum(duplicated(w5))

str(pef)
str(w5)
str(all_wave5)

# Convert character to number: pidlink
w5 <- w5 %>%
  mutate(pidlink_num = as.numeric(pidlink))

w5 <- w5 %>%
  mutate(pidlink_num = as.integer(pidlink))

#mengcopy paste variable pidlink asli
w5$pidlink_chr = w5$pidlink

#mereplace pidlink dengan versi int(num)
w5$pidlink = w5$pidlink_num

glimpse(w5)


