#Code R untuk UAS Biostatika Dasar Semester Genap 2026

#mengaktifkan library readr
library(readr)

#membaca data pef
datapef = read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/datapef.csv")

#memilih data pef khusus dewasa
#mengaktifkan library dplyr
library(dplyr)
datapef = datapef %>% filter(age>18.0)

# menyiapkan data untuk ujian tiap mahasiswa
set.seed(12345) #ganti angka 12345 dengan 5 digit terakhir npm

# memilih secara random 2500 observasi
sample_size <- 2500
ds12345 <- datapef[sample(nrow(datapef), sample_size), ] #ganti angka 12345 dengan 5 digit terakhir npm

#melihat nama variabel
names(ds12345) #ganti angka 12345 dengan 5 digit terakhir npm

#memilih berdasarkan nama variables
ds12345  = select(ds12345, age,sex,height,smoking, pef) #ganti angka 12345 dengan 5 digit terakhir npm

#mensave data yang digunakan dalam bentuk/format csv
write.csv(ds12345,"ds12345.csv") #ganti angka 12345 dengan 5 digit terakhir npm

#melihat working direktori tempat saving data
getwd()
