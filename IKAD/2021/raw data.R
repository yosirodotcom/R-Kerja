#update.packages()
pacman::p_load(pacman, rio, tidyverse, magrittr)
rm(list = ls())

x1 <- read.table(file = "dosen_an.txt", sep ="\t", colClasses = c("character", "character", "character", "character"), header = T)
tbl_dosen <- x1 %>% as_tibble()

x2 <- read.table(file = "mata_kuliah.txt", sep = "\t", header = T)
tbl_mk <- x2 %>% as_tibble()

x3 <- read.table(file = "dosen_mk.txt", sep = "\t", header = T)
tbl_dosen_mk <- x3 %>% as_tibble()

master1 <- dplyr::left_join(tbl_dosen_mk, tbl_dosen, by = "nama_dosen")
master1 <- left_join(master1, tbl_mk, by = "kode_mk")
master1 %<>% select(-semester.y) 
master1 %<>% rename(semester = semester.x)

## Untuk melihat dosen apa saja yang mengampuh satu mata kuliah yang sama

list_mk <- tbl_mk$mata_kuliah

for(i in list_mk){
        print(master1 %>% 
                      select(nama_pangkat, mata_kuliah, semester, kelas) %>% 
                      filter(mata_kuliah == i, semester == "II") %>% 
                      arrange(kelas))
}

## Untuk melihat daftar mata kuliah per semester
list_semester <- unique(tbl_mk$semester)
x4 <- master1 %>% 
        select(mata_kuliah, semester) %>% 
        arrange(mata_kuliah) %>% 
        distinct()
x4
        

for(i in list_semester){
        print("---------------------")
        print(paste("Semester ", i))
        print("---------------------")
        print(x4 %>% filter(semester == i) %>% select(mata_kuliah) )
}


save.image("D:/Cloud Data/Dropbox/R Programming/R-Kerja/IKAD/2021/raw_data.RData")
