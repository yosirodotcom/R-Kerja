# Load Packages
pacman::p_load(pacman, dplyr, rio, RODBC)


# Set Working Directory
setwd("C:/Users/ASUS/Desktop")


# Connect to Database
dta <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/ASUS/OneDrive - Universitas Terbuka/UPT Bahasa/2 Yosi Folder/Database Projects/UPT BAHASA Database Master.accdb")

# Load Tables
tbl_mhs_data <- sqlFetch(dta, "mhs_data_T") %>% as_tibble()
tbl_mhs_kelas <- sqlFetch(dta, "mhs_kelas_T") %>% as_tibble()
tbl_dosen <- sqlFetch(dta, "dosen") %>% as_tibble()
tbl_jurusan <- sqlFetch(dta, "jurusan") %>% as_tibble()
tbl_pengawas <- sqlFetch(dta, "pengawas") %>% as_tibble()
tbl_prodi <- sqlFetch(dta, "prodi") %>% as_tibble()
tbl_mhs_daftar_toefl <- sqlFetch(dta, "mhs_daftar_toefl_T") %>% as_tibble()
tbl_mhs_score_toefl <- sqlFetch(dta, "mhs_score_toefl_T") %>% as_tibble()
tbl_mhs_daftar_toeic <- sqlFetch(dta, "mhs_daftar_toeic_T") %>% as_tibble()
tbl_mhs_score_toeic <- sqlFetch(dta, "mhs_score_toeic_T") %>% as_tibble()
tbl_rumus_toefl_grammar <- sqlFetch(dta, "rumus_toefl_grammar") %>% as_tibble()
tbl_rumus_toefl_listening <- sqlFetch(dta, "rumus_toefl_listening") %>% as_tibble()
tbl_rumus_toefl_reading <- sqlFetch(dta, "rumus_toefl_reading") %>% as_tibble()
tbl_rumus_toeic_grammar <- sqlFetch(dta, "rumus_toeic_reading") %>% as_tibble()
tbl_rumus_toeic_listening <- sqlFetch(dta, "rumus_toeic_listening") %>% as_tibble()
tbl_rumus_toeic_grammar$ID <- as.numeric(tbl_rumus_toeic_grammar$ID)
tbl_rumus_toeic_listening$ID <- as.numeric(tbl_rumus_toeic_listening$ID)



# Load Table Result
# Load Table Result

tahun <- "2022"
bulan <- "09"
tanggal <- "14"

for(i in 3:10){
        assign(paste0("a",i,sep=""),
               import(paste("Toeic",i,"_",tahun,"_",bulan,"_",tanggal,"_Result.xlsx", sep = "")) %>%
                       as_tibble())
}

# b3 <- import("B3.xlsx") %>% as_tibble()
# b4 <- import("B4.xlsx") %>% as_tibble()
# b5 <- import("B5.xlsx") %>% as_tibble()
# b6 <- import("B6.xlsx") %>% as_tibble()
# b7 <- import("B7.xlsx") %>% as_tibble()
# b8 <- import("B8.xlsx") %>% as_tibble()
# b9 <- import("B9.xlsx") %>% as_tibble()
# b10 <- import("B10.xlsx") %>% as_tibble()


# Simplify Table Resutl

for(i in 3:10){
        x <- paste0("a",i,sep="")
        assign(x, dplyr::select(get(x), id_peserta = `No. Peserta`, 
                                l1 = `Section 1`, 
                                r1 = `Section 2`))
        
}

for(i in 3:10){
        x <- paste0("a",i,sep="")
        assign(x, dplyr::mutate(get(x), id_peserta = as.numeric(id_peserta), 
                                l1 = as.numeric(l1),
                                r1 = as.numeric(r1)))
        
}


# b3 <- b3 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b3$l1 <- as.numeric(b3$l1)
# b3$r1 <- as.numeric(b3$r1)
# b3$id_peserta <- as.numeric(b3$id_peserta)
# b4 <- b4 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b4$l1 <- as.numeric(b4$l1)
# b4$r1 <- as.numeric(b4$r1)
# b4$id_peserta <- as.numeric(b4$id_peserta)
# b5 <- b5 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b5$l1 <- as.numeric(b5$l1)
# b5$r1 <- as.numeric(b5$r1)
# b5$id_peserta <- as.numeric(b5$id_peserta)
# b6 <- b6 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b6$l1 <- as.numeric(b6$l1)
# b6$r1 <- as.numeric(b6$r1)
# b6$id_peserta <- as.numeric(b6$id_peserta)
# b7 <- b7 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b7$l1 <- as.numeric(b7$l1)
# b7$r1 <- as.numeric(b7$r1)
# b7$id_peserta <- as.numeric(b7$id_peserta)
# b8 <- b8 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b8$l1 <- as.numeric(b8$l1)
# b8$r1 <- as.numeric(b8$r1)
# b8$id_peserta <- as.numeric(b8$id_peserta)
# b9 <- b9 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b9$l1 <- as.numeric(b9$l1)
# b9$r1 <- as.numeric(b9$r1)
# b9$id_peserta <- as.numeric(b9$id_peserta)
# b10 <- b10 %>% dplyr::select(id_peserta = `No. Peserta`, l1 = `Section 1`, r1 = `Section 2`) 
# b10$l1 <- as.numeric(b10$l1)
# b10$r1 <- as.numeric(b10$r1)
# b10$id_peserta <- as.numeric(b10$id_peserta)

# combine result table (a table)
b <- bind_rows(a3, a4, a5, a6, a7, a8, a9, a10) # %>% replace_na(list(l1 = 0, r1 = 0))
min_b <- b %>% summarize(min(id_peserta)) %>% pull()

# simplify score ect table (b table)
a <- tbl_mhs_score_toeic %>% dplyr::filter(ID >= min_b) %>% dplyr::select(id_peserta = ID, ISO = mhs_daftar_toeic_ID, centang)

# Join a and b table
result <- left_join(a, b, by = "id_peserta", suffix = c(".a", ".b")) 
result <- result %>% mutate(l1 = ifelse(l1==0,1,l1), r1 = ifelse(r1==0,1,r1))
result <- result %>% mutate(centang = ifelse(!is.na(l1), 1, 0)) 

# Calculate score
tbl_score <- merge(result, tbl_rumus_toeic_listening, by.x = "l1", by.y = "ID", all.x = T) 
tbl_score <- merge(tbl_score, tbl_rumus_toeic_grammar, by.x = "r1", by.y = "ID", all.x = T) 
tbl_score %<>% as_tibble() %>% arrange(id_peserta)
tbl_score <- tbl_score %>% mutate(skor = listening+reading) 
tbl_score$listening <- as.numeric(tbl_score$listening)
tbl_score$reading <- as.numeric(tbl_score$reading)
tbl_score$skor <- as.numeric(tbl_score$skor)
#tbl_score <- tbl_score %>% mutate(l1 = ifelse(l1 == 0, NA, l1), r1 = ifelse(r1 == 0, NA, r1))
tbl_score <- tbl_score %>% dplyr::select(ID = id_peserta, l1, r1, l2 = listening, r2 = reading, skor, hadir = centang) 

export(tbl_score, file = "tbl_score.xlsx")
