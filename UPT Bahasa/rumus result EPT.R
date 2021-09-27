# Load Packages
pacman::p_load(pacman, tidyverse, rio, RODBC, magrittr)
rm(list=ls())

# Set Working Directory
setwd("C:/Users/ASUS/Desktop")


# Connect to Database
dta <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/Cloud Data/OneDrive - Universitas Terbuka/UPT Bahasa/2 Yosi Folder/Database Projects/UPT BAHASA Database Master.accdb")

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

tahun <- "2020"
bulan <- "07"
tanggal <- "08"

for(i in 6:13){
        assign(paste0("a",i,sep=""),
               import(paste("TOEFL_",i,"_",tahun,"_",bulan,"_",tanggal,"_Result.xlsx", sep = "")) %>%
                       as_tibble())
}

# Simplify Table Result

for(i in 6:13){
        x <- paste0("a",i,sep="")
        assign(x, dplyr::select(get(x), id_peserta = `No. Peserta`, 
                                l1 = `Section 1`, 
                                g1 = `Section 2`, 
                                r1 = `Section 3`))
        
}

for(i in 6:13){
        x <- paste0("a",i,sep="")
        assign(x, dplyr::mutate(get(x), id_peserta = as.numeric(id_peserta), 
                                l1 = as.numeric(l1), 
                                g1 = as.numeric(g1), 
                                r1 = as.numeric(r1)))
        
}


# combine result table (a table)
b <- bind_rows(a6, a7, a8, a9, a10, a11, a12, a13) %>% # replace_na(list(l1 = 0, g1 = 0, r1 = 0))
min_b <- b %>% summarize(min(id_peserta)) %>% pull()

# simplify score ept table (b table)
a <- tbl_mhs_score_toefl %>% dplyr::filter(ID >= min_b) %>% dplyr::select(id_peserta = ID, ISO = mhs_daftar_toefl_ID, centang)

# Join a and b table
result <- left_join(a, b, by = "id_peserta", suffix = c(".a", ".b")) 
result <- result %>% filter(!is.na(l1))
result <- result %>% mutate(l1 = ifelse(l1==0,1,l1),
                            g1 = ifelse(g1==0,1,g1),
                            r1 = ifelse(r1==0,1,r1))
result <- result %>% mutate(centang = ifelse(!is.na(l1), 1, 0)) 

# Calculate score
tbl_score <- merge(result, tbl_rumus_toefl_listening, by.x = "l1", by.y = "ID", all.x = T) 
tbl_score <- merge(tbl_score, tbl_rumus_toefl_grammar, by.x = "g1", by.y = "ID", all.x = T) 
tbl_score <- merge(tbl_score, tbl_rumus_toefl_reading, by.x = "r1", by.y = "ID", all.x = T) 


tbl_score %<>% as_tibble() %>% arrange(id_peserta)
tbl_score <- tbl_score %>% mutate(skor = ((listening+grammar+reading)/3)*10) 

tbl_score$skor <- round(tbl_score$skor, digits = 0)

tbl_score %<>% mutate(skor = ifelse(skor < 310, 310, skor) )

tbl_score <- tbl_score %>% dplyr::select(ID = id_peserta, l1, g1, r1, l2 = listening, g2 = grammar, r2 = reading, skor, hadir = centang)
tbl_score %<>% arrange(ID)

export(tbl_score, file = "tbl_score.xlsx")
