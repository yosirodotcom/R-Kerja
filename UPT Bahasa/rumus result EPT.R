# Load Packages
pacman::p_load(pacman, dplyr, tidyr, rio, RODBC, magrittr)
rm(list=ls())

# Set Working Directory
setwd("C:/Users/ASUS/Downloads")


# Connect to Database
dta <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=G:/Other computers/My computer/Documents/8 Database/New UPTBAHASA - v37.accdb")

# Load Tables

tbl_rumus_toefl_listening <- sqlFetch(dta, "tbl_RumusToeflListening") %>% as_tibble()
tbl_rumus_toefl_grammar <- sqlFetch(dta, "tbl_RumusToeflGrammar") %>% as_tibble()
tbl_rumus_toefl_reading <- sqlFetch(dta, "tbl_RumusToeflReading") %>% as_tibble()

tbl_rumus_toefl_listening$ID <- as.numeric(tbl_rumus_toefl_listening$ID)
tbl_rumus_toefl_grammar$ID <- as.numeric(tbl_rumus_toefl_grammar$ID)
tbl_rumus_toefl_reading$ID <- as.numeric(tbl_rumus_toefl_reading$ID)


tbl_toefl <- sqlFetch(dta, "tbl_EPT_skor") %>% as_tibble()


# Load Table Result

tahun <- "2023"
bulan <- "06"
tanggal <- "22"
versi <- "6"

## Untuk 1 versi
b <- import(paste("EPT_",versi,"_",tahun,"_",bulan,"_",tanggal,"_Result.xlsx", sep = "")) %>%
        as_tibble()
b <- dplyr::select(b, id_peserta = `No. Peserta`,
                   l1 = `Section 1`,
                   g1 = `Section 2`,
                   r1 = `Section 3`)
b <- dplyr::mutate(b, id_peserta = as.numeric(id_peserta),
                   l1 = as.numeric(l1),
                   g1 = as.numeric(g1),
                   r1 = as.numeric(r1))

## Untuk banyak versi

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
b <- bind_rows(a6, a7, a8, a9, a10, a11, a12, a13) #%>%  replace_na(list(l1 = 0, g1 = 0, r1 = 0))


min_b <- b %>% summarize(min(id_peserta)) %>% pull()

# simplify score ept table (b table)
a <- tbl_toefl %>% dplyr::filter(ID >= min_b) %>% dplyr::select(id_peserta = ID, ISO = EPT_Daftar_ID, Hadir)

# Join a and b table
result <- left_join(a, b, by = "id_peserta", suffix = c(".a", ".b")) 
# result <- result %>% filter(!is.na(l1))
# result <- result %>% mutate(l1 = ifelse(l1==0,1,l1),
#                             g1 = ifelse(g1==0,1,g1),
#                             r1 = ifelse(r1==0,1,r1))
result <- result %>% mutate(Hadir = ifelse(!is.na(l1), 1, 0)) 

# Calculate score
tbl_score <- merge(result, tbl_rumus_toefl_listening, by.x = "l1", by.y = "ID", all.x = T) 
tbl_score <- merge(tbl_score, tbl_rumus_toefl_grammar, by.x = "g1", by.y = "ID", all.x = T) 
tbl_score <- merge(tbl_score, tbl_rumus_toefl_reading, by.x = "r1", by.y = "ID", all.x = T) 


tbl_score %<>% as_tibble() %>% arrange(id_peserta)
tbl_score <- tbl_score %>% mutate(skor = ((listening+grammar+reading)/3)*10) 

tbl_score$skor <- round(tbl_score$skor, digits = 0)

tbl_score %<>% mutate(skor = ifelse(skor < 310, 310, skor) )

tbl_score <- tbl_score %>% dplyr::select(ID = id_peserta, l1, g1, r1, l2 = listening, g2 = grammar, r2 = reading, skor, hadir = Hadir)
tbl_score %<>% arrange(ID)

export(tbl_score, file = "tbl_score.xlsx")
