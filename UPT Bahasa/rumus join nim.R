
update.packages ()
pacman::p_load(pacman, dplyr, rio, RODBC, magrittr)

setwd("C:/Users/ASUS/Desktop")

dta <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/ASUS/OneDrive - Universitas Terbuka/UPT Bahasa/2 Yosi Folder/Database Projects/UPT BAHASA Database Master.accdb")

tabel1 <- sqlFetch(dta, "mhs_data_T")

tabel2 <- read.delim(file = "clipboard", header = T) %>% as_tibble()

# Cek duplikat NIM
tabel2$nim[duplicated(tabel2$nim)]


tabel1$tanggal_lahir <- as.Date(tabel1$tanggal_lahir, tz = "Asia/Bangkok", format = "%d-%m-%y")
tabel2$nim <- as.character(tabel2$nim)
tabel1$nama <- toupper(tabel1$nama)
tabel1$tempat_lahir <- toupper(tabel1$tempat_lahir)
tabel2$nama <- toupper(tabel2$nama)
tabel_join <- left_join(tabel2, tabel1, by = "nim")
tabel_join1 <- tabel_join %>% dplyr::select(nama.x, nim, tempat_lahir, tanggal_lahir, sex) %>% arrange(nama.x)



tabel_join2 <- tabel_join1 %>% group_by(nim) %>% slice(1) %>% arrange(nama.x)



# Export 1 by 1
export(tabel_join2, file = "BDP 6C.xlsx")

# Join all file

name_file <- as.list(sprintf("%s.xlsx", seq(1:2)))

import_file <- lapply(name_file, import)
export(import_file, "tphp.xlsx")

# book <- loadWorkbook("tabel_join.xlsx")
# createSheet(book, kelas)
# writeWorksheet(book, tabel_join2, sheet = kelas)
# saveWorkbook(book, file = "tabel_join.xlsx")







