library(readxl)
library(stringr)
library(tidyverse)

prodi <- read_excel("C://Users/raziv/Documents/Semester 5/PSD/Program_Studi_UB.xlsx")
author <- read_excel("C://Users/raziv/Documents/Semester 5/PSD/Detail_Authors_404.xlsx")

#FILTER DAN SUBSETTING DATA
prodi <- prodi[which(prodi$Status == "Aktif"),]
prodi$Program_Studi <- str_c(prodi$Jenjang," - ", prodi$Program_Studi)
colnames(prodi)[2] <- "Departemen"
colnames(prodi)[1] <- "Kode_Departemen"
prodi <- prodi[,c(1,2,3,5)]

#CHECK KODE DEPARTEMEN YANG SAMA
tablekode <- as.data.frame(table(prodi$Kode_Departemen))
sum(tablekode$Freq != 1)

#MERGING DATA
data <- left_join(author,prodi,by="Departemen")
#MEMPERBAIKI KOMA DALAM DATA
data$SINTA_Score_Overall <- as.numeric(gsub(",", ".", gsub("\\.", "", data$SINTA_Score_Overall)))
data$SINTA_Score_3Yr <- as.numeric(gsub(",", ".", gsub("\\.", "", data$SINTA_Score_3Yr)))
#MEMBUAT VARIABEL BARU KATEGORI SINTA SCORE OVERALL
data <- data %>%
  mutate(
    Kategori_Sinta_Score = dplyr::case_when(
      SINTA_Score_Overall < mean(data$SINTA_Score_Overall)     ~ "Di Bawah Rata-Rata",
      SINTA_Score_Overall >= mean(data$SINTA_Score_Overall)    ~ "Di Atas Rata-Rata"
    ),
    Kategori_Sinta_Score = factor(
      Kategori_Sinta_Score, 
      levels = c("Di Bawah Rata-Rata","Di Atas Rata-Rata")
    )
  )

#Sorting Data
data[c("Jenjang","Departemen")] <- str_split_fixed(data$Departemen," - ",2)
data <- data %>% relocate(`Kode_Departemen`, .before = Departemen)
data <- data %>% relocate(`Jenjang`, .before = Departemen)
data <- data %>% relocate(`Status`, .before = `Subject List`)
data <- data %>% relocate(`Akreditasi`, .before = `Subject List`)
data <- data %>% relocate(Kategori_Sinta_Score , .before = SINTA_Score_3Yr)

#MEMPERBAIKI PENULISAN SUBJECT LIST
data$`Subject List` <- str_to_title(data$`Subject List`)
data$`Subject List` <- gsub(" Of "," of ", data$`Subject List`)
data$`Subject List` <- gsub(" And "," and ", data$`Subject List`)
data$`Subject List` <- gsub(" In "," in ", data$`Subject List`)
data$`Subject List` <- gsub(" Dan "," dan ", data$`Subject List`)
data$`Subject List` <- gsub("Ui/Ux","UI/UX", data$`Subject List`)
rio::export(data, "Data Managing.xlsx")


