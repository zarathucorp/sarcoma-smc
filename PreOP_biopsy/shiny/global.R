library(readxl)  ## For xlsx read
library(dplyr)   ## For %>% (ctrl + M 누르면 나옴)
library(data.table)
## Set directory
setwd("/home/js/ShinyApps/LeeKW/sarcoma_preOP_biopsy")

## Read all sheet
a <- excel_sheets("sarcoma data sheet SMC 20200604.xlsx") %>% 
  lapply(function(x){read_excel("sarcoma data sheet SMC 20200604.xlsx", sheet = x, skip = 2, na = c("UK", "ND"))})


## Merge sheet 1-7, by 환자번호
b <- a[[1]] %>% left_join(a[[2]], by = "환자번호") %>% left_join(a[[3]], by = "환자번호") %>% left_join(a[[4]], by = "환자번호") %>% 
  left_join(a[[5]], by = "환자번호") %>% left_join(a[[6]], by = "환자번호") %>% left_join(a[[7]], by = "환자번호")

## Missing values
b[["ECOG\r\n\r\n0/1/2/3/4"]][which(is.na(b[["ECOG\r\n\r\n0/1/2/3/4"]]))] <- "0"                ## Missing as 0
b[["EBL\r\n(ml)"]] <- ifelse(b[["EBL\r\n(ml)"]] == "UK", NA, as.numeric(b[["EBL\r\n(ml)"]]))   ## Missing(UK) as NA

## Define Age
b$Age <-  as.numeric(b[["수술날짜\r\n\r\ndd-mm-yyyy"]] - b[["생년월일\r\n\r\ndd-mm-yyyy"]])/365.25

## 변수만들기 
c <- b %>% filter(`Primary 수술여부\r\n\r\n0. Primary tumor\r\n1. Residual after incomplete resection\r\n2. Local recurrence.x` == "0" & `환자번호` != "21733889") %>% 
  filter(!(`수술 전 Biopsy\r\n\r\n0. None\r\n1. Primary site\r\n2. Local recurrence site\r\n3. Metastatic site` == 1 & `Type of needle\r\n\r\n0. Core\r\n1. FNA\r\n2. N/A\r\n3. Unknown` %in% 2:3)) %>% 
  mutate(biopsy_preop_primary = as.integer(`수술 전 Biopsy\r\n\r\n0. None\r\n1. Primary site\r\n2. Local recurrence site\r\n3. Metastatic site` == "1"),
         type_needle = `Type of needle\r\n\r\n0. Core\r\n1. FNA\r\n2. N/A\r\n3. Unknown`) %>% 
  mutate(type_needle = ifelse(type_needle == 0, "Core needle", ifelse(type_needle == 1, "FNA", "Excisional biopsy")))


## 변수 모아나갈 데이터
out <- c %>% select(환자번호, Age, `성별\r\n\r\nM/F`, biopsy_preop_primary, type_needle)
names(out)[3] <- "Sex"                                                                  ## 변수명 바꾸기
#out
out[out$`환자번호` == 31050857, "Sex"] <- "F"


## PreOp biopsy
out$liposarcoma_preop <- as.integer(c[["preOP Bx. 결과\r\n\r\n0. WD \r\n1. DD \r\n2. Pleomorphic \r\n3. LMS\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other"]] %in% c("0", "1", "1or 2", "2"))
out$liposarcoma_postop <- as.integer((c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other.y"]] %in% c(0, 1, 2)) |
                          (c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other.y"]] == 7) &
                             grepl("liposarcoma|Liposarcoma", c[["Other \r\n\r\ncomment"]]))  


## Death
out$death <- as.integer(c[["사망여부\r\n\r\n0.Alive\r\n1.Dead\r\n2.Unknown.y"]])
out$death <- ifelse(out$death == 2, NA, out$death)
## 관찰기간
out$day_FU <- as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]] - c[["수술날짜\r\n\r\ndd-mm-yyyy"]])

out$recur_local <- c[["재발#1\r\n\r\n0: 무\r\n1: 유.x"]]
out$recur_site <- c$`Site of local recurrence`
out$recur_site <- ifelse(out$recur_site == "6", NA, out$recur_site)
out$recur_day <- ifelse(c[["재발#1\r\n\r\n0: 무\r\n1: 유.x"]] == 1, 
                        as.numeric(as.Date(as.integer(c[["Date of local recurrence"]]), origin = "1899-12-30") - as.Date(c[["수술날짜\r\n\r\ndd-mm-yyyy"]])),
                        as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]] - c[["수술날짜\r\n\r\ndd-mm-yyyy"]]))
out$recur_day[is.na(out$recur_day)] <- as.numeric(as.Date("02/10/18", "%m/%d/%y") - as.Date("10/02/12", "%m/%d/%y"))


out$Rtx_dose <- c[["RT dose\r\n(Gy)"]]


cond1 <- c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]] %in% c("1", "5")
cond2 <- (c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]] == "4") & (c[["Tisuue expander insertion \r\n유뮤\r\n\r\n0. No\r\n1. Yes"]] == "1")

out$Rtx_tissue_expander <- as.integer(cond1 | cond2)


out$Rtx_preop <- as.integer(c[["수술전 \r\nRT 여부\r\n\r\n0.No\r\n1.Yes"]])
out$Chemo_preop <- as.integer(c[["수술전 \r\nChemo 여부\r\n\r\n0.No\r\n1.Yes"]])
out$Neoadjuvant <- as.integer(out$Rtx_preop | out$Chemo_preop)


out$meta_lung <- c[["Lung metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$meta_liver <- c[["Liver metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$meta_liver <- ifelse(out$meta_liver == 3, NA, out$meta_liver)
out$meta_bm <- c[["Bone metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$meta_abd <- c[["Intra-abdominal metastasis\r\n\r\n0. No\r\n1. Yes"]]
out$multifocal <- c[["Mutifocality 여부\r\n\r\n0. No\r\n1. Yes"]]


## 동반절제 장기 수 
info.resection <- c %>% 
  select(starts_with("동반절제")) %>% 
  mutate_at(1:25, as.integer) %>%                        ## Other comment 빼고 나머지
  mutate_at(26, function(x){as.integer(!is.na(x))})      ## Other comment는 결측이면 0 아니면 1

out$num_resected_organ <- rowSums(info.resection, na.rm = T)

## Rtx: preOP RTx or postop RTx with tissue expander, Rtx dose 는 아까 만듦.
out$Rtx_total <- as.integer(c[["수술 전후 RT 여부\r\n\r\n0.No\r\n1.Yes"]])

## Chemo: Preop(neoadjuvant) 는 아까함 
out$Chemo_postop <- as.integer(c[["Adjuvant chemo 여부\r\n\r\n0.No\r\n1.Yes"]])
out$Chemo_both <- as.integer(out$Chemo_preop | out$Chemo_postop)

out$tumor_size<-c[["종양 크기\r\n(Tumor size, mm)\r\n다발성인 경우 largest tumor size"]]
out$resection_margin <- as.integer(c[["Surgical margins\r\n\r\n0. R0/R1\r\n1. R2\r\n2. Not available"]])
out$resection_margin <- ifelse(out$resection_margin == 2, NA, out$resection_margin)

out$FNCLCC_grade <- c[["FNCLCC grade\r\n\r\n1. total score 2-3\r\n2. total score 4-5\r\n3. total score 6,7,8"]]
out$FNCLCC_grade <- ifelse(out$FNCLCC_grade == "UK", NA, out$FNCLCC_grade)

out$sarcomatosis_pattern <- as.integer(c[["Site of local recurrence"]] == "4")


## Variable list: For select UI in ShinyApps
varlist <- list(
  Base = c("biopsy_preop_primary", "Age", "Sex", "type_needle", "liposarcoma_preop", "liposarcoma_postop", "recur_site", names(out)[12:ncol(out)]),
  Event = c("death", "recur_local", "sarcomatosis_pattern"),
  Day = c("day_FU", "recur_day")
)


## Exclude 환자번호 :이제부터 data.table 패키지 사용
out <- data.table(out[, -1])

## 범주형 변수: 범주 5 이하, recur_site
factor_vars <- c(names(out)[sapply(out, function(x){length(table(x))}) <= 5], c( "recur_site"))
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]


## Label: Use jstable::mk.lev 
library(jstable)
out.label <- mk.lev(out)

## Label 0, 1 인건 No, Yes 로 바꿈
vars.01 <- names(out)[sapply(lapply(out, levels), function(x){identical(x, c("0", "1"))})]

for (v in vars.01){
  out.label[variable == v, val_label := c("No", "Yes")]
}

## Label: preop primay Biopsy
out.label[variable == "biopsy_preop_primary", `:=`(var_label = "PreOP biopsy", val_label = c("No", "Yes"))]
