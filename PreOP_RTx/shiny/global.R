library(readxl)
library(dplyr)

#read data

#setwd("/home/js/ShinyApps/LeeKW/sarcoma_preOP_RT")
a <- excel_sheets("sarcoma data sheet SMC 20200811.xlsx") %>% 
  lapply(function(x){read_excel("sarcoma data sheet SMC 20200811.xlsx",sheet=x,skip=2, na = "UK")})
b <- a[[1]] %>% 
  left_join(a[[2]], by = "환자번호") %>% left_join(a[[3]], by = "환자번호") %>% left_join(a[[4]], by = "환자번호") %>%
  left_join(a[[5]], by = "환자번호") %>% left_join(a[[6]], by = "환자번호") %>% left_join(a[[7]], by = "환자번호")

#Age 계산
b$Age <- as.numeric(b[["수술날짜\r\n\r\ndd-mm-yyyy.x"]] - b[["생년월일\r\n\r\ndd-mm-yyyy"]])/365.25


#c <- b %>% 
#  filter(`Primary 수술여부\r\n\r\n0. Primary tumor\r\n1. Residual after incomplete resection\r\n2. Local recurrence.x`== 0)

c <- b

out <- c %>% select(환자번호,Age,`성별\r\n\r\nM/F`)
names(out)[3] <- "Sex"; names(out)[1] <- "ID"
out$Sex <- as.factor(out$Sex)

out$primaryTumor <- ifelse(c[["Primary 수술여부\r\n\r\n0. Primary tumor\r\n1. Residual after incomplete resection\r\n2. Local recurrence.x"]] == 0, 1, 0)

out$preOpRTx <- ifelse(c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]] %in% c("1", "5"), T,
                       ifelse(c[["RT timing\r\n\r\n0.None \r\n1.Preop only\r\n2. IORT only\r\n3.Preop + IORT\r\n4.Postop only\r\n5.Preop + postop boost\r\n6.IORT + postop"]] %in% "4", F, NA))


#TE : 삽입(1)=TRUE, 삽입x(0)=FALSE
out$TE <- ifelse(c[["Tisuue expander insertion \r\n유뮤\r\n\r\n0. No\r\n1. Yes"]] == "1", T,
                 ifelse(c[["Tisuue expander insertion \r\n유뮤\r\n\r\n0. No\r\n1. Yes"]]== "0", F, NA))
#Group
#"1" : preOP RTx 시행o
#"2" : preOP PTx 시행x, TE 삽입 x
#"3" : preOP RTx 시행x, TE 삽입 o
#"UK" : 위 셋에 속하지 않는 환자
out$Group <- as.factor(ifelse(out$preOpRTx, "1" , ifelse(out$TE == F, "2", "3")))
out$Group1_23 <- as.factor(ifelse(out$Group == 1, 1, 23))

out$Death<-ifelse(c[["사망여부\r\n\r\n0.Alive\r\n1.Dead\r\n2.Unknown.y"]] == "1", T,
                  ifelse(c[["사망여부\r\n\r\n0.Alive\r\n1.Dead\r\n2.Unknown.y"]]== "0", F, NA)) %>% as.integer
## 관찰기간
out$day_FU <- as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]] - c[["수술날짜\r\n\r\ndd-mm-yyyy.x"]])

out$recur_local <- c[["재발#1\r\n\r\n0: 무\r\n1: 유.x"]]
out$recur_site <- c$`Site of recurrence`
out$recur_site <- ifelse(out$recur_site == "6", NA, out$recur_site)
out$recur_day <- ifelse(c[["재발#1\r\n\r\n0: 무\r\n1: 유.x"]] == 1, 
                        as.numeric(as.Date(as.integer(c[["Date of local recurrence"]]), origin = "1899-12-30") - as.Date(c[["수술날짜\r\n\r\ndd-mm-yyyy.x"]])),
                        as.numeric(c[["마지막 f/u\r\n\r\ndd-mm-yyyy"]] - c[["수술날짜\r\n\r\ndd-mm-yyyy.x"]]))


#BMI
height <- as.numeric(c[["키\r\n(cm)"]])/100
weight <- as.numeric(c[["몸무게\r\n(kg)"]])
out$BMI <- weight/height/height
out$BMI_cat <- factor(ifelse(out$BMI < 18.5, "< 18.5", ifelse(out$BMI < 25, "< 25", ifelse(out$BMI < 30, "< 30", "≥ 30"))))


#DM : 1=TRUE, 0=FALSE
out$DM <- as.integer(c[["DM\r\n\r\n0. No\r\n1.yes"]])

#HTN : 1=TRUE, 0=FALSE
out$HTN <- as.integer(c[["HTN\r\n\r\n0. No\r\n1.yes"]])

#COPD : 1=TRUE, 0=FALSE
out$COPD <- as.integer(c[["COPD\r\n\r\n0. No\r\n1.yes"]])

#Coronary artery disease : 1=TRUE, 0=FALSE
out$CoronaryArteryDisease <- as.integer(c[["Coronary artery disease\r\n\r\n0. No\r\n1.yes"]])

#Chronic renal disease : 1=TRUE, 0=FALSE
out$ChronicRenalDisease <- as.integer(c[["Chronic renal disease\r\n\r\n0. No\r\n1.yes"]])


#Prev abdominal OP Hx : 0="무", 1="유", 2="기타(laparo)"
out$PrevAbdominalOp <- c[["이전\r\nabdominal op Hx \r\n여부\r\n\r\n0. 무\r\n1. 유\r\n2. 기타(laparo)"]]
#out$PrevAbdominalOp <- as.factor(out$PrevAbdominalOp)

#PreOP chemo : 1=TRUE, 0=FALSE
out$preOpChemo <- as.integer(c[["Neoadjuvant chemo 여부\r\n\r\n0.No\r\n1.Yes"]])


#Hb
out$Hb <- as.numeric(c[["수술전 \r\n피검사\r\n\r\nHb\r\n(g/dL)"]])
out$Hb_below9 <- as.integer(out$Hb < 9)  
out$Hb_below10 <- as.integer(out$Hb < 10)


#Albumin
out$Albumin <- as.numeric(c[["수술전 피검사\r\n\r\nAlbumin\r\n(g/dL)"]])
out$Albumin_below3 <- as.integer(out$Albumin < 3)

#PLT
out$PLT <- as.numeric(c[["수술전 피검사\r\n\r\nPlatelet\r\n(1000/uL)"]])
out$PLT_below50 <- as.integer(out$PLT < 50)
out$PLT_below100 <- as.integer(out$PLT < 100)

#PT INR
out$PT_INR <- as.numeric(c[["수술전 피검사\r\n\r\nPT(INR)"]])
out$PT_INR_over1.5 <- as.integer(out$PT_INR > 1.5)

#Tumor size
out$TumorSize <- as.numeric(c[["종양크기\r\nFirst dimension\r\n(mm)"]])




#Tumor histologic subtype
#LPS : 0. WD Liposarcoma / 1. DD Liposarcoma / 2. Pleomorphic Liposarcoma / 7. 중 comment 에 liposarcoma
#nonLPS : 3. Leiomyosarcoma / 4. MPNST / 5. Solitary fibrous tumor / 6. PEComa / 7. 중 comment 에 liposarcoma 없음.
out$Liposarcoma_postop <- as.integer((c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other"]] %in% c(0, 1, 2)) |
                                       (c[["병리결과\r\n\r\n0. WD Liposarcoma\r\n1. DD Liposarcoma\r\n2. Pleomorphic Liposarcoma\r\n3. Leiomyosarcoma\r\n4. MPNST\r\n5. Solitary fibrous tumor\r\n6. PEComa\r\n7. Other"]] == 7) &
                                       grepl("liposarcoma|Liposarcoma", c[["Other \r\n\r\ncomment"]]))  

#FNCLCC grade
out$FNCLCC_grade <- as.factor(c[["FNCLCC grade\r\n\r\n1/2/3/UK"]])

#Tumor Resection
#R0/R1="0", R2="1", other=NA ("2"도 NA에 포함)
out$Resection <- c[["Surgical margins\r\n\r\n0. R0/R1\r\n1. R2: post OP 1주 CT에서 있을시 포함,debulking op\r\n2. Not available"]]
out$Resection <- as.factor(ifelse(out$Resection=="2", NA, out$Resection))

#Combined Organ Resection
# "colon resection" : Rt. + Lt. + rectum 
# "small bowel resection" : small bowel + duodenum
# "pancreas resection" : distal pan + PD 
# "liver resection" 
# "major vessel resection" : iliac a & v, IVC, aorta
out$resection_liver <- as.integer(c[["동반절제 장기\r\nLiver\r\n\r\n0. No\r\n1. Yes"]])
out$resection_largebowel <- as.integer(c[["동반절제 장기\r\nLeft colon\r\n\r\n0. No\r\n1. Yes"]] == 1 | c[["동반절제 장기\r\nRight colon\r\n\r\n0. No\r\n1. Yes"]] == 1 | c[["동반절제 장기\r\nRectum\r\n\r\n0. No\r\n1. Yes"]] == 1)
out$resection_uterus <- as.integer(c[["동반절제 장기\r\nUterus\r\n\r\n0. No\r\n1. Yes"]])
out$resection_kidney <- as.integer(c[["동반절제 장기\r\nKidney\r\n\r\n0. No\r\n1. Yes"]])
out$resection_spleen <- as.integer(c[["동반절제 장기\r\nspleen\r\n\r\n0. No\r\n1. Yes"]])
out$resection_pancreas <- as.integer(c[["동반절제 \r\n장기\r\nPanreatico-duodenectomy\r\n\r\n0. No\r\n1. Yes"]] == 1 | c[["동반절제 \r\n장기\r\nDistal pancreas\r\n\r\n0. No\r\n1. Yes"]] == 1)
out$resection_smallbowel <- as.integer(c[["동반절제 장기\r\nSmall bowel\r\n\r\n0. No\r\n1. Yes"]] == 1 | c[["동반절제 장기\r\nDuodenum\r\n\r\n0. No\r\n1. Yes"]] == 1)
out$resection_stomach <- as.integer(c[["동반절제 장기\r\nStomach\r\n\r\n0. No\r\n1. Yes"]])

out$num_resected_organ <- rowSums(select(out, grep("resection_", names(out), value = T)), na.rm = T)


out$resection_vascular <- as.integer(
  (c[["동반절제 장기\r\nIliac vein\r\n\r\n0. No\r\n1. Yes"]] == "1") | (c[["동반절제 장기\r\nIVC\r\n\r\n0. No\r\n1. Yes"]] == "1") | (c[["동반절제 장기\r\nIliac artery\r\n\r\n0. No\r\n1. Yes"]] == "1") | (c[["동반절제 장기\r\nAorta\r\n\r\n0. No\r\n1. Yes"]] == "1")
)

#OP time
out$opTime <- as.numeric(c[["수술시간\r\n(min)"]])

#intra OP transfusion
out$intraOpTransfusion <- as.integer(c[["PRBC 수혈 수"]])

#Estimated blood loss
out$EBL <- as.numeric(c[["EBL\r\n(ml)"]])


out$ClavienDindoComplication01 <- as.integer(c[["Clavien-Dindo complication \r\n\r\n0. No\r\n1. Yes"]])
out$ClavienDindoComplication_wo_2 <-ifelse(out$ClavienDindoComplication01 == 1 & c[["Clavien-Dindo grade \r\n\r\n2/3a/3b/4a/4b/5"]]=="2",0,out$ClavienDindoComplication01)
out$ClavienDindoGrade <- c[["Clavien-Dindo grade \r\n\r\n2/3a/3b/4a/4b/5"]]
out$ClavienDindoGrade <- as.factor(ifelse(out$ClavienDindoGrade== "0" | is.na(out$ClavienDindoGrade) , "1", out$ClavienDindoGrade))



#post OP transfusion
out$postOpTransfusion <- as.integer(c[["수술 후 PRBC 수혈 여부\r\n\r\n0. No\r\n1. Yes"]])

#ICU care
out$ICUcare <- as.integer(c[["ICU 입실여부\r\n\r\n0. No\r\n1. Yes"]])

#Return to OR
out$ReOP <- as.integer(c[["합병증으로 인한 Re-op 여부\r\n\r\n0. No\r\n1. Yes"]])

#Hospital Stay after OP
out$HospitalDay <- as.numeric(c[["재원일수(days)"]])

out$RTgray <- c[["RT dose\r\n(Gy)"]]

out$preOP_Rtx_to_OP_day <- c$`preOP RTx to OP duration`


for (vname in names(c)[c(130:137, 139:141)]){
  vn.new <- gsub(" ", ".", strsplit(vname, "\r")[[1]][1])
  out[[vn.new]] <- as.integer(c[[vname]])
}



## Variable list: For select UI in ShinyApps
varlist <- list(
  Base = c("Group", "Group1_23", "primaryTumor", "Age", "Sex", "BMI", "BMI_cat",  "DM", "HTN", "COPD", "CoronaryArteryDisease", "ChronicRenalDisease", "PrevAbdominalOp", "preOpChemo", 
           "Hb", "Hb_below9", "Hb_below10", "Albumin", "Albumin_below3", "PLT", "PLT_below50", "PLT_below100", "PT_INR", "PT_INR_over1.5", "TumorSize", "Liposarcoma_postop",
           "FNCLCC_grade", "Resection", "num_resected_organ", grep("resection_", names(out), value = T), "opTime", "intraOpTransfusion", "EBL", "preOP_Rtx_to_OP_day"),
  Complication = c("ClavienDindoComplication01", "ClavienDindoComplication_wo_2", "ClavienDindoGrade", "postOpTransfusion", "ICUcare", "ReOP", "HospitalDay", "RTgray"),
  Event = c("Death", "recur_local"),
  Day = c("day_FU", "recur_day")
)


library(data.table)
## Exclude 환자번호 :이제부터 data.table 패키지 사용
out <- data.table(out[!is.na(out$Group), unlist(varlist)])

## 범주형 변수: 범주 5 이하, recur_site
factor_vars <- c(names(out)[sapply(out, function(x){length(table(x))}) <= 5])
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
conti_vars <- setdiff(names(out), factor_vars)

## Label: Use jstable::mk.lev 
library(jstable)
out.label <- mk.lev(out)

## Label 0, 1 인건 No, Yes 로 바꿈
vars.01 <- names(out)[sapply(lapply(out, levels), function(x){identical(x, c("0", "1"))})]

for (v in vars.01){
  out.label[variable == v, val_label := c("No", "Yes")]
}

## Label: Specific
out.label[variable == "Group", `:=`(var_label = "Group", val_label = c("PreOP RTx", "RTx & No TE", "RTx & TE"))]
out.label[variable == "Group1_23", `:=`(var_label = "Group", val_label = c("PreOP RTx", "RTx"))]
out.label[variable == "primaryTumor", `:=`(var_label = "Primary tumor", val_label = c("No", "Yes"))]

out.label[variable == "Resection", `:=`(var_label = "Resection", val_label = c("R0/R1", "R2"))]
out.label[variable == "preOP_Rtx_to_OP_day", `:=`(var_label = "preOP RTx to OP duration")]
out.label[variable == "ClavienDindoComplication_wo_2", `:=`(var_label = "Clavien-Dindo complication", val_label = c("1/2", "3a/3b/4/5"))]
out.label[variable == "ClavienDindoComplication01", `:=`(var_label = "Clavien-Dindo complication (No/Yes)", val_label = c("No", "Yes"))]


