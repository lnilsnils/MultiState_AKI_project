library(foreign)
library(mstate)
library(survival)
library(epiDisplay)
library(meta)
library(tidyverse)


#wide format
data_wide <- read.spss(file = "widetabel.sav", to.data.frame = TRUE, add.undeclared.levels="no")

#loading long format
data_final <- read.spss(file = "Logtiduniaal_FINAL_3years.sav", to.data.frame = TRUE)


Patient_unique <- unique(data_final$Patient_Nummer)
#exclude patient that are not in wide format
PatientNotInWide <- setdiff(unique(data_final$Patient_Nummer), data_wide$Patient_Nummer)
PatientsUniqueWide <- Patient_unique[!Patient_unique %in% PatientNotInWide]

data_wide <- data_wide[data_wide$Patient_Nummer %in% PatientsUniqueWide,]

#function that commbines all levels of AKIs into one variable:

group_binary <- function(data){
  new_var <- vector("integer", nrow(data))
  data[is.na(data)] <- 0
  for (i in 1:nrow(data)){
    if (sum(data[i,] >= 1)){
      new_var[i] <- 1
    }
  }
  return(new_var)
}

#for AKI = 1
#change levels of variable "AKIY_N" from no/yes to 0/1

AKI1_status <- data_wide$AKIY_N
levels(AKI1_status) <- c(0,1)
#for AKI = 2 
AKI2_status <- group_binary(data_wide[,37:39])

#for AKI >= 3
AKI3_status <- group_binary(data_wide[,41:43])

#change levels of variable "Died" and "relapse" from no/yes to 0/1
levels(data_wide$Overleden) <- c(0,1)
levels(data_wide$relapse) <- c(0,1)
levels(data_wide$SCT) <- c(0,1)



#create dataframe with relevant variables
data_multi <- data.frame(data_wide$Patient_Nummer ,data_wide$Overlijden_diagnoseTijd_DG ,data_wide$Overleden, 
                         data_wide$AKIstart, AKI1_status, data_wide$AKI_per2_start, AKI2_status, data_wide$AKI_per3_start, AKI3_status,
                         data_wide$SCT_DG, data_wide$SCT, data_wide$Diagnose_Cat, data_wide$Patient_geslacht, data_wide$relapse)



colnames(data_multi) <- c("PatientNumber", "Day of death after diagnosis", "Died", "AKI1 start", "AKI1 status", "AKI2 start", "AKI2 status", 
                          "AKI3 start", "AKI3 status", "days from diagnosis to SCT", "SCT indicator", "Diagnose Group", "Sex", "Relapse")



#change AKI=1905 to NA
data_multi$`SCT indicator`[is.na(data_multi$`SCT indicator`)] <- 0
data_multi$`AKI1 start`[is.na(data_multi$`AKI1 start`)] <- 1095
data_multi$`AKI2 start`[is.na(data_multi$`AKI2 start`)] <- 1095
data_multi$`AKI3 start`[is.na(data_multi$`AKI3 start`)] <- 1095


#two more to remove values outside of observational window (>1095)
data_multi$`Day of death after diagnosis`[is.na(data_multi$`Day of death after diagnosis`)] <- 1095
data_multi$`days from diagnosis to SCT`[is.na(data_multi$`days from diagnosis to SCT`)] <- 1095


data_multi <- data_multi[data_multi$`Day of death after diagnosis` <= 1095 & data_multi$`AKI1 start` <= 1095 & data_multi$`AKI2 start` <= 1095 & data_multi$`AKI3 start` <= 1095 & data_multi$`days from diagnosis to SCT` <= 1095, ]



#split STC indicator variable into STC1, STC2, and STC3
SCT1 <- vector("integer", nrow(data_multi))
SCT2 <- vector("integer", nrow(data_multi))
SCT3 <- vector("integer", nrow(data_multi))
for (i in 1:length(data_multi$`days from diagnosis to SCT`)){
  if (data_multi$`SCT indicator`[i] == 1){
    if (data_multi$`days from diagnosis to SCT`[i] <= data_multi$`AKI1 start`[i]){SCT1[i] <- 1}
    
    if (data_multi$`days from diagnosis to SCT`[i] >= data_multi$`AKI1 start`[i] &
        data_multi$`days from diagnosis to SCT`[i] <= data_multi$`AKI2 start`[i]){SCT2[i] <- 1}
    
    if (data_multi$`days from diagnosis to SCT`[i] >= data_multi$`AKI2 start`[i] &
        data_multi$`days from diagnosis to SCT`[i] <= data_multi$`AKI3 start`[i]){SCT3[i] <- 1}
  }
} 

data_multi <- cbind(data_multi, SCT1, SCT2, SCT3)

#remove STC indicator
data_multi <- data_multi[,-11]

#split days from diagnosis to SCT into STC1_start, STC2_start, STC3_start
STC1_start <- vector("integer", nrow(data_multi))
STC2_start <- vector("integer", nrow(data_multi))  
STC3_start <- vector("integer", nrow(data_multi))

for (i in 1:length(data_multi$`days from diagnosis to SCT`)){
  if (data_multi$SCT1[i] == 1){STC1_start[i] <- data_multi$`days from diagnosis to SCT`[i]}
  if (data_multi$SCT2[i] == 1){STC2_start[i] <- data_multi$`days from diagnosis to SCT`[i]}
  if (data_multi$SCT3[i] == 1){STC3_start[i] <- data_multi$`days from diagnosis to SCT`[i]}
}

complete_multi <- data_multi[, c(1, 2, 3, 4, 5, 6, 7, 8, 9)] 
complete_multi <- cbind(complete_multi, STC1_start, data_multi$SCT1, STC2_start, data_multi$SCT2, STC3_start, data_multi$SCT3)

colnames(complete_multi) <- c("PatientNumber", "Day of death after diagnosis", "Died", "AKI1 start", "AKI1 status", "AKI2 start", "AKI2 status",
                              "AKI3 start", "AKI3 status", "SCT1 start", "SCT1", "SCT2 start", "SCT2", "SCT3 start", "SCT3")

complete_multi <- as.data.frame(x = complete_multi)


#change 0 in AKI1 start, AKI2 start, and AKi3 start to NA
complete_multi$`SCT1 start`[complete_multi$`SCT1 start`==0] <- NA
complete_multi$`SCT2 start`[complete_multi$`SCT2 start`==0] <- NA
complete_multi$`SCT3 start`[complete_multi$`SCT3 start`==0] <- NA

#change 1095 for AKI1 start, AKI2 start, and AKI3 start back to NA
complete_multi$`AKI1 start`[complete_multi$`AKI1 start`==1095] <- NA
complete_multi$`AKI2 start`[complete_multi$`AKI2 start`==1095] <- NA
complete_multi$`AKI3 start`[complete_multi$`AKI3 start`==1095] <- NA


#creating transition matrix
transMulti <- transMat(x = list(c(2, 5, 8), c(3, 6, 8), c(4, 7, 8), c(8), c(2, 8), c(3, 8), c(4, 8), c()), 
                       names = c("Diagnosis", "AKI1", "AKI2", "AKI3", "SCT1", "SCT2", "SCT3", "Death"))
#all possible paths
paths(transMulti)


complete_multi$Died <- as.numeric(complete_multi$Died)
complete_multi$Died[complete_multi$Died == 1] <- 0
complete_multi$Died[complete_multi$Died == 2] <- 1


complete_multi$`AKI1 status` <- as.numeric(complete_multi$`AKI1 status`)
complete_multi$`AKI1 status`[complete_multi$`AKI1 status` == 1] <- 0
complete_multi$`AKI1 status`[complete_multi$`AKI1 status` == 2] <- 1



#problem 1:1095 days would be study period but problem if other time values larger than 1095
complete_multi$`Day of death after diagnosis`[is.na(complete_multi$`Day of death after diagnosis`)] <- 1093
complete_multi$`AKI1 start`[is.na(complete_multi$`AKI1 start`)] <- 1094
complete_multi$`AKI2 start`[is.na(complete_multi$`AKI2 start`)] <- 1095
complete_multi$`AKI3 start`[is.na(complete_multi$`AKI3 start`)] <- 1096
complete_multi$`SCT1 start`[is.na(complete_multi$`SCT1 start`)] <- 1097
complete_multi$`SCT2 start`[is.na(complete_multi$`SCT2 start`)] <- 1098
complete_multi$`SCT3 start`[is.na(complete_multi$`SCT3 start`)] <- 1099


#problems 2: two patients have two events on the same day subject; changed one of the events by one day
complete_multi <- complete_multi[-(397),]
complete_multi[495, 4] <- 202


#transofrming into long format
msprep_model <- msprep(data = complete_multi, trans = transMulti, time = c(NA, "AKI1 start", "AKI2 start", "AKI3 start", "SCT1 start", "SCT2 start", "SCT3 start", "Day of death after diagnosis"), 
                       status = c(NA, "AKI1 status", "AKI2 status", "AKI3 status", "SCT1", "SCT2", "SCT3", "Died"), keep = "PatientNumber")


events_model <- events(msprep_model)

#given the small number of patient that transition to and from STC2 and STC3 (i.e., sample size too small to form senisble transition probabilties), we will remove both states:
#also removing patient
multi_final <- complete_multi[, 1:11]






#excluding age to be able to include it as covariate in the multi-state model

age <- vector("integer")  
for (i in PatientsUniqueWide){
  one_age <- data_final$Datum_Leeftijd_JR[data_final$Patient_Nummer==i][1]
  age <- append(age, one_age)
}
age_cov <- as.data.frame(cbind(PatientsUniqueWide, age))
colnames(age_cov) <- c("PatientNumber", "age")

###medication covariate

#select all medication from wide table
medication_binary <- as.matrix(data_wide[, 108:176])
#excluded because no patient took medication:
medication_binary <- medication_binary[ , !colnames(medication_binary) %in% c("bleomycine", "acetylsalicylzuur_meto", "captopril", "deferasirox", "lisinopril", "lithium", "lomustine", "losartan", "pamidroninezuur", "topiramaat", "valcyte", "zonisamide")]


log_reg <- glm(AKI1_status ~ medication_binary, family = binomial(link = "logit"))
summary(log_reg)
logistic.display(log_reg)


#exlcuded emthexate, oxaliplatine, rituximab, acetylsalicylzuur, ganciclovir, valganciclovir, brincidofovir, cidofovir, ambisome, caspofungin, cefotaxim, 
#cefuroxim, clindamycine, daptomycine, celecoxib, ibuprofen, indometacine, naproxen, tacrolimus, sirolimus

drop_meds <- c("emthexate", "oxaliplatine", "rituximab", "acetylsalicylzuur", "amikacine", "brincidofovir", "cefotaxim", 
               "cefuroxim", "celecoxib", "ciclosporine", "cidofovir", "clindamycine", "colistine", "daptomycine", "foscarnet", "ganciclovir", "ibuprofen", 
               "indometacine", "isoniazide", "rifampicine", "Sandimmune", "sirolimus", "valganciclovir", "valsartan", "zoledroninezuur", "metronidazol")



medication_binary_include <- medication_binary[, !(colnames(medication_binary) %in% drop_meds)]


#remove naproxen and tacrolimus; both show large CI

drop_meds2 <- c("tacrolimus")

medication_binary_final <- medication_binary_include[, !(colnames(medication_binary_include) %in% drop_meds2)]



#merging complete_multi with the covariate age. Not all age values are known for the patients in complete_multi, therefore we use all.x = TRUE and all.y = TRUE
multi_with_age <- merge(multi_final, age_cov, by = "PatientNumber", all.x = TRUE)


#merging complete_multi with medication_covariates
#multi_with_covariates <- merge(multi_with_age, medication_covariates, by = "PatientNumber")

#excluded carboplatine.2, carboplatine.7, cisplatine.9, holoxan.10, MTX.10, toposin.10, cyclofosfamide.8, piperacilline_tazo.5, naproxen.3, naproxen.8; error: Loglik converged before variable  x ; beta may be infinite. 
#excluded Evoltra.3, Busilvex.3, Busilvex.7, Busilvex.8, aciclovir.7, toposin.10, Evoltra.3, Busilvex.3, Busilvex.7, Busilvex.8, aciclovir.7, cyclofosfamide.2, cyclofosfamide.3, diclofenac.3, enalapril.3, enalapril.7, enalapril.10, gentamicine.2, morfine.2, morfine.5, pentamidine.2, pentamidine.3, pentamidine.7, pentamidine.8, pentamidine.9, piperacilline_tazo.2, piperacilline_tazo.3, piperacilline_tazo.8, prograft.3, prograft.7, prograft.8, prograft.10, teicoplanine.2, teicoplanine.7, teicoplanine.10, tobramycine.3, tobramycine.5, tobramycine.6, tobramycine.7, tobramycine.8, tobramycine.9, tobramycine.10, trimethoprim.3, trimethoprim.5, trimethoprim.9, trimethoprim.10, vancomycine.2, vancomycine.5, vancomycine.10, voriconazol.5, voriconazol.7, voriconazol.9, itraconazol.3, itraconazol.8, itraconazol.9, itraconazol.10, ambisome.2, ambisome.7, ambisome.10, naproxen.5, naproxen.9, naproxen.10; error: Ran out of iterations and did not converge




##including chemotherapy medication

#merging complete_multi with individual medication_covariates
medicine_ind_cov <-cbind(data_wide$Patient_Nummer, medication_binary_final)
colnames(medicine_ind_cov)[1] <- "PatientNumber"

multi_covariates <- merge(multi_with_age, medicine_ind_cov, by = "PatientNumber")


transFinal <- transMat(x = list(c(2, 5, 6), c(3, 6), c(4, 6), c(6), c(2, 6), c()), 
                       names = c("Diagnosis", "AKI1", "AKI2", "AKI3", "SCT1", "Death"))

msprep_fin <- msprep(data = multi_covariates, trans = transFinal, time = c(NA, "AKI1 start", "AKI2 start", "AKI3 start", "SCT1 start", "Day of death after diagnosis"), 
                     status = c(NA, "AKI1 status", "AKI2 status", "AKI3 status", "SCT1", "Died"), keep = c("PatientNumber", "age", "carboplatine", "cisplatine", "endoxan",               
                                                                                                           "holoxan", "MTX", "toposin", "Evoltra", "Busilvex", "aciclovir", "ambisome", "amfotericine", "caspofungin", "ceftazidim", "cotrimoxazol", "cyclofosfamide", "diclofenac",           
                                                                                                           "enalapril", "gentamicine", "morfine", "naproxen", "pentamidine", "piperacilline_tazo", "prograft", "teicoplanine", "tobramycine", "trimethoprim", "valaciclovir", "vancomycine", "voriconazol", "itraconazol"))





Correct_msprep <- function(dataset_msprep, dataset_long){
  msprep_corrected <- data.frame()
  
  
  for (i in 1:nrow(dataset_msprep)){
    if (dataset_msprep[i, "status"] == 1){
      
      start_point <- dataset_msprep[i, "Tstart"]
      stopping_point <- dataset_msprep[i, "Tstop"]
      
      
      
      Final_eachPatient <- dataset_long[dataset_long[,"Patient_Nummer"] ==  dataset_msprep[i, "PatientNumber"],]
      per_trans <- Final_eachPatient[Final_eachPatient$Datum_DiagnoseTijd_DG >= start_point & Final_eachPatient$Datum_DiagnoseTijd_DG <= stopping_point,]
      
      
      
      meds_per_trans <- colSums(per_trans[,c("carboplatine", "cisplatine", "endoxan", "holoxan", "MTX", "toposin", "Evoltra", "Busilvex", "aciclovir", "ambisome", "amfotericine", 
                                             "caspofungin", "ceftazidim", "cotrimoxazol", "cyclofosfamide", "diclofenac", "enalapril", "gentamicine", "morfine", "naproxen", 
                                             "pentamidine", "piperacilline_tazo", "prograft", "teicoplanine", "tobramycine", "trimethoprim", "valaciclovir", "vancomycine", "voriconazol", "itraconazol")])
      
      meds_per_trans[meds_per_trans >=1] <- 1
      
      dataset_msprep[i, 11:40] <- meds_per_trans
      
      msprep_corrected <- rbind(msprep_corrected, dataset_msprep[i, ])
      
    }
    else{
      
      
      dataset_msprep[i, 11:40] <- 0
      msprep_corrected <- rbind(msprep_corrected, dataset_msprep[i, ])
      
    }
  }
  colnames(msprep_corrected) <- colnames(dataset_msprep)   
  return(msprep_corrected)
}



msprep_final_corrected <- Correct_msprep(dataset_msprep = msprep_fin, dataset_long = data_final)   


