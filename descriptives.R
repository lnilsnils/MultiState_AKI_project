###descriptives for report
colnames(data_wide)[1] <- "PatientNumber"
data_descriptives <- merge(data_multi, age_cov, by = "PatientNumber", all.x = TRUE)

#mean age at diagnosis; checking whether the same mean is givien by the data set that was used to fit the mutli-state model and the one for descriptives

mean_age <- mean(data_descriptives$age[is.na(data_wide_age$age) == FALSE])
sd_age <- sd(data_descriptives$age[is.na(data_wide_age$age) == FALSE])
ninetyfiveCI_age <- c(mean_age - sd_age, mean_age + sd_age)


setdiff(data_wide$Patient_Nummer, unique(msprep_covar$PatientNumber))
#to check: same patients are included
setdiff(unique(msprep_covar$PatientNumber), data_multi$PatientNumber)
setdiff(data_multi$PatientNumber, unique(msprep_covar$PatientNumber))



descriptives <- function(data){
  des <- list() 
  des[[1]] <- nrow(data)
  des[[2]] <- table(data$Sex)
  des[[3]] <- round(sum(data$Sex=="M") / length(data$Sex), 2)
  des[[4]] <- table(data$`Diagnose Group`)
  des[[5]] <- round(table(data$`Diagnose Group`) / length(data$Sex), 2)
  des[[6]] <- table(data$Died)
  des[[7]] <- round(sum(data$Died== 1) / length(data$Died), 2)
  des[[8]] <- table(rowSums(cbind(data$SCT1, data$SCT2, data$SCT3)))
  des[[9]] <- table(rowSums(cbind(data$SCT1, data$SCT2, data$SCT3))) / length(data$PatientNumber)
  des[[10]] <- table(data$Relapse)
  des[[11]] <- round(table(data$Relapse) / length(data$Relapse), 2)
  names(des) <- c("N", "sex", "%sex", "diagnoses", "%diagnoses", "died", "%dies", "SCT", "SCT%", "relapse", "%relapse")
  return(des)
}
#overall descriptives
descriptives(data_descriptives)
#descriptives for patients who experience 1, 2, or more AKI episodes
descriptives(data_descriptives[data_descriptives$'AKI1 status'==1 & data_descriptives$'AKI2 status'==0,])
descriptives(data_descriptives[data_descriptives$'AKI2 status'==1 & data_descriptives$'AKI3 status'==0,])
descriptives(data_descriptives[data_descriptives$'AKI3 status'==1,])
#descriptives for patients who did not experience an AKI episode
descriptives(data_descriptives[data_descriptives$'AKI1 status'== 0 & data_descriptives$'AKI2 status'==0 & data_descriptives$'AKI3 status'==0,])
#descriptives AKI total
descriptives(data_descriptives[data_descriptives$'AKI1 status'==1,])