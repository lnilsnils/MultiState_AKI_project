#patient-specific transition probabilities
par(mfrow=c(2, 2))  

pt357 <- probtrans(patient_fit357, predt = 0, method = "aalen")
plot(pt357, main = list("Leukemia patient"), type = "filled", 
     col = c("beige", "bisque", "bisque 1", "bisque 2", "bisque 3", "aliceblue"), xlab = "Days since diagnosis")

pt373 <- probtrans(patient_fit373, predt = 0, method = "aalen")
plot(pt373, main = list("CNS cancer patient"), type = "filled", 
     col = c("beige", "bisque", "bisque 1", "bisque 2", "bisque 3", "aliceblue"), xlab = "Days since diagnosis")
#renal tumor
pt451 <- probtrans(patient_fit14, predt = 0, method = "aalen")
plot(pt451, main = list("Renal Tumor Patient"), type = "filled", 
     col = c("beige", "bisque", "bisque 1", "bisque 2", "bisque 3", "aliceblue"), xlab = "Days since diagnosis")
#Lymphoma
pt371 <- probtrans(patient_fit371, predt = 0, method = "aalen")
plot(pt371, main = list("Lymphoma Patient"), type = "filled", 
     col = c("beige", "bisque", "bisque 1", "bisque 2", "bisque 3", "aliceblue"), xlab = "Days since diagnosis")

par(mfrow=c(1, 1))