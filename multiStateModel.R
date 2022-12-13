#overview of transitions
events(msprep_covar)

#naproxen, ambisome now also include                                                                                                       
colnames(medication_binary_final)

#inlcude transition specific covariates
covar <- c("age", "carboplatine", "cisplatine", "endoxan", "holoxan", "MTX", "toposin", "Evoltra", "Busilvex", "aciclovir", "ambisome", "amfotericine", "caspofungin", "ceftazidim", "cotrimoxazol", "cyclofosfamide", "diclofenac",           
           "enalapril", "gentamicine", "morfine", "naproxen", "pentamidine", "piperacilline_tazo", "prograft", "teicoplanine", "tobramycine", "trimethoprim", "valaciclovir", "vancomycine", "voriconazol", "itraconazol")

msprep_covar <- expand.covs(msprep_fin, covar, longnames = FALSE)



msprep_covar$Tstop[msprep_covar$Tstop==0] <- 1




cfinal <- coxph(Surv(Tstart, Tstop, status) ~ 
                  carboplatine.1 + carboplatine.4 + carboplatine.6 +
                  cisplatine.1 + cisplatine.4 + cisplatine.6 + 
                  endoxan.1 + endoxan.4 + endoxan.6  +
                  holoxan.1 + holoxan.4 + holoxan.6  + 
                  MTX.1 + + MTX.4 + MTX.6 +
                  aciclovir.1 + aciclovir.4 + aciclovir.6 +
                  ambisome.1 + ambisome.4 + ambisome.6 +
                  toposin.1 + toposin.4 + toposin.6 +
                  Evoltra.1 + 
                  Busilvex.1 + Busilvex.4 + Busilvex.6 +
                  amfotericine.1 + amfotericine.4 + amfotericine.6 + 
                  caspofungin.1 + caspofungin.4 + caspofungin.6 +
                  ceftazidim.1 + ceftazidim.4 + ceftazidim.6 + 
                  cotrimoxazol.1 + cotrimoxazol.4 + cotrimoxazol.6 +
                  cyclofosfamide.1 + cyclofosfamide.4 + 
                  diclofenac.1 + diclofenac.4 + diclofenac.6 +
                  ceftazidim.1 + ceftazidim.4 +ceftazidim.6 + 
                  enalapril.1 + enalapril.4 + enalapril.6 +
                  gentamicine.1 + gentamicine.4 + gentamicine.6 + 
                  morfine.1 + morfine.4 + morfine.6 + 
                  pentamidine.1 + pentamidine.4 + pentamidine.6 + 
                  piperacilline_tazo.1 + 
                  prograft.1 + prograft.4 + prograft.6 +
                  teicoplanine.1 + teicoplanine.4 + teicoplanine.6 + 
                  tobramycine.1 + tobramycine.4 +
                  vancomycine.1 + vancomycine.4 + vancomycine.6 +
                  voriconazol.1 + voriconazol.4 + voriconazol.6 + 
                  itraconazol.1 + itraconazol.4 + 
                  valaciclovir.1 + valaciclovir.4 + valaciclovir.6 +
                  strata(trans), data = msprep_covar)



summary(cfinal)
#proportional hazard assumption
cox.zph(cfinal)
