#overview of transitions
events(msprep_covar)

#naproxen, ambisome now also include                                                                                                       
colnames(medication_binary_final)

#inlcude transition specific covariates
covar <- c("age", "carboplatine", "cisplatine", "endoxan", "holoxan", "MTX", "toposin", "Evoltra", "Busilvex", "aciclovir", "ambisome", "amfotericine", "caspofungin", "ceftazidim", "cotrimoxazol", "cyclofosfamide", "diclofenac",           
           "enalapril", "gentamicine", "morfine", "naproxen", "pentamidine", "piperacilline_tazo", "prograft", "teicoplanine", "tobramycine", "trimethoprim", "valaciclovir", "vancomycine", "voriconazol", "itraconazol")

msprep_covar <- expand.covs(msprep_fin, covar, longnames = FALSE)



msprep_covar$Tstop[msprep_covar$Tstop==0] <- 1



#excluded carboplatine.2, carboplatine.7, cisplatine.9, holoxan.10, MTX.10, toposin.10, cyclofosfamide.8, piperacilline_tazo.5, naproxen.3, naproxen.8; error: Loglik converged before variable  x ; beta may be infinite. 
#excluded Evoltra.3, Busilvex.3, Busilvex.7, Busilvex.8, aciclovir.7, toposin.10, Evoltra.3, Busilvex.3, Busilvex.7, Busilvex.8, aciclovir.7, cyclofosfamide.2, cyclofosfamide.3, diclofenac.3, enalapril.3, enalapril.7, enalapril.10, gentamicine.2, morfine.2, morfine.5, pentamidine.2, pentamidine.3, pentamidine.7, pentamidine.8, pentamidine.9, piperacilline_tazo.2, piperacilline_tazo.3, piperacilline_tazo.8, prograft.3, prograft.7, prograft.8, prograft.10, teicoplanine.2, teicoplanine.7, teicoplanine.10, tobramycine.3, tobramycine.5, tobramycine.6, tobramycine.7, tobramycine.8, tobramycine.9, tobramycine.10, trimethoprim.3, trimethoprim.5, trimethoprim.9, trimethoprim.10, vancomycine.2, vancomycine.5, vancomycine.10, voriconazol.5, voriconazol.7, voriconazol.9, itraconazol.3, itraconazol.8, itraconazol.9, itraconazol.10, ambisome.2, ambisome.7, ambisome.10, naproxen.5, naproxen.9, naproxen.10, caspofungin.3, caspofungin.8 ; error: Ran out of iterations and did not converge


#returned NAs for naproxen.7, trimethoprim.7, piperacilline_tazo.7, piperacilline_tazo.9, piperacilline_tazo.10, pentamidine.10, cyclofosfamide.5, cyclofosfamide.7, cyclofosfamide.9, cyclofosfamide.10, Busilvex.10, carboplatine.9, carboplatine.10,

#removed because of high 95%CI (>10): carboplatine.5, carboplatine.8, cisplatine.7, cisplatine.8, cisplatine.10, endoxan.3 , endoxan.7, endoxan.8, endoxan.9, endoxan.10, holoxan.2, holoxan.4, holoxan.5, holoxan.7, holoxan.9, MTX.5, MTX.7, MTX.9, toposin.7, toposin.9, 
#Evoltra.5, Evoltra.6, Evoltra.7, Evoltra.8, Evoltra.9, Evoltra.10, Busilvex.2, Busilvex.5, Busilvex.9, aciclovir.5, aciclovir.9, ambisome.5, ambisome.9, amfotericine.2, amfotericine.3, amfotericine.5, amfotericine.7, amfotericine.8, amfotericine.9, amfotericine.10, ceftazidim.5, 
#ceftazidim.7, ceftazidim.8, ceftazidim.9, ceftazidim.10, cotrimoxazol.7, cotrimoxazol.8, cotrimoxazol.9, cotrimoxazol.10, cyclofosfamide.6, diclofenac.7, diclofenac.9, diclofenac.10, enalapril.2, enalapril.5, enalapril.8, enalapril.9, gentamicine.5, gentamicine.7, gentamicine.9, 
#gentamicine.10, morfine.7 ,morfine.8, morfine.10,  pentamidine.5, piperacilline_tazo.4, piperacilline_tazo.6,  prograft.5, prograft.9 , teicoplanine.5, teicoplanine.8, teicoplanine.9, tobramycine.4, trimethoprim.6, trimethoprim.8, vancomycine.7, vancomycine.8, vancomycine.9, voriconazol.2, 
#voriconazol.3, voriconazol.8, voriconazol.10, itraconazol.5, itraconazol.7, naproxen.1, naproxen.2, naproxen.4, naproxen.6

#next round; removed because of high 95%CI (>10): Evoltra.2, Evoltra.5, Evoltra.6, Evoltra.7, Evoltra.8, Evoltra.9, Evoltra.10, ambisome.8, aciclovir.5, aciclovir.5, ceftazidim.3, cyclofosfamide.4, morfine.3, prograft.2, trimethoprim.2   

###FINAL MODEL

#caspofungin, ambisome included because of clinical decision (95%CI upper limit above 10)
cfinal <- coxph(Surv(Tstart, Tstop, status) ~ age.1 + age.2 + age.3 + age.4 + age.5 + age.6 +age.7 + age.8 + age.9 + age.10 +
                  carboplatine.1 + carboplatine.3 + carboplatine.4 + carboplatine.6 + 
                  cisplatine.1 + cisplatine.3 + cisplatine.4 + cisplatine.5 + cisplatine.6 +
                  endoxan.1 + endoxan.2 + endoxan.4 + endoxan.5 + endoxan.6 +
                  holoxan.1 + holoxan.3 + holoxan.6 + holoxan.8 + 
                  MTX.1 + MTX.2 + MTX.3 + MTX.4 + MTX.6 + MTX.8 +
                  aciclovir.3 + aciclovir.8 + aciclovir.10 +
                  ambisome.3 + ambisome.6 +
                  toposin.1 + toposin.2 +  toposin.3 + toposin.5 + toposin.6 + toposin.7 + toposin.8 + toposin.9 + toposin.10 +
                  Evoltra.1 + Evoltra.4 + 
                  Busilvex.1 + Busilvex.4 + Busilvex.6 +
                  amfotericine.1 + amfotericine.4 + amfotericine.6 + 
                  caspofungin.1 + caspofungin.2 + caspofungin.4 + caspofungin.5 + caspofungin.6 + caspofungin.7 + caspofungin.9 + caspofungin.10 +
                  aciclovir.1 + aciclovir.2 + aciclovir.4 + aciclovir.6 + 
                  ambisome.1 + ambisome.4 + 
                  ceftazidim.2 + 
                  cotrimoxazol.1 + cotrimoxazol.2 + cotrimoxazol.3 + cotrimoxazol.4 + cotrimoxazol.5 + cotrimoxazol.6 + 
                  cyclofosfamide.1 +
                  diclofenac.1 + diclofenac.2 + diclofenac.4 + diclofenac.5 + diclofenac.6 + diclofenac.8 + 
                  ceftazidim.1 + ceftazidim.4 + ceftazidim.6 +
                  enalapril.1 + enalapril.4 + enalapril.6 + 
                  gentamicine.1 + gentamicine.3 + gentamicine.4 + gentamicine.6 + gentamicine.8 + 
                  morfine.1 + morfine.4 + morfine.6 + morfine.9 + 
                  pentamidine.1 + pentamidine.4 + pentamidine.6 + 
                  piperacilline_tazo.1 + 
                  prograft.1 + prograft.4 +prograft.6 + 
                  teicoplanine.1 +  teicoplanine.3 + teicoplanine.4 + teicoplanine.6 + 
                  tobramycine.1 + tobramycine.2 + 
                  trimethoprim.1 + trimethoprim.4 +
                  vancomycine.1 + vancomycine.3 + vancomycine.4 + vancomycine.6 +
                  voriconazol.4 + voriconazol.6 + 
                  itraconazol.1 + itraconazol.2 + itraconazol.4 +itraconazol.6 + 
                  strata(trans), data = msprep_covar)

summary(cfinal)
#proportional hazard assumption
cox.zph(cfinal)
