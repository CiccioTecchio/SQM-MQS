plotTableShapiro <- function(effort, complexActor, uaw, avguc, uucw){ #aggiungere pdfName
  s_Effort = shapiro.test(effort)
  s_ComplexAct = shapiro.test(complexActor)
  s_uaw = shapiro.test(uaw)
  s_avguc = shapiro.test(avguc)
  s_uucw = shapiro.test(uucw)
  #print("Tore")
  #print(as.numeric(s_Effort[1]$statistic))
  #print(as.numeric(s_Effort[2]$p.value))
  #print(s_Effort)
  #print(s_Effort[1])
  #print(s_Effort[2])
  #print(s_ComplexAct)
  #print(s_uaw)
  #print(s_avguc)
  #print(s_uucw)
  tbl <- matrix(c(s_Effort[1],s_Effort[2],
                  s_ComplexAct[1], s_ComplexAct[2],
                  s_uaw[1], s_uaw[2],
                  s_avguc[1], s_avguc[2],
                  s_uucw[1], s_uucw[2]), 
                ncol=5, byrow=FALSE)
  
  # tbl <- matrix(c(s_Effort[1]$statistic, s_Effort[2],
  #                 as.numeric(s_ComplexAct[1]$statistic), as.numeric(s_ComplexAct[2]$p.value),
  #                 as.numeric(s_uaw[1]$statistic), as.numeric(s_uaw[2]$p.value),
  #                 as.numeric(s_avguc[1]$statistic), as.numeric(s_avguc[2]$p.value),
  #                 as.numeric(s_uucw[1]$statistic), as.numeric(s_uucw[2]$p.value)), 
  #               ncol=5, byrow=FALSE)
  
  
  rownames(tbl) <- c("W", "p-value")
  colnames(tbl) <- c("Effort", "ComplexActor", "UAW", "AVGUC", "UUCW")
  
  tbl.table <- as.table(tbl)
  #print(tbl)
  #pdf(file="shapiro.pdf", height = 11, width = 8.5)
  #grid.table(tbl)
  #dev.off()
  return(tbl)
}

TestLinearita <- function(effort, complexActor, uaw, avguc, uucw){
  eff_complexAct = cor.test(effort, complexActor, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
  eff_uaw = cor.test(effort, uaw, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
  eff_avguc = cor.test(effort, avguc, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
  eff_uucw = cor.test(effort, uucw, alternative = c("two.sided"), method = c("pearson"), exact = NULL, conf.level = 0.95)
  
  tbl <- matrix(c(as.numeric(eff_complexAct[4]$estimate),
                  as.numeric(eff_uaw[4]$estimate),
                  as.numeric(eff_avguc[4]$estimate),
                  as.numeric(eff_uucw[4]$estimate)
  ), ncol=1, byrow = TRUE)
  
  rownames(tbl) <- c("Effort/ComplexAct", "Effort/UAW", "Effort/AVGUC", "Effort/UUCW")
  colnames(tbl) <- c("Coefficiente di correlazione(r)")
  
  tbl.table <- as.table(tbl)
  #print(tbl)
  #pdf(file="linearita.pdf", height = 11, width = 8.5)
  #grid.table(tbl)
  #dev.off()
  return(tbl)
}

TestOmoschedasticita <- function(effort, complexActor, uaw, avguc, uucw){
  bpComplexActor = bptest(effort ~ complexActor)
  bpUaw = bptest(effort ~ uaw)
  bpAvguc = bptest(effort ~ avguc)
  bpUucw = bptest(effort ~ uucw)
  
  #print(bp)
  #print(bp[1])#BP
  #print(bp[2])#df
  #print(bp[4])#p-value
  
  tbl <- matrix(c(as.numeric(bpComplexActor[1]), as.numeric(bpComplexActor[4]),
                  as.numeric(bpUaw[1]), as.numeric(bpUaw[4]),
                  as.numeric(bpAvguc[1]), as.numeric(bpAvguc[4]),
                  as.numeric(bpUucw[1]), as.numeric(bpUucw[4])
  ), ncol=2, byrow = TRUE)
  
  rownames(tbl) <- c("Effort/ComplexAct", "Effort/UAW", "Effort/AVGUC", "Effort/UUCW")
  colnames(tbl) <- c("BP", "p-value")
  
  tbl.table <- as.table(tbl)
  #print(tbl)
  #pdf(file="linearita.pdf", height = 11, width = 8.5)
  #grid.table(tbl)
  #dev.off()
  return(tbl)
}