excel_to_numeric <- function(db, column) {
  return(unname(unlist(db[, column])))
}

plotTableSummary <- function(){
  tbl <- matrix(c(71, min(effort), max(effort), mean(effort), median(effort), sd(effort), quantile(effort, 0.25), quantile(effort, 0.75),
                  71, min(complexActor), max(complexActor), mean(complexActor), median(complexActor), sd(complexActor), quantile(complexActor, 0.25), quantile(complexActor, 0.75),
                  71, min(uaw), max(uaw), mean(uaw), median(uaw), sd(uaw), quantile(uaw, 0.25), quantile(uaw, 0.75),
                  71, min(avguc), max(avguc), mean(avguc), median(avguc), sd(avguc), quantile(avguc, 0.25), quantile(avguc, 0.75),
                  71, min(uucw), max(uucw), mean(uucw), median(uucw), sd(uucw), quantile(uucw, 0.25), quantile(uucw, 0.75)
  ), ncol=8, byrow=TRUE)
  rownames(tbl) <- c("Effort", "ComplexActor", "UAW", "AVGUC", "UUCW")
  colnames(tbl) <- c("Osservazioni", "Minimo", "Massimo", "Media", "Mediana", "DeviazioneStd","PrimoQ", "TerzoQ")
  tbl.table <- as.table(tbl)
  #print(tbl)
  #pdf(file="summary.pdf", height = 11, width = 8.5)
  #grid.table(tbl)
  #dev.off()
  return(tbl)
}

plotResiduali <- function(modello){
plot(fitted(modello), resid(modello),
     xlab="Fitted values",
     ylab="Residuals", 
     main="Scatter plot of Residuals"
)
}