# Fonction pour créer la courbe de survie
biostats_survie2 <- function(dat, deces, duree_obs, gp, by, xmax){
  fit.gp <-  survminer::surv_fit(survival::Surv(duree_obs, deces) ~ gp, data = dat, conf.type = "log-log")
  plot_pfs <- survminer::ggsurvplot(fit.gp, data=dat,conf.int = F, surv.median.line = "h", legend.title="",legend.labs=paste(levels(gp)),
                                    risk.table = "nrisk_cumcensor", risk.table.y.text = F,
                                    xlab = "Follow-up (in months)", ylab = "Progression-free survival probability", censor.size = 5,xlim=c(0,xmax),
                                    break.time.by = by, tables.height = 0.25, pval = T, fontsize = 3, palette = "lancet", linetype=1)
  return(plot_pfs)
}
