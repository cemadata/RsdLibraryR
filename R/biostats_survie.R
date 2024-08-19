##################################
##### Fonction courbe survie #####
#################################

biostats_survie <- function(dta, dc, dl, mod.dc, gp, by) {

  var.dc <- dta[,dc]
  var.dl <- dta[,dl]
  var.gp <- gp
  var.dc <- var.dc %in% mod.dc

  fit.gp <- survminer::surv_fit(survival::Surv(var.dl, var.dc) ~ dta[,var.gp], data = dta, conf.type = "log-log")
  OSgp <-   survminer::ggsurvplot(fit.gp, conf.int = F, surv.median.line = "h",
                                  risk.table = "nrisk_cumcensor", risk.table.y.text = F,
                                  legend.labs = paste(levels(dta[,var.gp])),
                                  xlab = "Follow-up (in months)", censor.size = 3,
                                  break.time.by = by, tables.height = 0.25, pval = T, fontsize = 3, palette = "lancet")
  return(list(plot.gp = OSgp, fit.gp))
}
