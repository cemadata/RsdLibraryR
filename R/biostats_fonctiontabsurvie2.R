# Fonction pour créer la table de survie
biostats_fonctiontabsurvie2 <- function(dat, duree_obs, deces, gp){
  fit.gp <-  survminer::surv_fit(survival::Surv(duree_obs, deces) ~ gp, data = dat, conf.type = "log-log")
  surv.res <- matrix(paste(round(summary(fit.gp, time = c(6,12,24,60))$surv,2)*100, "% [", round(summary(fit.gp, time = c(6,12,24,60))$lower,2)*100, " - ",
                           round(summary(fit.gp, time = c(6,12,24,60))$upper,2)*100, "]", sep = ""),
                     nrow = 4, ncol = length(levels(gp)), byrow = F,
                     dimnames = list(c("Survival rate at M6","Survival rate at M12","Survival rate at M24", "Survival rate at M60"),paste(levels(gp))))
  pfs <- surv_median(fit.gp)

  surv.res <- rbind(surv.res, paste(round(pfs$median,1), " months [", round(pfs$lower,1), " - ", round(pfs$upper,1), "]", sep = ""))
  surv.res <- as.data.frame(surv.res)
  rownames(surv.res)[5] <- "Median PFS"
  surv.res <- as.data.frame(surv.res)
  return(surv.res)}
