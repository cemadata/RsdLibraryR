##################################
##### Fonction table survie #####
#################################

biostats_fonctiontabsurvie <- function(dat, duree_obs, deces){
  fit.gp <- survfit(Surv(duree_obs, deces) ~ ARMCD, data = dat, conf.type = "log-log")
  surv.res <- matrix(paste(round(summary(fit.gp, time = c(6,12,24))$surv,3)*100, "% [", round(summary(fit.gp, time = c(6,12,24))$lower,3)*100, " - ",
                           round(summary(fit.gp, time = c(6,12,24))$upper,3)*100, "]", sep = ""),
                     nrow = 3, ncol = 2, byrow = F,
                     dimnames = list(c("Survival rate at M6","Survival rate at M12","Survival rate at M24"),levels(dat$ARMCD)))
  os <- surv_median(fit.gp)
  surv.res <- rbind(surv.res, paste(round(os$median,1), " months [", round(os$lower,1), " - ", round(os$upper,1), "]", sep = ""))
  surv.res <- as.data.frame(surv.res)
  rownames(surv.res)[4] <- "Median overall survival"
  surv.res <- as.data.frame(surv.res)
  return(surv.res)}
