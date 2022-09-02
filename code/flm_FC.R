# Function to run regression analysis

flm_FC <- function(dat, mean_FC, term) {
	
	# DESCRIPTION
	# This function runs regression analysis on functional connectivity
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# mean_FC = within or between network
	#
	# OUTPUT
	# vif.lme(mod): variance inflation factor of each regressor in the model
	# res: confidence interval and p value of each fixed variable
	# anova(mod): regression results of the model 
	# anova(mod_P): post-hoc regression analysis on Peer condition
	# anova(mod_C): post-hoc regression analysis on Character condition
	#
	#
	
	if (term == "Age") {
		fl <- as.formula(paste0(mean_FC, "~ social*age + mental + gender + 
					mean_FD+IQ"))
		fl2 <- as.formula(paste0(mean_FC, "~ age + mental + gender + 
					 mean_FD+IQ"))
	} else if (term == "RT") {
		fl <- as.formula(paste0(mean_FC, "~ social*RT + mental + gender + 
					mean_FD+IQ"))
		fl2 <- as.formula(paste0(mean_FC, "~ RT + mental + gender + mean_FD+IQ"))
	}
	
	mod <- lme(fl, random = ~1|Subject, data = dat)
	
	res <- data.frame( "Beta.CI" = paste(round(summary(mod)$coefficients$fixed, 3),			     
					     "(",round(summary(mod)$coefficients$fixed-1.96*sqrt(diag(mod$varFix)), 2), 
					     ",", round(summary(mod)$coefficients$fixed+1.96*sqrt(diag(mod$varFix)), 2),")", 
					     sep=""),
			   "P.value" = round(2 * pt(-abs(summary(mod)$coefficients$fixed/sqrt(diag(mod$varFix))), 
			   			 summary(mod)$fixDF[[1]]), 3))
	
	# post-hoc regression analysis
	mod_P <- lme(fl2, random = ~1|Subject, data = dat[dat$social == "P",],
		     control=(msMaxIter=100))
	mod_C <- lme(fl2, random = ~1|Subject, data = dat[dat$social == "C",],
		     control = lmeControl(maxIter = 500, msMaxIter = 500, 
		     			   msMaxEval=500,tolerance = 0.1, 
		     			   msTol = 0.1,sing.tol=1e-20))
	
	# variance inflation factor 
	vif.lme <- function (fit) {
		## adapted from rms::vif
		v <- vcov(fit)
		nam <- names(fixef(fit))
		## exclude intercepts
		ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
		if (ns > 0) {
			v <- v[-(1:ns), -(1:ns), drop = FALSE]
			nam <- nam[-(1:ns)] }
		d <- diag(v)^0.5
		v <- diag(solve(v/(d %o% d)))
		names(v) <- nam
		v }
	
	return(list(vif.lme(mod),res, anova(mod),anova(mod_P), anova(mod_C))) 
}
