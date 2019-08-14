# description & objectives -------------------------------------------------------------
# Bayes analysis
# Linear regression
# AQUASTAT data
# Rebound effect
#Is this supervised learning or unsupervised learning? 
#Is this a classification problem or is it a regression problem? 
#Is this a prediction problem or an inference problem?


# usefull links -----------------------------------------------------------
#https://mran.revolutionanalytics.com/snapshot/2018-01-06/web/packages/rstanarm/vignettes/continuous.html
#https://cran.r-project.org/web/packages/rstanarm/
#https://cran.r-project.org/web/packages/bayesplot/
#https://ourcodingclub.github.io/2018/04/30/stan-2.html
#https://arxiv.org/pdf/1709.01449.pdf
#https://github.com/jgabry/bayes-vis-paper/blob/master/bayes-vis.R

# packages -----------------------------------------------------------
library(rstan)
library(bayesplot)
library(tidyverse)
library("rethinking")
library(reshape2)
library(rstanarm)
library(corrplot)
library("PerformanceAnalytics")
rstan_options(auto_write = TRUE)
library(cowplot)


# functions ---------------------------------------------------------------
length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# working directory -------------------------------------------------------


# importing data ---------------------------------------------------------------
#testdata <- read.csv2("/Users/mariusderenthal/Google Drive/Global Change Geography/3.Semester/Risk and Uncertainty/MAP/Data/aquastat.csv", header = T)
#preprocessing ste
#1. Delete empty rows at the top
#2. Delete Metadata at the bottom
#3. Split data into columns and use dec = .
africa <- read.csv2("Data/africa_flat.csv", header = T)
america <- read.csv2("Data/americas_flat.csv", header = T)
asia <- read.csv2("Data/asia_flat.csv", header = T)
europe <- read.csv2("Data/europe_flat.csv", header = T)
ocean <- read.csv2("Data/ocean_flat.csv", header = T)

data = rbind(africa,america,asia,europe,ocean)

# data exploration ------------------------------------------------------------
  # visual ------------------------------------------------------------------

# length(unique(data$Area))           #9 countries
# length(unique(data$Variable.Id))  #61 variables
# length(unique(data$Year))           #57 years
# 
# #as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# #data$Value <- as.numeric.factor(data$Value)
# 
# 
#   #damn capacity
# ggplot(subset(data, Variable.Id == 4197), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   scale_y_log10() + # convert to log scale
#   labs(y="Total dam capacity log-scale", 
#        x="Year", 
#        title="Time Series: Total Dam Capacity")
# 
# ggplot(subset(data, Variable.Id == 4471), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Dam capacity per capita", 
#        x="Year", 
#        title="Time Series: Dam capacity per capita")
# 
# ggplot(data, aes(x=subset(data$Value, Variable.Id == 4471), y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Dam capacity per capita", 
#        x="Year", 
#        title="Time Series: Dam capacity per capita")
# 
# 
# 
#   #water withdrawl
# ggplot(subset(data, Variable.Id == 4253), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   scale_y_log10() + # convert to log scale
#   labs(y="Total water withdrawn log-scale", 
#        x="Year", 
#        title="Time Series: Total Water Withdrawn")
# 
# ggplot(subset(data, Variable.Id == 4257), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Total water withdrawal per capita", 
#        x="Year", 
#        title="Time Series: Total water withdrawal per capita")
# 
# ggplot(subset(data, Variable.Id == 4250), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Agricultural water withdrawal", 
#        x="Year", 
#        title="Time Series: Agricultural water withdrawal")
# 
# ggplot(subset(data, Variable.Id == 4252), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Industrial water withdrawall", 
#        x="Year", 
#        title="Time Series: Industrial water withdrawal")
# 
# ggplot(subset(data, Variable.Id == 4475), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Irrigation water withdrawal", 
#        x="Year", 
#        title="Time Series: Irrigation water withdrawal")
# 
#   #socio-economic
# ggplot(subset(data, Variable.Id == 4104), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Population Total", 
#        x="Year", 
#        title="Time Series: Population Total")
# 
# ggplot(subset(data, Variable.Id == 4107), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Population Density", 
#        x="Year", 
#        title="Time Series: Population Density")
# 
# ggplot(subset(data, Variable.Id == 4474), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Prevalence of undernourishment (3-year average)", 
#        x="Year", 
#        title="Time Series: Prevalence of undernourishment (3-year average)")
# 
# 
# ggplot(subset(data, Variable.Id == 4114), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="Total population with access to safe drinking-water (JMP)", 
#        x="Year", 
#        title="Time Series: Total population with access to safe drinking-water (JMP)")
# 
# 
# 
#   #environmental
# ggplot(subset(data, Variable.Id == 4549), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_point() +
#   scale_y_log10() + # convert to log scale
#   labs(y="Environmental Flow Requirements", 
#        x="Year", 
#        title="Time Series: Environmental Flow Requirements")
# 
#   #irrigation
# ggplot(subset(data, Variable.Id == 4328), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_line() +
#   #scale_y_log10() + # convert to log scale
#   labs(y="% of the area equipped for irrigation actually irrigated", 
#        x="Year", 
#        title="Time Series: % of the area equipped for irrigation actually irrigated")
# 
# ggplot(subset(data, Variable.Id == 4330), aes(x=Year, y=Value, col = factor(Area))) + 
#   geom_line() +
#   scale_y_log10() + # convert to log scale
#   labs(y="% of irrigation potential equipped for irrigation", 
#        x="Year", 
#        title="Time Series: % of irrigation potential equipped for irrigation")



# data cleaning -------------------------------------------------------------
  # variable selection
  dam_cap <- subset(data, Variable.Id == 4197)      #total dam capacity
  dam_cap_per <- subset(data, Variable.Id == 4471)  #dam capacity per capita
  
  tot_wat <- subset(data, Variable.Id == 4253)      #total water withdrawl
  tot_wat_per <- subset(data, Variable.Id == 4257)  #total water withdrawl per capita
  agr_wat <- subset(data, Variable.Id == 4250)      #agricultural withdrawl
  ind_wat <- subset(data, Variable.Id == 4252)      #industrial withdrawl
  
  pop_tot <- subset(data, Variable.Id == 4104)      #total population
  pop_dens <- subset(data, Variable.Id == 4107)     #population density
  pop_under <- subset(data, Variable.Id == 4474)    #prevalence of undernourishment 
  pop_access <- subset(data, Variable.Id == 4114)   #Total population with access to safe drinking-water
  
  irri1 <- subset(data, Variable.Id == 4328)        #% of the area equipped for irrigation actually irrigated
  irri2 <- subset(data, Variable.Id == 4330)        #% of irrigation potential equipped for irrigation
  
  data_sub <- rbind(dam_cap,dam_cap_per,
                    tot_wat,tot_wat_per,agr_wat,ind_wat,
                    pop_tot,pop_dens,pop_under,pop_access,
                    irri1,irri2)
  
  #remove columns
  data_sub_long <- data_sub[,-c(2,4,7:8)]
  
  #transform to wide format
  data_sub_wide <-data_sub_long %>% 
    dplyr::group_by_at(vars(c(-Value))) %>%             # group by everything other than the value column. 
    dplyr::mutate(row_id=1:n()) %>% ungroup() %>%       # build group index
    spread(key=Variable.Name , value =Value) %>%        # spread
    dplyr::select(-row_id)                              # drop the index
  
  
  #aggreagate data into AQUASTAT timesteps
  #create bins
  data_sub_wide$bins <- cut(data_sub_wide$Year, 
                            breaks=seq(from = 1958, to = 2018, 5), 
                            labels=seq(from = 1960, to = 2015, 5))
  
  data_sub_wide <- data_sub_wide[,-2]
  
  #calculate mean based on bins
  data_sub_wide_mean <- data_sub_wide %>%
    group_by(Area, bins) %>%
    summarize(dam_cap = mean(`Total dam capacity`, na.rm = TRUE),
              dam_cap_per = mean(`Dam capacity per capita`, na.rm = TRUE),
      
              tot_wat = mean(`Total water withdrawal`, na.rm = TRUE),
              tot_wat_per = mean(`Total water withdrawal per capita`, na.rm = TRUE),
              agr_wat = mean(`Agricultural water withdrawal`, na.rm = TRUE), 
              ind_wat = mean(`Industrial water withdrawal`, na.rm = TRUE),
              
              pop_tot = mean(`Total population`, na.rm = TRUE),
              pop_dens = mean(`Population density`, na.rm = TRUE),
              pop_under = mean(`Prevalence of undernourishment (3-year average)`, na.rm = TRUE),
              pop_access = mean(`Total population with access to safe drinking-water (JMP)`, na.rm = TRUE),
              
              irri1 = mean(`% of irrigation potential equipped for irrigation`, na.rm = TRUE),
              irri2 = mean(`% of the area equipped for irrigation actually irrigated`, na.rm = TRUE))
  
  dat <- data_sub_wide_mean

  # create subset for analysis
  sub_tot <- dat[,c(1,2,4,6,10,11,12)] # all variables of interest
  sub_1 <- dat[,c(1,2,4)] # Area, bins, dam_cap_per
  sub_2 <- dat[,c(1,2,6)] # Area, bins, withdrawal_per
  sub_2.1 <- dat[,c(1,2,4,6)] # Area, bins,dam_cap_per, withdrawal_per
  sub_3 <- dat[,c(1,2,4,6,10)] # Area, bins, withdrawal_per, pop_dens
  sub_4 <- dat[,c(1,2,11)] # Area, bins, pop_under
  sub_5 <- dat[,c(1,2,12)] # Area, bins, pop_access
  sub_5.1 <- dat[,c(1,2,4,6,12)] # Area, bins, pop_access

  # choose subest to work with
  sub <- sub_3
  
  # filter out countires & remove NAs
  sub <- subset(sub, !Area %in% c("SUR|Suriname","TKM|Turkmenistan","ISL|Iceland","CAN|Canada","SGP|Singapore","BGD|Bangladesh","GHA|Ghana"))#, "GHA|Ghana", "IRQ|Iraq", "ZMB|Zambia", "ZWE|Zimbabwe"))
  sub_na <- na.omit(sub)
  sub_na <- subset(sub_na, dam_cap_per > 0)
  length(unique(sub_na$Area))

  #calulate data availability
    data_size<- sub_na %>%
    group_by(Area) %>%
    summarise(count=n())
  
    mean(data_size$count)
  
  #z-score normalisation
  #The disadvantage with min-max normalization technique is that it tends to bring data towards the mean.
  #If there is a need for outliers to get weighted more than the other values, 
  #z-score standardization technique suits better
  dat_z <- sub_na
  # names of variables I don't want to scale
  varnames <- c("Area", "bins")
  # index vector of columns which must not be scaled
  index <- names(dat_z) %in% varnames
  # scale only the columns not in index
  temp <- scale(dat_z[, !index])
  dat_z[, !index] <- temp
  

  #min-max normalisation 
  dat_norm <- sub_na
  varnames <- c("Area", "bins")
  index <- names(dat_norm) %in% varnames
  temp <- as.data.frame(lapply(dat_norm[, !index], normalize))
  dat_norm[, !index] <- temp
  
  #choose data set
  sub_na = dat_z
  
  
  # plotting ----------------------------------------------------------
  hist(sub_na$tot_wat_per, nclass = 100)
  hist(sub_na$dam_cap_per,nclass = 100)
  hist(sub_na$pop_dens,nclass = 100)
  
  ggplot(sub_na, aes(x=tot_wat_per, y= dam_cap_per)) + 
    geom_point() +
    #scale_y_log10() + 
    #scale_x_log10() +
    labs(y="Dam capacity per capita", 
         x="Total water withdrawal per capita", 
         title="Supply-Demand Relationship")
  
   ggplot(sub_na, aes(x=pop_dens, y= dam_cap_per, col = bins)) + 
    geom_point() +
    scale_y_log10() + # convert to log scale
    scale_x_log10() +
    labs(y="Dam capacity per capita", 
         x="Population dens", 
         title="Supply-Demand Relationship")
  
  ggplot(sub_na, aes(x=bins, y= dam_cap_per)) + 
    geom_point() +
    #scale_y_log10() + # convert to log scale
    #scale_x_log10() +
    labs(y="Dam capacity per capita", 
         x="bins", 
         title="Supply-Demand Relationship")
  
  ggplot(sub_na, aes(x=pop_tot, y= dam_cap_per)) + 
    geom_point() +
    #scale_y_log10() + # convert to log scale
    labs(y="Dam capacity per capita", 
         x="Total population", 
         title="Supply-Demand Relationship")
  
  ggplot(sub_na, aes(x=pop_tot, y= tot_wat, col = factor(Area))) + 
    geom_point() +
    #scale_y_log10() + # convert to log scale
    labs(y="Total water withdrawal per capita", 
         x="Total population", 
         title="Supply-Demand Relationship")
  
  ggplot(dat_norm, aes(x= pop_under, y= dam_cap_per)) + 
    geom_point() +
    #scale_y_log10() + # convert to log scale
    labs(y="Total water withdrawal per capita", 
         x="Population access", 
         title="Supply-Demand Relationship")

  # correlation analysis ----------------------------------------------------
  cor_a <- cor(sub_na[,4:5])
  
  corrplot(cor_a, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)

  # Insignificant correlation are crossed
  corrplot(cor_a, type="upper", order="hclust", 
           p.mat = cor_a, sig.level = 0.01, insig = "blank")
  
  
  chart.Correlation(sub_na[,3:8], histogram=TRUE, pch=19)
  
  col<- colorRampPalette(c("blue", "white", "red"))(20)
  heatmap(cor_a, col = col, symm = TRUE)
  
  
  
  
# baseline modeling -------------------------------------------------------
  # prior selection ------------------------------------------------------
  curve( dnorm( x , 0 , 1 ) , from= -5 , to = 5 )
  curve( dcauchy( x , 0 , 1 ) , from= -5 , to= 5 )
  
  #prior predictive simulation
  set.seed(2971)
  N <- 50                   # 100 lines
  a <- rnorm( N , 0 , 0.2)
  b <- rnorm( N , 0.5, 0.5 )
  plot( NULL , xlim=range(sub_na$tot_wat_per) , ylim=c(-5,5) ,
        xlab="tot_wat_per" , ylab="capacity" )
  abline( h=0 , lty=1 )
  abline( h=0 , lty=1 , lwd=0.5 )
  mtext( "b ~ dnorm(0,1)" )
  xbar <- mean(sub_na$tot_wat_per)
  
  for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                          from=min(sub_na$tot_wat_per) , to=max(sub_na$tot_wat_per) , add=TRUE ,col=col.alpha("black",0.2) )
  
  
  
  set.seed(2971)
  N <- 50                   # 100 lines
  a <- rnorm( N , 0 , 0.2)
  b <- rnorm( N , -0.5, 0.5 )
  plot( NULL , xlim=range(sub_na$tot_wat_per) , ylim=c(-5,5) ,
        xlab="tot_wat_per" , ylab="capacity" )
  abline( h=0 , lty=1 )
  abline( h=0 , lty=1 , lwd=0.5 )
  mtext( "b ~ dnorm(0,1)" )
  xbar <- mean(sub_na$pop_dens)
  for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                          from=min(sub_na$tot_wat_per) , to=max(sub_na$tot_wat_per) , add=TRUE ,col=col.alpha("black",0.2) )
  
  
  
  plot(rnorm(N, a, b))
  
  
  # Prior predictive simulations --------------------------------------------
  # Plot: prior predictive with vague priors
  set.seed(seed = 1923840483)

  rm(data_all)
  data_all <- data.frame()
  
  
  N= 20
  for (i in 1:N){
    
  
    Nsim = length(sub_na$tot_wat_per) 
    
    sigma <- abs(rnorm(1, 0, 1))
    beta0 <- rnorm(1, 0, 0.2)
    beta1 <- rnorm(1, 0.5, 0.5)
    beta2 <- rnorm(1, -0.5, 0.5)
    
    data1 <- data.frame(
    tot_cap = sub_na$tot_wat_per,
    dam_cap = sub_na$dam_cap_per,
    pop_d = sub_na$pop_dens,
    
    sim_tot= beta0 + beta1* sub_na$tot_wat_per + beta2*sub_na$pop_dens + rnorm(Nsim, mean = 0, sd = sigma),
    sim_with = beta0 + beta1 * sub_na$tot_wat_per + rnorm(Nsim, mean = 0, sd = sigma),
    sim_dens =  beta0 + beta2* sub_na$pop_dens + rnorm(Nsim, mean = 0, sd = sigma)
    )
  
  data_all <- rbind(data_all,data1) 
  }
  
  theme_set(bayesplot::theme_default(base_size = 18))
  theme_update(axis.text = element_text(size = 20))
  
  p_comp <- ggplot(data_all, aes(x = dam_cap, y = sim_tot)) + 
    geom_point(alpha = 0.5, color = "red") +
    labs(title = "Observed vs. Simulated",
         x = "Observed dam capacity ",
         y = "Simulated dam capacity")#, 


  p_tot <- ggplot(data_all, aes(x = tot_cap, y = sim_tot)) + 
    geom_point(alpha = 0.5, color = "red") +
    labs(title = "Multiple Regression",
         x = "Additive predictors",
         y = "Simulated dam capacity")

 
   p_dens <- ggplot(data_all, aes(x = pop_d, y = sim_dens)) + 
    geom_point(alpha = 0.5, color = "blue") +
    labs(title = "Simple Regression",
         x = "Population Density",
         y = "Simulated dam capacity")
 
   
   p_with <- ggplot(data_all, aes(x = tot_cap, y = sim_with)) + 
     geom_point(alpha = 0.5, color = "blue") +
     labs(title = "Simple Regression",
          x = "Total water withdrawal per capita",
          y = "Simulated dam capacity")

   
  
  #grid.arrange(p_dens, p_with,p_tot,p_comp, ncol=2)
  plot_grid(p_dens, p_with,p_tot,p_comp, labels = "AUTO")
  
  
  my_prior <- normal(location = c(0.5, -0.5), scale = c(0.5, 0.5), autoscale = FALSE)
  

  # fit model ---------------------------------------------------------------
  #fit STAN
  fity <- stan("lm.stan", 
              data = list(N = length(sub_na$tot_wat_per), y = sub_na$dam_cap_per, x = sub_na$tot_wat_per, z = sub_na$pop_dens), 
              chains = 4, 
              iter = 2000)
  
  #mcmc_trace(as.array(fit), pars = c("beta0", "beta1", "sigma"))
  #mcmc_areas(as.matrix(fit), pars = c("beta0", "beta1", "sigma"))
  

  # fit rstanarm GLM
  # fit_gamma <- stan_glm(dam_cap_per ~ tot_wat_per,
  #                 data = sub_na,  
  #                 prior_intercept = normal(0,1), 
  #                 prior = normal(0,1),
  #                 #QR = TRUE,
  #                 #group = ???,
  #                 #prior_PD = T,
  #                 family = Gamma)
  
  fit0 <- stan_glm(dam_cap_per ~ 1,
                  data = sub_na,  
                  #prior = normal(1, 0.5),
                  prior_intercept = normal(0, 0.2),
                  #prior_aux = NULL,
                  #QR = TRUE,
                  #group = ???,
                  family = gaussian(link = "identity"))
  
  fitexp <- stan_glm(dam_cap_per ~ tot_wat_per,
                     data = sub_na,  
                     prior = normal(1, 0.5),
                     prior_intercept = normal(0, 0.2),
                     prior_aux = exponential(1),
                     #QR = TRUE,
                     #group = ???,
                     family = gaussian(link = "identity"))
  
  fit1 <- stan_glm(dam_cap_per ~ tot_wat_per,
                   data = sub_na,  
                   prior = normal(1, 0.5),
                   prior_intercept = normal(0, 0.2),
                   prior_aux = normal(0, 1),
                   #QR = TRUE,
                   #group = ???,
                   family = gaussian(link = "identity"))
  
  fit1.1 <- stan_glm(dam_cap_per ~ tot_wat_per + pop_dens,
                   data = sub_na,  
                   prior = my_prior,
                   prior_intercept = normal(0, 0.2),
                   prior_aux = normal(0, 1),
                   QR = TRUE,
                   #group = ???,
                   family = gaussian(link = "identity"))
  
  
  
  fit1.1.1 <- update(fit1, formula = . ~ tot_wat_per + pop_dens)
  
  fit1.1.2 <- update(fit1, formula = . ~ tot_wat_per + pop_under)
  fit1.2 <- update(fit1, formula = . ~ tot_wat_per + pop_access)
  fit1.3 <- update(fit1, formula = . ~ tot_wat_per + pop_access + pop_under)
  fit1.4 <- update(fit1, formula = . ~ tot_wat_per + pop_access + pop_under + pop_dens)
  
  
  
  fit2 <- update(fit1, formula = . ~ pop_dens)
  fit2.1 <- update(fit1, formula = . ~ pop_dens + tot_wat_per)
  
  # model comparison --------------------------------------------------------
  loo <- loo(fit)
  loo0 <- loo(fit0)
  looexp <- loo(fitexp)
  loo1 <- loo(fit1)
  loo1.1 <- loo(fit1.1)
  loo1.1.1 <- loo(fit1.1.1)
  loo1.1.2 <- loo(fit1.1.2)
  
  loo1.2 <- loo(fit1.2)
  loo1.3 <- loo(fit1.3)
  loo1.4 <- loo(fit1.4)
  
  loo2 <- loo(fit2)
  loo2.1 <- loo(fit2.1)
  
  (comp <- compare_models(loo1,loo1.1,loo1.1.1))
  (comp <- compare_models(loo0, loo1,loo1.1))
  #model is preferred as it has the highest expected log predicted density (elpd) or, 
  #equivalently, the lowest value of the LOO Information Criterion (looic). 
  
  summary(fit)
  prior_summary(fit)
  
  #choose model 
  fit <- fit1.1
  
  # plot the regression model -----------------------------------------------
  n_draws <- 4000
  
  # Draw from the linear predictor, no residual variance
  posterior_line_draws <- posterior_linpred(fit, draws = n_draws) 
  #head(posterior_line_draws)
  
  posterior_line_draws_plot <- posterior_line_draws %>%
    as.data.frame(.) %>%
    gather(.) %>%
    mutate(draw = rep(1:n_draws, length(fit$data$tot_wat_per)),
           x = rep(fit$data$tot_wat_per, each = n_draws))
  
  ggplot(posterior_line_draws_plot %>% 
           filter(draw %in% sample(1:n_draws, 30))) +
    geom_point(data = sub_na, aes(x = tot_wat_per, y = dam_cap_per), col = "grey") +
    geom_line(aes(x = x, y = value, group = draw), alpha = 0.3) +
    theme_bw()
  
  posterior_line_draws_plot_summary <- posterior_line_draws_plot %>%
    group_by(x) %>%
    summarize(q025 = quantile(value, 0.025),
              q25 = quantile(value, 0.25),
              q50 = quantile(value, 0.5),
              q75 = quantile(value, 0.75),
              q975 = quantile(value, 0.975))
  
  ggplot(posterior_line_draws_plot_summary) +
    geom_point(data = sub_na, aes(x = tot_wat_per, y = dam_cap_per), col = "grey") +
    geom_ribbon(aes(x = x, ymin = q025, ymax = q975), alpha = 0.4) +
    geom_ribbon(aes(x = x, ymin = q25, ymax = q75), alpha = 0.8) +
    geom_line(aes(x = x, y = q50)) +
    theme_bw() +
    labs(title = "Parameter uncertainty")
  

  # Draw from the whole model with residual variance
  posterior_draws <- posterior_predict(fit, draws = n_draws) 
  
  posterior_draws_plot <- posterior_draws %>%
    as.data.frame(.) %>%
    gather(.) %>%
    mutate(draw = rep(1:n_draws, length(fit$data$tot_wat_per)),
           x = rep(fit$data$tot_wat_per, each = n_draws))
  
  posterior_draws_plot_summary <- posterior_draws_plot %>%
    group_by(x) %>%
    summarize(q025 = quantile(value, 0.025),
              q25 = quantile(value, 0.25),
              q50 = quantile(value, 0.5),
              q75 = quantile(value, 0.75),
              q975 = quantile(value, 0.975))
  
  ggplot(posterior_draws_plot_summary) +
    geom_point(data = sub_na, aes(x = tot_wat_per, y = dam_cap_per), col = "grey") +
    geom_ribbon(aes(x = x, ymin = q025, ymax = q975), alpha = 0.4) +
    geom_ribbon(aes(x = x, ymin = q25, ymax = q75), alpha = 0.8) +
    geom_line(aes(x = x, y = q50)) +
    theme_bw() +
    labs(title = "Model uncertainty",
         x = "Additive predictors",
         y = "Dam capacity per capita")
  
  # MCMC draws --------------------------------------------------------------
  # posterior uncertainty intervals
  mcmc_intervals(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma")) #Central posterior uncertainty intervals
  mcmc_areas(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma"))
  posterior_interval(as.matrix(fit), prob = 0.9, type = "central",
                     pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma"))
  
  
  # univariate marginal posterior distributions
  mcmc_hist(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma")) #plots marginal posterior distributions (combining all chains):
  mcmc_hist_by_chain(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma")) #view separate histograms of each of the four Markov chains
  
  mcmc_dens_overlay(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma"))
  mcmc_violin(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma"))
  
  # bivariate plots
  mcmc_scatter(as.array(fit), pars = c( "tot_wat_per","pop_dens"))
  mcmc_hex(as.array(fit), pars = c( "tot_wat_per","pop_dens"))
  mcmc_pairs(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma"))
  
  # trace plots
  mcmc_trace(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma")) 
  mcmc_trace(as.array(fit), pars = c("(Intercept)", "tot_wat_per","pop_dens", "sigma"), 
             facet_args = list(ncol = 1, strip.position = "left"))
  
  # MCMC diagnostics --------------------------------------------------------
  plot(fit, plotfun = "rhat")
  # graphical posterior predictive checks (PPCs) ---------------------------------------------------
  posterior_draws <- posterior_predict(fit, draws = 100)
  ppc_dens_overlay(y = fit$data$dam_cap_per, yrep = posterior_draws)
  #In the plot above, the dark line is the distribution of the observed outcomes y
  #and each of the 50 lighter lines is the kernel density estimate of one of the replications
  #of y from the posterior predictive distribution (i.e., one of the rows in yrep).
  #ppc_hist(y = fit$data$dam_cap_per, yrep = posterior_draws)
  #ppc_error_hist(y = fit$data$dam_cap_per, yrep = posterior_draws)
  
  mea <- pp_check(fit, plotfun = "stat", stat = "mean")
  plot_grid(mea, med, labels = "AUTO")
  grid.arrange(p_dens, p_with,p_tot,p_comp, ncol=2)
  
  hi <-pp_check(fit, plotfun = "hist", nreps = 5)
  de <- pp_check(fit, plotfun = "dens_overlay")
  grid.arrange(de, hi, ncol=2)

  #Distributions of test statistics 
  prop_zero <- function(x) mean(x <= 0)
  prop_zero(fit$data$dam_cap_per)
  prop_zero(posterior_draws)
  
  ppc_stat(fit$data$dam_cap_per, posterior_draws, stat = "prop_zero", binwidth = 0.005)

  # make new predictions ----------------------------------------------------
  n <- 40
  x_new <- runif(n, -15, 15)
  n_draws <- 30
  posterior_draws_new <- posterior_predict(fit,
                                           newdata = data.frame(x = x_new), 
                                           draws = n_draws)
  head(posterior_draws_new)
  
  posterior_draws_new_plot <- posterior_draws_new %>%
    as.data.frame(.) %>%
    gather(.) %>%
    mutate(draw = rep(1:n_draws, length(x_new)),
           x_new = rep(x_new, each = n_draws))
  
  ggplot(posterior_draws_new_plot, aes(x = x_new, y = value, group = draw)) +
    geom_point() +
    theme_bw()
  

  # plotting ----------------------------------------------------------------
  #simple regression line
  base <- 
    ggplot(sub_na, aes(x = tot_wat_per, y = dam_cap_per)) + 
    geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) 
    #scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))
  
  base + 
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], 
                color = "skyblue4", size = 1)
  
  
  #uncertainty - plot the estimated regression line at each draw from the posterior distribution
  draws <- as.data.frame(fit)
  colnames(draws)[1:2] <- c("a", "b")
  
  base + 
    geom_abline(data = draws, aes(intercept = a, slope = b), 
                color = "skyblue", size = 0.2, alpha = 0.25) + 
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], 
                color = "skyblue4", size = 1)
  
  
  #ONLY for models with multiple predictors
  draws <- as.data.frame(as.matrix(fit))
  colnames(draws)[1:2] <- c("a", "b")
  ggplot(kidiq, aes(x = tot_wat_per, y = dam_cap_per)) + 
    geom_point(size = 1) +
    geom_abline(data = draws, aes(intercept = a, slope = b), 
                color = "skyblue", size = 0.2, alpha = 0.25) + 
    geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], 
                color = "skyblue4", size = 1)
  
  
  
  
  reg0 <- function(x, ests) cbind(1, 0, x) %*% ests 
  reg1 <- function(x, ests) cbind(1, 1, x) %*% ests
  
  args <- list(ests =
                 coef(fit))
  kidiq$clr <- sub_na$Area
  lgnd <- guide_legend(title = NULL)
  base2 <- ggplot(sub_na, aes(x = tot_wat_per, fill = relevel(Area, ref = "China"))) + 
    geom_point(aes(y = dam_cap_per), shape = 21, stroke = .2, size = 1) + 
    guides(color = lgnd, fill = lgnd) + 
    theme(legend.position = "right")
  base2 + 
    stat_function(fun = reg0, args = args, aes(color = "China"), size = 1.5) +
    stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)

  