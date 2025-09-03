
################################################################################
## Load the required packages for the practical
################################################################################
## Load the required packages for the practical
# If the packages have not been installed, please use the command install.packages('package_name')

library('ggplot2') # We will use ggplot2 for plotting figures
library('patchwork') # We will use patchwork for creating multi-panel plots
library('EpiNow2') # We will use EpiNow2 for estimating the reproduction number from a time-series

######################################################################################################################################################
# 1. Reading in and visualising the data
######################################################################################################################################################
# Read in the dataset 'simulated_data.csv'
# The data set includes the daily infection incidence

df1 <- read.csv('simulated_data.csv')
df1$date <- as.Date(df1$date)

################################################################################
# 1.a To complete
################################################################################
# Visualise the time-series of infection incidence as a figure

plt1 <- ggplot(df1, aes(x=date, y=incidence))+
  geom_point()+
  geom_line()+
  theme_bw(base_size=14)+
  xlab("Time (days)")+
  ylab("Infection incidence")


plt1

#####################################################################################################################################################
# 2. Use EpiNow2 to estimate the smoothed trend in the infection incidence
#####################################################################################################################################################
# We will make use of the EpiNow2 package to estimate smoothed trends in  the
# infection incidence.

# EpiNow2 requires that the generation distribution is defined.
# We will assume that the generation interval is given by a Gamma distribution
# with mean of 5 days and standard deviation of 2 days.
gamma_distribution <- EpiNow2::Gamma(mean=5, sd=2, max=14)

model1 <- epinow(
  data = data.frame(date=df1$date, confirm=df1$incidence),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model1, 'epinow_model_fits/model_fit_example.rds')
model_fit_example <- readRDS('epinow_model_fits/model_fit_example.rds')

summary_example <- model_fit_example$estimates$summarised

plt_inc1 <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  xlab("Date")+
  ylab("Infection incidence")

plt_inc1

################################################################################
# 2.a To complete
################################################################################
# Create a new figure that is the same as above but also includes the infection
# incidence data

plt_inc2 <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=incidence))+
  xlab("Date")+
  ylab("Infection incidence")

plt_inc2

################################################################################
# 2.b To complete
################################################################################
# The summary_example object also includes EpiNow2's modelled estimates of the Reproduction 
# number (variable==R) and the growth rate (variable==growth_rate)
#
# Visualise how the growth rate and reproduction number varied over time in two
# separate figures.
#
# On each figure mark the threshold for epidemic growth/decline with a horizontal 
# dashed line


plt_gr <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="growth_rate",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw(base_size=14)+
  xlab("Date")+
  ylab("Growth rate")

plt_gr

plt_R <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="R",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  geom_hline(yintercept = 1, linetype="dashed")+
  theme_bw(base_size=14)+
  xlab("Date")+
  ylab("Reproduction number")

plt_R

################################################################################
# 2.c To complete
################################################################################
# Plot infection incidence, growth rate, and reproduction number as one figure 
# with multiple panels. 

plt_inc2 + plt_gr + plt_R +plot_layout(nrow=3)


#####################################################################################################################################################
# 3. Use EpiNow2 to investigate the effect of assumed 
#    generation interval on estimates of the reproduction number
#####################################################################################################################################################

################################################################################
# 3.a To complete
################################################################################
# Use the EpiNow2 package to fit the model estimating R assuming:
# 1. A generation interval with: mean=2, sd=1
# 2. A generation interval with: mean=9, sd=3

gi_low <- EpiNow2::Gamma(mean=2, sd=1, max=14)
gi_high <- EpiNow2::Gamma(mean=9, sd=3, max=14)

###### Fill in #######

model_gi_low <- epinow(
  data = data.frame(date=df1$date, confirm=df1$incidence),
  generation_time = gt_opts(gi_low),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model_gi_low, 'epinow_model_fits/model_gi_low.rds')

model_gi_high <- epinow(
  data = data.frame(date=df1$date, confirm=df1$incidence),
  generation_time = gt_opts(gi_high),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model_gi_high, 'epinow_model_fits/model_gi_high.rds')

####### Pre-fitted models #########
# Confirm code above is correct and runs, before loading pre-fitted models

model_fit_example <- readRDS('epinow_model_fits/model_fit_example.rds')
model_gi_low <- readRDS('epinow_model_fits/model_gi_low.rds')
model_gi_high <- readRDS('epinow_model_fits/model_gi_high.rds')

################################################################################
#3.b To complete
################################################################################
# Produce a figure comparing the estimated reproduction numbers for each GI

summary_gi_low <- model_gi_low$estimates$summarised
summary_gi_original <- model_fit_example$estimates$summarised
summary_gi_high <- model_gi_high$estimates$summarised

plt3b <- ggplot()+
  geom_line(data = summary_gi_low[summary_gi_low$variable=="R",], aes(x=date, y=median, col='Low'))+
  geom_ribbon(data = summary_gi_low[summary_gi_low$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Low'), alpha=0.2)+
  geom_ribbon(data = summary_gi_low[summary_gi_low$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Low'), alpha=0.2)+
  geom_line(data = summary_gi_original[summary_gi_original$variable=="R",], aes(x=date, y=median, col='Original'))+
  geom_ribbon(data = summary_gi_original[summary_gi_original$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Original'), alpha=0.2)+
  geom_ribbon(data = summary_gi_original[summary_gi_original$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Original'), alpha=0.2)+
  geom_line(data = summary_gi_high[summary_gi_high$variable=="R",], aes(x=date, y=median, col='High'))+
  geom_ribbon(data = summary_gi_high[summary_gi_high$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='High'), alpha=0.2)+
  geom_ribbon(data = summary_gi_high[summary_gi_high$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='High'), alpha=0.2)+
  theme_bw(base_size=14)+
  scale_fill_brewer("Generation interval",palette = "Dark2")+
  scale_color_brewer("Generation interval",palette = "Dark2")+
  geom_hline(yintercept = 1, linetype="dashed")+
  xlab("Date")+
  ylab("Reproduction number")

plt3b

################################################################################
# 3.c To complete
################################################################################
# Produce a figure comparing the estimated growth rates for each GI

plt3c <- ggplot()+
  geom_line(data = summary_gi_low[summary_gi_low$variable=="growth_rate",], aes(x=date, y=median, col='Low'))+
  geom_ribbon(data = summary_gi_low[summary_gi_low$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Low'), alpha=0.2)+
  geom_ribbon(data = summary_gi_low[summary_gi_low$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Low'), alpha=0.2)+
  geom_line(data = summary_gi_original[summary_gi_original$variable=="growth_rate",], aes(x=date, y=median, col='Original'))+
  geom_ribbon(data = summary_gi_original[summary_gi_original$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Original'), alpha=0.2)+
  geom_ribbon(data = summary_gi_original[summary_gi_original$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Original'), alpha=0.2)+
  geom_line(data = summary_gi_high[summary_gi_high$variable=="growth_rate",], aes(x=date, y=median, col='High'))+
  geom_ribbon(data = summary_gi_high[summary_gi_high$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='High'), alpha=0.2)+
  geom_ribbon(data = summary_gi_high[summary_gi_high$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='High'), alpha=0.2)+
  theme_bw(base_size=14)+
  scale_fill_brewer("Generation interval",palette = "Dark2")+
  scale_color_brewer("Generation interval",palette = "Dark2")+
  geom_hline(yintercept = 0, linetype="dashed")+
  xlab("Date")+
  ylab("Growth rate")


plt3c


#####################################################################################################################################################
# 4. Compare estimates of Rt using four different datasets
#    1) infections (as before)
#    2) cases
#    3) hospitalisations
#    4) deaths
#####################################################################################################################################################
# Assume the default GI distribution used originally
gamma_distribution <- EpiNow2::Gamma(mean=5, sd=2, max=14)

################################################################################
# 4.a To complete
################################################################################
# Fit a model estimating Rt to each time-series.
# Note that you have previously fitted a model to the infection time series


model2 <- epinow(
  data = data.frame(date=df1$date, confirm=df1$cases),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model2, 'epinow_model_fits/model_cases.rds')

model3 <- epinow(
  data = data.frame(date=df1$date, confirm=df1$hospitalisations),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model3, 'epinow_model_fits/model_hospitalisations.rds')

model4 <- epinow(
  data = data.frame(date=df1$date, confirm=df1$deaths),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model4, 'epinow_model_fits/model_deaths.rds')


####### Pre-fitted models #########
# Confirm code above is correct and runs, before loading pre-fitted models

model1 <- readRDS('epinow_model_fits/model_fit_example.rds') # model fit to infection incidence
model2 <- readRDS('epinow_model_fits/model_cases.rds') # model fit to cases
model3 <- readRDS('epinow_model_fits/model_hospitalisations.rds') # model fit to hospitalisations
model4 <- readRDS('epinow_model_fits/model_deaths.rds') # model fit to deaths

################################################################################
# 4.b To complete
################################################################################
# Produce a four-panel plot showing the fit of each model to each time series

summary1 <- model1$estimates$summarised
summary2 <- model2$estimates$summarised
summary3 <- model3$estimates$summarised
summary4 <- model4$estimates$summarised


plt4b.1 <- ggplot()+
  geom_line(data = summary1[summary1$variable=="infections",], aes(x=date, y=median))+
  geom_ribbon(data = summary1[summary1$variable=="infections",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary1[summary1$variable=="infections",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=incidence))+
  xlab("Date")+
  ylab("Infection incidence")

plt4b.2 <- ggplot()+
  geom_line(data = summary2[summary2$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary2[summary2$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary2[summary2$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=cases))+
  xlab("Date")+
  ylab("Cases")

plt4b.3 <- ggplot()+
  geom_line(data = summary3[summary3$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary3[summary3$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary3[summary3$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=hospitalisations))+
  xlab("Date")+
  ylab("Hospitalisations")

plt4b.4 <- ggplot()+
  geom_line(data = summary4[summary4$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary4[summary4$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary4[summary4$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=deaths))+
  xlab("Date")+
  ylab("Deaths")

plt4b.1 + plt4b.2+ plt4b.3+ plt4b.4 +plot_layout(nrow=4)


################################################################################
# 4.c To complete
################################################################################
# Produce a plot comparing R estimates made using each of the four different time series

plt4c <- ggplot()+
  geom_line(data = summary1[summary1$variable=="R",], aes(x=date, y=median, col='Infections'))+
  geom_ribbon(data = summary1[summary1$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Infections'), alpha=0.2)+
  geom_ribbon(data = summary1[summary1$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Infections'), alpha=0.2)+
  geom_line(data = summary2[summary2$variable=="R",], aes(x=date, y=median, col='Cases'))+
  geom_ribbon(data = summary2[summary2$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Cases'), alpha=0.2)+
  geom_ribbon(data = summary2[summary2$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Cases'), alpha=0.2)+
  geom_line(data = summary3[summary3$variable=="R",], aes(x=date, y=median, col='Hospitalisations'))+
  geom_ribbon(data = summary3[summary3$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Hospitalisations'), alpha=0.2)+
  geom_ribbon(data = summary3[summary3$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Hospitalisations'), alpha=0.2)+
  geom_line(data = summary4[summary4$variable=="R",], aes(x=date, y=median, col='Deaths'))+
  geom_ribbon(data = summary4[summary4$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Deaths'), alpha=0.2)+
  geom_ribbon(data = summary4[summary4$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Deaths'), alpha=0.2)+
  theme_bw(base_size=14)+
  scale_fill_brewer("Time series",palette = "Dark2")+
  scale_color_brewer("Time series",palette = "Dark2")+
  geom_hline(yintercept = 1, linetype="dashed")+
  xlab("Date")+
  ylab("Reproduction number")

plt4c



#####################################################################################################################################################
# 5. Account for delay distributions when estimating Rt from:
#    1) infections (as before)
#    2) cases
#    3) hospitalisations
#    4) deaths
#####################################################################################################################################################
# Data has been collected that allows the following delay distributions to be estimated:
# 1. The incubation period (Delay from infection to symptoms)
# 2. Delay from symptoms to case reporting
# 3. Delay from symptoms to hospitalisation
# 4. Delay from symptoms to death


incubation_delay <- EpiNow2::Gamma(mean=3, sd=2, max = 14)
symptom_case_delay <- EpiNow2::Gamma(mean=2, sd=1.6, max=7)
symptom_hosp_delay <- EpiNow2::Gamma(mean=10, sd=3, max=28)
symptom_death_delay <- EpiNow2::Gamma(mean=15, sd=5, max=28)


# EpiNow2 distributions (as specified above) can be added together and included in model fitting
# Derive an expression for the delay from infection to: (1) cases; (2) hospitalisation; and (3) death
###### Fill in ##########
case_delay = incubation_delay + symptom_case_delay # 0
hosp_delay = incubation_delay + symptom_hosp_delay # 0
death_delay = incubation_delay + symptom_death_delay # 0

################################################################################
# 5.a To complete
################################################################################
# Fit a model estimating Rt to each time-series.
# Note that you have already previously fitted a model to the infection time series
# The epinow function allows a delay distribution to be specified e.g. delays = delay_opts(case_delay)

model2_delay <- epinow(
  data = data.frame(date=df1$date, confirm=df1$cases),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(case_delay),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model2_delay, 'epinow_model_fits/model_cases_delay.rds')

model3_delay <- epinow(
  data = data.frame(date=df1$date, confirm=df1$hospitalisations),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(hosp_delay),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model3_delay, 'epinow_model_fits/model_hospitalisations_delay.rds')

model4_delay <- epinow(
  data = data.frame(date=df1$date, confirm=df1$deaths),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(death_delay),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model4_delay, 'epinow_model_fits/model_deaths_delay.rds')

####### Pre-fitted models #########
# Confirm code above is correct and runs, before loading pre-fitted models

model1 <- readRDS('epinow_model_fits/model_fit_example.rds') # model fit to infection incidence
model2_delay <- readRDS('epinow_model_fits/model_cases_delay.rds') # model fit to cases with delay
model3_delay <- readRDS('epinow_model_fits/model_hospitalisations_delay.rds') # model fit to hospitalisations with delay
model4_delay <- readRDS('epinow_model_fits/model_deaths_delay.rds') # model fit to deaths with delay


################################################################################
# 5.b To complete
################################################################################
# Produce a plot comparing R estimates made using each of the four different time series
# Compare it to your previous plot from 4.c

summary1 <- model1$estimates$summarised
summary2_delay <- model2_delay$estimates$summarised
summary3_delay <- model3_delay$estimates$summarised
summary4_delay <- model4_delay$estimates$summarised

plt5b <- ggplot()+
  geom_line(data = summary1[summary1$variable=="R",], aes(x=date, y=median, col='Infections'))+
  geom_ribbon(data = summary1[summary1$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Infections'), alpha=0.2)+
  geom_ribbon(data = summary1[summary1$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Infections'), alpha=0.2)+
  geom_line(data = summary2_delay[summary2_delay$variable=="R",], aes(x=date, y=median, col='Cases'))+
  geom_ribbon(data = summary2_delay[summary2_delay$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Cases'), alpha=0.2)+
  geom_ribbon(data = summary2_delay[summary2_delay$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Cases'), alpha=0.2)+
  geom_line(data = summary3_delay[summary3_delay$variable=="R",], aes(x=date, y=median, col='Hospitalisations'))+
  geom_ribbon(data = summary3_delay[summary3_delay$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Hospitalisations'), alpha=0.2)+
  geom_ribbon(data = summary3_delay[summary3_delay$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Hospitalisations'), alpha=0.2)+
  geom_line(data = summary4_delay[summary4_delay$variable=="R",], aes(x=date, y=median, col='Deaths'))+
  geom_ribbon(data = summary4_delay[summary4_delay$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50,  fill='Deaths'), alpha=0.2)+
  geom_ribbon(data = summary4_delay[summary4_delay$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90,  fill='Deaths'), alpha=0.2)+
  theme_bw(base_size=14)+
  scale_fill_brewer("Time series",palette = "Dark2")+
  scale_color_brewer("Time series",palette = "Dark2")+
  geom_hline(yintercept = 1, linetype="dashed")+
  xlab("Date")+
  ylab("Reproduction number")

plt5b


################################################################################
# 5.c To complete
################################################################################
# Produce a four-panel plot showing the fit of each model to each time series

plt5c.1 <- ggplot()+
  geom_line(data = summary1[summary1$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary1[summary1$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary1[summary1$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=incidence))+
  xlab("Date")+
  ylab("Infection incidence")

plt5c.2 <- ggplot()+
  geom_line(data = summary2_delay[summary2_delay$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary2_delay[summary2_delay$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary2_delay[summary2_delay$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=cases))+
  xlab("Date")+
  ylab("Cases")

plt5c.3 <- ggplot()+
  geom_line(data = summary3_delay[summary3_delay$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary3_delay[summary3_delay$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary3_delay[summary3_delay$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=hospitalisations))+
  xlab("Date")+
  ylab("Hospitalisations")

plt5c.4 <- ggplot()+
  geom_line(data = summary4_delay[summary4_delay$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary4_delay[summary4_delay$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary4_delay[summary4_delay$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df, aes(x=date, y=deaths))+
  xlab("Date")+
  ylab("Deaths")

plt5c.1 + plt5c.2+ plt5c.3+ plt5c.4 +plot_layout(nrow=4)

################################################################################
# 5.d To complete
################################################################################
# The summary objects produced by epinow2 (e.g. 'model_fit_example$estimates$summarised')
# also includes estimates of the underlying infection incidence as another variable
# (e.g. vairable=="infections"). Add the estimates of infection incidence to the
# plots made in 5.c 


plt5d.1 <- plt5c.1 + 
  geom_line(data = summary1[summary1$variable=="infections",], aes(x=date, y=median), col='darkred')+
  geom_ribbon(data = summary1[summary1$variable=="infections",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), fill='darkred', alpha=0.2)+
  geom_ribbon(data = summary1[summary1$variable=="infections",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90),fill='darkred', alpha=0.2)

plt5d.2 <- plt5c.2 + 
  geom_line(data = summary2_delay[summary2_delay$variable=="infections",], aes(x=date, y=median), col='darkred')+
  geom_ribbon(data = summary2_delay[summary2_delay$variable=="infections",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), fill='darkred', alpha=0.2)+
  geom_ribbon(data = summary2_delay[summary2_delay$variable=="infections",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90),fill='darkred', alpha=0.2)

plt5d.3 <- plt5c.3 + 
  geom_line(data = summary3_delay[summary3_delay$variable=="infections",], aes(x=date, y=median), col='darkred')+
  geom_ribbon(data = summary3_delay[summary3_delay$variable=="infections",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), fill='darkred', alpha=0.2)+
  geom_ribbon(data = summary3_delay[summary3_delay$variable=="infections",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90),fill='darkred', alpha=0.2)

plt5d.4 <- plt5c.4 + 
  geom_line(data = summary4_delay[summary4_delay$variable=="infections",], aes(x=date, y=median), col='darkred')+
  geom_ribbon(data = summary4_delay[summary4_delay$variable=="infections",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), fill='darkred', alpha=0.2)+
  geom_ribbon(data = summary4_delay[summary4_delay$variable=="infections",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90),fill='darkred', alpha=0.2)


plt5d.1 + plt5d.2+ plt5d.3+ plt5d.4 + plot_layout(nrow=4)




