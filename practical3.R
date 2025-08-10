
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


# We can visualise the modelled output.
# The variable == "reported_cases" in the below code specifies the modelled epidemic
# time-series for comparison to the data we fit to.  
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











################################################################################
# 2.c To complete
################################################################################
# Plot infection incidence, growth rate, and reproduction number as one figure 
# with multiple panels. 









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












####### Pre-fitted models #########
# Confirm code above is correct and runs, before loading pre-fitted models

model_fit_example <- readRDS('epinow_model_fits/model_fit_example.rds')
model_gi_low <- readRDS('epinow_model_fits/model_gi_low.rds')
model_gi_high <- readRDS('epinow_model_fits/model_gi_high.rds')

################################################################################
#3.b To complete
################################################################################
# Produce a figure comparing the estimated reproduction numbers for each GI








################################################################################
# 3.c To complete
################################################################################
# Produce a figure comparing the estimated growth rates for each GI







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









################################################################################
# 4.c To complete
################################################################################
# Produce a plot comparing R estimates made using each of the four different time series








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


# EpiNow2 distributions (as specified above) can be added together (e.g. new_delay = delay1 +delay2)
# and included in model fitting.
# Derive an expression for the delay from infection to: (1) cases; (2) hospitalisation; and (3) death
###### Fill in ##########
case_delay = 0 # FILL IN
hosp_delay = 0 # FILL IN
death_delay = 0 # FILL IN

################################################################################
# 5.a To complete
################################################################################
# Fit a model estimating Rt to each time-series.
# Note that you have already previously fitted a model to the infection time series
# The epinow function allows a delay distribution to be specified e.g. delays = delay_opts(case_delay)







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






################################################################################
# 5.c To complete
################################################################################
# Produce a four-panel plot showing the fit of each model to each time series






################################################################################
# 5.d To complete
################################################################################
# The summary objects produced by epinow2 (e.g. 'model_fit_example$estimates$summarised')
# also includes estimates of the underlying infection incidence as another variable
# (e.g. vairable=="infections"). Add the estimates of infection incidence to the
# plots made in 5.c 









