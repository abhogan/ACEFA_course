# ACEFA short course: Practical 2
# Authors: Alexandra Hogan, Oliver Eales, Freya Shearer

# Simulating an SIR model

######################################################
# In this section of the course, we are going to
# replicate the SIR model that we looked at in Excel in the earlier
# tutorial, to see how we can simulate this in R

######################################################
# Deterministic model

# Instead of formulating the model as a set of difference equations (in discrete time)
# we are going to formulate the model as a set of differential equations (in continuous time).

# Install the required differential equation solver package
# install.packages("deSolve")
library(deSolve)
library(tidyverse)

sir_ode <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Total population
N <- 1e3
I0 <- 10
S0 <- N - I0
R0_init <- 0
init_state <- c(S = S0, I = I0, R = R0_init)

# Time vector
times <- seq(0, 100, by = 1)

# Epidemiological parameters
gamma <- 0.2
beta <- 0.5
R0 <- beta/gamma
params <- c(beta = beta, gamma = gamma, N = N)

# Simulate the model, and store the outputs in a dataframe
out <- as.data.frame(ode(y = init_state, times = times, func = sir_ode, parms = params))

# Reformat dataframe (ggplot2 works with "long" data)
out_long <- out %>%
  pivot_longer(c(S, I, R)) %>%
  mutate(name = factor(name, levels = c("S", "I", "R"))) %>%
  rename(state = name)

# Plot the model outputs
ggplot(out_long, aes(x = time, y = value, col = state)) +
  geom_line(size = 1) +
  labs(title = "Deterministic model",
       x = "Time (days)", y = "Number of people") +
  theme_minimal() +
  scale_color_manual(values = c("#4E94D8", "#8CD871", "#D971CD"))

# What is the final attack rate proportion?
# Because of our model structure, we know that all the ever-infected population end up in R
max(out$R)/N

######################################################
# Now implement this model as a stochastic process

# Initialise states
S <- S0
I <- I0
R <- R0
dt <- 0.1
max_time <- 100/dt

# Set up a dataframe to store results
stoch_out <- data.frame(time = 0, S = S, I = I, R = R)

# Stochastic simulation
#set.seed(123) # ensure you produce same set of random outputs each time
time <- 0

while (time < max_time && I > 0) {
  # Calculate probability of infection and recovery for each S and I
  prob_infection <- beta * I / N *dt
  prob_recovery <- gamma * dt

  # Infection events
  n_infect = qbinom(runif(1), S, prob_infection)
  # Recovery events
  n_recovered = qbinom(runif(1), I, prob_recovery)

  # Update states
  S <- S - n_infect
  I <- I + n_infect - n_recovered
  R <- R + n_recovered

  # Time to next event
  time <- time + dt

  # Record state
  stoch_out <- rbind(stoch_out, data.frame(time = time, S = S, I = I, R = R))

}

# Plot results
stoch_out_long <- pivot_longer(stoch_out,
                               cols = c("S", "I", "R"),
                               names_to = "State", values_to = "Value")

ggplot(stoch_out_long, aes(x = time, y = Value, color = State)) +
  geom_line(size = 1) +
  labs(title = "Stochastic model",
       x = "Time (days)", y = "Number of people") +
  theme_minimal() +
  scale_color_manual(values = c("#4E94D8", "#8CD871", "#D971CD"))

