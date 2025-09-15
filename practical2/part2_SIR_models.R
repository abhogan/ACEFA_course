# ACEFA short course: Practical 2
# Authors: Alexandra Hogan, Oliver Eales, Freya Shearer

# Simulating deterministic and stochastic SIR models in R

######################################################
# In this section of the course, we are going to
# replicate the SIR models that we looked at in Excel in the earlier
# tutorial, to see how we can simulate these models in R

######################################################
# Set up functions
# SIR ODE model - deterministic
sir_ode <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# Stochastic simulation
#set.seed(123) # you can uncomment to ensure you produce same set of random outputs each time
sir_stoch <- function(max_time = max_time,
                      dt = dt,
                      beta = beta,
                      gamma = gamma,
                      S = S0,
                      I = I0,
                      R = R0){
  time <- 0
  # Set up a dataframe to store results
  stoch_out <- data.frame(time = 0, S = S0, I = I0, R = R0)
  while (time < max_time && I > 0) {
    # Calculate probability of infection and recovery for each S and I
    prob_infection <- beta * I / N *dt
    prob_recovery <- gamma * dt

    # Implement the stochastic process for Infection and Recovery events.
    # Note that there are a number of ways that this could be done.

    # Infection events
    n_infect = rbinom(1, S, prob_infection)
    # Recovery events
    n_recovered = rbinom(1, I, prob_recovery)

    # Update states
    S <- S - n_infect
    I <- I + n_infect - n_recovered
    R <- R + n_recovered

    # Time to next event
    time <- time + dt

    # Record state
    stoch_out <- rbind(stoch_out, data.frame(time = time, S = S, I = I, R = R))
  }
  return(stoch_out)
}

######################################################
# Install the required differential equation solver package
# install.packages("deSolve")
library(deSolve)
library(tidyverse)

# Set up the population size and initial conditions
N <- 1e3 # Total population
I0 <- 10 # Initial infecteds
S0 <- N - I0 # Susceptibles
R0 <- 0 # Initial recovered
init_state <- c(S = S0, I = I0, R = R0)

# Time parameters
max_time <- 100
dt <- 0.1
times <- seq(0, max_time, by = dt)

# Epidemiological parameters
gamma <- 0.2
beta <- 0.5

###############################################
# Deterministic model

# Instead of formulating the model as a set of difference equations (in discrete time)
# we are going to formulate the model as a set of differential equations (in continuous time).

# Simulate the model, and store the outputs in a dataframe
params <- c(beta = beta, gamma = gamma, N = N)
out <- as.data.frame(ode(y = init_state, times = times, func = sir_ode, parms = params))

# Reformat dataframe (ggplot2 works with "long" data)
out_long <- out %>%
  pivot_longer(c(S, I, R)) %>%
  mutate(name = factor(name, levels = c("S", "I", "R"))) %>%
  rename(State = name,
         Value = value)

# Plot the model outputs
ggplot(out_long, aes(x = time, y = Value, col = State)) +
  geom_line(size = 1) +
  labs(title = "Deterministic model",
       x = "Time (days)", y = "Number of people") +
  theme_minimal() +
  scale_color_manual(values = c("#4E94D8", "#8CD871", "#D971CD"))

# What is the final attack rate proportion?
# Hint: because of our model structure, we know that all the ever-infected population end up in R


# What is the timing of the epidemic peak? (Hint: use the slice_max() function)


# Plot the effective reproduction number over time










######################################################
# Now implement this model as a stochastic process

# Initialise states
dt <- 0.1
max_time <- 100/dt

stoch_out <- sir_stoch(max_time = max_time,
                        dt = dt,
                        beta = beta,
                        gamma = gamma,
                        S = S0,
                        I = I0,
                        R = R0)

# Plot results
stoch_out_long <- pivot_longer(stoch_out,
                               cols = c("S", "I", "R"),
                               names_to = "State", values_to = "Value") %>%
  mutate(State = factor(State, levels = c("S", "I", "R")))

ggplot(stoch_out_long, aes(x = time, y = Value, color = State)) +
  geom_line(size = 1) +
  labs(title = "Stochastic model",
       x = "Time (days)", y = "Number of people") +
  theme_minimal() +
  scale_color_manual(values = c("#4E94D8", "#8CD871", "#D971CD"))

# What happens if you change N to a different population size?


######################################################
# What if we want to plot the deterministic and stochastic models on the same figure?
out_long$Model <- "Deterministic"
stoch_out_long$Model <- "Stochastic"

out_combined <- rbind(out_long, stoch_out_long)

ggplot(out_combined, aes(x = time, y = Value, color = State, linetype = Model)) +
  geom_line(size = 1) +
  labs(title = "Different models",
       x = "Time (days)", y = "Number of people") +
  theme_minimal() +
  scale_color_manual(values = c("#4E94D8", "#8CD871", "#D971CD"))

########################################################
# What about if I want to visualise multiple stochastic simulations?

df <- NULL
for(i in 1:10){
  stoch_out <- sir_stoch(max_time = max_time,
                         dt = dt,
                         beta = beta,
                         gamma = gamma,
                         S = S0,
                         I = I0,
                         R = R0) %>%
    mutate(run = i)
  df <- rbind(df, stoch_out)
}

ggplot(df, aes(x = time, y = I, group = run)) +
  geom_line(col = "darkgrey") +
  theme_minimal() +
  labs(x = "Time (days)", y = "Number of people")

###########################################################
# Modelling incidence over time.

sir_ode_inc <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    dInc <-   ############################
    return(list(c(dS, dI, dR, dInc)))
  })
}
init_state2 <- c(S = S0, I = I0, R = R0, Inc = 0)

out2 <- as.data.frame(ode(y = init_state2, times = times, func = sir_ode_inc, parms = params))

head(out2)

# Reformat dataframe (ggplot2 works with "long" data). Note that incidence in our model outputs is cumulative to need to calculate the difference at each timestep





















###########################################################
