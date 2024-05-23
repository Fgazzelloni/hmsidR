# install.packages("keras3")
# keras3::install_keras(backend = "tensorflow")
library(keras3)
library(deSolve)
library(ggplot2)

# Define the SEIR model
SEIR <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Differential equations
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    
    # Return the rates of change
    list(c(dS, dE, dI, dR))
  })
}

# Parameters
parameters <- c(
  beta = 0.3,  # Infection rate
  sigma = 0.2, # Incubation rate
  gamma = 0.1 # Recovery rate
)

# Initial state values
N <- 1000
initial_state <- c(S = 999, E = 1, I = 0, R = 0)

# Time points
times <- seq(0, 160, by = 1)

# Solve the model
output <- ode(y = initial_state, times = times, func = SEIR, parms = parameters)

# Convert output to a data frame
output <- as.data.frame(output)


# Example social media data (features and labels)
set.seed(123)
n <- 1000  # number of samples
p <- 10    # number of features

# Generate random features and labels
social_features <- matrix(rnorm(n * p), nrow = n, ncol = p)
infection_labels <- sample(0:1, n, replace = TRUE)

# Combine into a data frame
social_data <- data.frame(social_features)
social_data$infection <- infection_labels



# Define the model
model <- keras_model_sequential(input_shape = c(p)) 

# simple model
model %>% 
  layer_dense(units = 1) %>% 
  layer_activation('relu') %>%
  #layer_dropout(rate = 0.3) %>%
  #layer_dense(units = 64) %>% 
  #layer_activation('relu') %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense( units = 1, activation = 'sigmoid')


#model %>%
#  layer_dense(units = 128,  
#              input_shape = c(10)) %>%
#  layer_activation('relu') %>%
#  #layer_dropout(rate = 0.4) %>%
#  #layer_dense(units = 64) %>%
#  #layer_activation('relu') %>%
#  #layer_dropout(rate = 0.3) %>%
#  layer_dense(units = 1)%>%
#  layer_activation('sigmoid') 

# Compile the model
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the model
history <- model %>% fit(
  x = as.matrix(social_data[, 1:p]),
  y = social_data$infection,
  epochs = 30, 
  batch_size = 128, 
  validation_split = 0.2
  #epochs = 30,
  #batch_size = 32,
  #validation_split = 0.2
)

history2 <- model %>% fit(
  x = as.matrix(social_data[, 1:p]),
  y = social_data$infection,
  epochs = 30/2, 
  batch_size = 128/2, 
  validation_split = 0.2
  #epochs = 30,
  #batch_size = 32,
  #validation_split = 0.2
)


# Predict infection status for new social media data
new_social_data <- matrix(rnorm(p * 160), nrow = 160, ncol = p)
predicted_infections <- model %>% predict(new_social_data)
predicted_infections <- ifelse(predicted_infections > 0.5, 1, 0)

# Adjust the SEIR parameters based on predictions
adjusted_parameters <- parameters
adjusted_parameters["beta"] <- 
  adjusted_parameters["beta"] * (1 + mean(predicted_infections))

# Solve the SEIR model with adjusted parameters
adjusted_output <- ode(y = initial_state, 
                       times = times, 
                       func = SEIR, 
                       parms = adjusted_parameters)

# Convert output to a data frame
adjusted_output <- as.data.frame(adjusted_output)

# Plot the results
ggplot() +
  geom_line(data = output, aes(x = time, y = I, color = "Original Infections")) +
  geom_line(data = adjusted_output, aes(x = time, y = I, color = "Adjusted Infections")) +
  labs(y = "Infectious Population", color = "Scenario") +
  theme_minimal() +
  ggtitle("SEIR Model with and without Social Media Adjustments")

