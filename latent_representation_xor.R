library(tidyverse)
library(glue)
library(gganimate)

hidden_1000 <- read_csv("hidden_1000.csv")
hidden_1000 %>% 
  mutate(epoch = 1000,
         col = c(0, 1, 1, 0),
         x_axis = c("H1", "H1", "H1", "H1"),
         y_axis = c("H2", "H2", "H2", "H2")) %>% 
  select(epoch, col, everything()) -> hidden_1000
hidden_2000 <- read_csv("hidden_2000.csv")
hidden_2000 %>% 
  mutate(epoch = 2000,
         col = c(0, 1, 1, 0),
         x_axis = c("H1", "H1", "H1", "H1"),
         y_axis = c("H2", "H2", "H2", "H2")) %>% 
  select(epoch, col, everything()) -> hidden_2000
hidden_3000 <- read_csv("hidden_3000.csv")
hidden_3000 %>% 
  mutate(epoch = 3000,
         col = c(0, 1, 1, 0),
         x_axis = c("H1", "H1", "H1", "H1"),
         y_axis = c("H2", "H2", "H2", "H2")) %>% 
  select(epoch, col, everything()) -> hidden_3000
hidden_4000 <- read_csv("hidden_4000.csv")
hidden_4000 %>% 
  mutate(epoch = 4000,
         col = c(0, 1, 1, 0),
         x_axis = c("H1", "H1", "H1", "H1"),
         y_axis = c("H2", "H2", "H2", "H2")) %>% 
  select(epoch, col, everything()) -> hidden_4000

raw <- tribble(
  ~epoch, ~col, ~H1, ~H2, ~x_axis, ~y_axis,
  0, 0, 0, 0, "X1", "X2",
  0, 1, 1, 0, "X1", "X2",
  0, 1, 0, 1, "X1", "X2",
  0, 0, 1, 1, "X1", "X2"
)

df <- bind_rows(raw, hidden_1000, hidden_2000, hidden_3000, hidden_4000)

# example plot
hidden_4000 %>% 
  ggplot(aes(H1, H2, col = factor(col))) +
  geom_point(size = 5) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.title.position = "panel",
        plot.title = element_text(size = 20)) +
  labs(title = 'epoch: 4000',
       x = '',
       y = '')

# animated plot
df %>% 
  ggplot(aes(H1, H2, col = factor(col))) +
  geom_point(size = 5) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.title.position = "panel",
        plot.title = element_text(size = 20)) +
  transition_states(epoch, transition_length = 2, state_length = 1) +
  labs(title = 'epoch: {closest_state}',
       x = '',
       y = '') +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> xor_anim

xor_anim <- animate(xor_anim)
anim_save("xor_anim_epochs.gif", xor_anim)
# this is nice!
#####################################33

# Make another gif to show how the input space (with the original datapoints) is being transformed. Kind of like
# what Chris Olah showed

library(LaplacesDemon)

# make a starting grid of points
df <- crossing(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01))

# what does this look like?
df %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.1)

# Create weight and biases matrices
# this is for a model I trained on 08/25/2021 (code in https://colab.research.google.com/drive/1lWZeKpy3EhVa7RzJ5HP453grDwzHXgts?usp=sharing)

hidden_weights <- t(matrix(c(3.6851254,  2.5282872, -4.051222 , -2.277597), nrow = 2, ncol = 2))
hidden_biases_df <- tibble(b1 = rep(-2.349773, nrow(df)), b2 = rep(1.0717324, nrow(df))) # we do this because of no broadcasting

df_output =  as.matrix(df) %*% hidden_weights + as.matrix(hidden_biases_df)
hidden_df = invlogit(df_output)
hidden_df <- data.frame(hidden_df)
colnames(hidden_df) <- c("x", "y")

# what does the hidden state look like?
hidden_df %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.1)

grid_start <- df %>% 
  mutate(id = row_number())
grid_end <- hidden_df %>% 
  mutate(id = row_number())

grid_all <- bind_rows(
  mutate(grid_start, time = 1),
  mutate(grid_end, time = 2)
)

x_breaks <- unique(grid_start$x)
y_breaks <- unique(grid_start$y)

p <- ggplot(aes(x = x, y = y), data = grid_all) +
  geom_point(size = 0.05) +
  scale_x_continuous(breaks = x_breaks, minor_breaks = NULL)+
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL)+
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p

p + gganimate::transition_states(time, wrap = FALSE)

# add xor points
xor <- tibble(x = c(0, 0, 1, 1),
              y = c(0, 1, 0, 1),
              col = c(0, 1, 1, 0))
hidden_weights <- t(matrix(c(3.6851254,  2.5282872, -4.051222 , -2.277597), nrow = 2, ncol = 2))
hidden_biases_xor <- tibble(b1 = rep(-2.349773, nrow(xor)), b2 = rep(1.0717324, nrow(xor)))

xor_output =  as.matrix(xor[-3]) %*% hidden_weights + as.matrix(hidden_biases_xor)
hidden_xor = invlogit(xor_output)
hidden_xor <- data.frame(hidden_xor)
colnames(hidden_xor) <- c("x", "y")

hidden_xor <- hidden_xor %>% 
                mutate(col = c(0, 1, 1, 0))

# plot these hidden values
hidden_xor %>% 
  ggplot(aes(x, y, col = factor(col))) +
  geom_point() +
  scale_fill_manual(values = c("red", "blue"))

xor_start <- xor %>% 
  mutate(id = row_number())
xor_end <- hidden_xor %>% 
  mutate(id = row_number())

xor_all <- bind_rows(
  mutate(xor_start, time = 1),
  mutate(xor_end, time = 2)
)

x_breaks <- unique(grid_start$x)
y_breaks <- unique(grid_start$y)

p <- ggplot() +
  geom_point(aes(x = x, y = y), data = grid_all, size = 0.001) +
  geom_point(aes(x, y, col = factor(col)), data = xor_all, size = 5, alpha = 1) +
  scale_fill_manual(values = c("red", "blue")) +
  #scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
  #scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_fixed()+
  theme_minimal()+
  theme(#axis.text = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_blank(),
        legend.position = "none")
p

p + gganimate::transition_states(time, wrap = FALSE)

# save animated plots
# animated plot
ggplot() +
  geom_point(aes(x = x, y = y), data = grid_all, size = 0.001) +
  geom_point(aes(x, y, col = factor(col)), data = xor_all, size = 5, alpha = 1) +
  scale_fill_manual(values = c("red", "blue")) +
  #scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
  #scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_fixed()+
  theme_minimal()+
  theme(#axis.text = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_blank(),
    legend.position = "none") +
  transition_states(time, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> xor_anim_colah_style

xor_anim_colah_style <- animate(xor_anim_colah_style)
anim_save("xor_anim_colah_style.gif", xor_anim_colah_style)

ggplot() +
  geom_point(aes(x = x, y = y), data = grid_all, size = 0.001) +
  geom_point(aes(x, y, col = factor(col)), data = xor_all, size = 5, alpha = 1) +
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
    #axis.text = element_text(size = 15),
    axis.title = element_blank(),
    legend.position = "none") +
  transition_states(time, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> xor_anim_colah_style_plain

xor_anim_colah_style_plain <- animate(xor_anim_colah_style_plain)
anim_save("xor_anim_colah_style_plain.gif", xor_anim_colah_style_plain)

###########################
# First linearly transform and nonlinearly transform
# make a starting grid of points
df_1 <- crossing(x = seq(0, 1, 0.001), y = 0)
df_2 <- crossing(x = seq(0, 1, 0.001), y = 1)
df_3 <- crossing(x = 0, y = seq(0, 1, 0.001))
df_4 <- crossing(x = 1, y = seq(0, 1, 0.001))

df <- bind_rows(df_1, df_2, df_3, df_4)

# what does this look like?
df %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.1)

# Create weight and biases matrices
# this is for a model I trained on 08/25/2021 (code in https://colab.research.google.com/drive/1lWZeKpy3EhVa7RzJ5HP453grDwzHXgts?usp=sharing)

# linear hidden values
hidden_weights <- t(matrix(c(3.6851254,  2.5282872, -4.051222 , -2.277597), nrow = 2, ncol = 2))
hidden_biases_df <- tibble(b1 = rep(-2.349773, nrow(df)), b2 = rep(1.0717324, nrow(df))) # we do this because of no broadcasting

df_output =  as.matrix(df) %*% hidden_weights + as.matrix(hidden_biases_df)
hidden_df = df_output
hidden_df <- data.frame(hidden_df)
colnames(hidden_df) <- c("x", "y")

# what does the hidden state look like?
hidden_df %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.1)

# nonlinear hidden values
hidden_nonlin = invlogit(df_output)
hidden_nonlin <- data.frame(hidden_nonlin)
colnames(hidden_nonlin) <- c("x", "y")

# what does the hidden state look like?
hidden_nonlin %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.1)

grid_start <- df %>% 
  mutate(id = row_number())
grid_inter <- hidden_df %>% 
  mutate(id = row_number())
grid_end <- hidden_nonlin %>% 
  mutate(id = row_number())

grid_all <- bind_rows(
  mutate(grid_start, time = 1),
  mutate(grid_inter, time = 2),
  mutate(grid_end, time = 3)
)

p <- ggplot(aes(x = x, y = y), data = grid_all) +
  geom_point(size = 0.05) +
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p

p + gganimate::transition_states(time, wrap = FALSE)

# add xor points
xor <- tibble(x = c(0, 0, 1, 1),
              y = c(0, 1, 0, 1),
              col = c(0, 1, 1, 0))

# linear hidden values
hidden_weights <- t(matrix(c(3.6851254,  2.5282872, -4.051222 , -2.277597), nrow = 2, ncol = 2))
hidden_biases_xor <- tibble(b1 = rep(-2.349773, nrow(xor)), b2 = rep(1.0717324, nrow(xor)))

xor_output =  as.matrix(xor[-3]) %*% hidden_weights + as.matrix(hidden_biases_xor)
hidden_xor = xor_output
hidden_xor <- data.frame(hidden_xor)
colnames(hidden_xor) <- c("x", "y")

hidden_xor <- hidden_xor %>% 
  mutate(col = c(0, 1, 1, 0))

# plot these hidden values
hidden_xor %>% 
  ggplot(aes(x, y, col = factor(col))) +
  geom_point() +
  scale_fill_manual(values = c("red", "blue"))

# nonlinear hidden xor values
xor_nonlin = invlogit(xor_output)
xor_nonlin <- data.frame(xor_nonlin)
colnames(xor_nonlin) <- c("x", "y")

xor_nonlin <- xor_nonlin %>% 
  mutate(col = c(0, 1, 1, 0))

# plot these hidden values
xor_nonlin %>% 
  ggplot(aes(x, y, col = factor(col))) +
  geom_point() +
  scale_fill_manual(values = c("red", "blue"))

xor_start <- xor %>% 
  mutate(id = row_number())
xor_inter <- hidden_xor %>% 
  mutate(id = row_number())
xor_end <- xor_nonlin %>% 
  mutate(id = row_number())

xor_all <- bind_rows(
  mutate(xor_start, time = 1),
  mutate(xor_inter, time = 2),
  mutate(xor_end, time = 3)
)

p <- ggplot() +
  geom_point(aes(x = x, y = y), data = grid_all, size = 0.001) +
  geom_point(aes(x, y, col = factor(col)), data = xor_all, size = 5, alpha = 1) +
  scale_fill_manual(values = c("red", "blue")) +
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none")
p

p + gganimate::transition_states(time, wrap = FALSE)

# save animation
ggplot() +
  geom_point(aes(x = x, y = y), data = grid_all, size = 0.001) +
  geom_point(aes(x, y, col = factor(col)), data = xor_all, size = 5, alpha = 1) +
  scale_fill_manual(values = c("red", "blue")) +
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  transition_states(time, transition_length = 2, state_length = 1) +
  enter_appear() +
  exit_disappear(early = TRUE) +
  ease_aes("sine-in-out") -> xor_anim_colah_style_accurate

xor_anim_colah_style_accurate <- animate(xor_anim_colah_style_accurate)
anim_save("xor_anim_colah_style_accurate.gif", xor_anim_colah_style_accurate)
