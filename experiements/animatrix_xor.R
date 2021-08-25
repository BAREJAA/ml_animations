library(tidyverse)
library(LaplacesDemon)

# make a starting grid of points
df <- crossing(x = seq(0, 1, 0.01), y = seq(0, 1, 0.01))

# what does this look like?
df %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.1)

# Create a "fake" neural network with some made up weights e.g.
h1_weights <- matrix(c(0.1, 0.6), nrow = 2, ncol = 1)
h2_weights <- matrix(c(-0.2, 0.2), nrow = 2, ncol = 1)

# create the corresponding H1 and H2 neuron values
H1 <- invlogit(as.matrix(df) %*% h1_weights)
H2 <- invlogit(as.matrix(df) %*% h2_weights)

hidden <- tibble(x = H1, y = H2)

grid_start <- df %>% 
                mutate(id = row_number())
grid_end <- hidden %>% 
            mutate(id = row_number())

grid_all <- bind_rows(
  mutate(grid_start, time = 1),
  mutate(grid_end, time = 2)
)

x_breaks <- unique(grid_start$x)
y_breaks <- unique(grid_start$y)

p <- ggplot(aes(x = x, y = y), data = grid_all)+
  geom_point(size = 0.01)+
  scale_x_continuous(breaks = x_breaks, minor_breaks = NULL)+
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL)+
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p

p + gganimate::transition_states(time, wrap = FALSE)

# try with just four points
xor <- tibble(x = c(0, 1, 0, 1),
              y = c(0, 0, 1, 1),
              col = c("red", "blue", "blue", "red"))
h1_xor <- invlogit(as.matrix(xor[-3]) %*% h1_weights)
h2_xor <- invlogit(as.matrix(xor[-3]) %*% h2_weights)

hidden <- tibble(x = h1_xor, y = h2_xor, col = c("red", "blue", "blue", "red"))

xor_start <- xor %>% 
  mutate(id = row_number())
xor_end <- hidden %>% 
  mutate(id = row_number())

xor_all <- bind_rows(
  mutate(xor_start, time = 1),
  mutate(xor_end, time = 2)
)

x_breaks <- unique(grid_start$x)
y_breaks <- unique(grid_start$y)

p <- ggplot() +
  geom_point(aes(x = x, y = y), data = grid_all, size = 0.001) +
  geom_point(aes(x, y, col = col), data = xor_all, size = 5, alpha = 1) +
  scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_fixed()+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
p

p + gganimate::transition_states(time, wrap = FALSE)
