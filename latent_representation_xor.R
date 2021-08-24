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
anim_save("xor_anim.gif", xor_anim)
# this is nice, but I'm not sure it works... :S
# How to get the axis labels to change (from X1 and X2 fir the first plot to H1 and H2 for the subsequent ones?)
