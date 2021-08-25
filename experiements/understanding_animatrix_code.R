# understanding `transform_df_coords`

transform_df_coords <- function(df, ..., m = diag(length(df))){
  
  df_names <- names(df)
  
  df_coords <- df %>% 
    select(...)
  
  df_coords_names <- names(df_coords)
  
  df_matrix <- df_coords %>% 
    as.matrix() %>% 
    t()
  
  df_coords_new <- (m %*% df_matrix) %>% 
    t() %>% 
    as_tibble() %>% 
    set_names(df_coords_names)
  
  df_other <- df %>% 
    select(-one_of(df_coords_names))
  
  bind_cols(df_coords_new, df_other) %>% 
    select(df_names)
}

# example
transform_df_coords(tibble(x = 1:4, y = 1:4), x, y, m = matrix(1:4, nrow = 2)) %>% 
  knitr::kable()

df = tibble(x = 1:4, y = 1:4)
m = matrix(1:4, nrow = 2)

df_names <- names(df)
df_coords <- df %>% 
  select(x, y)
df_coords_names <- names(df_coords)
df_matrix <- df_coords %>% 
  as.matrix() %>% 
  t()
m %*% df_matrix # matrix mutliplication
m %*% df_matrix %>% t() # transpose
df_coords_new <- (m %*% df_matrix) %>% 
  t() %>% 
  as_tibble() %>% 
  set_names(df_coords_names)
df_other <- df %>% 
  select(-one_of(df_coords_names)) # produces an empty tibble with 4 rows and 0 columns
# why do this?
bind_cols(df_coords_new, df_other) %>% 
  select(df_names) # this is just df_coords_new

# conclusion: this function performs a matrix multiplication between the transformation ("m") and the input matrix (the first argument)
# so in the case of XOR, the raw input would be the first argument (including the X1 and X2 values), and "m" would be the
# first weights matrix
###########################33

  
