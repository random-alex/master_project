# Скрипт для считывания одного файла и вытаскивания матрицы спиновых корреляций, построение 3D графика.

# prereq ------------------------------------------------------------------
library(rhdf5)
dir <- 'C:/Users/ZotovAV/Desktop/study/dip/Square model temperature loop/data/'


# functions ---------------------------------------------------------------

my_prep_pairs <- function(pairs) {
  str_extract_all(pairs,'[0-9]',simplify = F) %>%
    unlist() %>% 
    .[c(3,4)] %>% 
    str_c(sep = '_',collapse = '_')
}

my_read <- function(fol) {
  data1 <- h5read(fol,'simulation')
  data_param <- h5read(fol,'parameters')
  df <- tibble(temp = data_param$T,
               pairs = data1$results$`Spin Correlations`$labels,
               mean = data1$results$`Spin Correlations`$mean$value,
               error = data1$results$`Spin Correlations`$mean$error) %>% 
    rowwise() %>% 
    mutate(pairs = my_prep_pairs(pairs)) %>% 
    ungroup() %>% 
    separate( 'pairs',c('x','y'))
}
my_prep_plot <- function(df,t) {
  temp1 <- df %>% 
    filter(temp == t) %>% 
    .$mean
  dim(temp1) <- c(NROW(unique(df$x)),NROW(unique(df$y)))
  return(temp1)
}

# read files --------------------------------------------------------------

lst_types <- h5ls(lst[1],F)


df <- tibble(dir = list.files(dir,pattern = '.out.h5',full.names = T)) %>% 
  mutate(data = map(dir,my_read)) %>% 
  unnest(data) %>% 
  select(-dir) %>% 
  mutate_at(vars(x,y),funs(as.numeric))


# some plot ---------------------------------------------------------------

df %>% 
  filter(x == y) %>% 
  mutate(temp = as.factor(temp)) %>% 
  ggplot(aes(x,mean,col = temp)) +
  geom_line() +
  geom_point() +
  theme_bw()


df_t01 <- my_prep_plot(df,0.1)
df_t1 <- my_prep_plot(df,1)



plot_ly() %>% 
  add_surface( z = ~df_t01) %>% 
  layout(
    title = "Temp = 0.1")

