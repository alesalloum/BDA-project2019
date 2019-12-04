library(rstan)
library(ggplot2)
library(dplyr)
library(bayesplot)
#rstan_options(auto_write = TRUE)

# Read data
df <-read.csv("data/employee_data1.csv")
colnames(df)[1] <- "Age" 

lookup <- c("Female" = 0, "Male" = 1)
df$Gender <- lookup[df$Gender]


#datalist <- list("Age" = df$Age, "HourlyRate" = df$HourlyRate)

predictor_matrix <- data.frame("Education" = df$Education, "Gender" = df$Gender)

datalist <- list(N = dim(df)[1], 
                 K = 2,
                 x = predictor_matrix, 
                 y = df$MonthlyIncome)

stan_objekti <- stan("multiple_predictors.stan", data=datalist)

df_maker <- function(x_var, y_var, stan_objekti){
  
  df = data.frame(x = x_var, y = y_var)
  draws_mu <- extract(stan_objekti, pars = "mu", permuted=TRUE)

  mu <- apply(draws_mu$mu, 2, quantile, c(0.05, 0.5, 0.95)) %>%
    t() %>% data.frame(x = df$x, .)
  
  colnames(mu) <- c("vuosi", "x5", "x50", "x95")
  
  palautettava <- list("taulukko" = df, "keskiarvot" = mu)
  return(palautettava)
}

infopack <- df_maker(df$Education, df$MonthlyIncome, stan_objekti)

posterior <- as.matrix(stan_objekti)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("alpha", "beta[1]", "beta[2]", "sigma"),
           prob = 0.9) + plot_title


color_scheme_set("green")
mcmc_hist(posterior, pars = c("beta[2]", "sigma"))

color_scheme_set("brightblue")

color_scheme_set("pink")
mcmc_pairs(posterior, pars = c("beta[2]", "sigma"),
           off_diag_args = list(size = 1.5))

sim_tulokset = as.data.frame(extract(stan_objekti, permuted = TRUE))
age_range <- seq(18, 60)

infopack <- df_maker(df$HourlyRate, stan_objekti)

ky_trend_plot <- ggplot() + ggtitle("Kymenlaakson sairaanhoitopiiri") + 
  geom_point(aes(x, y), data = infopack$taulukko, size = 1) +
  labs(y = "30 päivän readmissio (%)", x= "Vuosi") +
  guides(linetype = F) + geom_line(aes(df$Education, x50), data = infopack$keskiarvot, color = 'red') +
  geom_line(aes(df$Education, x5), data = infopack$keskiarvot, color = 'red', linetype = "dashed") + 
  geom_line(aes(df$Education, x95), data = infopack$keskiarvot, color = 'red', linetype = "dashed")

ky_trend_plot