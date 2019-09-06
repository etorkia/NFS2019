#Authored by Robert Brown III for the Need for Speed Study 2019, prepared for Technology Partnerz Ltd.
#Please cite any use of this code or Libraries.

library(leonRdo)
#set.seed(42)
csv_path = "c:\\tmp\\Test_Returns.csv"

# Import data and clean it
Test_Returns = read.csv(csv_path)[, -1]
sectors = colnames(Test_Returns)
sectors = sub("_Returns", "", sectors)
colnames(Test_Returns) = sectors

# Initial Value
initial.value = 2.5e5

# Calculate the mean return and annual volatility of each sector
mean_return_all = colMeans(Test_Returns[, seq_along(sectors)])
names(mean_return_all) = sectors

volatility_all = sapply(seq_along(sectors), function(s)
  sd(Test_Returns[, s]))
names(volatility_all) = sectors

annualized_vol = volatility_all * sqrt(12)

# Calculate the covariance of the sectors
cov_mat = cov(Test_Returns)


# Calculate the correlation of the sectors
cor_mat = cor(Test_Returns, method = "spearman")


# Set up allocation vector and matrix
Allocations_sm = rep(1/length(sectors), length(sectors))
Allocations = matrix(rep(Allocations_sm, length(sectors)), nrow = length(sectors), byrow = T)

# Establish a set of test quantiles
Dist_Pvalues = c(c(0.01,0.05),
                  seq(0.1, 0.9, 0.1),
                  c(0.95,0.99,0.999,0.9999,0.99999,0.999999)
                  )

trial_length = c(10000, 50000, 100000)
numcycles = 20

port_sim_results_q = matrix(0, nrow = numcycles * length(trial_length), ncol = length(Dist_Pvalues) + 6)
colnames(port_sim_results_q) = c("Mean", "Std_Dev", "Min", "Max", Dist_Pvalues, "Trials", "Duration [sec]")

theor_results_q = port_sim_results_q
sect_sim_results_q = port_sim_results_q

for (t in seq_along(trial_length)) {
  for (c in 1:numcycles) {

# Using the aggregate portfolio mean and volatility, find the return distribution
# of the portfolio via...
# a simulated Normal with 1M trials.
#Sys.sleep(3)
port_sim_start = Sys.time()
# Determine the portfolio level mean return and volatility
Portfolio_Var = mean((Allocations %*% cov_mat) %*% Allocations_sm)
Portfolio_Std = sqrt(Portfolio_Var)
Portfolio_Mean = (Allocations_sm %*% mean_return_all)[1,1]

#Portfolio_Returns_Dist = (1 + rnorm(trial_length[t], Portfolio_Mean, Portfolio_Std)) * initial.value
#Calculated_Portfolio_Results = quantile(Portfolio_Returns_Dist, Dist_Pvalues)
#port_sim_stop = Sys.time()
#sim_dur = difftime(port_sim_stop, port_sim_start)
#port_sim_results_q[(t - 1) * numcycles + c, ] = c(mean(Calculated_Portfolio_Results),
                                                   #sd(Calculated_Portfolio_Results),
                                                   #min(Calculated_Portfolio_Results),
                                                   #max(Calculated_Portfolio_Results),
                                                   #Calculated_Portfolio_Results,
                                                   #trial_length[t],
                                                   #sim_dur)

## a theoretical Normal
##theor_sim_start = Sys.time()
#Calculated_Portfolio_Results2 = (1 + qnorm(Dist_Pvalues, Portfolio_Mean, Portfolio_Std)) * initial.value
#theor_sim_stop = Sys.time()
#theor_dur = difftime(theor_sim_stop, theor_sim_start)
#theor_results_q[(t - 1) * numcycles + c, ] = c(Portfolio_Mean,
                                                #Portfolio_Std,
                                                #0,
                                                #Inf,
                                                #Calculated_Portfolio_Results2,
                                                #trial_length[t],
                                                #theor_dur)

# Correlate the sectors with Iman-Conover
      sector_sim_start <- Sys.time()
      Sim_sector_returns <- sapply(sectors, function(s)
          rnorm(trial_length[t], mean_return_all[s], volatility_all[s])
  )

      Sim_sector_returns_cor <- correlate_vars(Sim_sector_returns, cor_mat)
      colnames(Sim_sector_returns_cor) <- sectors

      Sim_portfolio_pct_return <- (Sim_sector_returns_cor %*% Allocations_sm)
      Sim_portfolio_return <- (1 + Sim_portfolio_pct_return) * initial.value
      sector_sim_stop <- Sys.time()
      sector_dur <- difftime(sector_sim_stop, sector_sim_start)

      Sector_up_portfolio_results <- quantile(Sim_portfolio_return, Dist_Pvalues)
      sect_sim_results_q[(t - 1) * numcycles + c,] <- c(mean(Sim_portfolio_return),
                                                   sd(Sim_portfolio_return),
                                                   min(Sim_portfolio_return),
                                                   max(Sim_portfolio_return),
                                                   Sector_up_portfolio_results,
                                                   trial_length[t],
                                                   sector_dur)

  }
}

#results_path_port = paste0("c:\\tmp\\CRAN_results_port.csv")
#results_path_theor = paste0("c:\\tmp\\R_results_theor.csv")
results_path_sect = paste0("c:\\tmp\\R_results_sect.csv")

#write.csv(port_sim_results_q, file = results_path_port)
 #write.csv(theor_results_q, file = results_path_theor)
write.csv(sect_sim_results_q, file = results_path_sect)
