# ------------------------------------------------------------------------------
# Read in simulated lapse data from Dutang

library(tidyverse)

lapse_readin <- read.delim('C:/Emma/Thesis/dataset/simulated.portfolio-3 months.txt',
                           header = TRUE, sep=' ')

lapse_all <- lapse_readin %>% 
  as_tibble %>% 
  mutate(issue.date = as.Date(issue.date),
         end.date = as.Date(end.date)) %>% 
  mutate_if(is.character,as.factor)

# full years in scope 1996 - 2009

min(lapse_all %>% filter(!is.na(end.date)) %>% pull(end.date))
max(lapse_all %>% filter(!is.na(end.date)) %>% pull(end.date))

# ratio censored data?
lapse_all %>%
  filter(is.na(end.date)) %>%
  nrow()

lapse_all %>%
  summarise(ratio = mean(is.na(end.date))) %>%
  pull(ratio)


# ------------------------------------------------------------------------------
# Chapter 2.1: VISUALISATION

lapse_all %>%
  summarise(n_unique_policies = n_distinct(id.contract))

lapse_all <- lapse_all %>%
  mutate(end.date = coalesce(end.date, as.Date("2010-05-31"))) %>% 
  na.omit(lapse_all) %>% 
  filter(year(issue.date) < 2009) 


save(lapse_all, file = "lapse_all")

nrow(lapse_all)
lapse_all %>%
  filter(annual.premium==0) %>%
  summarise(n_zeroprem = n_distinct(id.contract))

summary(lapse_all)


# total nr of contracts
lapse_all %>%
  summarise(
    n_unique_policies = n_distinct(id.contract),
    total_surrenders = sum(surrender.bit, na.rm = TRUE), # Summing surrenders directly
    surrender_ratio = total_surrenders / n_unique_policies
  )

# Calculate the average surrender.bit for all specified variables in one step
final_result <- lapse_all %>%
  pivot_longer(cols = all_of(c("acc.death.amount", "underwriting.age","living.place", "premium.frequency", "gender", "risk.state")), names_to = "variable", values_to = "level") %>%
  group_by(variable, level) %>%
  summarise(avg_surrender = mean(surrender.bit, na.rm = TRUE), .groups = "drop")

# Print the final result
print(final_result)


# visualization features (graph chapter 2.1)
library(ggplot2)
library(gridExtra)
library(dplyr)

# graphs for 'lapse_all'
blue_color <- "#1D8DB0"

# List of categorical variables
categorical_vars <- c("acc.death.amount", "gender", "premium.frequency", "risk.state",
                      "underwriting.age", "living.place", "termination.cause")

# Create a list to store the plots
plot_list <- list()

# Loop through each categorical variable and create a bar plot with percentages
for (var in categorical_vars) {
  # Calculate percentages
  percentages <- lapse_all %>%
    group_by(.data[[var]]) %>%
    summarise(n = n()) %>%
    mutate(percentage = round((n / sum(n)) * 100),
           label = paste0(percentage, "%"))
  
  p <- ggplot(lapse_all, aes(x = .data[[var]])) +
    geom_bar(fill = blue_color) +
    geom_text(data = percentages, aes(y = n / 2, label = label), color = "black", size = 3) +
    labs(title = var, x = var, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(color = blue_color),
          axis.text.x = element_text(angle = 45, hjust = 1))
  plot_list[[var]] <- p
}

# Create a histogram for annual.premium
premium_hist <- ggplot(lapse_all, aes(x = annual.premium)) +
  geom_histogram(fill = blue_color, color = "black") +
  labs(title = "annual.premium", x = "annual.premium", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(color = blue_color))

# Add the premium histogram to the plot list
plot_list[["annual.premium"]] <- premium_hist

# Arrange the plots in a 2x4 grid with increased vertical spacing
grid.arrange(grobs = plot_list, nrow = 2, ncol = 4, heights = c(1, 1)) 

# Save as PNG
png("portfolio_overview.png", width = 12, height = 6, units = "in", res = 300)
grid.arrange(grobs = plot_list, nrow = 2, ncol = 4, heights = c(1, 1))
dev.off()
cat("Plot grid saved as portfolio_overview.png\n")


# ------------------------------------------------------------------------------
# Chapter 2.2: Lapse risk quantification

# Putting data set into correct format for lapse modelling
# Select policies that were active at the start of that year and not already lapsed
# Add a data year specifier
# Add a Boolean to show whether that policy lapsed in that year or not
# Add the duration in force at start of the year

lapses_peryear <- lapply(1996:2009, function(year){
  lapse_all %>% 
    filter(year(issue.date) < year & year(end.date)>= year) %>% 
    mutate(data_year = year) %>% 
    mutate(surrenders = case_when(year(end.date) == year & surrender.bit == 1 ~ TRUE,
                                  TRUE ~ FALSE)) %>% 
    mutate(duration_if = time_length(difftime(as.Date(paste0(year-1,'-12-31')), issue.date), "years"))
}) %>% do.call(rbind, .)

lapse_classification_dataset <- lapses_peryear %>% 
  mutate(start_year = year(issue.date)) %>% 
  select(policy_id = id.contract, data_year, #start_year, 
         surrenders, duration_if, acc_death_rider = acc.death.amount,
         gender, prem_freq = premium.frequency, annual_prem = annual.premium,
         risk_class = risk.state, ageph = underwriting.age, 
         living_place = living.place)
save(lapse_classification_dataset, file = 'lapse_classification_dataset')


# Exploratory data analysis
lapse_classification_dataset %>%
  filter(surrenders == TRUE) %>%  # Keep only rows where surrenders == TRUE
  group_by(data_year) %>%
  summarise(total_surrenders = n(), .groups = "drop")  # Count number of surrenders

lapse_classification_dataset %>%
  filter(surrenders == TRUE) %>%  # Keep only rows where surrenders == TRUE
  mutate(duration_bin = floor(duration_if)) %>%  # Bin duration_if into 1-year groups
  group_by(duration_bin) %>%
  summarise(total_surrenders = n(), .groups = "drop")


# ------------------------------------------------------------------------------
# Reading in market returns for 1995 - 2010

# Equity Indices (Stock Market)
# 1. S&P 500 (^GSPC)
# 2. Russell 2000 (^RUT for small-cap stocks)

# Bonds & Fixed Income
# 3. U.S. Corporate Bonds (BAMLCC0A0CMTRIV from FRED)

# Risk-Free Rates
# 4. 1-Year Treasury Yield (DGS1 from FRED)

#install.packages(c("tidyquant", "quantmod", "lubridate"))
library(tidyquant)
library(quantmod)
library(lubridate)

# Stock Market Indexes (from Yahoo Finance)
tickers <- c("^GSPC", "^RUT")  # S&P 500, Nasdaq 100, Russell 2000, MSCI World ETF
stock_data <- tq_get(tickers, from = "1995-01-01", to = "2010-12-31", get = "stock.prices")

# Calculate yearly returns for stocks
stock_returns <- stock_data %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(yearly_return = (last(adjusted) - first(adjusted)) / first(adjusted)) %>%
  pivot_wider(names_from = symbol, values_from = yearly_return)

# Bonds & Risk-Free Rates (from FRED)
fred_symbols <- c("DGS1", "BAMLC0A0CMEY")  # 1Y Treasury, Bank of America Corpobond yield
fred_data <- tq_get(fred_symbols, from = "1995-01-01", to = "2010-12-31", get = "economic.data")

# Convert FRED daily rates to yearly averages
bond_returns <- fred_data %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(yearly_return = mean(price, na.rm = TRUE)/100) %>%
  pivot_wider(names_from = symbol, values_from = yearly_return) %>%
  mutate(BAMLC0A0CMEY = ifelse(is.na(BAMLC0A0CMEY) & year == 1995, BAMLC0A0CMEY[year == 1996], BAMLC0A0CMEY))

# Combine stock and bond returns
market_returns <- left_join(stock_returns, bond_returns, by = "year") %>% rename(
  Fund1 = `^GSPC`,        # S&P 500
  Fund2 = `^RUT`,         # Russell 2000
  Fund3 = BAMLC0A0CMEY, # Corporate Bonds
  Fund4 = DGS1             # Treasury Bonds / Risk-Free Rate
)
save(market_returns, file = 'market_returns')


# ------------------------------------------------------------------------------
# Define profit sharing mechanism
set.seed(123)  # For reproducibility

profit_sharing <- market_returns %>%
  mutate(
    # Profit sharing rate for Fund1 (S&P 500) is reduced by a random percentage between 1% and 5%, but not below 0
    PS_rate_fund1 = pmax(Fund1 * (1 - runif(n(), 0.01, 0.05)), 0),
    
    # Profit sharing rate for Fund2 (Russell 2000) depends on the return:
    # - If return is less than 1%, rate is 0
    # - If return is between 1% and 3%, rate is 2%
    # - If return is between 3% and 5%, rate is 5%
    # - If return is greater than 5%, rate is reduced by a random percentage between 1% and 5%
    # - Otherwise, rate is 4%
    PS_rate_fund2 = case_when(
      Fund2 < 0.01 ~ 0,
      Fund2 < 0.03 ~ 0.02,
      Fund2 < 0.05 ~ 0.05,
      Fund2 * (1 - runif(n(), 0.01, 0.05)) > 0.05 ~ Fund2 * (1 - runif(n(), 0.01, 0.05)),
      TRUE ~ 0.04
    ),
    
    # Profit sharing rate for Fund3 (Corporate Bonds) is reduced by a random percentage between 1% and 5%, but not below 0.01 = guaranteed interest rate
    PS_rate_fund3 = pmax(Fund3 - runif(n(), 0.01, 0.05), 0.01),
    
    # Profit sharing rate for Fund4 (Treasury Bonds / Risk-Free Rate) remains unchanged
    PS_rate_fund4 = pmax(Fund4, 0)
  ) %>%
  select(year, PS_rate_fund1, PS_rate_fund2, PS_rate_fund3, PS_rate_fund4)
save(profit_sharing, file = 'profit_sharing')


load('lapse_classification_dataset')
load('market_returns')
load('profit_sharing')

# plot market data
ggplot(
  market_returns %>%
    filter(year >= 1995 & year <= 2009) %>%
    pivot_longer(cols = starts_with("Fund"), names_to = "Fund", values_to = "Return"),
  aes(x = year, y = Return, color = Fund)
) +
  geom_line(size = 1.2) +
  geom_point(size = 2.2) +
  scale_color_manual(values = fund_colors) +
  labs(
    title = "Market returns by fund (1995–2009)",
    x = "Year",
    y = "Annual Return",
    color = "Fund"
  ) +
  theme_minimal(base_size = 14)

ggsave("market_returns_plot.png", width = 10, height = 5, dpi = 300, bg = "white")


set.seed(123)  # For reproducibility
# Randomly assign a fund to each policy that has surrendered, with probabilities based on the year (based on market returns)
surr_true <- lapse_classification_dataset %>% filter(surrenders == TRUE) %>%
  mutate(
    fund = case_when(
      data_year <= 1998 ~ sample(c("Fund4", "Fund3", "Fund1", "Fund2"), n(), replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.15)),
      data_year == 1999 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
      data_year >= 2000 & data_year <= 2002 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.15)),
      data_year == 2003 | data_year == 2004 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
      data_year == 2005 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.25)),
      data_year == 2006 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
      data_year == 2007 | data_year == 2008 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)),
      data_year >= 2009 & data_year <= 2010 ~ sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25)),
      TRUE ~ NA_character_
    )
  )

# Filter policy_id which never has surrender = TRUE
never_surrender <- lapse_classification_dataset %>%
  group_by(policy_id) %>%
  summarise(ever_surrender = any(surrenders == TRUE), .groups = "drop") %>%
  filter(!ever_surrender) %>%
  mutate(fund = sample(c("Fund1", "Fund2", "Fund3", "Fund4"), n(), replace = TRUE))

# Bind never_surrender and surr_true together
combined_data <- bind_rows(surr_true %>% select(policy_id, fund), never_surrender %>% select(policy_id, fund))

# Add fund info back to full lapse data set
lapse_classification_dataset <- lapse_classification_dataset %>%
  left_join(combined_data, by = "policy_id")

# Look at distribution of fund choices
lapse_classification_dataset %>%
  group_by(fund) %>% summarise(total_surrenders = n(), .groups = "drop")


# ------------------------------------------------------------------------------
# Chapter 2.3: dynamic dataset simulation and modification

library(dplyr)
library(tidyr) # For pivot_longer

profit_sharing_long <- profit_sharing %>%
  pivot_longer(
    cols = starts_with("PS_rate_fund"),
    names_to = "fund_ps_col",
    values_to = "base_ps_rate"
  ) %>%
  mutate(fund = case_when(
    fund_ps_col == "PS_rate_fund1" ~ "Fund1",
    fund_ps_col == "PS_rate_fund2" ~ "Fund2",
    fund_ps_col == "PS_rate_fund3" ~ "Fund3",
    fund_ps_col == "PS_rate_fund4" ~ "Fund4",
    TRUE ~ NA_character_
  ),
  # If base_ps_rate is for 'year', it's awarded for 'year + 1'
  data_year_awarded = year + 1
  ) %>%
  select(data_year_awarded, fund, base_ps_rate) # Select the new column for joining


# graph (psrate/data year)
fund_colors <- c("Fund1" = "#52BDEC",  # red
                 "Fund2" = "#00407A",  # blue
                 "Fund3" = "#DD8A2E",  # green
                 "Fund4" = "#FFDE00")  # purple

# Create and save the plot
ggplot(profit_sharing_long, aes(x = data_year_awarded, y = base_ps_rate, color = fund)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.2) +
  scale_color_manual(values = fund_colors) +
  labs(title = "Profit sharing rates by fund over time",
       x = "Year Awarded",
       y = "PS Rate",
       color = "Fund") +
  theme_minimal(base_size = 14) 

ggsave("profit_sharing_rates.png", width = 10, height = 5, dpi = 300, bg = "white")



# --- Function to assign individual profit sharing rates with a one-year lag ---
assign_ps <- function(
    lapse_data,
    profit_sharing_rates_lookup,
    lapsers_shape1,
    lapsers_shape2,
    non_lapsers_shape1,
    non_lapsers_shape2
) {
  
  message(paste0("Lapsers Beta Parameters: alpha = ", lapsers_shape1, ", beta = ", lapsers_shape2))
  message(paste0("Non-Lapsers Beta Parameters: alpha = ", non_lapsers_shape1, ", beta = ", non_lapsers_shape2))
  
  # Join lapse_data with profit_sharing_rates_lookup
  lapse_data_with_base_ps <- lapse_data %>%
    left_join(profit_sharing_rates_lookup, by = c("data_year" = "data_year_awarded", "fund"))
  
  # Calculate the multiplier B using rbeta directly
  lapses_withPS_result <- lapse_data_with_base_ps %>%
    mutate(
      B = case_when( # Renamed from N to B
        surrenders == TRUE ~ rbeta(n(), shape1 = lapsers_shape1, shape2 = lapsers_shape2),
        surrenders == FALSE ~ rbeta(n(), shape1 = non_lapsers_shape1, shape2 = non_lapsers_shape2),
        TRUE ~ NA_real_ # Should not happen if 'surrenders' is always TRUE/FALSE
      ),
      # Calculate ps_rate using B, ensuring ps_rate is not negative
      ps_rate = case_when(
        !is.na(base_ps_rate) ~ pmax(0, base_ps_rate * B), # Renamed from N to B
        TRUE ~ NA_real_
      )
    ) %>%
    select(-base_ps_rate) # Remove the intermediate base_ps_rate column
  
  return(lapses_withPS_result)
}

# Which parameters?
# lapsers: peak around 0.25
ggplot(data.frame(x = seq(0, 1, length.out = 500)), aes(x)) +
  geom_line(aes(y = dbeta(x, 2, 4)), color = "#1D8DB0") +
  labs(title = "Beta(2, 4) Distribution", x = "Value", y = "Density") +
  theme_minimal()

# non-lapsers: peak around 0.75
ggplot(data.frame(x = seq(0, 1, length.out = 500)), aes(x)) +
  geom_line(aes(y = dbeta(x, 4, 2)), color = "#1D8DB0") +
  labs(title = "Beta(4, 2) Distribution", x = "Value", y = "Density") +
  theme_minimal()

# Apply the modified function to the dataset
set.seed(123)
lapses_withPS <- assign_ps(
  lapse_data = lapse_classification_dataset,
  profit_sharing_rates_lookup = profit_sharing_long,
  lapsers_shape1 = 2,
  lapsers_shape2 = 4,
  non_lapsers_shape1 = 4,
  non_lapsers_shape2 = 2
)

# updated dataset
summary(lapses_withPS$ps_rate)
summary(lapses_withPS$N)
# head(lapses_withPS)

# --- Visualizations ---

# Convert 'surrenders' to a factor for better plotting labels
lapses_withPS$surrenders_label <- factor(lapses_withPS$surrenders,
                                         levels = c(TRUE, FALSE),
                                         labels = c("Lapsers (surrendered)", "Non-Lapsers (did not surrender)"))

# --- Graphs/Plots ---

# Plot 1: Distribution of Multiplier (B)
plot_B <- ggplot(lapses_withPS, aes(x = B, fill = surrenders_label)) + # Renamed x-axis aesthetic
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribution of multiplier (B) by lapse status (Beta distribution)", # Updated title
    x = "Multiplier (B)", # Updated x-axis label
    y = "Density",
    fill = "Policy status"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Lapsers (surrendered)" = "#DD8A2E", "Non-Lapsers (did not surrender)" = "#1D8DB0")) +
  theme(legend.position = "bottom")

print(plot_B)

ggsave("plot_B_distribution.png", plot = plot_B, width = 8, height = 8, dpi = 300, bg = "white")

# Plot 2: Distribution of ps_rate
plot_ps_rate <- ggplot(lapses_withPS, aes(x = ps_rate, fill = surrenders_label)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Distribution of profit sharing rate (ps_rate) by lapse status",
    x = "Profit sharing rate (ps_rate)",
    y = "Density",
    fill = "Policy status"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Lapsers (surrendered)" = "#DD8A2E", "Non-Lapsers (did not surrender)" = "#1D8DB0")) +
  theme(legend.position = "bottom")

print(plot_ps_rate)

ggsave("plot_ps_rate.png", plot = plot_ps_rate, width = 8, height = 8, dpi = 300, bg = "white")



# extend market data
lapses_withPS <- lapses_withPS %>%
  select(-surrenders_label, -B) %>%
  # 1. Ensure the data is ordered correctly for lagged and cumulative calculations within each policy_id group.
  arrange(policy_id, data_year) %>%
  group_by(policy_id) %>%
  mutate(
    # 2. Calculate ps_lag1 and ps_lag2 (previous and two-previous ps_rates)
    # If a previous observation doesn't exist for a policy_id, it will be NA.
    ps_lag1 = lag(ps_rate, n = 1),
    ps_lag2 = lag(ps_rate, n = 2),
    # ps_lag3 = lag(ps_rate, n = 3),
    
    # 3. Calculate 'i_k' for each observation based on its duration_if
    # This 'i_k_current_obs' is the specific return/rate for that year's period,
    # adjusted pro-rata if duration_if < 1.
    i_k_current_obs = ifelse(duration_if < 1, ps_rate * duration_if, ps_rate),
    
    # Calculate the cumulative product of (1 + i_k_current_obs) for each policy.
    # This represents the total compounded growth factor up to the current data_year.
    cumulative_growth_factor = cumprod(1 + i_k_current_obs),
    
    # 4. Calculate avg_ps based on the specified conditions:
    avg_ps = case_when(
      duration_if == 0 ~ NA_real_, # Average rate undefined if no duration
      TRUE ~ cumulative_growth_factor^(1 / duration_if) - 1 # Annualized geometric mean
    )
  ) %>%
  ungroup() %>% # Ungroup the data after calculations are done
  # Remove the temporary columns used for calculation if they are not needed in the final dataset
  select(-i_k_current_obs, -cumulative_growth_factor)

lapses_withPS <- lapses_withPS %>%
  mutate(
    # Calculate differences
    change_10 = ps_lag1 - ps_rate,
    change_21 = ps_lag2 - ps_lag1,
    #change_32 = ps_lag3 - ps_lag2,
    
    # Calculate mean of differences
    mean_01 = rowMeans(cbind(ps_rate, ps_lag1), na.rm = FALSE),
    mean_12 = rowMeans(cbind(ps_lag1, ps_lag2), na.rm = FALSE),
    #mean_23 = rowMeans(cbind(ps_lag2, ps_lag3), na.rm = FALSE),
    
    # Calculate volatility (standard deviation)
    vol_012 = apply(cbind(ps_rate, ps_lag1, ps_lag2), 1, sd, na.rm = FALSE),
    #vol_123 = apply(cbind(ps_lag1, ps_lag2, ps_lag3), 1, sd, na.rm = FALSE),
    
    # Calculate mean of all three years
    mean_012 = rowMeans(cbind(ps_rate, ps_lag1, ps_lag2), na.rm = FALSE)
    #mean_123 = rowMeans(cbind(ps_lag1, ps_lag2, ps_lag3), na.rm = FALSE)
  )

save(lapses_withPS, file = 'lapses_withPS')


# manipulate categorical/scenario variables: annual premium, prem freq, smoking status, living place (+ later on ageph)
assign_scenario_vars <- function(
    lapses_withPS,
    # Annual Premium Parameters
    ap_mean_mult_true = 0.85,  # Mean multiplier for annual_prem when surrenders=TRUE
    ap_sd_mult_true = 0.1,    # SD multiplier for annual_prem when surrenders=TRUE
    ap_mean_mult_false = 1.15, # Mean multiplier for annual_prem when surrenders=FALSE
    ap_sd_mult_false = 0.1,   # SD multiplier for annual_prem when surrenders=FALSE
    ap_switch_prob = 0.25,     # Probability of switching annual_prem value
    
    # Living Place Parameters
    lp_params = list(
      p_switch_true = 0.2,  # Probability of switching for surrenders=TRUE
      p_switch_false = 0.2, # Probability of switching for surrenders=FALSE
      probs_true = list('EastCoast' = 0.6, 'Other' = 0.25, 'WestCoast' = 0.15), # Probs for surrenders=TRUE
      probs_false = list('WestCoast' = 0.6, 'Other' = 0.25, 'EastCoast' = 0.15) # Probs for surrenders=FALSE
    ),
    
    # Premium Frequency Parameters
    pf_params = list(
      p_switch_true = 0.2,
      p_switch_false = 0.2,
      probs_true = list('Semi-annual' = 0.4, 'Quarterly' = 0.25, 'Monthly' = 0.2, 'Annual' = 0.1, 'Other' = 0.05),
      probs_false = list('Other' = 0.4, 'Annual' = 0.25, 'Monthly' = 0.2, 'Quarterly' = 0.1, 'Semi-annual' = 0.05)
    ),
    
    # Risk Class Parameters
    rc_params = list(
      p_switch_true = 0.2,
      p_switch_false = 0.2,
      probs_true = list(
        'SubStd-smoker' = 0.3, 'Prefered-smoker' = 0.2, 'Prefered-nonSmoker' = 0.15,
        'SubStd-nonSmoker' = 0.15, 'Standard-nonSmoker' = 0.1, 'Standard-smoker' = 0.1
      ),
      probs_false = list(
        'Standard-smoker' = 0.3, 'Standard-nonSmoker' = 0.2, 'SubStd-nonSmoker' = 0.15,
        'Prefered-nonSmoker' = 0.15, 'Prefered-smoker' = 0.1, 'SubStd-smoker' = 0.1
      )
    )
) {
  
  lapses_sim <- lapses_withPS
  
  # Annual Premium Manipulation:
  lapses_sim <- lapses_sim %>%
    mutate(
      # Draw a binary switch (1 with probability ap_switch_prob, 0 otherwise)
      ap_switch = rbinom(n(), 1, prob = ap_switch_prob),
      # If ap_switch is 1, multiply annual_prem by a random factor; otherwise, keep original.
      annual_prem = if_else(
        ap_switch == 1,
        annual_prem * rnorm(n(),
                            mean = if_else(surrenders == TRUE, ap_mean_mult_true, ap_mean_mult_false),
                            sd = if_else(surrenders == TRUE, ap_sd_mult_true, ap_sd_mult_false)),
        annual_prem
      ),
      # Ensure annual_prem remains non-negative
      annual_prem = pmax(0, annual_prem)
    ) %>%
    # Categorical Variable Manipulation:
    # Group by 'surrenders' to apply different logic for lapsers vs. non-lapsers
    group_by(surrenders) %>%
    mutate(
      # Determine current group's surrender status and associated parameters
      current_surrender_status = first(surrenders),
      lp_p_switch = if_else(current_surrender_status == TRUE, lp_params$p_switch_true, lp_params$p_switch_false),
      pf_p_switch = if_else(current_surrender_status == TRUE, pf_params$p_switch_true, pf_params$p_switch_false),
      rc_p_switch = if_else(current_surrender_status == TRUE, rc_params$p_switch_true, rc_params$p_switch_false),
      
      lp_probs = if_else(current_surrender_status == TRUE, list(lp_params$probs_true), list(lp_params$probs_false)),
      pf_probs = if_else(current_surrender_status == TRUE, list(pf_params$probs_true), list(pf_params$probs_false)),
      rc_probs = if_else(current_surrender_status == TRUE, list(rc_params$probs_true), list(rc_params$probs_false)),
      
      # Draw binary switches for each categorical variable in this group
      do_switch_lp = rbinom(n(), 1, prob = lp_p_switch) == 1,
      do_switch_pf = rbinom(n(), 1, prob = pf_p_switch) == 1,
      do_switch_rc = rbinom(n(), 1, prob = rc_p_switch) == 1,
      
      # Apply changes to 'living_place'
      living_place = {
        new_lp <- .data$living_place # Start with original values
        if (any(do_switch_lp)) { # Only sample if there are any rows to switch
          # Extract levels and probabilities from the list
          levels_to_sample <- names(lp_probs[[1]])
          probs_to_use <- unlist(lp_probs[[1]])
          new_lp[do_switch_lp] <- sample(levels_to_sample, size = sum(do_switch_lp), replace = TRUE, prob = probs_to_use)
        }
        new_lp # Return the modified vector
      },
      # Apply changes to 'prem_freq'
      prem_freq = {
        new_pf <- .data$prem_freq
        if (any(do_switch_pf)) {
          levels_to_sample <- names(pf_probs[[1]])
          probs_to_use <- unlist(pf_probs[[1]])
          new_pf[do_switch_pf] <- sample(levels_to_sample, size = sum(do_switch_pf), replace = TRUE, prob = probs_to_use)
        }
        new_pf
      },
      # Apply changes to 'risk_class'
      risk_class = {
        new_rc <- .data$risk_class
        if (any(do_switch_rc)) {
          levels_to_sample <- names(rc_probs[[1]])
          probs_to_use <- unlist(rc_probs[[1]])
          new_rc[do_switch_rc] <- sample(levels_to_sample, size = sum(do_switch_rc), replace = TRUE, prob = probs_to_use)
        }
        new_rc
      }
    ) %>%
    ungroup() %>% # Always ungroup after group_by
    select(-ap_switch, -starts_with("do_switch_"), -starts_with("lp_p_"), -starts_with("pf_p_"), -starts_with("rc_p_"),
           -starts_with("lp_probs"), -starts_with("pf_probs"), -starts_with("rc_probs"), -current_surrender_status) # Remove temporary columns
  
  return(lapses_sim)
}

set.seed(123)
lapses_sim <- assign_scenario_vars(lapses_withPS)

# manipulate ageph
assign_ageph <- function(
    lapses_sim,
    young_to_middle = 18, # Denominator for initial Young->Middle probability (1/N)
    middle_to_old = 20,   # Denominator for initial Middle->Old probability (1/N)
    ym_prob_increase_on_surrender_mult = 1.5, # Multiplier for Young->Middle prob if surrenders=TRUE
    mo_prob_decrease_on_surrender_mult = 0.5 # Multiplier for Middle->Old prob if surrenders=TRUE
) {
  
  # Ensure the data is sorted by policy_id and data_year for sequential processing
  lapses_sim_processed <- lapses_sim %>%
    arrange(policy_id, data_year) %>%
    group_by(policy_id) %>%
    mutate(
      # This block will be executed for each policy_id group
      new_ageph = {
        # Use pick() to select relevant columns from the current group data
        policy_data_subset <- pick(ageph, surrenders)
        initial_ageph <- policy_data_subset$ageph[1] # Ageph of the first observation for this policy
        n_obs <- nrow(policy_data_subset)
        
        # Initialize the vector to store the new ageph values for this policy
        ageph_vec <- rep(NA_character_, n_obs)
        
        if (initial_ageph == "Old") {
          # If the policy starts as 'Old', it always stays 'Old'
          ageph_vec <- rep("Old", n_obs)
        } else if (initial_ageph == "Middle") {
          # If the policy starts as 'Middle', it has a probabilistic transition to 'Old'
          current_denom_mo <- middle_to_old
          moved_to_old <- FALSE
          
          for (i in 1:n_obs) {
            if (moved_to_old) {
              # Once moved to 'Old', it stays 'Old' for subsequent observations
              ageph_vec[i] <- "Old"
            } else {
              # Calculate the base probability for Middle to Old transition
              prob_mo <- 1 / current_denom_mo
              
              # Adjust probability if surrenders is TRUE
              if (policy_data_subset$surrenders[i] == TRUE) {
                prob_mo <- prob_mo * mo_prob_decrease_on_surrender_mult
              }
              prob_mo <- min(prob_mo, 1) # Cap probability at 1
              
              # Check if the transition occurs
              if (runif(1) < prob_mo) {
                ageph_vec[i] <- "Old"
                moved_to_old <- TRUE
              } else {
                ageph_vec[i] <- "Middle" # Stays 'Middle' if transition doesn't occur
              }
            }
            # Decrease the denominator for the next year, ensuring it doesn't go below 1
            if (current_denom_mo > 1) {
              current_denom_mo <- current_denom_mo - 1
            }
          }
        } else if (initial_ageph == "Young") {
          # If the policy starts as 'Young', it has a probabilistic Young->Middle
          # and then a fixed Middle->Old transition
          current_denom_ym <- young_to_middle
          moved_to_middle <- FALSE
          years_in_middle_count <- 0 # Counter for years spent in 'Middle' after transition
          moved_to_old <- FALSE
          
          for (i in 1:n_obs) {
            if (moved_to_old) {
              # Once moved to 'Old', it stays 'Old'
              ageph_vec[i] <- "Old"
            } else if (moved_to_middle) {
              # If already moved to 'Middle', count years and transition to 'Old' after fixed period
              years_in_middle_count <- years_in_middle_count + 1
              if (years_in_middle_count >= middle_to_old) {
                ageph_vec[i] <- "Old"
                moved_to_old <- TRUE
              } else {
                ageph_vec[i] <- "Middle"
              }
            } else { # Still 'Young'
              # Calculate the base probability for Young to Middle transition
              prob_ym <- 1 / current_denom_ym
              
              # Adjust probability if surrenders is TRUE
              if (policy_data_subset$surrenders[i] == TRUE) {
                prob_ym <- prob_ym * ym_prob_increase_on_surrender_mult
              }
              prob_ym <- min(prob_ym, 1) # Cap probability at 1
              
              # Check if the transition occurs
              if (runif(1) < prob_ym) {
                ageph_vec[i] <- "Middle"
                moved_to_middle <- TRUE
                years_in_middle_count <- years_in_middle_count + 1 # Start counting from this observation
              } else {
                ageph_vec[i] <- "Young" # Stays 'Young' if transition doesn't occur
              }
            }
            # Decrease the denominator for the next year, ensuring it doesn't go below 1
            if (current_denom_ym > 1) {
              current_denom_ym <- current_denom_ym - 1
            }
          }
        }
        ageph_vec # Return the generated ageph vector for this policy
      }
    ) %>%
    ungroup() %>% # Ungroup the data frame
    mutate(ageph = new_ageph) %>% # Overwrite the original 'ageph' column with the new values
    select(-new_ageph) # Remove the temporary 'new_ageph' column
  
  return(lapses_sim_processed)
}

set.seed(123)
lapses_data <- assign_ageph(lapses_sim)
save(lapses_data, file = 'lapses_data')

# load('lapses_data')
# library(writexl)
# write_xlsx(lapses_data, "lapses_data.xlsx")
