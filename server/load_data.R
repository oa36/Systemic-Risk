#Load and aggregate data weekly 
daily_returns <- readr::read_delim("data/returns.csv", delim = ";",
                                   col_types = cols(.default = "c")) %>% 
  readr::type_convert(., locale = readr::locale(decimal_mark = ","))

returns_weekly <- daily_returns %>% 
  melt(id = "Date") %>% 
  rename("returns" = "value") %>% 
  mutate(week = floor_date(as.Date(Date, format = "%d.%m.%y"), unit = "week" )) %>% select(Date, week, everything())  %>% 
  group_by(week, variable) %>% 
  summarise(mean(returns)) %>% 
  tidyr::spread(key = "variable", value = "mean(returns)")%>% 
  ungroup() 

spy500 <- returns_weekly %>% select("SP500")

spy500 <- spy500 %>% mutate_all(dplyr::lag) %>% na.omit()

returns_weekly <- returns_weekly %>% filter(week != "1999-12-26") # filter out first observation as it will be lost when lagging state variables

returns_weekly <- returns_weekly %>% select(-SP500)

#state variables -- shift 1 lag 
state_var_weekly <- readr::read_delim("data/state_variables_ed.csv", delim = ";",
                                      col_types = cols(.default = "c")) %>% 
  readr::type_convert(., locale = readr::locale(decimal_mark = ",")) %>%
  melt(id="Date")%>% 
  mutate(week = floor_date(as.Date(Date, format = "%d.%m.%y"), unit = "week" )) %>% select(Date, week, everything())%>% 
  group_by(week, variable) %>% 
  summarise(mean(value)) %>%
  tidyr::spread(key = "variable", value = "mean(value)") %>% ungroup()

state_var_weekly <- state_var_weekly %>% mutate_all(dplyr::lag) %>% na.omit() %>% mutate(week = returns_weekly$week)

state_var_weekly <- state_var_weekly %>% cbind(spy500)

#get and calculate market-cap weekly 
market_cap_weekly <- readr::read_delim("data/market_cap.csv", delim = ";",
                                       col_types = cols(.default = "c")) %>%
  readr::type_convert(., locale = readr::locale(decimal_mark = ",")) %>% 
  melt(id="Date") %>% 
  mutate(date = floor_date(as.Date(Date, format = "%d.%m.%y"), unit = "week" )) %>% 
  group_by(date, variable) %>% 
  summarise(mean(value)) %>% 
  tidyr::spread(key = "variable", value = "mean(value)") %>% ungroup() %>%
  filter(date != "1999-12-26") # filter out first observation as its lost when lagging state variables 


#Load quarterly balance sheet data
##first load pre-manually-matched tickers&balance_sheet_data company ID to merge it in the next step with company data we have
tickers <- readr::read_delim("data/balance_sheet_data/match_tickers.csv", delim = ";",
                             col_types = cols(.default = "c")) %>%
  readr::type_convert(., locale = readr::locale(decimal_mark = ",")) %>% 
  mutate(permno = as.numeric(permno))

##load balance sheet data/ratios (extracted from Adrian and Brunnermeier data) & merge it with our data tickers

call_bs_data <- function(){
  
  old <-  readr::read_delim("data/balance_sheet_data/old.csv", delim = ",",
                            col_types = cols(.default = "c")) %>%
    tidyr::drop_na() %>%  mutate(permno = as.numeric(permno))
  
  new <- readr::read_delim("data/balance_sheet_data/new.csv", delim = ",",
                           col_types = cols(.default = "c")) %>%
    tidyr::drop_na() %>%  mutate(permno = as.numeric(permno))
  
  data <- rbind(old, new) %>% distinct()
  
  balance_data <- data %>% 
    dplyr::left_join(tickers, by = c("permno" = "permno")) %>% tidyr::drop_na() 
  
}

balance_sheet_data <- call_bs_data()