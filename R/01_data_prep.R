#prueba3 is the data

# Step 1: Convert date columns to Date type if not already
prueba3 <- prueba3 %>%
  mutate(virus_intro = as.Date(virus_intro),
         First_vac = as.Date(First_vac))
#WEEK1----
# For each Zone
##Zone pre----
colnames(prueba3)
table(prueba3$Zone)

pre<- prueba3%>%
  filter(Zone=="pre")

# Step 2: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-11")

# Step 3: Create a sequence of time steps (daily)
start_date <- min(pre$virus_intro, na.rm = TRUE)
end_date <- max(pre$virus_intro, na.rm = TRUE)
time_steps <- seq(from = start_date, to = end_date, by = "day")

# Step 4: Create a data frame for storing results
results <- data.frame(date = time_steps)

# Step 5: Calculate cases, infectious, and susceptible farms, and determine vaccination status for each time step
prepared_data <- results %>%
  rowwise() %>%
  mutate(
    # Calculate the number of cases at the current date
    cases = sum(pre$virus_intro == date, na.rm = TRUE),
    
    # Calculate the number of infectious farms (those still within their infectious period of 26 days)
    infectious = sum(pre$virus_intro <= date & 
                       (pre$virus_intro + 26) >= date, na.rm = TRUE),
    
    recovered= sum((pre$virus_intro + 26) < date, na.rm=TRUE),
    
    # Calculate the number of susceptible farms
    susceptible = sum(is.na(pre$virus_intro) | 
                        pre$virus_intro > date, na.rm = TRUE),
    N= susceptible+infectious+recovered,
    
    # Determine vaccination status based on the current date relative to the threshold date
    vaccination = ifelse(date < threshold_date, 0, 1),
    
    # Calculate deltaT (time difference in days between current and previous step)
    deltaT = ifelse(date == lag(date), 1, as.numeric(difftime(date, lag(date), units = "days")))
  ) %>%
  ungroup() %>%
  mutate(
    deltaT = ifelse(is.na(deltaT), 1, deltaT),
   
    # Calculate the offset as log(infectious / susceptible * deltaT), ensuring there are no zero values
    offset = log(ifelse(susceptible > 0, infectious / N * deltaT, 1))
 
