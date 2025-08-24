#prueba3 is the dataset of the 2001 FMD epidemic in Uruguay, restricted to police sections where outbreaks occurred.
prueba3<- prueba3 %>%
  dplyr::select(Zone, virus_intro)
#For reproducibility, we also provide a simulated dataset containing only the variables virus_intro and Zone.

#WEEK1----
##Zone pre----
pre<- prueba3%>%
  filter(Zone=="pre")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-11")

# Step 2: Create a sequence of time steps (daily)
start_date <- min(pre$virus_intro, na.rm = TRUE)
end_date <- max(pre$virus_intro, na.rm = TRUE)
time_steps <- seq(from = start_date, to = end_date, by = "day")

# Step 3: Create a data frame for storing results
results <- data.frame(date = time_steps)

# Step 4: Calculate cases, infectious, and susceptible farms, and determine vaccination status for each time step
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
  )


# Step 5: Fit the GLM including vaccination as a predictor
glm_model_vac <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                     offset = offset, 
                     family = binomial(link = "cloglog"), 
                     data = prepared_data)

vac_beta <- glm(cbind(cases, susceptible - cases) ~ 0+vaccination, 
                offset = offset, 
                family = binomial(link = "cloglog"), 
                data = prepared_data)
summary(vac_beta)
r_pre<-tidy(vac_beta,exponentiate = T, conf.int = T)
r_pre$zone<-"pre"

# Display the summary of the model
summary(glm_model_vac)

r_pre2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r_pre2$zone<-"pre"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone A----

A<- prueba3%>%
  filter(Zone=="A")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-12")

# Step 2: Create a sequence of time steps (daily)
start_date <- min(A$virus_intro, na.rm = TRUE)
end_date <- max(A$virus_intro, na.rm = TRUE)
time_steps <- seq(from = start_date, to = end_date, by = "day")

# Step 3: Create a data frame for storing results
results <- data.frame(date = time_steps)

# Step 4: Calculate cases, infectious, and susceptible farms, and determine vaccination status for each time step
prepared_data <- results %>%
  rowwise() %>%
  mutate(
    # Calculate the number of cases at the current date
    cases = sum(A$virus_intro == date, na.rm = TRUE),
    
    # Calculate the number of infectious farms (those still within their infectious period of 26 days)
    infectious = sum(A$virus_intro <= date & 
                       (A$virus_intro + 26) >= date, na.rm = TRUE),
    
    recovered= sum((A$virus_intro + 26) < date, na.rm=TRUE),
    
    # Calculate the number of susceptible farms
    susceptible = sum(is.na(A$virus_intro) | 
                        A$virus_intro > date, na.rm = TRUE),
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
  )


# Step 5: Fit the GLM including vaccination as a predictor
glm_model_vac <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                     offset = offset, 
                     family = binomial(link = "cloglog"), 
                     data = prepared_data)

vac_beta <- glm(cbind(cases, susceptible - cases) ~ 0+vaccination, 
                offset = offset, 
                family = binomial(link = "cloglog"), 
                data = prepared_data)
summary(vac_beta)
r_A<-tidy(vac_beta,exponentiate = T, conf.int = T)
r_A$zone<-"A"
# Display the summary of the model
summary(glm_model_vac)
r_A2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r_A2$zone<-"A"

relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone B----

B<- prueba3%>%
  filter(Zone=="B")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-17")

# Step 2: Create a sequence of time steps (daily)
start_date <- min(B$virus_intro, na.rm = TRUE)
end_date <- max(B$virus_intro, na.rm = TRUE)
time_steps <- seq(from = start_date, to = end_date, by = "day")

# Step 3: Create a data frame for storing results
results <- data.frame(date = time_steps)

# Step 4: Calculate cases, infectious, and susceptible farms, and determine vaccination status for each time step
prepared_data <- results %>%
  rowwise() %>%
  mutate(
    # Calculate the number of cases at the current date
    cases = sum(B$virus_intro == date, na.rm = TRUE),
    
    # Calculate the number of infectious farms (those still within their infectious period of 26 days)
    infectious = sum(B$virus_intro <= date & 
                       (B$virus_intro + 26) >= date, na.rm = TRUE),
    
    recovered= sum((B$virus_intro + 26) < date, na.rm=TRUE),
    
    # Calculate the number of susceptible farms
    susceptible = sum(is.na(B$virus_intro) | 
                        B$virus_intro > date, na.rm = TRUE),
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
  )


# Step 5: Fit the GLM including vaccination as a predictor
glm_model_vac <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                     offset = offset, 
                     family = binomial(link = "cloglog"), 
                     data = prepared_data)

vac_beta <- glm(cbind(cases, susceptible - cases) ~ 0+vaccination, 
                offset = offset, 
                family = binomial(link = "cloglog"), 
                data = prepared_data)
summary(vac_beta)
r_B<-tidy(vac_beta,exponentiate = T, conf.int = T)
r_B$zone<-"B"

# Display the summary of the model
summary(glm_model_vac)
r_B2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r_B2$zone<-"B"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone C----

C<- prueba3%>%
  filter(Zone=="C")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-22")

# Step 2: Create a sequence of time steps (daily)
start_date <- min(C$virus_intro, na.rm = TRUE)
end_date <- max(C$virus_intro, na.rm = TRUE)
time_steps <- seq(from = start_date, to = end_date, by = "day")

# Step 3: Create a data frame for storing results
results <- data.frame(date = time_steps)

# Step 4: Calculate cases, infectious, and susceptible farms, and determine vaccination status for each time step
prepared_data <- results %>%
  rowwise() %>%
  mutate(
    # Calculate the number of cases at the current date
    cases = sum(C$virus_intro == date, na.rm = TRUE),
    
    # Calculate the number of infectious farms (those still within their infectious period of 26 days)
    infectious = sum(C$virus_intro <= date & 
                       (C$virus_intro + 26) >= date, na.rm = TRUE),
    
    recovered= sum((C$virus_intro + 26) < date, na.rm=TRUE),
    
    # Calculate the number of susceptible farms
    susceptible = sum(is.na(C$virus_intro) | 
                        C$virus_intro > date, na.rm = TRUE),
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
  )


# Step 5: Fit the GLM including vaccination as a predictor
glm_model_vac <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                     offset = offset, 
                     family = binomial(link = "cloglog"), 
                     data = prepared_data)

vac_beta <- glm(cbind(cases, susceptible - cases) ~ 0+vaccination, 
                offset = offset, 
                family = binomial(link = "cloglog"), 
                data = prepared_data)
summary(vac_beta)
r_C<-tidy(vac_beta,exponentiate = T, conf.int = T)
r_C$zone<-"C"
# Display the summary of the model
summary(glm_model_vac)
r_C2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r_C2$zone<-"C"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone D----

D<- prueba3%>%
  filter(Zone=="D")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-28")

# Step 2: Create a sequence of time steps (daily)
start_date <- min(D$virus_intro, na.rm = TRUE)
end_date <- max(D$virus_intro, na.rm = TRUE)
time_steps <- seq(from = start_date, to = end_date, by = "day")

# Step 3: Create a data frame for storing results
results <- data.frame(date = time_steps)

# Step 4: Calculate cases, infectious, and susceptible farms, and determine vaccination status for each time step
prepared_data <- results %>%
  rowwise() %>%
  mutate(
    # Calculate the number of cases at the current date
    cases = sum(D$virus_intro == date, na.rm = TRUE),
    
    # Calculate the number of infectious farms (those still within their infectious period of 26 days)
    infectious = sum(D$virus_intro <= date & 
                       (D$virus_intro + 26) >= date, na.rm = TRUE),
    
    recovered= sum((D$virus_intro + 26) < date, na.rm=TRUE),
    
    # Calculate the number of susceptible farms
    susceptible = sum(is.na(D$virus_intro) | 
                        D$virus_intro > date, na.rm = TRUE),
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
  )

# Step 5: Fit the GLM including vaccination as a predictor
glm_model_vac <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                     offset = offset, 
                     family = binomial(link = "cloglog"), 
                     data = prepared_data)

vac_beta <- glm(cbind(cases, susceptible - cases) ~ 0+vaccination, 
                offset = offset, 
                family = binomial(link = "cloglog"), 
                data = prepared_data)
summary(vac_beta)
r_D<-tidy(vac_beta,exponentiate = T, conf.int = T)
r_D$zone<-"D"
# Display the summary of the model
summary(glm_model_vac)
r_D2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r_D2$zone<-"D"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

#Final table week1----

bvac1<-rbind(r_pre,r_A, r_B, r_C,r_D)
bvac1<-bvac1[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(bvac1)<-c("zone" ,"beta_vac1", "conf.low", "conf.high")

bunvac_rel1<-rbind(r_pre2,r_A2, r_B2, r_C2,r_D2)
bunvac1<-bunvac_rel1%>%
  filter(term=="(Intercept)")
bunvac1<-bunvac1[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(bunvac1)<-c("zone" ,"beta_unvac", "conf.low", "conf.high")


brel1<-bunvac_rel1%>%
  filter(term=="vaccination")

brel1<-brel1[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(brel1)<-c("zone" ,"beta_rel1", "conf.low", "conf.high")

brel1$VE<-(1-brel1$beta_rel1)*100
brel1$VE.low<-(1-brel1$conf.high)*100
brel1$VE.high<-(1-brel1$conf.low)*100
brel1<-brel1[,c("zone", "VE", "VE.low", "VE.high")]
#merged_df <- left_join(df1, df2, by = "key_column")
table1<-left_join(bunvac1, bvac1,by="zone")
table1<-left_join(table1, brel1, by="zone")

table1$beta_unvac<- sprintf("%.4f [%.4f - %.4f]", 
                            table1$beta_unvac, table1$conf.low.x, 
                            table1$conf.high.x)

table1$beta_vac1<- sprintf("%.4f [%.4f - %.4f]", 
                           table1$beta_vac1, table1$conf.low.y, 
                           table1$conf.high.y)

table1$VE1<- sprintf("%.2f [%.2f - %.2f]", 
                     table1$VE, table1$VE.low, 
                     table1$VE.high)
table_1<-table1[,c("zone", "beta_unvac", "beta_vac1", "VE1")]
Table_VE1<-table1[,c("zone",  "VE1")]

betas1<-table1[,c(1,2,5)]

write.csv(table_1, "ve_1week.csv")
table_1$VE1

 
