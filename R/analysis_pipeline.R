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

 #WEEK 2----

##Zone pre----

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-18")
threshold_date2<-as.Date("2001-05-11")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

# interval = 1 from the vaccination date (11/05), marking herds as vaccinated
# vaccination = 1 one week later (18/05), when protection is assumed effective
# → this allows excluding the “grey window” (11–18/05), when animals were vaccinated but not yet fully protected

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))

model.pre <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                 offset = offset, 
                 family = binomial(link = "cloglog"), 
                 data = subset_data)

tidy.pre_two<-tidy(model.pre,exponentiate = T, conf.int = T)
tidy.pre2<-tidy(model.pre,exponentiate = T, conf.int = T)
tidy.pre2$zone<-"pre"
tidy.pre2<-tidy.pre2[2,c(1,2,6,7,8)]
tidy.pre2$VE<-(1-tidy.pre2$estimate)*100
tidy.pre2$VE.lower<-(1-tidy.pre2$conf.high)*100
tidy.pre2$VE.upper<-(1-tidy.pre2$conf.low)*100

tidy.pre2<-tidy.pre2[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE pre----
tidy.pre2

summary(vac_beta)
r2_pre<-tidy(vac_beta,exponentiate = T, conf.int = T)
r2_pre$zone<-"pre"

# Display the summary of the model
summary(glm_model_vac)
r2_pre2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r2_pre2$zone<-"pre"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone A----

A<- prueba3%>%
  filter(Zone=="A")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-19")
threshold_date2<-as.Date("2001-05-12")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.A <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)
tidy.Atwo<-tidy(model.A,exponentiate = T, conf.int = T)
tidy.A2<-tidy(model.A,exponentiate = T, conf.int = T)
tidy.A2$zone<-"A"
tidy.A2<-tidy.A2[2,c(1,2,6,7,8)]
tidy.A2$VE<-(1-tidy.A2$estimate)*100
tidy.A2$VE.lower<-(1-tidy.A2$conf.high)*100
tidy.A2$VE.upper<-(1-tidy.A2$conf.low)*100

tidy.A2<-tidy.A2[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE A----
tidy.A2

summary(vac_beta)
r2_A<-tidy(vac_beta,exponentiate = T, conf.int = T)
r2_A$zone<-"A"

# Display the summary of the model
summary(glm_model_vac)
r2_A2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r2_A2$zone<-"A"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone B----

B<- prueba3%>%
  filter(Zone=="B")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-24")
threshold_date2<-as.Date("2001-05-17")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.B <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)

tidy.Btwo<-tidy(model.B,exponentiate = T, conf.int = T)
tidy.B2<-tidy(model.B,exponentiate = T, conf.int = T)
tidy.B2$zone<-"B"
tidy.B2<-tidy.B2[2,c(1,2,6,7,8)]
tidy.B2$VE<-(1-tidy.B2$estimate)*100
tidy.B2$VE.lower<-(1-tidy.B2$conf.high)*100
tidy.B2$VE.upper<-(1-tidy.B2$conf.low)*100

tidy.B2<-tidy.B2[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE B----
tidy.B2

summary(vac_beta)
r2_B<-tidy(vac_beta,exponentiate = T, conf.int = T)
r2_B$zone<-"B"
# Display the summary of the model
summary(glm_model_vac)
r2_B2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r2_B2$zone<-"B"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone C----

C<- prueba3%>%
  filter(Zone=="C")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-28")
threshold_date2<-as.Date("2001-05-22")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.C <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)
tidy.Ctwo<-tidy(model.C,exponentiate = T, conf.int = T)
tidy.C2<-tidy(model.C,exponentiate = T, conf.int = T)
tidy.C2$zone<-"C"
tidy.C2<-tidy.C2[2,c(1,2,6,7,8)]
tidy.C2$VE<-(1-tidy.C2$estimate)*100
tidy.C2$VE.lower<-(1-tidy.C2$conf.high)*100
tidy.C2$VE.upper<-(1-tidy.C2$conf.low)*100

tidy.C2<-tidy.C2[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE C----
tidy.C2

summary(vac_beta)
r2_C<-tidy(vac_beta,exponentiate = T, conf.int = T)
r2_C$zone<-"C"

# Display the summary of the model
summary(glm_model_vac)
r2_C2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r2_C2$zone<-"C"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone D----

D<- prueba3%>%
  filter(Zone=="D")

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-06-04")
threshold_date2<-as.Date("2001-05-28")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.D<- glm(cbind(cases, susceptible - cases) ~ vaccination, 
              offset = offset, 
              family = binomial(link = "cloglog"), 
              data = subset_data)

tidy.Dtwo<-tidy(model.D,exponentiate = T, conf.int = T)

tidy.D2<-tidy(model.D,exponentiate = T, conf.int = T)
tidy.D2$zone<-"D"
tidy.D2<-tidy.D2[2,c(1,2,6,7,8)]
tidy.D2$VE<-(1-tidy.D2$estimate)*100
tidy.D2$VE.lower<-(1-tidy.D2$conf.high)*100
tidy.D2$VE.upper<-(1-tidy.D2$conf.low)*100

tidy.D2<-tidy.D2[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE D----
tidy.D2

####Table VE week 2----
VE_week2<-rbind(tidy.pre2,tidy.A2,tidy.B2,tidy.C2, tidy.D2)
write.csv(VE_week2, "VE_week2.csv")

summary(vac_beta)
r2_D<-tidy(vac_beta,exponentiate = T, conf.int = T)
r2_D$zone<-"D"

# Display the summary of the model
summary(glm_model_vac)
r2_D2<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r2_D2$zone<-"D"

relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

#Final table Week 2----

bvac2<-rbind(r2_pre,r2_A, r2_B, r2_C,r2_D)
bvac2<-bvac2[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(bvac2)<-c("zone" ,"beta_vac2", "conf.low", "conf.high")

bunvac_rel2<-rbind(r2_pre2,r2_A2, r2_B2, r2_C2,r2_D2)

brel2<-bunvac_rel2%>%
  filter(term=="vaccination")

brel2<-brel2[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(brel2)<-c("zone" ,"beta_rel2", "conf.low", "conf.high")
brel2$VE<-(1-brel2$beta_rel2)*100
brel2$VE.low<-(1-brel2$conf.high)*100
brel2$VE.high<-(1-brel2$conf.low)*100
brel2<-brel2[,c("zone", "VE", "VE.low", "VE.high")]
#merged_df <- left_join(df1, df2, by = "key_column")
table2<-left_join(bvac2,brel2, by="zone")

table2$beta_vac2<- sprintf("%.4f [%.4f - %.4f]", 
                           table2$beta_vac2, table2$conf.low, 
                           table2$conf.high)
table2$VE2<- sprintf("%.2f [%.2f - %.2f]", 
                     table2$VE, table2$VE.low, 
                     table2$VE.high)
table_2<-table2[,c("zone", "beta_vac2", "VE2")]

write.csv(table_2, "ve_2week.csv")
###beta 2 week----
beta2<- table2[,c(1,2)]

#Outputs Betas----

outputs_2<-rbind(tidy.pre_two, tidy.Atwo, tidy.Btwo, tidy.Ctwo,
                 tidy.Dtwo)
#WEEK 3----

##Zone pre----

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-25")
threshold_date2<-as.Date("2001-05-11")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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
subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.pre <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
                 offset = offset, 
                 family = binomial(link = "cloglog"), 
                 data = subset_data)
tidy.prethree<-tidy(model.pre,exponentiate = T, conf.int = T)
tidy.pre3<-tidy(model.pre,exponentiate = T, conf.int = T)
tidy.pre3$zone<-"pre"
tidy.pre3<-tidy.pre3[2,c(1,2,6,7,8)]
tidy.pre3$VE<-(1-tidy.pre3$estimate)*100
tidy.pre3$VE.lower<-(1-tidy.pre3$conf.high)*100
tidy.pre3$VE.upper<-(1-tidy.pre3$conf.low)*100

tidy.pre3<-tidy.pre3[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE pre----
tidy.pre3

summary(vac_beta)
r3_pre<-tidy(vac_beta,exponentiate = T, conf.int = T)
r3_pre$zone<-"pre"

# Display the summary of the model
summary(glm_model_vac)
r3_pre3<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r3_pre3$zone<-"pre"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone A----
# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-26")
threshold_date2<-as.Date("2001-05-12")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.A <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)
tidy.Athree<-tidy(model.A,exponentiate = T, conf.int = T)
tidy.A3<-tidy(model.A,exponentiate = T, conf.int = T)
tidy.A3$zone<-"A"
tidy.A3<-tidy.A3[2,c(1,2,6,7,8)]
tidy.A3$VE<-(1-tidy.A3$estimate)*100
tidy.A3$VE.lower<-(1-tidy.A3$conf.high)*100
tidy.A3$VE.upper<-(1-tidy.A3$conf.low)*100

tidy.A3<-tidy.A3[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE A----
tidy.A3

summary(vac_beta)
r3_A<-tidy(vac_beta,exponentiate = T, conf.int = T)
r3_A$zone<-"A"

# Display the summary of the model
summary(glm_model_vac)
r3_A3<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r3_A3$zone<-"A"

relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone B----

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-05-31")
threshold_date2<-as.Date("2001-05-17")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.B <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)

tidy.Bthree<-tidy(model.B,exponentiate = T, conf.int = T)
tidy.B3<-tidy(model.B,exponentiate = T, conf.int = T)
tidy.B3$zone<-"B"
tidy.B3<-tidy.B3[2,c(1,2,6,7,8)]
tidy.B3$VE<-(1-tidy.B3$estimate)*100
tidy.B3$VE.lower<-(1-tidy.B3$conf.high)*100
tidy.B3$VE.upper<-(1-tidy.B3$conf.low)*100

tidy.B3<-tidy.B3[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE B----
tidy.B3

summary(vac_beta)
r3_B<-tidy(vac_beta,exponentiate = T, conf.int = T)
r3_B$zone<-"B"

# Display the summary of the model
summary(glm_model_vac)
r3_B3<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r3_B3$zone<-"B"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone C----

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-06-05")
threshold_date2<-as.Date("2001-05-22")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.C <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)

tidy.Cthree<-tidy(model.C,exponentiate = T, conf.int = T)
tidy.C3<-tidy(model.C,exponentiate = T, conf.int = T)
tidy.C3$zone<-"C"
tidy.C3<-tidy.C3[2,c(1,2,6,7,8)]
tidy.C3$VE<-(1-tidy.C3$estimate)*100
tidy.C3$VE.lower<-(1-tidy.C3$conf.high)*100
tidy.C3$VE.upper<-(1-tidy.C3$conf.low)*100

tidy.C3<-tidy.C3[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE C----
tidy.C3

summary(vac_beta)
r3_C<-tidy(vac_beta,exponentiate = T, conf.int = T)
r3_C$zone<-"C"
# Display the summary of the model
summary(glm_model_vac)
r3_C3<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r3_C3$zone<-"C"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

##Zone D----

# Step 1: Define the threshold date for creating the dummy variable for vaccination
threshold_date <- as.Date("2001-06-11")
threshold_date2<-as.Date("2001-05-28")

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
    interval= ifelse(date < threshold_date2, 0, 1),
    
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

subset_data <- prepared_data %>% 
  filter((vaccination == 0 & interval == 0) | 
           (vaccination == 1 & interval == 1))
model.D <- glm(cbind(cases, susceptible - cases) ~ vaccination, 
               offset = offset, 
               family = binomial(link = "cloglog"), 
               data = subset_data)

tidy.Dthree<-tidy(model.D,exponentiate = T, conf.int = T)
tidy.D3<-tidy(model.D,exponentiate = T, conf.int = T)
tidy.D3$zone<-"D"
tidy.D3<-tidy.D3[2,c(1,2,6,7,8)]
tidy.D3$VE<-(1-tidy.D3$estimate)*100
tidy.D3$VE.lower<-(1-tidy.D3$conf.high)*100
tidy.D3$VE.upper<-(1-tidy.D3$conf.low)*100

tidy.D3<-tidy.D3[,c("zone" ,"VE", "VE.lower", "VE.upper")]
###VE D----
tidy.D3

###Output 3----
outputs_3<- rbind(tidy.prethree, tidy.Athree, tidy.Bthree, tidy.Cthree, 
                  tidy.Dthree)

####Table VE week 3----
VE_week3<-rbind(tidy.pre3,tidy.A3,tidy.B3,tidy.C3, tidy.D3)
write.csv(VE_week3, "VE_week3.csv")

summary(vac_beta)
r3_D<-tidy(vac_beta,exponentiate = T, conf.int = T)
r3_D$zone<-"D"
# Display the summary of the model
summary(glm_model_vac)
r3_D3<-tidy(glm_model_vac,exponentiate = T, conf.int = T)
r3_D3$zone<-"D"
relative_risk2 <- exp(coef(glm_model_vac)[2])
(1-relative_risk2)*100

#Final table Week 3----

bvac3<-rbind(r3_pre,r3_A, r3_B, r3_C,r3_D)
bvac3<-bvac3[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(bvac3)<-c("zone" ,"beta_vac3", "conf.low", "conf.high")

bunvac_rel3<-rbind(r3_pre3,r3_A3, r3_B3, r3_C3,r3_D3)

brel3<-bunvac_rel3%>%
  filter(term=="vaccination")

brel3<-brel3[,c("zone" ,"estimate", "conf.low", "conf.high")]
colnames(brel3)<-c("zone" ,"beta_rel3", "conf.low", "conf.high")
brel3$VE<-(1-brel3$beta_rel3)*100
brel3$VE.low<-(1-brel3$conf.high)*100
brel3$VE.high<-(1-brel3$conf.low)*100
brel3<-brel3[,c("zone", "VE", "VE.low", "VE.high")]

table3<-left_join(bvac3,brel3, by="zone")


table3$beta_vac3<- sprintf("%.4f [%.4f - %.4f]", 
                           table3$beta_vac3, table3$conf.low, 
                           table3$conf.high)
table3$VE3<- sprintf("%.2f [%.2f - %.2f]", 
                     table3$VE, table2$VE.low, 
                     table3$VE.high)
table_3<-table3[,c("zone", "beta_vac3", "VE3")]

write.csv(table_3, "ve_3week.csv")

###beta 3 week----
beta3<- table_3[,c(1,2)]

#General Table Betas----

betas<-left_join(betas1, beta2, by="zone")
betas_general<- left_join(betas, beta3, by="zone")

write.csv(betas_general, "betas_general.csv")

#General Table VE----

gral_table<-left_join(VE_week2, VE_week3, by="zone")

gral_table$VE2<- sprintf("%.2f [%.2f - %.2f]", 
                         gral_table$VE.x, gral_table$VE.lower.x, 
                         gral_table$VE.upper.x)
gral_table$VE3<- sprintf("%.2f [%.2f - %.2f]", 
                         gral_table$VE.y, gral_table$VE.lower.y, 
                         gral_table$VE.upper.y)

gral_table<-gral_table[,c("zone", "VE2", "VE3")]


gral_table_final<- left_join(Table_VE1, gral_table, by="zone")


write.csv(gral_table_final, "gral_table_VE.csv")


#Plot----

brel1
VE_week2
VE_week3

data_plot_b<- merge(brel1, VE_week2, by="zone")
colnames(data_plot_b)<-c("zone", "VE1", "VE1.lower", "VE1.upper",
                         "VE2", "VE2.lower", "VE2.upper")
data_plot_b<-merge(data_plot_b, VE_week3, by="zone")
colnames(data_plot_b)<-c("zone", "VE1", "VE1.lower", "VE1.upper",
                         "VE2", "VE2.lower", "VE2.upper",
                         "VE3", "VE3.lower", "VE3.upper" )

data_long <- data_plot_b %>%
  pivot_longer(
    cols = -zone,  # Keep the 'zone' column intact
    names_to = c("Week", "Metric"),  # Create 'Week' and 'Metric' columns
    names_pattern = "VE(\\d+)(?:\\.(.*))?",  # Match week numbers and optional metrics
    values_to = "Value"
  ) %>%
  mutate(
    Metric = ifelse(Metric == "" | is.na(Metric), "VE", Metric),  # Replace "" or NA with "VE"
    Week = as.numeric(Week)  # Convert Week to numeric
  ) %>%
  pivot_wider(
    names_from = Metric,  # Spread Metric into separate columns
    values_from = Value
  )

library(ggplot2)

#facets
ggplot(data_long, aes(x = Week, y = VE)) +
  geom_line(aes(color = zone, group = zone), size = 1) +  # Line for VE
  geom_point(aes(color = zone), size = 3) +              # Points for VE
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = zone), alpha = 0.2) +  # Confidence intervals
  facet_wrap(~zone, scales = "free_y") +                 # Facet per zone
  labs(
    title = "Vaccine Effectiveness by Zone and Week",
    x = "Weeks After Vaccination",
    y = "Vaccine Effectiveness (VE)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "none"  # Hide legend since each facet is for one zone
  )

# Ensure the correct order for facets and X-axis
data_long <- data_long %>%
  mutate(
    zone = factor(zone, levels = c("pre", "A", "B", "C", "D"))
  )
str(data_long$Week)

# Plot
ggplot(data_long, aes(x = Week, y = VE)) +
  geom_line(aes(color = zone, group = zone), size = 1) +  # Line for VE
  geom_point(aes(color = zone), size = 3) +              # Points for VE
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = zone), alpha = 0.2) +  # Confidence intervals
  facet_wrap(~zone, scales = "fixed") +                  # Facet per zone, fixed Y-axis
  scale_x_discrete(limits = c(1, 2, 3)) +                # Ensure X-axis shows only 1, 2, 3
  labs(
    title = "Vaccine Effectiveness by Zone and Week",
    x = "Weeks After Vaccination",
    y = "Vaccine Effectiveness (VE)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    strip.text = element_text(size = 12),  # Adjust facet labels
    legend.position = "none"               # Hide legend
  )


ggplot(data_long, aes(x = Week, y = VE)) +
  geom_ribbon(
    aes(x = Week, ymin = lower, ymax = upper, fill = zone),  # Explicitly set x, ymin, ymax
    alpha = 0.2
  ) +
  geom_line(aes(color = zone, group = zone), size = 1) +  # Line for VE
  geom_point(aes(color = zone), size = 3) +              # Points for VE
  facet_wrap(~zone, scales = "fixed") +                  # Facet per zone, fixed Y-axis
  labs(
    title = "Vaccine Effectiveness by Zone and Week",
    x = "Weeks After Vaccination",
    y = "Vaccine Effectiveness (VE)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    strip.text = element_text(size = 12),  # Adjust facet labels
    legend.position = "none"               # Hide legend
  )

faceted_plot<-ggplot(data_long, aes(x = Week, y = VE)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = zone),
    alpha = 0.2
  ) +
  geom_line(aes(color = zone, group = zone), size = 1) +
  geom_point(aes(color = zone), size = 3) +
  facet_wrap(~zone, scales = "fixed") +
  scale_x_discrete(limits = c(1, 2, 3)) +  # Ensure X-axis shows only 1, 2, 3
  scale_fill_manual(
    values = c(
      pre = "lightblue",
      A = "yellow",
      B = "green",
      C = "red",
      D = "purple"
    )
  ) +
  scale_color_manual(
    values = c(
      pre = "lightblue",
      A = "yellow",
      B = "green",
      C = "red",
      D = "purple"
    )
  ) +
  labs(
    title = "",
    x = "Weeks After Vaccination",
    y = "Vaccine Effectiveness (VE)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    strip.text = element_text(size = 10),  # Adjust facet labels
    legend.position = "none"               # Hide legend since colors are intuitive
  )

#Plot VE paper----
# Reset layout and set tight margins
par(mfrow = c(1, 1))
par(mar = c(2, 2, 2, 2))  # Minimal margins
png("VE faceted.png", width = 1200, height = 1000, res = 300)  # High resolution for quality

faceted_plot

dev.off()


