series <- c("1 Series", "2 Series", "3 Series", "4 Series", "5 Series" )

bmw_series_group <- bmw %>%
  filter(model %in% series) %>%
  filter(fuelType == c("Diesel", "Petrol")) %>%
  mutate(fuelType = as.factor(fuelType)) %>%
  filter(transmission == c("Automatic", "Manual")) %>%
  mutate(transmission = as.factor(transmission)) %>%
  mutate(mlg = case_when(
                      mileage <= 20000 ~ "Lowest",
                      mileage > 20000 & mileage <= 45000 ~ "Low",
                      mileage > 45000 & mileage <= 70000 ~ "Medium",
                      mileage > 70000 ~ "High",
                      TRUE ~ as.character(mileage)
                        )) %>%
  mutate(mlg = as.factor(mlg)) %>%
  mutate(age = 2020 - year) %>%
  unite("merged", c(model, transmission, fuelType, mlg), sep = " ")

bmw_series_count <- bmw_series_group %>%
  group_by(merged) %>%
  count() 

bmw_series_group <- bmw_series_group %>%
  full_join(bmw_series_count, by = c("merged" = "merged")) %>%
  filter(n > 10)

bmw_series_group <- bmw_series_group %>%
  separate(merged, c("model", "specification", "transmission", "fuelType", "mlg"), sep = " ") 

bmw_series_group <- bmw_series_group %>%
  unite("merged", c(model, specification, transmission, fuelType), sep = " ", remove = FALSE) %>%
  mutate(merged = as.factor(merged), mlg = as.factor(mlg)) %>%
  mutate(specification = NULL)

bmw_series_group
