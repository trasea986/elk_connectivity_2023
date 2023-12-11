library(tidyverse)

df <- read.csv("./data/CombinedElkGenomics_Metadata_INProgress11302023.csv")

#count by year after breaking up collection date

df <- df %>%
  separate (collection_date,c("month", "day", "year_c"), sep = "/")

#note 318-321 are missing a full date, so fix here
df[c(318:321),6] <- 2017

df %>%
  group_by(year_c) %>%
  tally()

df %>%
  group_by(Ownership) %>%
  tally()

df %>%
  group_by(age_in_years) %>%
  tally()

df %>%
  group_by(sex) %>%
  tally()

df %>%
  group_by(Ownership, sex) %>%
  tally()

df %>%
  summarise_all(~ sum(is.na(.)))

time_samples <- df %>%
  group_by(year_c, Ownership) %>%
  tally()

ggplot(time_samples, aes(x=year_c, y = n, color = Ownership, shape = Ownership, label = n)) +
  geom_point(size = 3) +
  #geom_text(vjust = -1)+
  scale_color_manual(values = c("MN" = "darkblue", "ND" = "darkgreen", "MUN" = "darkgrey")) +
  scale_shape_manual(values = c("MN" = 16, "ND" = 17, "MUN" = 15)) +
  theme_bw()


cm_total <- time_samples %>%
  group_by(Ownership) %>%
  mutate(cumulative_total = cumsum(n))

ggplot(cm_total, aes(x=year_c, y = cumulative_total, color = Ownership, shape = Ownership, label = n)) +
  geom_point(size = 3) +
  geom_text(vjust = -1)+
  scale_color_manual(values = c("MN" = "darkblue", "ND" = "darkgreen", "MUN" = "darkgrey")) +
  scale_shape_manual(values = c("MN" = 16, "ND" = 17, "MUN" = 15)) +
  theme_bw()

max(df$age_in_years, na.rm = TRUE)

ggplot(df, aes(x = age_in_years)) +
  geom_histogram() +
  theme_bw()
