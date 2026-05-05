suppressMessages(library(dplyr))
options(readr.show_types = FALSE)
library(readr)
library(readxl)
library(stringr)

airbnb_price <- read_csv('data/airbnb_price.csv', show_col_types=FALSE)
airbnb_room_type <- read_excel('data/airbnb_room_type.xlsx')
airbnb_last_review <- read_tsv('data/airbnb_last_review.tsv', show_col_types=FALSE)

listings <- airbnb_price %>%
  inner_join(airbnb_room_type, by = "listing_id") %>%
  inner_join(airbnb_last_review, by = "listing_id")

review_dates <- listings %>%
  mutate(last_review_date = as.Date(last_review, format = "%B %d %Y")) %>%
  summarize(first_reviewed = min(last_review_date),
            last_reviewed = max(last_review_date))

private_room_count <- listings %>%
  mutate(room_type = str_to_lower(room_type)) %>%
  count(room_type) %>%
  filter(room_type == "private room")

nb_private_rooms <- private_room_count$n

avg_price <- listings %>%
  mutate(price_clean = str_remove(price, " dollars") %>% as.numeric()) %>%
  summarize(avg_price = mean(price_clean)) %>%
  as.numeric()

review_dates$nb_private_rooms = nb_private_rooms
review_dates$avg_price = round(avg_price, 2)
print(review_dates)
