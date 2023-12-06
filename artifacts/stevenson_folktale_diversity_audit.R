# Load diversity audit data
da_intake <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_590_dfw_diversity_audit/main/artifacts/Stevenson%20Elementary%20DFW%20diversity%20audit%20data%20collection%20(Responses)%20-%20Form%20Responses%201.csv", stringsAsFactors = FALSE)

#convert "Barcode" column to numeric data type 
da_intake$Barcode <- as.numeric(da_intake$Barcode)

class(da_intake$Barcode)

# remove na rows from da_intake
da_intake_na <- na.omit(da_intake)

# Load Follett Metadata
metadata <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_590_dfw_diversity_audit/main/artifacts/CRWReportJob1245096.csv", stringsAsFactors = FALSE)

# convert "Barcode" column to numeric
metadata$Barcode <- as.numeric(metadata$Barcode)

class(metadata$Barcode)

# load circulation data
circulation <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_590_dfw_diversity_audit/main/artifacts/circulation_report.csv" , stringsAsFactors = F)

# load dplyr, ggplot, stringr, tidytext
library(tidyverse)
library(ggplot2)
library(stringr)
library(tidytext)

# remove scientific notation
options(scipen=999)


# merge dataframes, remove repeat rows
da_data <- left_join(da_intake_na, metadata, by = "Barcode", relationship = "many-to-many") %>% 
  distinct(Barcode, Timestamp, .keep_all = TRUE)

# join circulation data to folktale data
folktale_circulation <- left_join(da_data,circulation, by = "Title.Subtitle")

# learn spread of publication year
publication_info <- da_data %>% 
  group_by(Publication.Year) %>% 
  summarize(books = n())

# learn spread of circulation by publication year
circulation_info <- folktale_circulation %>% 
  group_by(Publication.Year) %>% 
  summarize(total_checkouts = sum(Checkouts...Local.Patrons))

# merge data

circulation_by_year <- left_join(publication_info,circulation_info, by = "Publication.Year")

write.csv(circulation_by_year, "/Users/connor/Desktop/College/UW/FQ23/DFW-diversity-audit/circulation_by_year.csv")

#Average publication year: 1996
mean(da_data$Publication.Year, na.rm = T)

#Median publication year: 1996
median(da_data$Publication.Year, na.rm = T)

# plot spread of publication years
publication_year_graph <- ggplot(data = publication_info) + 
  geom_col(mapping = aes(
    x = Publication.Year,
    y = n
    ) , fill = "blue"
  ) +
  scale_x_continuous(
    breaks = seq(from = 1950, to = 2020, by = 10)
  ) +
  labs(
    title = "Count of Books by Publication Year" ,
    caption = "The average year of publication is 1996" ,
    x = "Publication Year" ,
    y = "Number of Books"
  )
print(publication_year_graph)

#learn spread of languages represented
language_info <- da_intake %>% 
  group_by(Language) %>% 
  summarise(n=n())
# convert the language_info dataframe into table for report

#counting spread of diverse identity indicators
diversity_info <- da_data %>% 
  mutate(Representation.indicators.from.cover.and.summary = str_split(Representation.indicators.from.cover.and.summary, ", ")) %>% 
  unnest_longer(Representation.indicators.from.cover.and.summary) %>%
  mutate(Representation.indicators.from.cover.and.summary = tolower(str_trim(Representation.indicators.from.cover.and.summary))) %>% 
   group_by(Representation.indicators.from.cover.and.summary) %>% 
   summarize(n=n())

#slice top 10, rename cells for legibility
top_10_diversity_info <- diversity_info %>% slice_max(n=10, order_by = n)
help("write.csv")
getwd
write.csv(top_10_diversity_info, file = "stevenson_top_ten_representation_groups.csv")
# 101 books with NO representation indicators at all

#graph diversity info
diversity_graph <- ggplot(data = top_10_diversity_info) +
  geom_col(mapping = aes(
    x = n,
    y = reorder(Representation.indicators.from.cover.and.summary, n)
  ), fill = "blue") +
  labs(
    title = "Most Frequent Instances of Representation" ,
    x = "Number of Instances" ,
    y = "Representative Group"
  ) + 
  geom_text(aes(x = n, y = reorder(Representation.indicators.from.cover.and.summary, n), label = n), hjust = -.25) +
  scale_y_discrete(
    breaks = c("Animals and/or mythical creatures" , "" , "Race/Ethnicity" , "indigenous" , "AAPI" , "africa" , "latin america" , "multicultural" , "china" , "Economic Welfare"),
    labels = c("Stories about animals and/or mythical creatures" , "White cishet able-bodied representation" , "BIPOC representation" , "Stories from Native American cultures" , "Stories from Asian and Pacific Islander cultures" , "Stories from African cultures" , "Stories from Latin American cultures" , "Multicultural stories" , "Stories from China" , "Stories about economic welfare")
  )

diversity_graph

# Text analysis of titles to see most popular titles

titles <- da_data %>% 
  unnest_tokens(bigram, Title.Subtitle, token = "ngrams" , n = 4) %>% 
  count(bigram, sort = TRUE)

#diversity_info <- base_diversity_info %>% 
#  group_by(str_detect(Representation.indicators.from.cover.and.summary, rep_indicators)) %>% 
#  summarize(n=n())

#find break up of representation of humans v animals in stories around the world (using case_when()????)

# Add new column if animals present for title
da_data <- da_data %>% 
  mutate(has_animals = 
           case_when(str_detect(Representation.indicators.from.cover.and.summary, "Animals and/or mythical creatures") ~ 
                       "animals and/or mythical creatures" ))

# Split rows
da_data <- da_data %>% separate_rows(Representation.indicators.from.cover.and.summary, sep = ",")

# Clean up white space and lowercase
da_data <- da_data %>% 
  mutate(Representation.indicators.from.cover.and.summary = tolower(str_trim(Representation.indicators.from.cover.and.summary)))

# Filter out animals from representation indicators to just focus on other indicators
just_locations <- da_data %>% 
  filter(!Representation.indicators.from.cover.and.summary == "animals and/or mythical creatures")

# Count up indicators only for animal books
location_count <- just_locations %>%
  # Only count up the representation indicators if animals present
  filter(has_animals == "animals and/or mythical creatures") %>% 
  count(Representation.indicators.from.cover.and.summary)

#create character vector for team names
main_categories <- c("animals and/or mythical creatures","","race/ethnicity","economic welfare","religion","mental health","physical health")
#filter out for main diversity categories collected from audit 
main_categories_info <- diversity_info %>% filter(Representation.indicators.from.cover.and.summary %in% main_categories)

# Add new column if racial diversity present for title
#da_data <- da_data %>% 
#  mutate(has_people = 
#           case_when(str_detect(Representation.indicators.from.cover.and.summary, "race/ethnicity") ~ 
#                       "race/ethnicity" ))

# Filter out people from representation indicators to just focus on other indicators
#just_locations_ppl <- da_data %>% 
#  filter(!Representation.indicators.from.cover.and.summary == "race/ethnicity")

#location_count_ppl <- just_locations_ppl %>%
  # Only count up the representation indicators if people present
#  filter(has_people == "race/ethnicity") %>% 
#  count(Representation.indicators.from.cover.and.summary)
#Find avg age of books for each identity group

#merge dataframes for comparison 
#merge_list <- list(location_count,diversity_info)

#merge_list %>% reduce(full_join, by="Representation.indicators.from.cover.and.summary")

#tibble(merge_list)
#Find avg age of books for each identity group




