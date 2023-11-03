# Load diversity audit data
da_intake <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_590_dfw_diversity_audit/main/artifacts/Stevenson%20Elementary%20DFW%20diversity%20audit%20data%20collection%20(Responses)%20-%20Form%20Responses%201.csv", stringsAsFactors = FALSE)

#convert "Barcode" column to numbers 
da_intake$Barcode <- as.numeric(da_intake$Barcode)

class(da_intake$Barcode)

#remove na rows from da_intake
da_intake_na <- na.omit(da_intake)

# Load Follett Metadata
metadata <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_590_dfw_diversity_audit/main/artifacts/CRWReportJob1245096.csv", stringsAsFactors = FALSE)

#convert "Barcode" column to numbers 
metadata$Barcode <- as.numeric(metadata$Barcode)

class(metadata$Barcode)

#load dplyr, ggplot, stringr, tidytext
library(tidyverse)
library(ggplot2)
library(stringr)
library(tidytext)

#remove scientific notation
options(scipen=999)


#merge dataframes, remove repeat rows
da_data <- left_join(da_intake_na, metadata, by = "Barcode", relationship = "many-to-many") %>% 
  distinct(Barcode, Timestamp, .keep_all = TRUE)

# learn spread of publication year
publication_info <- da_data %>% 
  group_by(Publication.Year) %>% 
  summarize(n=n())

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
  group_by(Representation.indicators.from.cover.and.summary) %>% 
  summarize(n=n())

#slice top 10, rename cells for legibility
top_10_diversity_info <- diversity_info %>% slice_max(n=10, order_by = n)
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
    labels = c("Stories about animals and/or mythical creatures" , "White cishet able-bodied representation" , "BIPOC representation" , "Stories from Native American cultures" , "Stories from Asian and Pacific Islander cultures" , "Stories from African cultures" , "Stories from Latin American cultures" , "Multicultural stories" , "Stories from China" , "Stories about econimc welfare")
  )

diversity_graph

# Text analysis of titles to see most popular titles

titles <- da_data %>% 
  unnest_tokens(bigram, Title.Subtitle, token = "ngrams" , n = 4) %>% 
  count(bigram, sort = TRUE)

#diversity_info <- base_diversity_info %>% 
#  group_by(str_detect(Representation.indicators.from.cover.and.summary, rep_indicators)) %>% 
#  summarize(n=n())
