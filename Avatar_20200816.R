
# Setup----
library(tidyverse)
library(tidytext)
library(tidytuesdayR)
library(stringi)
#library(tidytext)

tt_data <- tidytuesdayR::tt_load(2020, week = 33)
readme(tt_data)

avatar <- tuesdata$avatar

rating  <- avatar %>% 
  filter(character != "Scene Description") %>% 
  mutate(imdb_rating = if_else(book == "Water" & chapter_num == 20, 9.7, imdb_rating)) %>%
  count(book,chapter_num,chapter,writer, director,imdb_rating) %>%
  rename(total_line = n)

# number of lines each character 
character_lines <- avatar %>%
  filter(character != "Scene Description") %>%
  count(book, chapter_num,chapter, character) %>%
  rename(number_lines = n) 
  
# number of words each character 
character_words <- avatar %>%
    filter(character != "Scene Description") %>%
    mutate(words_per_line = stri_count_words(character_words)
                   ) %>%
       group_by(book,chapter_num, chapter, character) %>%
       summarise(sumwords_chapter = sum(words_per_line)) %>%
       ungroup()

cor(line_word_rating$sumwords_chapter , line_word_rating$number_lines)

line_word_rating <- 
   inner_join(character_lines, 
              character_words, 
              by = c("book","chapter_num","chapter", "character")) %>% 
    left_join(rating, by = c("book","chapter_num","chapter")) %>% 
     arrange(book, chapter_num,chapter, desc(number_lines), sumwords_chapter,increasing = FALSE) 

main.character <- c("Aang", "Sokka","Katara", "Toph", "Zuko")
main.villain <- c("Ozai",  "Amon", "Zaheer", "Azula", "Sozin")



line_word_rating %>% 
  filter(character %in% main.character) %>%
  mutate(   book = factor(book,levels = c('Water','Earth',"Fire")),
            character = factor(character, levels = main.character)) %>%
  
  ggplot(aes(x = chapter_num, y = sumwords_chapter )) + 
  geom_col(aes(color = character, fill = character )) +
  geom_line(aes(x = chapter_num, y = imdb_rating*200)) + 
  geom_point(aes(x = chapter_num, y = imdb_rating*200)) +
  coord_cartesian(xlim = c(1, 21)) + 
  scale_x_continuous(name="Chapter", breaks=seq(1,21,2)) + 
  scale_y_continuous("Sum of Word Count in Chapter", 
                     sec.axis = sec_axis(~ ./200, breaks=seq(1,10,1),
                                         name = "IMBD Rating")) +
   facet_wrap(~ book ,ncol = 3) +
  ggtitle("How the total words the main character spoke affects ibdb rating",
          subtitle = "Less words, higher rating!") +  
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

dev.new()
line_word_rating %>% 
  filter(character %in% main.character) %>%
  mutate(   book = factor(book,levels = c('Water','Earth',"Fire")),
            character = factor(character, levels = main.character)) %>%
  
  ggplot(aes(x = chapter_num, y = number_lines)) + 
  geom_col(aes(color = character, fill = character )) +
  geom_line(aes(x = chapter_num, y = imdb_rating*15)) + 
  geom_point(aes(x = chapter_num, y = imdb_rating*15)) +
  coord_cartesian(xlim = c(1, 21)) + 
  scale_x_continuous(name="Chapter", breaks=seq(1,21,2)) + 
  scale_y_continuous("Sum of Word Count in Chapter", 
                     sec.axis = sec_axis(~ ./15 , breaks=seq(1,10,1),
                                         name = "IMBD Rating")) +
  facet_wrap(~ book ,ncol = 3) +
  ggtitle("How number of lines the main character spoke affects ibdb rating",
          subtitle = "Less lines, higher rating!") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
  


     
    
  