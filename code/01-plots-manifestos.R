needs(tidyverse, SnowballC, tidytext, stopwords)

manifestos <- read_csv("data/bundestagswahl_2025_classified_df.csv")

manifestos_clean <- manifestos |> 
  mutate(
    text = stringi::stri_trans_nfc(manifestos_clean$text),
    party = stringi::stri_trans_nfc(manifestos_clean$party),
    predicted = stringi::stri_trans_nfc(manifestos_clean$predicted),
    label = case_when(
    predicted == "1 - Macroeconomics" ~ "Macroecnomics",
    predicted == "2 - Civil Rights" ~ "Civil Rights",
    predicted == "3 - Health" ~ "Health",
    predicted == "4 - Agriculture" ~ "Agriculture",
    predicted == "5 - Labor" ~ "Labor",
    predicted == "6 - Education" ~ "Education",
    predicted == "7 - Environment" ~ "Environment",
    predicted == "8 - Energy" ~ "Energy",
    predicted == "9 - Immigration" ~ "Immigration",
    predicted == "10 - Transportation" ~ "Transportation",
    predicted == "12 - Law and Crime" ~ "Law and Crime",
    predicted == "13 - Social Welfare" ~ "Social Welfare",
    predicted == "14 - Housing" ~ "Housing",
    predicted == "15 - Domestic Commerce" ~ "Domestic Commerce",
    predicted == "16 - Defense" ~ "Defense",
    predicted == "17 - Technology" ~ "Technology",
    predicted == "18 - Foreign Trade" ~ "Foreign Trade",
    predicted == "19.1 - International Affairs" ~ "International Affairs",
    predicted == "19.2 - European Integration" ~ "European Integration",
    predicted == "20 - Government Operations" ~ "Government Operations",
    predicted == "23 - Culture" ~ "Culture",
    predicted == "98 - Non-thematic" ~ "Non-thematic",
    predicted == "99 - Other" ~ "Other"
  ),
  text = str_replace_all(text, "www\\.bsw-vg\\.de", ""))


manifestos_clean |> 
  group_by(party) |> 
  count(label) |> 
  mutate(percent = n / sum(n)*100) |> 
  # filter(label %in% c("Immigration", "Environment")) |> 
  ggplot(aes(x = party, y = percent, fill = party)) +
  geom_col(position = "dodge2") +
  theme_minimal() +
  facet_wrap(~ label) +
  scale_fill_manual(values = c("afd" = "#3782fa", "cdu-csu" = "black", 
                                "spd" = "red", "fdp" = "#fcdf03", "grüne" = "#05e305",
                                "linke" = "#f507e1", "bsw" = "darkred")) +
  # rotate x labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "None") +
  # reorder x labels
  scale_x_discrete(limits = c("spd", "cdu-csu","grüne", "fdp", "afd", "linke", "bsw"),
                   labels = c("afd" = "AfD", "cdu-csu" = "CDU/CSU", 
                              "spd" = "SPD", "fdp" = "FDP", "grüne" = "Grüne",
                              "linke" = "Linke", "bsw" = "BSW")) +
  labs(
    title = "Thematic classification of manifesto sentences in %",
    y = "Share of Sentences",
    x = "Party"
  )


# calculate tf-idf for each party and label
manifestos_clean |> 
  unnest_tokens(word, text) |> 
  filter(!word %in% stopwords_vec) |> 
  filter(!str_detect(word, "\\d+")) |>
  mutate(word = wordStem(word, language = "german")) |>
  filter(label == "Environment") |>
  count(party, word) |>
  group_by(party) |> 
  ungroup() |> 
  bind_tf_idf(word, party, n) |>
  arrange(desc(tf_idf)) |> 
  group_by(party) |> 
  slice_max(tf_idf, n = 7) |>
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = party)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ party, scales = "free_y") + 
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 TF-IDF Wörter für Immigration nach Partei",
    y = "TF-IDF",
    x = "Wort"
  )
