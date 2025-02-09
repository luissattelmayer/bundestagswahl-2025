needs(tidyverse, fs, here)

manifestos_2025 <- dir_ls("manifestos/") |> 
  map_dfr(~ read_file(.x)) |>
  # pivot
  pivot_longer(cols = everything(), names_to = "party", values_to = "text") |> 
  # extract party name and year 
  mutate(party = str_replace_all(party, "manifestos/", ""),
         year = 2025,
         party = str_remove(party, "\\.txt*"))

manifestos_2025 <- manifestos_2025 |> 
  mutate(
    # Remove dots following one or two-digit numbers
    text = str_replace_all(text, "\\b\\d{1,2}\\.", ""),
    text = str_replace_all(text, "\n", " "),
    text = str_trim(text),
    # Split text into sentences on dots, ignoring dots followed by whitespace or end of text
    text = str_split(text, "(?<=\\.)\\s+(?=[A-Z])|(?<=\\.)$")
  ) |> 
  unnest(text) |> 
  # remove row if less than 4 characters
  filter(str_length(text) > 4) |>
  unique() |> 
  group_by(party, year) |>
  mutate(manifesto = str_c(party, year, sep = "_"))


manifestos_2025 |> 
  group_by(party) |> 
  count()


write_csv(manifestos_2025, here("data" ,"manifestos_2025.csv"))







