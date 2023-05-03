# Apply to actual texts

amcat4r::amcat_login("https://amcat4.labs.vu.nl/amcat")
docs = amcat4r::query_documents("deventer_sites", fields=c("url", "title", "text"), max_pages = 0)

d = docs |> slice_sample(n=100) |> rename(doc_id=url) |> mutate(text=str_c(title, text, sep="\n\n") |> replace_quotes()) |> filter(!is.na(doc_id), doc_id != "NA")
tokens = d |> udpipe('dutch') |> as_tibble() 

next_quotes = tokens |> select(doc_id, sentence_id, sentence) |> unique() |>  
  mutate(start_quote=str_detect(sentence, "^\\s*['\"“”‘’‚‛„‟‹›«»]"),
         next_start_quote=lead(start_quote) & lead(doc_id) == doc_id) |>
  select(doc_id, sentence_id, next_start_quote) 

tokens = inner_join(tokens, next_quotes)  |> select(-sentence, sentence=sentence_id)

library(tokenbrowser)
annotated = tokens |> annotate_tqueries("quote", queries) 
categorical_browser(annotated, category=annotated$quote, token_col="token") |> browseURL()

tokens |> filter(doc_id=="https://indebuurt.nl/deventer/besparen/bespaartip-in-deventer-kun-je-meedoen-aan-een-maandelijkse-kledingruil~203713/") |> View()
# foute parse
udpipe('Jezus reageert op deze vraag echter met het beroemde verhaal van de Barmhartige Samaritan en zegt dat niet de vraag is wie jouw naaste is, maar of jij naaste bent.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')


x = inner_join(tokens, next_quotes) |> select(-sentence, sentence=sentence_id)

x |> filter(sentence==24, doc_id == "https://indebuurt.nl/deventer/besparen/bespaarparel-in-de-winkel-van-ronald-kevin-en-romy-kun-je-terecht-als-je-het-minder-breed-hebt~202604/") |>
  annotate_tqueries("quote", queries) |> plot_tree(lemma, upos, annotation='quote')


annotated |> filter(sentence %in% c(29), doc_id=="https://indebuurt.nl/deventer/besparen/bespaartip-in-deventer-kun-je-meedoen-aan-een-maandelijkse-kledingruil~203713/") 


url = 'lexisnexis://df1b74ed58a01b883e223e2bd4f34a58'
x = tokens |> filter(doc_id == url) |> 
  annotate_tqueries("quote", queries)
categorical_browser(x, category=x$quote, token_col="token") |> browseURL()
plot_tree(x, sentence_i=11, lemma, upos, annotation = 'quote')
x |> filter(str_detect(token, "kijkt"))
