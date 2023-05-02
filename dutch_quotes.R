#install.packages("udpipe")
#install.packages("rsyntax")
library(tidyverse)

library(udpipe)

library(rsyntax)

say_verbs = c("zeggen", "benadrukken", "beweren", "aankondigen", "bekendmaken", "schrijven") # always a quote
vertel_verbs = c("stellen", "beamen", "vertellen", "vinden", "schetsen", "vervolgen", "weten", "erkennen", "erkent", "vragen", "vraag", "waarschuwen", "waarschuwt") # only a quote if quote marks etc are present
toevoegen_verbs = c("voegen", "kijken")  # verbs which require a participle, e.g. voegen -> toevoegen
quotation_mark = c('"', "'", "''")
verbs = c("accepteren", "antwoorden", "beamen", "bedenken", "bedoelen", "begrijpen", "bekenen", 
                    "beklemtonen", "bekrachtigen", "belijden", "beluisteren", "benadruken", "berekenen", "berichten", "beschouwen", "beschrijven", "beseffen", "betuigen", "bevestigen", "bevroeden", 
                    "beweren", "bewijzen", "bezweren", "biechten", "aan_bieden", "brengen", "brullen", "concluderen", "confirmeren", "constateren", "debiteren", "declareren", "demonstreren", "denken", "uit_dragen", 
                    "emailen", "erkenen", "expliceren", "expliciteren", "fantaseren", "formuleren", "aan_geven", "geloven", "horen", "hameren", "herinneren", "vol_houden", "aan_kondigen", "kwetteren", 
                    "toe_lichten", "bekend_maken", "hard_maken", "melden", "merken", "op_merken", "motiveren", "noemen", "nuanceren", "observeren", "onderschrijven", "onderstrepen", "onthullen", "ontsluieren", 
                    "ontvallen", "ontvouwen", "oordelen", "parafraseren", "postuleren", "preciseren", "presumeren", "pretenderen", "publiceren", "rapporteren", "realiseren", "redeneren", "refereren", 
                    "rekenen", "roepen", "aan_roeren", "ruiken", "schaten", "schetsen", "schilderen", "schreeuwen", "schrijven", "signaleeren", "snappen", "snateren", "specificeren", "uit_spreken", "staven", "stellen",
                    "vast_stellen","aan_stippen", "suggereren", "tateren", "aan_tekenen", "aan_tonen", "twitteren", "verbazen", "verhalen", "verklaren", "verklappen", "verkondigen", "vermoeden", "veronderstellen", "verraden", "vertellen", "na_vertellen", 
                    "verwachten", "verwittigen", "verwonderen", "verzekeren", "vinden", "voelen", "aan_voelen", "waarschuwen", "wedden", "weten", "aan_wijzen", "winden", "zeggen", "uiteen_zetten", "zien")

queries = list(

  # x stelt: 'y'
  #ystelt = tquery(lemma = verbs, 
  #                children(label='source', relation=c('nsubj')),
  #                parents(label='quote'),
  #                children(lemma =  quote_punctuation)),
  # x zegt: "..."
  ystelt2 = tquery(lemma = say_verbs, 
                   label='verb', fill = F,
                   children(label='source', relation=c('nsubj')),
                   children(label='quote', relation=c('obj', 'obl', 'parataxis', 'ccomp', 'advcl'))),
  # x doet iets en zegt ...
  ystelt3 = tquery(lemma = say_verbs, 
                   label='verb', fill = F, relation='conj',
                   parents(children(relation=c('nsubj'), label='source')),
                   children(label='quote', relation=c('obj', 'obl', 'parataxis', 'ccomp', 'advcl'))),
  # y, wordt door x gezegd
  ystelt2 = tquery(lemma = say_verbs, 
                   label='verb', fill = F,
                   children(label='source', relation=c('obl:agent')),
                   children(label='quote', relation=c('ccomp', 'advmod'))),
  # y, stelt x
  xstelt = tquery(label='quote', 
                  children(label='verb', lemma=say_verbs, relation='parataxis', fill=F,
                           children(label='source', relation='nsubj')
                  )),
  # y, volgens x
  volgens = tquery(label='quote',
                   children(relation=c('obl', 'parataxis', 'appos'), label='source',
                            children(lemma=c('aldus', 'volgens')))
  ),
  # x: ... (udpipe sometimes? splits sentence on :, so no quote)
  volgens = tquery(label='source',
                   next_start_quote=T,
                   relation='ROOT',
                   upos=c('NOUN', 'PROPN'),
                   children(lemma=":")
  ),
  # x vertelt: ... (udpipe splits sentence on :, so no quote)
  vertelt = tquery(label='verb',
                   lemma=vertel_verbs,
                   fill=F,
                   relation='ROOT',
                   children(lemma=":"),
                   children(relation='nsubj', label='source')
  ),  # x vertelt. "...." (quote in next sentence)
  vertelt3 = tquery(label='verb',
                   lemma=vertel_verbs,
                   fill=F,
                   relation='ROOT',
                   next_start_quote=T,
                   children(relation='nsubj', label='source')
  ),
  # "...", vertelt x
  vertelt2 = tquery(label='verb',
                   lemma=vertel_verbs,
                   relation='parataxis',
                   fill=F,
                   children(relation='nsubj', label='source'),
                   parents( label='quote', children(lemma=quotation_mark))
                           
  ),
  # "...", voegt x toe
  toevogen = tquery(label='verb',
                    lemma=toevoegen_verbs,
                    relation='parataxis',
                    fill=F,
                    children(relation='nsubj', label='source'),
                    children(relation="compound:prt"),
                    parents(label='quote', children(lemma=quotation_mark))
  ),
  # x: y (udpipe sometimes? does include quote)
  dubbelepunt2 = tquery(label='source',
                   upos=c('NOUN', 'PROPN'),
                   children(lemma=":"),
                   parents(label='quote',
                          children(lemma=quotation_mark))
  )
)


tokenize = function(x) list(a=str_split_1(x, "(\\b|\\s)+") |>trimws()|> str_subset("\\S"))


dev.off()
udpipe('Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte: "Een stem op de PVV, is een verloren stem". ', 'dutch') |> pull(sentence)
udpipe(tokens, 'dutch') 

udpipe('Lekker, stelt Piet.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(token, lemma, annotation='quote')
udpipe('Piet zegt dat het lekker is', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(token, lemma, annotation='quote')
udpipe('Volgens mij slaat dat echt nergens op.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Volgens Piet is cake eigenlijk best lekker!', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Onbegrijpelijk, zo noemt Piet de uitleg.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Cake eigenlijk best lekker, aldus Piet.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')

# y: x
udpipe(tokenize("Jan: Dit kan zo niet langer!"), 'dutch')|> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe(tokenize("De nieuwe partijleider: Dit kan zo niet langer!"), 'dutch')|> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe(tokenize("De nieuwe partijleider zegt: Dit kan zo niet langer!"), 'dutch')|> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe(tokenize("De nieuwe partijleider: 'Dit kan zo niet langer'."), 'dutch')|> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')

# Passief
udpipe('Ongebrijpelijk, wordt door Piet beweerd.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(token, lemma, annotation='quote')
udpipe('Dat het niet goed is wordt al lang door Piet gezegd', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(token, lemma, annotation='quote')



udpipe('Hij stelde zich onbegrijpelijk op.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Hij voelde zich niet lekker.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Hij voelde aan zijn hoofd.', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')

udpipe('Ans Hetebrij:', 'dutch') |> add_column(next_start_quote=T) |> annotate_tqueries("quote", queries) |> plot_tree(lemma, upos, annotation='quote')
udpipe('Ans Hetebrij:', 'dutch') |> add_column(next_start_quote=F) |> annotate_tqueries("quote", queries) |> plot_tree(lemma, upos, annotation='quote')
udpipe('De nieuwe partijleider:', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('De nieuwe partijleider vertelt:', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Aldus de nieuwe partijleider:', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, annotation='quote')
udpipe('Zaterdag 18 februari:', 'dutch') |> annotate_tqueries("quote", queries) |> plot_tree(lemma, upos, annotation='quote')

udpipe('Het Olster veer is nog niet gestremd en blijft volgens de normale dienstregeling varen', 'dutch')|> add_column(next_start_quote=F)  |> annotate_tqueries("quote", queries) |> plot_tree(lemma, upos, annotation='quote')
udpipe('Het Olster veer is nog niet gestremd en blijft varen, volgens de woordvoerder', 'dutch')|> add_column(next_start_quote=F)  |> annotate_tqueries("quote", queries) |> plot_tree(lemma, upos, annotation='quote')


# Apply to actual texts
replace_quotes = function(text) text |> str_replace_all("[“”„‟«»]", '"') |> str_replace_all('[‘’‚‛‹›]', "'")

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
