#install.packages("udpipe")
#install.packages("rsyntax")
library(tidyverse)
amcat4r::amcat_login("https://amcat4.labs.vu.nl/amcat")
docs = amcat4r::query_documents("deventer_sites", fields=c("url", "title", "text"))
docs = docs |> rename(doc_id=url) |> mutate(text=str_c(title, text, sep="\n\n"))

library(udpipe)
tokens = docs|> udpipe('dutch')

library(rsyntax)
plot_tree(tokens |> filter(), token, lemma, upos)

tokens |> select(doc_id, sentence_id, sentence) |> unique() |> View()

tokens |> filter(doc_id == 'https://www.salland747.nl/salland/bij-ripperda-verkocht-en-zuivel-van-salland-stopt-met-produceren/', sentence_id==7) |>
  plot_tree(token, lemma, upos)


tokens = tokens_dutchquotes |>
  split.data.frame(tokens_dutchquotes$sentence) |> 
  lapply("[[", "token") |>
  udpipe('dutch')

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
  zegtdat = tquery(lemma=verbs,
                   children(label='source', relation='nsubj'),
                   children(label='quote', relation='ccomp')),

  # x stelt: y
  ystelt = tquery(lemma = verbs, 
                  children(label='source', relation=c('nsubj')),
                  parents(label='quote'),
                  children(lemma =  quote_punctuation)),
  # x zegt: "..."
  ystelt2 = tquery(lemma = verbs, 
                 children(label='source', relation=c('nsubj')),
                 children(label='quote', relation=c('obj', 'obl'))),
  # y, stelt x
  xstelt = tquery(label='quote',
                  children(label='verb', lemma=verbs, relation='parataxis', 
                           children(label='source', relation='nsubj')
                  )),
  # y, volgens x
  volgens = tquery(label='quote',
  )
)

annotate_tqueries(tokens, "quote", queries) |> plot_tree(doc_id="5", token, lemma, annotation='quote')

dev.off()
udpipe('Vooruitblikkend naar de Tweede Kamerverkiezingen van 12 september stelde Rutte: "Een stem op de PVV, is een verloren stem". ', 'dutch') |> pull(sentence)

udpipe('Volgens mij slaat dat echt nergens op', 'dutch') |> plot_tree(lemma)
udpipe('Volgens Piet is cake eigenlijk best lekker', 'dutch') |> plot_tree(lemma)

                                                                                                                                                        