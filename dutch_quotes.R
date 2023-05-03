say_verbs = c("zeggen", "benadrukken", "beweren", "aankondigen", "bekendmaken", "schrijven") # always a quote
vertel_verbs = c("stellen", "beamen", "vertellen", "vinden", "schetsen", "vervolgen", "weten", "erkennen", "erkent", "vragen", "vraag", "waarschuwen", "waarschuwt") # only a quote if quote marks etc are present
toevoegen_verbs = c("voegen", "kijken")  # verbs which require a participle, e.g. voegen -> toevoegen
quotation_mark = c('"', "'", "''")
old_verbs = c("accepteren", "antwoorden", "beamen", "bedenken", "bedoelen", "begrijpen", "bekenen", 
                    "beklemtonen", "bekrachtigen", "belijden", "beluisteren", "benadruken", "berekenen", "berichten", "beschouwen", "beschrijven", "beseffen", "betuigen", "bevestigen", "bevroeden", 
                    "beweren", "bewijzen", "bezweren", "biechten", "aan_bieden", "brengen", "brullen", "concluderen", "confirmeren", "constateren", "debiteren", "declareren", "demonstreren", "denken", "uit_dragen", 
                    "emailen", "erkenen", "expliceren", "expliciteren", "fantaseren", "formuleren", "aan_geven", "geloven", "horen", "hameren", "herinneren", "vol_houden", "aan_kondigen", "kwetteren", 
                    "toe_lichten", "bekend_maken", "hard_maken", "melden", "merken", "op_merken", "motiveren", "noemen", "nuanceren", "observeren", "onderschrijven", "onderstrepen", "onthullen", "ontsluieren", 
                    "ontvallen", "ontvouwen", "oordelen", "parafraseren", "postuleren", "preciseren", "presumeren", "pretenderen", "publiceren", "rapporteren", "realiseren", "redeneren", "refereren", 
                    "rekenen", "roepen", "aan_roeren", "ruiken", "schaten", "schetsen", "schilderen", "schreeuwen", "schrijven", "signaleeren", "snappen", "snateren", "specificeren", "uit_spreken", "staven", "stellen",
                    "vast_stellen","aan_stippen", "suggereren", "tateren", "aan_tekenen", "aan_tonen", "twitteren", "verbazen", "verhalen", "verklaren", "verklappen", "verkondigen", "vermoeden", "veronderstellen", "verraden", "vertellen", "na_vertellen", 
                    "verwachten", "verwittigen", "verwonderen", "verzekeren", "vinden", "voelen", "aan_voelen", "waarschuwen", "wedden", "weten", "aan_wijzen", "winden", "zeggen", "uiteen_zetten", "zien")

queries = list(

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

replace_quotes = function(text) text |> str_replace_all("[“”„‟«»]", '"') |> str_replace_all('[‘’‚‛‹›]', "'")


