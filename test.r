library(tidyverse)

library(udpipe)

library(rsyntax)

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




