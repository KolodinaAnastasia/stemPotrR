stem_word <- function(word) {
  # PERFECTIVEGROUND
  word <- gsub("(ив|ивши|ившись|ыв|ывши|ывшись|[ая]в|[ая]вши|[ая]вшись)$", "", word, perl=TRUE)
  # REFLEXIVE
  word <- gsub("(с[яь])$", "", word, perl=TRUE)
  # ADJECTIVE
  word <- gsub("(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|ему|ому|их|ых|ую|юю|ая|яя|ою|ею)$", "", word, perl=TRUE)
  # PARTICIPLE
  word <- gsub("(ивш|ывш|ующ|[ая]ющ|[ая]щ)$", "", word, perl=TRUE)
  # VERB
  word <- gsub("(ила|ыла|ена|ейте|уйте|ите|или|ыли|ей|уй|ил|ыл|им|ым|ен|ило|ыло|ено|ят|ует|уют|ит|ыт|ены|ить|ыть|ишь|ую|ю|[ая]те|[ая]ют|[ая]т)$", "", word, perl=TRUE)
  # NOUN
  word <- gsub("(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я)$", "", word, perl=TRUE)
  # DERIVATIONAL
  word <- gsub("[^аеиоуыэюя]+[аеиоуыэюя]ость?$", "", word, perl=TRUE)
  # DER
  word <- gsub("ость?$", "", word, perl=TRUE)
  # SUPERLATIVE
  word <- gsub("(ейше|ейш)$", "", word, perl=TRUE)
  # I
  word <- gsub("и$", "", word, perl=TRUE)
  # P
  word <- gsub("ь$", "", word, perl=TRUE)
  # NN
  word <- gsub("нн$", "н", word, perl=TRUE)

  return(word)
}


filename<-readline(prompt = "Введите полный путь в файлу: ")

extract_words <- function(filename) {
  # Чтение файла
  text <- readLines(filename, encoding = "UTF-8")

  # Разбиение текста на отдельные слова
  words <- unlist(strsplit(paste(text, collapse = " "), "[[:punct:][:space:]]+"))

  # Удаление пустых элементов
  words <- words[words != ""]

  return(words)
}

result <- extract_words(filename)
for (i in 1: length (result)){
  result[i] = stem_word(result[i])
}

write(paste(result, collapse = " "), file = "result_of_stemmer.txt")
