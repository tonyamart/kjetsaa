# 00_data_preparation


## Data preparation

### PoeTree data

Select authors from PoeTree (based on Kjetsaa’s list of authors + Fet
A.A.)

``` r
poetree_authors <- get_authors(corpus = "ru") %>% 
  filter(str_detect(name, "Pushkin A.S.|Baratynsk|Lermontov|Zhukovsk|Vjazemsk|Tjutch|Ryleev|Batju|Koltsov|Delvig|Bestuzhev|Kjuhelb|Glinka F.N.|Jazykov|Benediktov|Kozlov I.I.|Shevyr|Polezh|Homja|Venevit|Fet")) %>% 
  select(id_, name)

poetree_authors
```

    # A tibble: 21 x 2
         id_ name                     
       <int> <chr>                    
     1   366 Baratynskij E.A.         
     2   135 Batjushkov K.N.          
     3   369 Benediktov V.G.          
     4   368 Bestuzhev-Marlinskij A.A.
     5   364 Delvig A.A.              
     6   209 Fet A.A.                 
     7   363 Glinka F.N.              
     8   201 Homjakov A.S.            
     9   284 Jazykov N.M.             
    10   268 Kjuhelbeker V.K.         
    # i 11 more rows

Get texts ids using authors’ ids

``` r
poem_ids <- get_poems(corpus = "ru", authors = poetree_authors$id_)

glimpse(poem_ids)
```

Get texts

``` r
vec_ids <- poem_ids$id_[1:3]

head(vec_ids) 
length(vec_ids) # 4842 poems

# call each text separately with lapply + catch errors where the function stopped
texts_list <- lapply(vec_ids, function(id){
  tryCatch( # try except
    get_text(corpus = "ru", poem_id = id),
    error = function(e) tibble(error = e$message, poem_id = id) 
    # if error attach info to relevant columns
  )
})

texts <- bind_rows(texts_list)

glimpse(texts)
```

Check errors

``` r
errors <- texts %>% 
  filter(!is.na(error)) %>% 
  pull(poem_id)

poem_ids %>% 
  filter(id_ %in% errors)
```

Tokenization

``` r
ru_poetree_tokens <- ru_poetree %>% 
  unnest_tokens(input = poem_text, output = word, token = "words")
```

Write data

``` r
#write.csv(ru_poetree, file = "../data/large_poetree_ru_19c.csv") # full texts
write.csv(ru_poetree_tokens, file = "../data/large_poetree_ru_19c_tokens.csv") # tokenized
```

NB: Next step: lemmatize the tokenized file with
`00_lemmatization.ipynb` notebook!

### Kjetsaa’s corpus

Load manually prepared data

``` r
kjetsaa <- read.delim("../data/kjetsaa_norm_corpus.csv", sep = ";") %>% 
  select(-X, -text_raw, -note)

ru_poetree <- read_csv("../data/large_poetree_ru_19c_lemma.csv") %>% select(-`...1`)
```

    New names:
    Rows: 1124893 Columns: 12
    -- Column specification
    -------------------------------------------------------- Delimiter: "," chr
    (7): corpus, id, title, author_name, word, lemma, analysis dbl (5): ...1,
    id_poem, year_created_from, year_created_to, id_author
    i Use `spec()` to retrieve the full column specification for this data. i
    Specify the column types or set `show_col_types = FALSE` to quiet this message.
    * `` -> `...1`

``` r
glimpse(kjetsaa)
```

    Rows: 298
    Columns: 4
    $ id         <int> 12083, 12085, 12036, 12105, 12112, NA, 12136, 12154, 12182,~
    $ text_final <chr> "\320\223\320\264\320\265 \320\275\320\260\321\210\320\260 ~
    $ author     <chr> "\320\220. \320\241. \320\237\321\203\321\210\320\272\320\2~
    $ header     <chr> "\320\240\320\276\320\267\320\260 : \302\253\320\223\320\26~

Add author names transliteration

``` r
# load prepared poetree data if needed
# ru_poetree <- read.csv("../data/poetree_ru_1810-1839.csv")

authors_translit <- tibble(
  author_name = unique(ru_poetree$author_name),
  author_name_ru = c("Е. А. Баратынский", "К. Н. Батюшков", "В. Г. Бенедиктов", 
                     "А. А. Бестужев-Марлинский", "А. А. Дельвиг", 
                     " А. А. Фет",
                     "Ф. Н. Глинка", "А. С. Хомяков", "Н. М. Языков", 
                     "В. К. Кюхельбекер",  "А. В. Кольцов", "И. И. Козлов", 
                     "М. Ю. Лермонтов", "А. И. Полежаев", "А. С. Пушкин", 
                     "К. Ф. Рылеев", "С. П. Шевырев", "Ф. И. Тютчев", 
                     "Д. В. Веневитинов",
                     "П. А. Вяземский", "В. А. Жуковский"
                     )
)

authors_translit
```

Attach transliterated names and tokenize

``` r
kjetsaa_tokens <- kjetsaa %>% 
  rename(author_name_ru = author) %>% 
  left_join(authors_translit, by = "author_name_ru") %>% 
  unnest_tokens(input = text_final, output = word, token = "words")
```

Write tokenized df  –\> to be lemmatized with `00_lemmatization.ipynb`

``` r
write.csv(kjetsaa_tokens, file = "../data/kjetsaa_norm_tokens.csv")
```

## Data overview

Load prepared data

### poetree

``` r
poetree_ru <- read.csv("../data/large_poetree_ru_19c_lemma.csv") %>% select(-X)

glimpse(poetree_ru)
```

    Rows: 1,124,893
    Columns: 11
    $ corpus            <chr> "ru", "ru", "ru", "ru", "ru", "ru", "ru", "ru", "ru"~
    $ id_poem           <int> 33734, 33734, 33734, 33734, 33734, 33734, 33734, 337~
    $ id                <chr> "br1-105", "br1-105", "br1-105", "br1-105", "br1-105~
    $ title             <chr> "\320\232...", "\320\232...", "\320\232...", "\320\2~
    $ year_created_from <dbl> 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821~
    $ year_created_to   <dbl> 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821~
    $ id_author         <int> 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 22~
    $ author_name       <chr> "Baratynskij E.A.", "Baratynskij E.A.", "Baratynskij~
    $ word              <chr> "\320\277\321\200\320\270\321\217\321\202\320\265\32~
    $ lemma             <chr> "\320\277\321\200\320\270\321\217\321\202\320\265\32~
    $ analysis          <chr> "[{'analysis': [{'lex': '\320\277\321\200\320\270\32~

Number of unique words:

``` r
poetree_ru %>% 
  count(lemma) %>% nrow()
```

    [1] 32137

Proportion of words written by each author

``` r
poetree_ru %>% 
  select(author_name, lemma) %>% 
  group_by(author_name) %>% 
  count(sort = T, name = "n_words") %>% 
  ungroup() %>% 
  mutate(perc = round(n_words / nrow(poetree_ru) * 100, 1))
```

    # A tibble: 21 x 3
       author_name      n_words  perc
       <chr>              <int> <dbl>
     1 Zhukovskij V.A.   244391  21.7
     2 Pushkin A.S.      187847  16.7
     3 Lermontov M.Ju.   127548  11.3
     4 Fet A.A.           75923   6.7
     5 Vjazemskij P.A.    66264   5.9
     6 Jazykov N.M.       58910   5.2
     7 Benediktov V.G.    49967   4.4
     8 Polezhaev A. I.    44272   3.9
     9 Baratynskij E.A.   38958   3.5
    10 Batjushkov K.N.    33389   3  
    # i 11 more rows

### Kjetsaa’s norm corpus

``` r
kjetsaa_norm <- read.delim("../data/kjetsaa_norm_corpus.csv", sep = ";") %>% 
  select(-X)

# number of lines
kjetsaa_norm %>% 
  separate_rows(text_final, sep = "\n") %>% 
  filter(text_final != "") %>% 
  nrow()
```

    [1] 10033

``` r
nrow(kjetsaa_norm) # number of texts
```

    [1] 298

Number of words & lemmas

``` r
kjetsaa_lemma <- read_csv("../data/kjetsaa_norm_lemma.csv")
```

    Rows: 44602 Columns: 4
    -- Column specification --------------------------------------------------------
    Delimiter: ","
    chr (3): author_name, word, lemma
    dbl (1): id

    i Use `spec()` to retrieve the full column specification for this data.
    i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# number of words
nrow(kjetsaa_lemma) 
```

    [1] 44602

``` r
# number of lemmas
kjetsaa_lemma %>% 
  count(lemma) %>% nrow()
```

    [1] 6267
