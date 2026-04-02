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

    # A tibble: 21 × 2
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
    # ℹ 11 more rows

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
kjetsaa <- read.delim("../data/kjetsaa_corpus/kjetsaa_norm_corpus.csv", sep = ";") %>% 
  select(-X, -text_raw, -note)

# load prepared data: unzip first
# utils::unzip("../data/poetree_ru_19c_lemma.zip")
ru_poetree <- read.csv("../data/large_poetree_ru_19c_lemma.csv") %>% select(-X)

glimpse(kjetsaa)
```

    Rows: 298
    Columns: 4
    $ id         <int> 12083, 12085, 12036, 12105, 12112, NA, 12136, 12154, 12182,…
    $ text_final <chr> "Где наша роза, \n Друзья мои? \n Увяла роза, \n Дитя зари.…
    $ author     <chr> "А. С. Пушкин", "А. С. Пушкин", "А. С. Пушкин", "А. С. Пушк…
    $ header     <chr> "Роза : «Где наша роза...»", "Певец : «Слыхали ль вы за рощ…

Add author names transliteration

``` r
# load prepared poetree data if needed
# ru_poetree <- read.csv("../data/poetree_ru_1810-1839.csv")

authors_translit <- data.frame(
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
    $ corpus            <chr> "ru", "ru", "ru", "ru", "ru", "ru", "ru", "ru", "ru"…
    $ id_poem           <int> 33734, 33734, 33734, 33734, 33734, 33734, 33734, 337…
    $ id                <chr> "br1-105", "br1-105", "br1-105", "br1-105", "br1-105…
    $ title             <chr> "К...", "К...", "К...", "К...", "К...", "К...", "К..…
    $ year_created_from <dbl> 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821…
    $ year_created_to   <dbl> 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821, 1821…
    $ id_author         <int> 220, 220, 220, 220, 220, 220, 220, 220, 220, 220, 22…
    $ author_name       <chr> "Baratynskij E.A.", "Baratynskij E.A.", "Baratynskij…
    $ word              <chr> "приятель", "строгий", "ты", "не", "прав", "несправе…
    $ lemma             <chr> "приятель", "строгий", "ты", "не", "правый", "неспра…
    $ analysis          <chr> "[{'analysis': [{'lex': 'приятель', 'gr': 'S,муж,од=…

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

    # A tibble: 21 × 3
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
    # ℹ 11 more rows

### Kjetsaa’s norm corpus

``` r
kjetsaa_norm <- read.delim("../data/kjetsaa_corpus/kjetsaa_norm_corpus.csv", sep = ";") %>% 
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
kjetsaa_lemma <- read.csv("../data/kjetsaa_corpus/kjetsaa_norm_lemma.csv")

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
