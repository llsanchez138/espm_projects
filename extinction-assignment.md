Extinctions Unit
================
Nazerke Sharauova, Latosha Sanchez

## Introduction

In this module, we are taking a look at if there is another imminent
mass extinction at hand. This will be done by estimating the current
rate of extinctions across species with the past extincion rate in
fossil records. We will be working with non-rectangular data, which are
unstructured textual data that must be sorted and shaped into usable
datasets for the analysis and mapping of data.

The loss of biodiversity is a critical problem faced across ecosystems.
The inclusion of diverse species across ecosystems heighten their
ecosystems, ensuring a natural sustainability for other life. When this
balance of diversity falls, a whole cycle may be disrupted, causing
critical environmental problems and threatening valuable ecosystem
resources, inhibiting the natural process. There are growing concerns
for the loss of species and evidence that extinction rates are on the
rise.

``` r
#install.packages("httr")
library(httr)
token = "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
```

The following is to test whether GET works, by requesting the version of
the data being retrieved.

``` r
resp <- httr::GET("https://apiv3.iucnredlist.org/api/v3/version")
#resp <- httr::GET("https://apiv3.iucnredlist.org/api/v3/species/page/0?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee")

version <- httr::content(resp)
version
```

    $version
    [1] "2019-2"

In this chunk of code, we form the list of URLs that will be requested
from the IUCN Red List.

``` r
species_ap <- "https://apiv3.iucnredlist.org/api/v3/species"
page <- "/page/"
page_number <- 0:10
query <- "?token="
token <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"

all_pages <- paste0 (species_ap, page, page_number, query, token)
all_pages[1:3]
```

    [1] "https://apiv3.iucnredlist.org/api/v3/species/page/0?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
    [2] "https://apiv3.iucnredlist.org/api/v3/species/page/1?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
    [3] "https://apiv3.iucnredlist.org/api/v3/species/page/2?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"

We then retrieve all the pages by mapping to the GET request, which were
requested from the URL lists above.

``` r
all_results <- map(all_pages, GET)
```

We then pull the information from the first GET request, then
identifying a single record. Another way to retrieve a single record is
further given
    (1)

``` r
all_results[[1]]
```

    Response [https://apiv3.iucnredlist.org/api/v3/species/page/0?token=9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee]
      Date: 2019-11-20 04:12
      Status: 200
      Content-Type: application/json; charset=utf-8
      Size: 2.86 MB

``` r
page0 <- content(all_results[[1]])
page0$result[[1]]
```

    $taxonid
    [1] 3
    
    $kingdom_name
    [1] "ANIMALIA"
    
    $phylum_name
    [1] "MOLLUSCA"
    
    $class_name
    [1] "GASTROPODA"
    
    $order_name
    [1] "STYLOMMATOPHORA"
    
    $family_name
    [1] "ENDODONTIDAE"
    
    $genus_name
    [1] "Aaadonta"
    
    $scientific_name
    [1] "Aaadonta angaurana"
    
    $infra_rank
    NULL
    
    $infra_name
    NULL
    
    $population
    NULL
    
    $category
    [1] "CR"

``` r
# Optional way to retrieve a record
rec <- all_results [[1]] %>% 
  content() %>% 
  getElement("result") %>% 
  getElement(1)
rec
```

    $taxonid
    [1] 3
    
    $kingdom_name
    [1] "ANIMALIA"
    
    $phylum_name
    [1] "MOLLUSCA"
    
    $class_name
    [1] "GASTROPODA"
    
    $order_name
    [1] "STYLOMMATOPHORA"
    
    $family_name
    [1] "ENDODONTIDAE"
    
    $genus_name
    [1] "Aaadonta"
    
    $scientific_name
    [1] "Aaadonta angaurana"
    
    $infra_rank
    NULL
    
    $infra_name
    NULL
    
    $population
    NULL
    
    $category
    [1] "CR"

The following code turns the gathered data into a rectangular dataframe
which will simplify the use of the information when mapping. Following
the transformation, we display the number of rows present and the first
five of the dataframe.

``` r
id_null <- rec$taxonid[[1]]
id_null[map_lgl(id_null, is.null)] <- NA

row_to_tibble <- function(row) {
  tibble(taxonid = row$taxonid,
         scientific_name = row$scientific_name, 
         category = row$category, 
         taxonomy_class = row$class,
         phylum = row$phylum_name,
         family = row$family_name
         )
  }

get_result <- function(x){
  x %>% content() %>% 
    getElement("result") %>% 
    map_dfr(row_to_tibble)
  }

all_species_df <- all_results %>% map_dfr(get_result) 

nrow(all_species_df)
```

    [1] 108161

``` r
head(all_species_df)
```

    # A tibble: 6 x 6
      taxonid scientific_name      category taxonomy_class phylum   family     
        <int> <chr>                <chr>    <chr>          <chr>    <chr>      
    1       3 Aaadonta angaurana   CR       GASTROPODA     MOLLUSCA ENDODONTID…
    2       4 Aaadonta constricta  EN       GASTROPODA     MOLLUSCA ENDODONTID…
    3       5 Aaadonta fuscozonata EN       GASTROPODA     MOLLUSCA ENDODONTID…
    4       6 Aaadonta irregularis CR       GASTROPODA     MOLLUSCA ENDODONTID…
    5       7 Aaadonta kinlochi    CR       GASTROPODA     MOLLUSCA ENDODONTID…
    6       8 Aaadonta pelewana    CR       GASTROPODA     MOLLUSCA ENDODONTID…

We then chose to narrow down the pool of species to only those that were
targeted in the study by Ceballos et al.

``` r
phylum_dfs <- split(all_species_df, all_species_df$phylum)
chordata_df <- phylum_dfs$CHORDATA
```

The following is used to separate species by their category of
extinction. For our report, we want to exclude those that are assumed to
be living, and identify the species that are conservatively and/or
highly conservatively extinct. We were able to distinguish the
categories by their codes which are conveyed as follows: EX - Highly
Conservative EW - Conservative PE - Conservative

Any categories that differed from the above were removed. After
filtering, we were able to identify the number of species categorized as
highly conservative, conservative, and those alive, respectively.

``` r
hi_conservative_ex <- chordata_df %>% 
  filter(category == "EX")

conservative_ex <- chordata_df %>% 
  filter(category %in% c("EX","EW","PE")) 

alive <- chordata_df %>%
  filter(!(category %in% c("EX","EW","PE")))

nrow(hi_conservative_ex)
```

    [1] 381

``` r
nrow(conservative_ex)
```

    [1] 403

``` r
nrow(alive)
```

    [1] 50016

Below we retrieve the narratives for each of the selected
categories.

``` r
# Given highly conservative and conservative extinct species, retrieve narratives for each

hi_cons_names <- c(hi_conservative_ex$scientific_name)
cons_names <- c(conservative_ex$scientific_name)

retrieve_narrative <- function(names){
  base <- "https://apiv3.iucnredlist.org"
  narrative <- "/api/v3/species/narrative/"
  urls <- paste0(base, narrative, names, query, token)
  resps <- map(urls, GET)
}

hi_con_resps <- retrieve_narrative(hi_cons_names)
con_resps <- retrieve_narrative(cons_names)
```

The following gives an example of the content in a given row. We will
utilized the content of the data to extract corresponding dates to the
categories of extinction.

``` r
rec <- content(hi_con_resps[[1]])
rec
```

    $name
    [1] "Mirogrex hulensis"
    
    $result
    $result[[1]]
    $result[[1]]$species_id
    [1] 73
    
    $result[[1]]$taxonomicnotes
    [1] "(M. Goren and N. Bogutskaya pers. comms.) indicate that this species should be be transferred to the genus <em>Mirogrex</em>. (Was originally published as a subspecies of <em>Mirogrex terraesanctae</em> (Steinitz. 1952)."
    
    $result[[1]]$rationale
    [1] "The Hula lake and adjacent marshes were drained in the 1950s. Drainage of the lake resulted in the species being restricted to marsh and pond areas in Hula nature reserve. The reserve management switched from using spring water to fishpond water in the species' habitat, resulting in decline and the extinction of the fish within six years of doing so. The species was last recorded in 1975."
    
    $result[[1]]$geographicrange
    [1] "This species was restricted to the Hula basin (Lake Huleh) in northern Israel."
    
    $result[[1]]$population
    [1] "This species is now extinct; it was last recorded in 1975."
    
    $result[[1]]$populationtrend
    NULL
    
    $result[[1]]$habitat
    [1] "<em>Mirogrex hulensis</em> lived in Lake Hula and its adjacent marshes. It reached 230 mm total length, spawned from February to April."
    
    $result[[1]]$threats
    [1] "The Hula swamp was artificially drained by 1957 in order to obtain good agricultural land. Such agricultural development led to massive reduction in the number and size of aquatic ecosystems, due to the diversion and exploitation of their sources. Drainage of the lake resulted in the species being restricted to marsh and pond areas in Hula nature reserve.&#160;The reserve management switched from using spring water to fishpond water in the species' habitat, resulting in decline and the extinction of the fish within six years of doing so."
    
    $result[[1]]$conservationmeasures
    NULL
    
    $result[[1]]$usetrade
    NULL

Below we extract the years from the content of the conservative and
highly conservative datasets.

``` r
hi_con_ex_content <- map(hi_con_resps, content)
con_ex_content <- map(con_resps, content)

helper <- function(con){
  result <- con$result[[1]]
  result[map_lgl(result, is.null)] <- NA
  as.data.frame(result)
}

get_max_value <- function(x) {
  if(identical(x, character(0))){
    return(NA)
  }
  max_value <- as.integer(x) %>% max()
  return(max_value)
}

to_narrative_df <- function(resp_content){
  df <- map_dfr(resp_content, helper) %>% 
   select(species_id, rationale) %>%
   mutate(years = stringr::str_extract_all(rationale, "\\d\\d\\d\\d")) %>%
   mutate(extinction_year = map_int(years, get_max_value)) %>%
   drop_na(extinction_year)
  df
}

hi_con_df <- to_narrative_df(hi_con_ex_content)
```

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

``` r
con_df <- to_narrative_df(con_ex_content)
```

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

    Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector
    
    Warning in bind_rows_(x, .id): binding character and factor vector,
    coercing into character vector

We further analyzed the gathered data by joining the extinction years.
From the new dataset, we counted by grouping, evaluating the counts to
quantify the cumulative extinctions and join totals by computing
percentages. The data is then plotted to visualize the entirety of the
data. The background rate has further been included in the comparison of
the rates between those from 200 years ago and present.

``` r
map_chr_safe <- possibly(map_chr, otherwise = NA)

get_period <- function(x) {
  if(is.na(x)) {return(-1)}
  if(x >= 1500 & x <= 1600) {return(1)}
  if(x >= 1600 & x <= 1700) {return(2)}
  if(x >= 1700 & x <= 1800) {return(3)}
  if(x >= 1800 & x <= 1900) {return(4)}
  if(x >= 1900 & x <= 2014) {return(5)}
  return(-1)
}

get_period_range <- function(x) {
  if(is.na(x)) return("Unavailable")
  if(x == 0) return("Before 1500")
  if(x == 1) return("1500-1600")
  if(x == 2) return("1600-1700")
  if(x == 3) return("1700-1800")
  if(x == 4) return("1800-1900")
  if(x == 5) return("1900-2014")
}

analyze_extinctions <- function(narrative_df, chordata_df){
  
  ceballos_classes <- c("MAMMALIA","AVES", "AMPHIBIA", "REPTILIA", "ACTINOPTERYGII", "CEPHALASPIDOMORPHI")

  # Join species data with extinction years from narrative, assign period, and count by grouping
  extinctions_df <- chordata_df %>% 
    left_join(narrative_df, by=c("taxonid" = "species_id")) %>%
    mutate(extinction_period = map_dbl(extinction_year,get_period)) %>%
    group_by(extinction_period, taxonomy_class) %>%
    summarise(num_extinct = n()) %>%
    filter(extinction_period > 0)
  
  # For missing periods per taxonomy_class, introduce zeroes
  for(taxon_class in ceballos_classes){
    for(period in 1:5){
      find_row <- extinctions_df %>%
        filter(taxonomy_class == taxon_class) %>%
        filter(extinction_period == period)
      if(nrow(find_row) == 0){
        extinctions_df[nrow(extinctions_df) + 1, ] = list(period, taxon_class, 0)
      }
    }
  }
  
  # Compute total evaluated counts
  totals_df <- chordata_df %>% 
    left_join(narrative_df, by=c("taxonid" = "species_id")) %>%
    filter(taxonomy_class %in% ceballos_classes) %>%
    group_by(taxonomy_class) %>%
    summarise(total_num = n())

  # Compute cumulative extinctions and join totals
  extinctions_df <- extinctions_df %>%
    arrange(extinction_period) %>%
    group_by(taxonomy_class) %>%
    mutate(cum_num_extinct = cumsum(num_extinct)) %>%
    left_join(totals_df, by=c("taxonomy_class" = "taxonomy_class"))
  
  # Create two new "classes", to match the work by Ceballos et al
  vertebrates_ceballos = c("MAMMALIA", "AVES", "AMPHIBIA")
  other_vertebrates_ceballos = c("REPTILIA", "AMPHIBIA", "ACTINOPTERYGII", "CEPHALASPIDOMORPHI")
  
  for(period in 1:5){
    num_ex <- 0
    cum_num_ex <- 0
    tot <- 0
    for(taxon_class in vertebrates_ceballos){
      tmp <- extinctions_df %>%
        filter(taxonomy_class == taxon_class) %>%
        filter(extinction_period == period)
      num_ex <- num_ex + tmp$num_extinct[[1]]
      cum_num_ex <- cum_num_ex + tmp$cum_num_extinct[[1]]
      tot <- tot + tmp$total_num[[1]]
    }
    extinctions_df[nrow(extinctions_df) + 1, ] = list(period, "VERTEBRATES", num_ex, cum_num_ex, tot)
  }
  
  for(period in 1:5){
    num_ex <- 0
    cum_num_ex <- 0
    tot <- 0
    for(taxon_class in other_vertebrates_ceballos){
      tmp <- extinctions_df %>%
        filter(taxonomy_class == taxon_class) %>%
        filter(extinction_period == period)
      num_ex <- num_ex + tmp$num_extinct[[1]]
      cum_num_ex <- cum_num_ex + tmp$cum_num_extinct[[1]]
      tot <- tot + tmp$total_num[[1]]
    }
    extinctions_df[nrow(extinctions_df) + 1, ] = list(period, "OTHER_VERTEBRATES", num_ex, cum_num_ex, tot)
  }

  # Compute cumulative percentages
  extinctions_df <- extinctions_df %>%
    arrange(extinction_period) %>%
    mutate(cum_percentage = cum_num_extinct/total_num) %>%
    filter(taxonomy_class %in% c("MAMMALIA", "AVES", "VERTEBRATES", "OTHER_VERTEBRATES")) %>%
    mutate(extinction_range = map_chr_safe(extinction_period, get_period_range))

  totals_for_background <- extinctions_df %>%
    filter(extinction_period == 1)
  all_totals <- sum(totals_for_background$total_num)

  # Insert background rate as data
  bg_df <- as_tibble(extinctions_df[nrow(extinctions_df), ])
  cumul <- 0
  for(period in 1:5){
    cumul <- cumul + all_totals/10000*2
    bg_df[nrow(bg_df) + 1, ] = list(period, "BACKGROUND", all_totals/10000*2, cumul, all_totals,cumul/all_totals,get_period_range(period))
  }
  bg_df <- bg_df[-c(1),]
  extinctions_df <- bind_rows(extinctions_df, bg_df)

  # Plot the results
  extinctions_df$cum_percentage = extinctions_df$cum_percentage*100.
  
  extinctions_df %>% 
    ggplot(aes(
       x = extinction_range, 
       y = cum_percentage, 
       color = taxonomy_class, 
       group=as.factor(taxonomy_class))) + 
    geom_line(size = 1) + 
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1") + 
    theme_bw() + 
    labs(
      title = "Cumulative Vertabrate Species Extinction Rate \n(EX, EW, PE) Over Time", 
         y = "Cumulative Speciece Extintion %", 
         x = "Time Interval", 
         caption = "Conservative estimate of the percentage of the number of species evaluated among mammals.") +
      theme(
    plot.title = element_text(face = 'bold'),
    axis.title = element_text(size = 11, face = 'bold'),
    axis.text = element_text(size = 9),
    plot.caption = element_text(face = 'italic')) +
    scale_color_discrete(name="Taxonomy Class", 
                         breaks=c("AVES",
                                  "BACKGROUND", 
                                  "MAMMILIA", 
                                  "OTHER_VERTEBRATES", 
                                  "VERTEBRATES"),
                         labels=c("Birds", 
                                  "Background", 
                                  "Mammals", 
                                  "Other Vertebrates", 
                                  "Vertebrates"))

}


analyze_extinctions(hi_con_df, chordata_df)
```

    Scale for 'colour' is already present. Adding another scale for
    'colour', which will replace the existing scale.

![](extinction-assignment_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
analyze_extinctions(con_df, chordata_df)
```

    Scale for 'colour' is already present. Adding another scale for
    'colour', which will replace the existing scale.

![](extinction-assignment_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

The graph above, exhibits the extinction rates across vertebrates. It
depicts the image that extinction rates have dramatically increased over
the past 200 years. To estimate the extinction rates, we gathered
available data, and the extinction or possible extinction, on the
desired vertebrate species from the 2014 IUCN Red List.

## Conclusion

Through our analysis, we confirm that the data is comparable to the
study conducted by the given research by Ceballos. Thus we can suggest
that the extinction rate has continued, and continues to rise at an
alarming rate.

## Citations

Ceballos, G., Ehrlich, P. R., Barnosky, A. D., García, A., Pringle, R.
M., & Palmer, T. M. (2015, June 19). Accelerated modern human–induced
species losses: Entering the sixth mass extinction. Science Advances, 1,
no. 5, e140023. Retrieved from Environmental Sciences (DOI:
10.1126/sciadv.1400253).
