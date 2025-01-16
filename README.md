
<!-- README.md is generated from README.Rmd. Please edit that file -->

# perhevapaavertailu

`perhevapaavertailu` on `R`-kielen `shiny`-frameworkilla ohjelmoitu
verkkosovellus, jonka avulla käyttäjä voi laskea perhe-etuuksien määriä
sekä omien tulojen että vapaiden jakamisen perusteella 1.6.2022 voimaan
tulevan uuden lainsäädännön puitteissa.

Sovellus internetissa: <https://perhevapaavertailu.kela.fi/>

## Kehittäminen

Voit kloonata lähdekoodin Githubista:
<https://github.com/kelaresearchandanalytics/perhevapaavertailu> ja
käynnistää sovelluksen paikallisesti `golem::run_dev()`.

Vaihtoehtoisesti voi asentaa sovelluksen R-pakettina

``` r
remotes::install_github("kelaresearchandanalytics/perhevapaavertailu")
```

ja käynnistää laskurin paikallisesti

``` r
perhevapaavertailu::run_app()
```

## Lähdeviite

``` r
citation("perhevapaavertailu")
#> To cite package 'perhevapaavertailu' in publications use:
#> 
#>   Keski-Säntti E, Eskelinen P, Isolankila T, Kainu M (2025).
#>   _perhevapaavertailu: A shiny application for calculating how sharing
#>   family leave between parents can affect your family income_. R
#>   package version 1.0.1.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {perhevapaavertailu: A shiny application for calculating how sharing family leave between parents can affect your family income},
#>     author = {Eeva-Maria Keski-Säntti and Petri Eskelinen and Tapio Isolankila and Markus Kainu},
#>     year = {2025},
#>     note = {R package version 1.0.1},
#>   }
```

## Lisenssi

Ks. [LICENSE.md](LICENSE.md)
