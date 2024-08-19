# Perhevapaavertailu (Family leave calculator)

`perhevapaavertailu` is a shiny application for calculating how sharing family leave between parents can affect your family income.

You can use this calculator to give you an idea of how sharing family leave between parents can affect your family income. 

This calculator is intended for working parents whose child was born on or after 4 September 2022. Please note that the results are only indicative.

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

## Citation

To cite package ‘perhevapaavertailu’ in publications use:

```
Keski-Säntti E, Eskelinen P, Isolankila T, Kainu M (2024). _perhevapaavertailu: A shiny application for calculating how
sharing family leave between parents can affect your family income_. R package version 1.0.0.
```

A BibTeX entry for LaTeX users is

```
@Manual{,
  title = {perhevapaavertailu: A shiny application for calculating how sharing family leave between parents can affect your family income},
  author = {Eeva-Maria Keski-Säntti and Petri Eskelinen and Tapio Isolankila and Markus Kainu},
  year = {2024},
  note = {R package version 1.0.0},
}
```

- MIT-lisenssi. Ks. [LICENSE.md](LICENSE.md)
