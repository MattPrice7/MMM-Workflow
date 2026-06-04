# MMM Shiny Dashboard

Run from this directory with:

```r
install.packages(c('shiny', 'plotly', 'DT', 'ggplot2', 'data.table'))
shiny::runApp('.')
```

The app reads `mmm_report_tables.rds`, which was created by `write_mmm_deck_shiny_app()`.
