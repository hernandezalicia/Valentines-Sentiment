# What is the sentiment surrounding Valentine's Day on Twitter?

This project collects tweets from Twitter containing the phrase "Valentine's Day", cleans them, and performs various sentiment and emotion analyses on the data.

To use the Shiny application associated with this project, visit my [shinyapps.io](https://hernandezalicia.shinyapps.io/ValentinesApp/) page.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine.


### Prerequisites

* R
* Install the following R packages:
    - `rtweet`, `dplyr`, `lubridate`, `tm`, `text2vec`, `qdap`, `textclean`, `qdapDictionaries`, `tidytext`, `radarchart`, `wordcloud`, `ggplot2`, `directlabels`, `data.table`, `textstem`

```
install.packages("rtweet")
```

### Helpful hints

* The current state of the */ValentinesApp/global.R* file takes a sample of 200,000 rows of the total data set. Remove this line to perform analysis on the entire data set.

## Author

* **Alicia Hernandez** - [LinkedIn](https://www.linkedin.com/in/ahernandez93/) | [github.com/hernandezalicia](https://github.com/hernandezaliica)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
