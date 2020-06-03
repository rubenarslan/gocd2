#' # Helper functions used throughout {.tabset .tabset-sticky}
#' documentation on the functions is interspersed through code comments
#'
#' ## set some options
#' dont show messages when loading libraries
library = function(...) suppressMessages(base::library(...))
#' never set strings as factors automatically (google for reason why)
options(stringsAsFactors = FALSE)
#' show four significant digits tops
options(digits = 4)
#' tend not to show scientific notation, because we're just psychologists
options(scipen = 7)
#' make output a bit wider
options(width = 110)
#' set a seed to make analyses depending on random number generation reproducible
set.seed(1710) # if you use your significant other's birthday make sure you stay together for the sake of reproducibility


#' ## Load packages
#' generate the site
library(rmarkdown)
#' set options for chunks
library(knitr)
#' my formr utility package to generate e.g. the bibliography
library(formr)
#' pretty-printed output
library(pander)
#' tidyverse date times
library(lubridate)
#' tidyverse strings
library(stringr)
#' extractor functions for models
library(broom)
#' grammar of graphics plots
library(ggplot2)
#' svg graphs
# library(svglite);
library(ggthemes)
library(codebook)

#' tidyverse: has a lot of naming conflicts, so always load last
library(tidyverse)

#' some packages may be needed without being loaded
fool_packrat = function() {
  # needed to install formr package
  library(devtools)
  library(rmarkdown)
  # needed to actually run rmarkdown in RStudio, but for some reason not in its dependencies
  library(formatR)
}

#' ## Output options
#' use pander to pretty-print objects (if possible)
opts_chunk$set(
  dev = "png"
)

#' don't split tables, scroll horizontally
panderOptions("table.split.table", Inf)

#' Set plot defaults
theme_set(theme_tufte(base_size = 20, base_family='Helvetica Neue'))


#' ## Spin R files
#' R scripts can be documented in markdown using Roxygen comments, as demonstrated here
#' This function turns all R files (that don't have an Rmd file of the same name and that don't start with an underscore _) into HTML pages
spin_R_files_to_site_html = function() {
  library(knitr)
  all_Rs = c(list.files(pattern = "^[^_].+\\.R$"), ".Rprofile")
  component_Rmds = list.files(pattern = "^_.+\\.Rmd$")
  temporary_Rmds = c()
  for (i in seq_along(all_Rs)) {
    if(all_Rs[i] == ".Rprofile") {
      Rmd_file = ".Rprofile.Rmd"
    } else {
      Rmd_file = paste0(all_Rs[i], "md")
    }
    if (!file.exists(Rmd_file)) {
      next_document = length(temporary_Rmds) + 1
      if(file.exists(all_Rs[i])) {
      temporary_Rmds[next_document] = spin(all_Rs[i], knit = FALSE, envir = new.env(), format = "Rmd")
      prepended_yaml = paste0(c("---
output:
  html_document:
    code_folding: 'show'
---

", readLines(temporary_Rmds[next_document])), collapse = "\n")
      cat(prepended_yaml, file = temporary_Rmds[next_document])
      }
    }
  }
  components_and_scripts = c(temporary_Rmds, component_Rmds)
  for (i in seq_along(components_and_scripts)) {
    opts_chunk$set(eval = FALSE, cache = FALSE)
    # if we call render_site on the .R file directly it adds a header I don't like
    rmarkdown::render_site(components_and_scripts[i], quiet = TRUE)
  }
  opts_chunk$set(eval = TRUE, cache = TRUE)
  unlink(temporary_Rmds)
}


#' ##### we use this function to automatically get nice tables
pander_handler = function(x, ...) {
  anyS3method = function(x) {
    classes = class(x)
    any(sapply(classes, FUN = function(classes) { !is.null(getS3method('pander',classes,TRUE)) } ))
  }
  if ("knit_asis" %in% class(x)) {
    x # obj is knit_asis already, don't touch it
    # (useful if e.g. pander is called with options in the doc)
  } else if (anyS3method(x)) {
    pander(x, row.names = F, ...) # if method available, pander
  } else if (isS4(x)) {
    show(x)
  } else {
    print(x)
  }
}





##### counting excluded participants

n_excluded = function(x) {
  excluded_new = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old)) {
    excluded = excluded_new
  } else {
    excluded = excluded_new - excluded_old
  }
  cat(excluded, "excluded\n")
  excluded_old <<- excluded_new
  excluded
}

### counting excluded days

n_excluded_days = function(x) {
  excluded_new_days = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_days)) {
    excluded = excluded_new_days
  } else {
    excluded = excluded_new_days - excluded_old_days
  }
  cat(excluded, "excluded\n")
  excluded_old_days <<- excluded_new_days
  excluded
}

### counting excluded days in social diary

n_excluded_social = function(x) {
  excluded_new_social = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_social)) {
    excluded = excluded_new_social
  } else {
    excluded = excluded_new_social - excluded_old_social
  }
  cat(excluded, "excluded\n")
  excluded_old_social <<- excluded_new_social
  excluded
}

### counting excluded participants in social network

n_excluded_network = function(x) {
  excluded_new_network = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_network)) {
    excluded = excluded_new_network
  } else {
    excluded = excluded_new_network - excluded_old_network
  }
  cat(excluded, "excluded\n")
  excluded_old_network <<- excluded_new_network
  excluded
}

###### strict exclusion
n_excluded_network_strict = function(x) {
  excluded_new_network_strict = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_network_strict)) {
    excluded = excluded_new_network_strict
  } else {
    excluded = excluded_new_network_strict - excluded_old_network_strict
  }
  cat(excluded, "excluded\n")
  excluded_old_network_strict <<- excluded_new_network_strict
  excluded
}

###### bar plot

bar_count = function(data, variable, na.rm = FALSE) {
  varname = deparse(substitute(variable))
  var = data %>% select_(varname) %>% .[[1]]
  if (na.rm == T) {
    var = var %>% na.omit()
  }
  var = factor(var, exclude = NULL)
  data$var = var

  ggplot(data, aes(x = var)) +
    geom_bar() +
    stat_count(aes(label = paste(..count.., "\n", scales::percent(round(..count../sum(count),2)))), hjust = -0.1, geom = "text", position = "identity", na.rm = T) +
    scale_y_continuous(expand = c(0.1, 0)) +
    xlab(varname) +
    coord_flip()
}



multi_rel = function(diary, lme = T, lmer = T) {
  mrel = diary %>%
    group_by(short) %>%
    filter(!is.na(risk_taking)) %>%
    filter(day_number <= 70) %>%
    gather(variable, value, -short, -day_number) %>%
    psych::multilevel.reliability(., "short", "day_number", lme = lme, lmer = lmer, items = "variable", values = "value", long = T, aov = F)
  mrel
}


robust_rowmeans <- function(x) {
  y <- rowMeans(x, na.rm = TRUE)
  y[is.nan(y)] <- NA_real_
  y
}


cortest_stretch <- function(d) {
  var_pairs <- t(combn(names(d), 2)) %>%
    as_data_frame() %>%
    setNames(c("x", "y"))

  p_values <- var_pairs %>%
    dplyr::mutate(r.test = purrr::map2(x, y, ~ stats::cor.test(d[[.x]], d[[.y]])),
                  r.test = purrr::map(r.test, broom::tidy)) %>%
    tidyr::unnest(r.test)
  p_values
}


cut_common_stem <- function(x) {
  i = 1
  while (i <= max(stringr::str_length(x)) & dplyr::n_distinct(stringr::str_sub(x, 1, i)) == 1) {
    i = i + 1
  }
  stringr::str_sub(x, i)
}



