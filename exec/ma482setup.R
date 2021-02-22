# Description: personal function for students in the course so that all
#              necessary functionality is incorporated.


## ---- Clean Slate ----
# Remove anything previously existing and start with clean slate
rm(list = ls(sorted=FALSE)[ls(sorted=FALSE) != "params"])
gc()

## ---- Load Packages ----
import::from(boot, boot)
import::from(car, Boot, linearHypothesis, .carEnv)
import::from(splines, ns)
import::from(rms, lsp, pol, rcs)
import::from(skimr, skim)
library(survival)
library(geepack)
library(lme4)
library(tidyverse)
library(broom)
library(broom.mixed)
library(reyes482)


## ---- Change Options ----
# Suppress status bar in dplyr.
# Suppress messages from summarise about groups.
# Change handling of ordered factors
options(dplyr.show_progress = FALSE,
        dplyr.summarise.inform = FALSE,
        contrasts = rep("contr.treatment", 2))


# Change theme for plots
theme_set(theme_bw(14))
theme_update(legend.position = "bottom",
             legend.box = "vertical")


# Specify chunk options
knitr::opts_chunk$set(
  prompt = FALSE,
  comment = "",
  out.width = ifelse(knitr::is_latex_output(), "0.8\\textwidth", "80%"),
  fig.align = "center",
  linewidth = 80)


## ---- Ensure Source Code Wraps ----
.hook_source = knitr::knit_hooks$get("source")

knitr::knit_hooks$set(
  source = function(x, options){
    # this hook is used only when linewidth option is not NULL
    if (!is.null(n <- options$linewidth)){
      x = reyes482:::split_lines(x)

      x = ifelse(nchar(x) > n, stringr::str_wrap(x, width = n, exdent = 2), x)
    }

    .hook_source(x, options)
  })


## ---- Create Special Blocks ----
eng_instructor <- function(options) {
  if (identical(options$echo, FALSE)) return()

  code = paste0(paste(options$code, collapse = '\n'), "\n"); type = options$type
  if (is.null(type)) return(code)

  if(!is.null(type) && type=="solution"){
    code = paste("__SOLUTION__:  ", code, sep="\n")
  }

  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) stop('The engine "instructor" is for R Markdown only')

  l1 = options$latex.options
  if (is.null(l1)) l1 = ''
  # protect environment options because Pandoc may escape the characters like
  # {}; when encoded in integers, they won't be escaped, but will need to
  # restore them later; see bookdown:::restore_block2
  if (l1 != '') l1 = paste(
    c('\\iffalse{', utf8ToInt(enc2utf8(l1)), '}\\fi{}'), collapse = '-'
  )
  h2 = ifelse(is.null(options$html.tag), 'div', options$html.tag)
  h3 = ifelse(is.null(options$html.before), '', options$html.before)
  h4 = ifelse(is.null(options$html.after), '', options$html.after)
  h5 = ifelse(is.null(options$html.before2), '', options$html.before2)
  h6 = ifelse(is.null(options$html.after2), '', options$html.after2)

  if (knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex"){
    sprintf("\\BeginKnitrBlock{%s}%s\n%s\n\\EndKnitrBlock{%s}",
            type, l1, code, type)
  } else {
    sprintf(
      '\\BeginKnitrBlock{%s}%s%s<%s class="%s" custom-style="%s">%s%s%s</%s>%s\\EndKnitrBlock{%s}',
      type, l1, h3, h2, type, type, h5, code, h6, h2, h4, type
    )
  }
}


knitr::knit_engines$set(c(knitr::knit_engines$get(),
                          "instructor" = eng_instructor))

