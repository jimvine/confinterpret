## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6.5, fig.height=6.5
)

library("confinterpret")


## ----include=FALSE-------------------------------------------------------

#' Arrange an interpretation_set object into a table, for printing etc.
tabulate_interpretation_set <- function(interpretation_set, name) {
  int_md <- as.data.frame(sapply(interpretation_set$interpretations,
                                 "[[", "interpretation_md"))
  rownames(int_md) <- LETTERS[1 : nrow(int_md)]
  colnames(int_md) <- name
  int_md
}

table_nums <- captioner::captioner(prefix = "Table")
figure_nums <- captioner::captioner(prefix = "Figure")

## ----echo=FALSE, results="asis"------------------------------------------

number_boundaries <- 1 : 5
number_regions <- number_boundaries + 1L
number_interpretations <- as.integer(number_regions * (number_regions + 1L) / 2)

numbers <- data.frame(number_boundaries,
                      number_regions,
                      number_interpretations,
                      row.names = NULL)
colnames(numbers) <- c("Number of boundaries, $n_b$",
                       "Number of regions, $n_r$",
                       "Number of interpretations")

knitr::kable(numbers,
             caption = table_nums(name = "numbers_b_r_i", 
                                  caption = "Number of regions and interpretations for each number of boundaries."))


## ----echo=FALSE, results="asis"------------------------------------------

region_orders <- function(number_regions) {
  
  number_interpretations <- number_regions * (number_regions + 1) / 2
  lower_regions <- rep(1 : number_regions, number_regions : 1)
  upper_regions <- unlist(mapply(function(x) { x : number_regions },
                                 1 : number_regions))
  regions <- data.frame(1 : number_interpretations,
                        paste("Region", lower_regions),
                        paste("Region", upper_regions))
  colnames(regions) <- c("Order", 
                         "Lower confidence level", 
                         "Upper confidence level")
  rownames(regions) <- LETTERS[1 : number_interpretations]
  return(regions)
}

boundary_2 <- region_orders(2)

knitr::kable(boundary_2, 
             caption = table_nums(name = "order_2", 
                                  caption = "Order of interpretations with two regions."))


## ----echo=FALSE, results="asis"------------------------------------------
boundary_3 <- region_orders(3)

knitr::kable(boundary_3,
             caption = table_nums(name = "order_3", 
                                  caption = "Order of interpretations with three regions."))

## ----echo=FALSE----------------------------------------------------------
dummy_i_s_label <- figure_nums("dummy-interpretation-set-plot", "Plot of a 'dummy' `interpretation_set` object, providing visual support for use while drafting interpretations.")

## ----dummy-interpretation-set-plot, fig.cap=dummy_i_s_label--------------
practical_superiority <- interpretation_set(
  boundary_names =c("Actual null", "Practical null"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
  interpretations = list(
    # A
    list(interpretation_short = "A",
         interpretation       = "A",
         interpretation_md    = "A"),
    # B
    list(interpretation_short = "B",
         interpretation       = "B",
         interpretation_md    = "B"),
    # C
    list(interpretation_short = "C",
         interpretation       = "C",
         interpretation_md    = "C"),
    # D
    list(interpretation_short = "D",
         interpretation       = "D",
         interpretation_md    = "D"),
    # E
    list(interpretation_short = "E",
         interpretation       = "E",
         interpretation_md    = "E"),
    # F
    list(interpretation_short = "F",
         interpretation       = "F",
         interpretation_md    = "F")))

# Set a nice colour scheme
grDevices::palette(RColorBrewer::brewer.pal(3,"RdYlBu"))

plot(practical_superiority)

## ----echo=FALSE----------------------------------------------------------
new_i_s_label <- figure_nums("new-interpretation-set-plot", "Plot of the newly-defined `interpretation_set` object, showing short versions of the drafted interpretations.")

## ----new-interpretation-set-plot, fig.cap=new_i_s_label------------------
practical_superiority <- interpretation_set(
  boundary_names =c("Actual null", "Practical null"),
  placeholders = c(comparison_intervention = "$comp",
                   tested_intervention = "$test"),
    interpretations = list(
      # A
      list(interpretation_short = "Inferior",
           interpretation       = "$test inferior to $comp",
           interpretation_md    = "$test **inferior** to $comp"),
      # B
      list(interpretation_short = "Not practically superior",
           interpretation       = "$test not practically superior to $comp",
           interpretation_md    = "$test **not practically superior** to $comp"),
      # C
      list(interpretation_short = "Inconclusive",
           interpretation       = paste("Inconclusive: $test not shown to",
                                        "be inferior or superior to $comp"),
           interpretation_md    = paste("**Inconclusive**: $test not shown to",
                                        "be inferior or superior to $comp")),
      # D
      list(interpretation_short = "Not practically superior",
           interpretation       = "$test not practically superior to $comp",
           interpretation_md    = "$test **not practically superior** to $comp"),
      # E
      list(interpretation_short = "Inconclusive",
           interpretation       = paste("Inconclusive: $test not inferior",
                                        "to $comp, but not shown to be", 
                                        "practically superior"),
           interpretation_md    = paste("**Inconclusive**: $test not inferior",
                                        "to $comp, but not shown to be", 
                                        "practically superior")),
      # F
      list(interpretation_short = "Superior",
           interpretation       = paste("$test superior to $comp, to a",
                                        "practically relevant extent"),
           interpretation_md    = paste("$test **superior** to $comp, to a",
                                        "practically relevant extent"))))

plot(practical_superiority)

## ----echo=FALSE, results="asis"------------------------------------------

knitr::kable(tabulate_interpretation_set(practical_superiority,
                                         "Practical superiority interpretations."),
             caption = "Practical superiority interpretations.")

## ------------------------------------------------------------------------
estimate_gateway <- c("prevalence difference" = 0.1032195)
ci_gateway <- matrix(c(0.04777727, 0.2064016),
                     nrow = 1,
                     dimnames = list("estimate", c("2.5 %", "97.5 %")))

estimate_specialist <- c("prevalence difference" = 0.1270894)
ci_specialist <- matrix(c(0.0296644, 0.1767746),
                        nrow = 1,
                        dimnames = list("estimate", c("2.5 %", "97.5 %")))


## ----echo=FALSE----------------------------------------------------------
re_int_label <- figure_nums("re-interpret-plot", "Plot showing a study result, re-interpreted using the newly-defined `interpretation_set` object.")

## ----re-interpret-plot, fig.height=5.5, fig.cap=re_int_label-------------
specialist_prac_sup <- confinterpret(ci_specialist,
                                     practical_superiority,
                                     boundaries = c(0, 0.05),
                                     comparison_labels = 
                                       c(comparison_intervention = 
                                           "Existing standard letter", 
                                         tested_intervention = "New letter"))

plot(specialist_prac_sup)

