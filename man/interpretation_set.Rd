% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpretation_set.R
\name{interpretation_set}
\alias{interpretation_set}
\title{Interpretation Sets}
\usage{
interpretation_set(boundary_names, placeholders = NULL, interpretations)
}
\arguments{
\item{boundary_names}{Character vector of boundary names. The length of this vector (i.e.,
the number of boundary names listed) determines the number of boundaries
for use with this interpretation_set, which also determines the number of
interpretations that must be provided (see Details).}

\item{placeholders}{Vector of named character elements, where each item contains a string
that is used within the interpretations as a placeholder, enabling a
specific value to be substituted.
Can be null.}

\item{interpretations}{An ordered list of interpretations, one for each valid combination of
confidence limits. See Details for information on the number, format, and
expected ordering of \code{interpretations} in a given
\code{interpretation_set}.}
}
\value{
An object of class \code{interpretation_set} (a list object).
}
\description{
A class to define a set of interpretations for confidence intervals,
depending on where the lower and upper confidence limits sit. A helper
function for producing objects of this class.
}
\details{
The set of boundaries specified in an \code{interpretation_set} can be
thought of as establishing a number of regions within which the lower and
upper confidence limits can sit. There is 1 more region than the number of
boundaries, since the set of regions is effectively 'less than boundary 1',
'between boundary 1 : n-1 and boundary 2 : n' and 'above boundary n'.

The valid combinations are those where the upper confidence limit is in a
region greater than or equal to the region of the lower confidence limit.
This establishes \code{sum(1 : n)} valid combinations, where n is the number
of regions (i.e., the number of boundaries + 1). An interpretation needs to
be provided for each of these combinations.

Interpretations are provided in order. The order is based on first specifying
all the cases where the lower confidence limit is in the bottom region, and
each of the regions for the upper confidence limit (again, starting from the
bottom and increasing to the top); next come all of the cases where the
lower confidence limit is in the second-from-bottom region (in this case the
valid regions for the upper confidence limit will start at the
second-from-bottom and go up to the top region); and so on. So for a 2
region (1 boundary) situation, the interpretations should be provided in the
following order:
   \tabular{lll}{
   Order   \tab Lower confidence level   \tab Upper confidence level \cr
   1 \tab Region 1 \tab Region 1 \cr
   2 \tab Region 1 \tab Region 2 \cr
   3 \tab Region 2 \tab Region 2 \cr
   }
For a 3 region (2 boundary) situation, the interpretations should be provided
in this order:
   \tabular{lll}{
   Order   \tab Lower confidence level   \tab Upper confidence level \cr
   1 \tab Region 1 \tab Region 1 \cr
   2 \tab Region 1 \tab Region 2 \cr
   3 \tab Region 1 \tab Region 3 \cr
   4 \tab Region 2 \tab Region 2 \cr
   5 \tab Region 2 \tab Region 3 \cr
   6 \tab Region 3 \tab Region 3 \cr
   }

Each interpretation should itself be a list object containing named items:
\code{interpretation_short}, \code{interpretation}, and
\code{interpretation_md}. These contain, respectively, a short interpretation
(one or two words), a version of the interpretation that would typically be
a little longer (a short phrase), and the \code{interpretation} phrase with
any markdown that is required. The markdown is typically used to emphasise
the key words from the interpretation.

See the vignette on extending \code{confinterpret} for a worked example of
how these list items are supplied.

Values for \code{placeholders} can be specified to enable sections of text
in interpretations to be replaced automatically. This can be used, for
example, to allow names or descriptions of interventions to be passed to
\code{\link{confinterpret}}, so that these can be returned in the final
interpretation rather than a generic description. \code{confinterpret}
uses \code{gsub} with \code{fixed=TRUE} to do substitutions for
\code{placeholders} entries, so values should be selected that will match
accordingly (and will not match extra items). Use of a non-alphanumeric
character within a placeholder can help to reduce accidental matches.

A \code{plot} method is provided for \code{interpretation_set} objects. See
\code{\link{plot.interpretation_set}} for details.

A \code{print} method is provided for \code{interpretation_set} objects.
}
