% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grow.R
\name{flametree_grow}
\alias{flametree_grow}
\title{Generate the data specifying a flametree}
\usage{
flametree_grow(
  seed = 286,
  time = 6,
  scale = c(0.8, 0.9),
  angle = c(-10, 10, 20),
  split = 2,
  prune = 0
)
}
\arguments{
\item{seed}{Integer-valued seed for the random number generator}

\item{time}{Number of generations to run the iterative process}

\item{scale}{Vector of possible values for the "size rescaling" at each iteration}

\item{angle}{Vector of possible angle changes (in degrees) at each iteration}

\item{split}{Maximum number of shoots to generate from each tip at each iteration}

\item{prune}{Probability with which a generated shoot is pruned}
}
\value{
A tibble with the following columns: coord_x, coord_y, seg_deg,
seg_len, seg_col, seg_wid, id_time, id_path, id_step, id_leaf

The two "coord" columns
specify the locations of a point. The "id" columns uniquely identify each
point: id_time specifies the generation, id_path identifies each segment, and
id_step contains the three values (0, 1 or 2) for the points that define each
segment. The segments consist of two end points (0 and 2) and one "control"
point (1) that is used to define a Bezier curve.

The three "seg" columns provide summary information about each segment:
seg_len is the length of the segment, seg_col is a value used to colour
the segment, and seg_wid is a size parameter used to define the width of
the segment
}
\description{
Generate the data specifying a flametree
}
\examples{
flametree_grow()
flametree_grow(time = 5)

}