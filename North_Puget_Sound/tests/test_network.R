# Run from the repository root: Rscript tests/test_network.R
source(file.path("R", "network.R"))
library(sf)

make_lines <- function(...) {
  st_sf(geometry = st_sfc(lapply(list(...), st_linestring), crs = 26910))
}
points <- function(xy) {
  xy <- matrix(xy, ncol = 2, byrow = TRUE)
  st_sf(id = seq_len(nrow(xy)), geometry = st_sfc(
    lapply(seq_len(nrow(xy)), function(i) st_point(xy[i, ])), crs = 26910
  ))
}
trace <- function(lines, barriers, roots = points(c(0, 0)), ...) {
  truncate_network_at_barriers(
    lines, barriers, root_geometry = roots, root_distance = 0.01,
    snap_distance = 0.5, endpoint_tolerance = 0.001,
    ambiguity_distance = 0.001, ...
  )
}
expect_near <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-8)))
}

# Interior cut retains the exact downstream partial reach, independent of
# vertex order; a second barrier upstream cannot change the downstream total.
chain <- make_lines(rbind(c(0, 0), c(0, 10), c(0, 20), c(0, 30)))
one <- trace(chain, points(c(0, 12)))
expect_near(one$summary$retained_m, 12)
expect_near(one$summary$removed_m, 18)
multiple <- trace(chain, points(c(0, 12, 0, 25, 0, 12)))
expect_near(multiple$summary$retained_m, 12)
expect_near(multiple$summary$removed_m, 18)
stopifnot(all(multiple$barrier_audit$applied))
reversed <- make_lines(rbind(c(0, 30), c(0, 20), c(0, 10), c(0, 0)))
expect_near(trace(reversed, points(c(0, 12)))$summary$retained_m, 12)

# Tributaries below the barrier remain accessible; those above it are removed.
branches <- make_lines(
  rbind(c(0, 0), c(0, 30)),
  rbind(c(0, 10), c(10, 10)),
  rbind(c(0, 20), c(10, 20))
)
branched <- trace(branches, points(c(0, 15)))
expect_near(branched$summary$retained_m, 25)
expect_near(branched$summary$removed_m, 25)
stopifnot(branched$summary$barriers_applied == 1)

# A barrier at a confluence needs a branch assignment and must be audited.
junction <- trace(branches, points(c(0, 10)))
stopifnot(junction$summary$barriers_ambiguous == 1,
          !junction$barrier_audit$applied,
          all(junction$lines$review_required))
expect_near(junction$summary$retained_m, 50)

# A split/rejoining channel supplies another route around one blocked branch.
loop <- make_lines(
  rbind(c(0, 0), c(0, 10)),
  rbind(c(0, 10), c(-5, 15), c(0, 20)),
  rbind(c(0, 10), c(5, 15), c(0, 20)),
  rbind(c(0, 20), c(0, 30))
)
alternate <- trace(loop, points(c(-5, 15)))
expect_near(alternate$summary$removed_m, 0)
expect_near(alternate$summary$retained_m, 20 + 20 * sqrt(2))

# Disconnected predictions have an unresolved length, not retained habitat.
disconnected <- make_lines(
  rbind(c(0, 0), c(0, 30)), rbind(c(100, 0), c(100, 10))
)
unresolved <- trace(disconnected, points(c(0, 12)))
expect_near(unresolved$summary$retained_m, 12)
expect_near(unresolved$summary$unresolved_m, 10)
stopifnot(all(!unresolved$lines$retained[unresolved$lines$unresolved]))

# Unmatched points and tied parallel reaches cannot silently cut the network.
unmatched <- trace(chain, points(c(20, 12)))
stopifnot(unmatched$summary$barriers_unmatched == 1,
          !unmatched$barrier_audit$applied)
expect_near(unmatched$barrier_audit$snap_distance_m, 20)
parallel <- make_lines(
  rbind(c(0, 0), c(0, 30)), rbind(c(0.2, 0), c(0.2, 30))
)
tied <- trace(parallel, points(c(0.1, 12)))
stopifnot(tied$summary$barriers_ambiguous == 1,
          !tied$barrier_audit$applied)

# Unary union removes duplicate prediction categories and multipart overlaps.
duplicated <- make_lines(rbind(c(0, 0), c(0, 30)),
                         rbind(c(0, 0), c(0, 30)))
expect_near(trace(duplicated, points(c(0, 12)))$summary$retained_m, 12)

# Real projected coordinates and an interpolated cut preserve a diagonal
# reach's entire length. Re-intersecting such cuts with their original lines
# is unsafe because floating-point coordinates may no longer be collinear.
diagonal <- make_lines(rbind(c(500000.123, 5400000.789),
                            c(501234.567, 5400567.891),
                            c(502000.098, 5401000.325)))
diagonal_trace <- truncate_network_at_barriers(
  diagonal, points(c(500500, 5400200)),
  root_geometry = points(c(500000.123, 5400000.789)),
  root_distance = 0.01, snap_distance = 100
)
expect_near(sum(diagonal_trace$lines$length_m),
            sum(as.numeric(st_length(diagonal))))
stopifnot(diagonal_trace$summary$barriers_applied == 1,
          diagonal_trace$summary$retained_m > 0,
          diagonal_trace$summary$removed_m > 0)

# A cut at a degree-two node separates the two incident reaches.
node_chain <- make_lines(rbind(c(0, 0), c(0, 10)),
                         rbind(c(0, 10), c(0, 30)))
node_cut <- trace(node_chain, points(c(0, 10)))
expect_near(node_cut$summary$retained_m, 10)
stopifnot(node_cut$barrier_audit$applied)

# Missing outlets fail explicitly instead of selecting an arbitrary endpoint.
failure <- try(trace(chain, points(c(0, 12)), roots = points(c(100, 100))),
               silent = TRUE)
stopifnot(inherits(failure, "try-error"))
cat("All network truncation checks passed.\n")
