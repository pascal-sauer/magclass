#' add_dimension
#'
#' Function adds a name dimension as dimension number "dim" with the name "add"
#' with an empty data column with the name "nm".
#'
#'
#' @param x MAgPIE object which should be extended.
#' @param dim The dimension number of the new dimension (e.g. 3.1)
#' @param add The name of the new dimension
#' @param nm The name of the first entry in dimension "add".
#' @return The extended MAgPIE object
#' @author Jan Philipp Dietrich, Benjamin Bodirsky
#' @seealso \code{\link{add_columns}},\code{\link{mbind}}
#' @examples
#'
#' a <- maxample("animal")
#' str(add_dimension(a, dim = 3.2))
#' str(add_dimension(a, dim = 2.3, nm = paste0("d", 1:3)))
#' @export
add_dimension <- function(x, dim = 3.1, add = NULL, nm = "dummy") { # nolint: object_name_linter.
  x <- clean_magpie(x, what = "sets")
  if (is.null(add)) {
    # create non-existing variant of dimension name starting with "new"
    sets <- getSets(x, fulldim = TRUE)
    add <- tail(make.unique(c(sets, "new"), sep = ""), 1)
  } else if (add %in% getSets(x, fulldim = TRUE)) {
    stop("Dimension \"", add, "\" does already exist. Please use a different name!")
  }
  maindim <- floor(dim)
  subdim  <- as.integer(sub("^.\\.", "", as.character(dim)))
  if (length(nm) > 1) {
    expand <- rep(seq_len(dim(x)[maindim]), length(nm))
    x <- x[expand, dim = maindim]
  }
  items <- getItems(x, dim = maindim, split = TRUE, full = TRUE)
  before <- seq_len(subdim - 1)
  after <- setdiff(seq_along(items), before)
  items <- c(items[before],
             list(rep(nm, each = dim(x)[maindim] / length(nm))),
             items[after])
  names(items)[subdim] <- add
  items <- Filter(Negate(is.null), items)
  getItems(x, dim = maindim, raw = TRUE) <- do.call(function(...) paste(..., sep = "."), items)
  getSets(x, fulldim = FALSE)[maindim] <- paste(names(items), collapse = ".")
  return(x)
}
