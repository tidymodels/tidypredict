# 32-bit float split thresholds -------------------
#
# Several model packages store split thresholds and feature values as 32-bit
# floats and compare them as floats, while R holds everything as doubles.
# Translating the comparison literally sends any row whose value rounds to the
# threshold down the wrong branch.
#
# The fix is to move the threshold to the midpoint between it and the adjacent
# float, so that a comparison in doubles partitions the values exactly the way
# the model's float comparison does. Which neighbour to use depends on which
# side of the comparison the threshold's own value belongs to:
#
# - `side = "lower"`, for `value < threshold`, as in xgboost. `mtcars$wt` of
#   3.19 is below the reported threshold of 3.19000006 as a double but equal to
#   it as a float, so xgboost sends it right where a naive translation sends it
#   left. Every value that rounds to the threshold must land above the boundary.
# - `side = "upper"`, for `value <= threshold`, as in Cubist. A `disp` of 95.1
#   is above the stored threshold of 95.099998 as a double but equal to it as a
#   float, so Cubist takes the `<=` branch where a naive translation does not.
#   Every value that rounds to the threshold must land below the boundary.

as_f32 <- function(x) {
  readBin(writeBin(x, raw(), size = 4), "double", size = 4, n = length(x))
}

f32_bits <- function(x) {
  readBin(writeBin(x, raw(), size = 4), "integer", size = 4, n = length(x))
}

f32_from_bits <- function(bits) {
  as_f32(readBin(
    writeBin(bits, raw(), size = 4),
    "double",
    size = 4,
    n = length(bits)
  ))
}

# One double away from x, in the direction of `dir`. Adjacent floats are about
# 2^29 doubles apart, so landing a bit or two further than the very next double
# makes no difference to which floats fall either side of the result.
next_double <- function(x, dir) {
  x + dir * pmax(abs(x), .Machine$double.xmin) * .Machine$double.eps
}

f32_split_boundary <- function(x, side = c("lower", "upper")) {
  side <- match.arg(side)

  out <- as.numeric(x)
  keep <- !is.na(out)
  if (!any(keep)) {
    return(out)
  }

  f32 <- as_f32(out[keep])
  bits <- f32_bits(f32)

  # Floats are stored as sign and magnitude, so stepping away from zero means
  # incrementing the bit pattern and stepping toward zero means decrementing it.
  # `step` is the increment for a positive value; negatives take the opposite.
  step <- if (side == "lower") -1L else 1L
  neighbour <- bits + step
  negative <- f32 < 0
  neighbour[negative] <- bits[negative] - step

  # Zero has no signed neighbour to decrement, so name the adjacent subnormal
  # directly: 0x80000001 below zero, 0x00000001 above it.
  neighbour[f32 == 0] <- if (side == "lower") -2147483647L else 1L

  mid <- (f32_from_bits(neighbour) + f32) / 2

  # The midpoint of two floats is exactly representable as a double, so a value
  # can sit precisely on it, and there the boundary cannot express what the
  # model does: rounding the midpoint to a float is a tie, resolved to whichever
  # of the two neighbours has an even mantissa. When that is the neighbour rather
  # than the threshold, the model reads the midpoint as being on the far side of
  # the split, so the boundary has to move one double past it to agree. About
  # half of all thresholds land this way.
  ties_to_neighbour <- as_f32(mid) != f32
  mid[ties_to_neighbour] <- next_double(
    mid[ties_to_neighbour],
    if (side == "lower") 1 else -1
  )

  out[keep] <- mid
  out
}
