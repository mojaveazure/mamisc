#' @include zzz.R
#'
NULL

#' Text Color
#'
#' Determine text color based on background color
#'
#' @param background A vector of background colors; supports R color names and
#' hexadecimal codes
#' @param threshold Intensity threshold for light/dark cutoff; intensities
#' greater than \code{theshold} yield \code{dark}, others yield \code{light}
#' @param w3c Use \href{http://www.w3.org/TR/WCAG20/}{W3C} formula for calculating
#' background text color; ignores \code{threshold}
#' @param dark Color for dark text
#' @param light Color for light text
#'
#' @return A named vector of either \code{dark} or \code{light}, depending on
#' \code{background}; names of vector are \code{background}
#'
#' @export
#'
#' @family color_theory
#'
#' @source \url{https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color}
#'
#' @examples
#' text_color(background = c('black', 'white', '#E76BF3'))
#'
text_color <- function(
  background,
  threshold = 186,
  w3c = FALSE,
  dark = 'black',
  light = 'white'
) {
  if (isTRUE(x = w3c)) {
    contrast <- luminance
    threshold <- sqrt(x = 1.05 * 0.05) - 0.05
  } else {
    contrast <- intensity
  }
  return(ifelse(
    test = contrast(color = background) > threshold,
    yes = dark,
    no = light
  ))
}

#' Blend two or more colors together
#'
#' @param ... Two or more colors to blend together; these can be in a vector
#' or standalone
#' @param as.rgb Return in RGB form, otherwise return in hexadecimal form
#'
#' @return The blended color in RGB form (1 x 3 matrix) or hexadecimal form
#'
#' @importFrom grDevices rgb col2rgb
#'
#' @export
#'
#' @family color_theory
#'
#' @examples
#' blend_colors('red', 'green')
#' if (requireNamespace('scales', quietly = TRUE)) {
#'   scales::show_col(c('red', blend_colors('red', 'green'), 'green'), ncol = 3)
#' }
#'
blend_colors <- function(..., as.rgb = FALSE) {
  # Assemble the arguments passed into a character vector
  colors <- as.character(x = c(...))
  if (length(x = colors) < 2) {
    stop("Please provide two or more colors to blend")
  }
  # Check for hexadecimal values for automatic alpha blending
  alpha.value <- 255
  if (sum(sapply(X = colors, FUN = grepl, pattern = '^#')) != 0) {
    hex <- colors[which(x = grepl(pattern = '^#', x = colors))]
    hex.length <- sapply(X = hex, FUN = nchar)
    # 9-character hexadecimal values specify alpha levels
    if (9 %in% hex.length) {
      hex.alpha <- hex[which(x = hex.length == 9)]
      hex.vals <- sapply(X = hex.alpha, FUN = substr, start = 8, stop = 9)
      dec.vals <- sapply(X = hex.vals, FUN = strtoi, base = 16)
      dec.vals <- dec.vals / 255 # Convert to 0:1 scale for calculations
      alpha.value <- dec.vals[1]
      # Blend alpha levels, going top-down
      for (val in dec.vals[-1]) {
        alpha.value <- alpha.value + (val * (1 - alpha.value))
      }
      alpha.value <- alpha.value * 255 # Convert back to 0:255 scale
    }
  }
  # Convert to a 3 by `length(colors)` matrix of RGB values
  rgb.vals <- sapply(X = colors, FUN = col2rgb)
  if (nrow(x = rgb.vals) != 3) {
    rgb.vals <- t(x = rgb.vals)
  }
  # Blend together using the additive method
  # Basically, resulting colors are the mean of the component colors
  blend <- as.matrix(x = rowMeans(x = rgb.vals))
  dimnames(x = blend) <- list(c('red', 'green', 'blue'), 'blend')
  # If we're returning RGB values, convert to matrix, just like col2rgb
  # Otherwise, return as hexadecimal; can be used directly for plotting
  if (!as.rgb) {
    blend <- rgb(t(x = blend), alpha = alpha.value, maxColorValue = 255)
  }
  return(blend)
}

#' Convert R colors to hexadecimal
#'
#' @param ... R colors
#'
#' @return The hexadecimal representations of input colors
#
#' @importFrom grDevices rgb col2rgb
#'
#' @export
#'
#' @family color_theory
#'
#' @examples
#' col2hex('black', 'red', 'grey')
#'
col2hex <- function(...) {
  colors <- as.character(x = c(...))
  alpha <- rep.int(x = 255, times = length(x = colors))
  if (sum(sapply(X = colors, FUN = grepl, pattern = '^#'))) {
    hex <- colors[which(x = grepl(pattern = '^#', x = colors))]
    hex.length <- sapply(X = hex, FUN = nchar)
    if (9 %in% hex.length) {
      hex.alpha <- hex[which(x = hex.length == 9)]
      hex.vals <- sapply(X = hex.alpha, FUN = substr, start = 8, stop = 9)
      dec.vals <- sapply(X = hex.vals, FUN = strtoi, base = 16)
      alpha[match(x = hex[which(x = hex.length == 9)], table = colors)] <- dec.vals
    }
  }
  colors <- t(x = col2rgb(col = colors))
  colors <- mapply(
    FUN = function(i, alpha) {
      return(rgb(colors[i, , drop = FALSE], alpha = alpha, maxColorValue = 255))
    },
    i = seq_len(length.out = nrow(x = colors)),
    alpha = alpha
  )
  return(colors)
}

#' Color Intensity
#'
#' Get the intensity and/or luminance of a color
#'
#' @param color A vector of colors
#'
#' @return A vector of intensities/luminances for each color
#'
#' @name contrast-theory
#' @rdname contrast-theory
#'
#' @importFrom grDevices col2rgb
#'
#' @export
#'
#' @family color_theory
#'
#' @source \url{https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color}
#'
#' @examples
#' intensity(color = c('black', 'white', '#E76BF3'))
#'
intensity <- function(color) {
  intensities <- apply(
    X = col2rgb(col = color),
    MARGIN = 2,
    FUN = function(col) {
      return(sum(as.vector(x = col) * c(0.299, 0.587, 0.114)))
    }
  )
  names(x = intensities) <- color
  return(intensities)
}

#' @name contrast-theory
#' @rdname contrast-theory
#'
#' @export
#'
#' @examples
#' luminance(color = c('black', 'white', '#E76BF3'))
#'
luminance <- function(color) {
  lums <- apply(
    X = col2rgb(col = color),
    MARGIN = 2,
    FUN = function(col) {
      col <- as.vector(x = col) / 255
      for (i in seq_along(along.with = col)) {
        col[i] <- ifelse(
          test = col[i] <= 0.03928,
          yes = col[i] / 12.92,
          no = ((col[i] + 0.055) / 1.055) ^ 2.4
        )
      }
      return(sum(col * c(0.2126, 0.7152, 0.0722)))
    }
  )
  names(x = lums) <- color
  return(lums)
}
