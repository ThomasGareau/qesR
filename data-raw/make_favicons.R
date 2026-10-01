#!/usr/bin/env Rscript

# Build the logo PNG and the website's favicons from man/figures/logo.svg
# (design.md sections 9.1 and 10, slice W).
#
# Usage (from the package root): Rscript data-raw/make_favicons.R
#
# man/figures/logo.svg is the source: a hexagon in outlined paths (no font
# needed), navy #0B3D91, gold #F2B632 and white. This script writes
#   man/figures/logo.png  480 pixels wide, for the README (GitHub, CRAN);
#   pkgdown/favicon/      which pkgdown copies to the site root.
# Everything is drawn locally with the magick and rsvg packages
# (development tools, not dependencies of qesR): no image is sent to an
# online favicon service, which is what pkgdown::build_favicons() would do.
# The website itself shows logo.svg (pkgdown copies it to the site root).
#
# The logo is a hexagon taller than wide, so the favicons centre it on a
# transparent square. Two sets of names are written, because pkgdown
# versions link different files from each page's <head>:
#   pkgdown <= 2.1.1  favicon-16x16.png, favicon-32x32.png and
#                     apple-touch-icon{,-60x60,-76x76,-120x120}.png;
#   later versions    favicon-96x96.png, favicon.svg, favicon.ico,
#                     apple-touch-icon.png and site.webmanifest.
# The icons of 48 pixels and less (favicon-16x16.png, favicon-32x32.png,
# favicon.ico and favicon.svg, the browser-tab icons) are drawn from a
# reduced logo: the hexagon with the q and its ballot X scaled up to fill
# it, and no wordmark, which is under 3 pixels tall at 32 pixels and noise
# at 16. The icons of 60 pixels and more are the full logo.
# favicon.svg is the reduced logo on a square canvas, so it stays vector.
# Paths in site.webmanifest are relative, because the site is served from
# /qesR/. Its theme colour is the logo navy, the site's primary colour.
#
# Rerun it whenever the logo changes, and commit man/figures/logo.png and
# pkgdown/favicon/ with it.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1]] else "."

svg_path <- file.path(root, "man", "figures", "logo.svg")
out <- file.path(root, "pkgdown", "favicon")
stopifnot(file.exists(svg_path), requireNamespace("rsvg", quietly = TRUE))
dir.create(out, showWarnings = FALSE, recursive = TRUE)

svg <- paste(readLines(svg_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
view <- as.numeric(strsplit(sub('.*viewBox="([^"]+)".*', "\\1", svg), " ")[[1]])
w <- view[[3]]
h <- view[[4]]
side <- max(w, h)

# A drawing on a square canvas, centred: the source of the icons.
on_square <- function(svg) {
  sub(
    "<svg[^>]*>",
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="%g %g %g %g" width="%g" height="%g">',
      view[[1]] - (side - w) / 2, view[[2]] - (side - h) / 2, side, side, side, side
    ),
    svg
  )
}

# The reduced logo of the small icons: without the comments and the
# wordmark (the only white path), and with the q and its X, centred on
# (248, 247), scaled by 1.45 about the centre of the hexagon (259, 300).
# That is the largest scale, in steps of 0.05, that keeps the stem's corners
# inside the hexagon's slanted sides.
hexagon <- '<path d="[^"]*" fill="#0B3D91"/>'
small_svg <- gsub("\\s*<!--.*?-->", "", svg, perl = TRUE)
small_svg <- gsub('\\s*<path fill="#FFFFFF" d="[^"]*"/>', "", small_svg)
stopifnot(grepl(hexagon, small_svg), !grepl('fill="#FFFFFF" d=', small_svg, fixed = TRUE))
small_svg <- sub(
  paste0("(", hexagon, ")"),
  '\\1\n  <g transform="translate(259 300) scale(1.45) translate(-248 -247)">',
  small_svg
)
small_svg <- sub("\\s*</svg>\\s*$", "\n  </g>\n</svg>", small_svg)

square_file <- tempfile(fileext = ".svg")
writeLines(on_square(svg), square_file, useBytes = TRUE)
small_file <- tempfile(fileext = ".svg")
small_square_svg <- on_square(small_svg)
writeLines(small_square_svg, small_file, useBytes = TRUE)

# Rendered at 4x and reduced, which keeps the small sizes sharp.
render <- function(file, width) {
  img <- magick::image_read_svg(file, width = 4 * width)
  magick::image_resize(img, sprintf("%dx", width), filter = "Lanczos")
}
# PNGs are reduced to a palette (255 colours lose nothing visible on a logo
# of three flat colours; the file is a fifth of the size) and written with
# the strongest zlib compression. Quantizing gives the transparent corners
# an alpha of 1/255, so the pixels transparent in the render are set back
# to fully transparent. Quantizing in sRGB keeps the colours exact (the
# default, linear RGB, darkens them).
write_png <- function(img, path) {
  clear <- magick::image_data(img, "rgba")[4, , ] == as.raw(0)
  img <- magick::image_quantize(img, max = 255, colorspace = "sRGB", dither = FALSE)
  px <- magick::image_data(img, "rgba")
  for (k in 1:4) {
    channel <- px[k, , ]
    channel[clear] <- as.raw(0)
    px[k, , ] <- channel
  }
  magick::image_write(magick::image_read(px), path, format = "png", quality = 95)
}

write_png(render(svg_path, 480), file.path(root, "man", "figures", "logo.png"))

icon <- function(size) render(if (size <= 48) small_file else square_file, size)
sizes <- c(
  "favicon-16x16.png" = 16, "favicon-32x32.png" = 32, "favicon-96x96.png" = 96,
  "apple-touch-icon.png" = 180, "apple-touch-icon-60x60.png" = 60,
  "apple-touch-icon-76x76.png" = 76, "apple-touch-icon-120x120.png" = 120,
  "web-app-manifest-192x192.png" = 192, "web-app-manifest-512x512.png" = 512
)
for (name in names(sizes)) {
  write_png(icon(sizes[[name]]), file.path(out, name))
}

# The .ico holds 16, 32 and 48 pixel images, of the reduced logo.
ico <- c(icon(16), icon(32), icon(48))
magick::image_write(ico, file.path(out, "favicon.ico"), format = "ico")

# favicon.svg: the reduced logo on its square canvas (comments removed above).
writeLines(small_square_svg, file.path(out, "favicon.svg"), useBytes = TRUE)
unlink(c(square_file, small_file))

manifest <- list(
  name = "qesR",
  short_name = "qesR",
  icons = list(
    list(src = "web-app-manifest-192x192.png", sizes = "192x192", type = "image/png"),
    list(src = "web-app-manifest-512x512.png", sizes = "512x512", type = "image/png")
  ),
  theme_color = "#0B3D91",
  background_color = "#ffffff",
  display = "standalone"
)
writeLines(
  jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
  file.path(out, "site.webmanifest"), useBytes = TRUE
)

files <- c(file.path(root, "man", "figures", c("logo.svg", "logo.png")), file.path(out, list.files(out)))
cat(sprintf("%-48s %7d bytes\n", files, file.size(files)), sep = "")
