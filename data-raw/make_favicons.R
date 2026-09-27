#!/usr/bin/env Rscript

# Build the website's favicons from man/figures/logo.png (design.md sections
# 9.1 and 10, slice W).
#
# Usage (from the package root): Rscript data-raw/make_favicons.R
#
# Writes pkgdown/favicon/, which pkgdown copies to the site root. Everything
# is drawn locally with the magick package (a development tool, not a
# dependency of qesR): no image is sent to an online favicon service, which
# is what pkgdown::build_favicons() would do.
#
# The logo is a hexagon taller than wide, so it is centred on a transparent
# square before scaling. Two sets of names are written, because pkgdown
# versions link different files from each page's <head>:
#   pkgdown <= 2.1.1  favicon-16x16.png, favicon-32x32.png and
#                     apple-touch-icon{,-60x60,-76x76,-120x120}.png;
#   later versions    favicon-96x96.png, favicon.svg, favicon.ico,
#                     apple-touch-icon.png and site.webmanifest.
# favicon.svg wraps a 96-pixel PNG, so it stays a few kilobytes. Paths in
# site.webmanifest are relative, because the site is served from /qesR/.
#
# Rerun it whenever the logo changes, and commit pkgdown/favicon/ with it.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1]] else "."

logo_path <- file.path(root, "man", "figures", "logo.png")
out <- file.path(root, "pkgdown", "favicon")
stopifnot(file.exists(logo_path))
dir.create(out, showWarnings = FALSE, recursive = TRUE)

logo <- magick::image_read(logo_path)
info <- magick::image_info(logo)
side <- max(info$width, info$height)
square <- magick::image_extent(
  magick::image_background(logo, "none"),
  geometry = sprintf("%dx%d", side, side), gravity = "center", color = "none"
)

icon <- function(size) {
  magick::image_resize(square, sprintf("%dx%d!", size, size), filter = "Lanczos")
}
write_png <- function(img, name) {
  magick::image_write(img, file.path(out, name), format = "png", flatten = FALSE)
}

sizes <- c(
  "favicon-16x16.png" = 16, "favicon-32x32.png" = 32, "favicon-96x96.png" = 96,
  "apple-touch-icon.png" = 180, "apple-touch-icon-60x60.png" = 60,
  "apple-touch-icon-76x76.png" = 76, "apple-touch-icon-120x120.png" = 120,
  "web-app-manifest-192x192.png" = 192, "web-app-manifest-512x512.png" = 512
)
for (name in names(sizes)) {
  write_png(icon(sizes[[name]]), name)
}

# The .ico holds 16, 32 and 48 pixel images.
ico <- c(icon(16), icon(32), icon(48))
magick::image_write(ico, file.path(out, "favicon.ico"), format = "ico")

# favicon.svg: a small PNG inside an SVG wrapper.
png_file <- tempfile(fileext = ".png")
magick::image_write(icon(96), png_file, format = "png")
b64 <- jsonlite::base64_enc(readBin(png_file, "raw", file.size(png_file)))
unlink(png_file)
svg <- paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="96" height="96" viewBox="0 0 96 96">',
  '<image width="96" height="96" href="data:image/png;base64,', b64, '"/></svg>\n'
)
writeLines(svg, file.path(out, "favicon.svg"), sep = "", useBytes = TRUE)

manifest <- list(
  name = "qesR",
  short_name = "qesR",
  icons = list(
    list(src = "web-app-manifest-192x192.png", sizes = "192x192", type = "image/png"),
    list(src = "web-app-manifest-512x512.png", sizes = "512x512", type = "image/png")
  ),
  theme_color = "#0468b9",
  background_color = "#ffffff",
  display = "standalone"
)
writeLines(
  jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
  file.path(out, "site.webmanifest"), useBytes = TRUE
)

files <- list.files(out)
cat(sprintf("%-32s %7d bytes\n", files, file.size(file.path(out, files))), sep = "")
