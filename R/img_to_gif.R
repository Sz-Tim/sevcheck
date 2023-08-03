#' Create a gif from a set of images
#'
#' @param files Vector of file paths to images, in order
#' @param steps_per_s Number of images per second
#' @param out_name File name of gif output
#' @param remove_img If \code{TRUE}, remove all img files; default \code{FALSE}
#'
#' @return Nothing
#' @export
img_to_gif <- function(files, steps_per_s, out_name, remove_img=F) {
  library(magick)
  cat(format(Sys.time(), "%F %T"), "-", out_name, "\n")
  files |>
    image_read() |>
    image_join() |>
    image_animate(delay=1/steps_per_s, optimize=T) |>
    image_write(out_name)
  if(remove_img) {
    file.remove(files)
  }
}
