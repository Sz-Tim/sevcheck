#' Create a gif from a set of images
#'
#' @param files Vector of file paths to images, in order
#' @param fps Frames per second
#' @param out_name File name of gif output
#' @param optimize Argument to magick::image_animate; If \code{TRUE}, difference 
#' between images is stored
#' @param remove_img If \code{TRUE}, remove all img files; default \code{FALSE}
#'
#' @return Nothing
#' @export
img_to_gif <- function(files, fps, out_name, optimize=T, remove_img=F) {
  library(magick)
  cat(format(Sys.time(), "%F %T"), "-", out_name, "\n")
  files |>
    image_read() |>
    image_join() |>
    image_animate(fps=fps, optimize=optimize) |>
    image_write(out_name)
  if(remove_img) {
    file.remove(files)
  }
}
