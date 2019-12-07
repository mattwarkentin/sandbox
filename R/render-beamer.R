#' Render R Markdown to Beamer Presentation
#'
#' @description Render R Markdown files destined to be Beamer PDF presentations with a double-rendering approach using a \code{LaTeX} engine of your choosing (or \code{pdflatex}, by default), to produce proper slide numbering.
#'
#' @param input R Markdown File. See details.
#' @param cleanup Logical. Should the intermediate \code{TeX} files be removed? Default is FALSE.
#' @param ... Additional named arguments passed to \code{\link[rmarkdown]{render}}.
#'
#' @details This function is a crude solution to the issue whereby R Markdown (\code{.Rmd}) documents being rendered to Beamer PDF presentations fail to render the total number of slides properly. This problem did not exist in previous verions, but currently exists.
#'
#' Basically, this function renders the R Markdown to Markdown to \code{TeX} with a call to \code{\link[rmarkdown]{render}}. Then, the function uses a \code{LaTeX} engined specified by \code{options(latex.engine = "your/path/here")} or tries \code{pdflatex}, by default. The \code{TeX} file is rendered twice and then opened for viewing.
#'
#' When specifying the \code{input} for this function, the path included in the input is used for writing any new files (including the final PDF). If only a file name is provided with no path, it is assumed that the current working directory should be used for writing.
#'
#' @return Invisibly, the rendered PDF path and file name.
#'
#' @export

render_beamer <- function(input = NULL, cleanup = FALSE, ...){
  assertthat::assert_that(fs::file_exists(input))

  bare_input <- fs::path_ext_remove(input)
  out_dir <- fs::path_dir(input)

  rmarkdown::render(input, ...)

  pre_dir <- rbind(tibble::enframe(fs::dir_ls(out_dir)),
                   glue::glue('{fs::as_fs_path(bare_input)}.pdf'),
                   glue::glue('{fs::as_fs_path(bare_input)}.tex'))

  if(!fs::file_exists(glue::glue('{bare_input}.tex'))){
    stop('You must keep the TeX file after knitting your Rmd.')
  }

  tex_engine <- getOption('latex.engine', default = Sys.which('pdflatex'))

  system(glue::glue('{tex_engine} --output-directory={out_dir} {bare_input}.tex'))
  system(glue::glue('{tex_engine} --output-directory={out_dir} {bare_input}.tex'))

  if(cleanup){
    post_dir <- tibble::enframe(fs::dir_ls(out_dir))
    to_remove <- dplyr::anti_join(post_dir, pre_dir, by = 'value')
    fs::file_delete(to_remove$value)
  }

  if(Sys.info()[1] %in% 'Windows'){
    system(glue::glue('start {bare_input}.pdf'))
  } else {
    system(glue::glue('open {bare_input}.pdf'))
  }

  invisible(glue::glue('{bare_input}.pdf'))
}
