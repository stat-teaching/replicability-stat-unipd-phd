mkdir_if <- function(dir){
  if(!dir.exists(dir)) dir.create(dir)
}

link_refs <- function(){
  file <- xfun::embed_file("refs_to_download.bib", text = " Download .bib file")
  sprintf('<button class="btn"><i class="fa fa-download"></i>%s</button>', file)
}

#' quiet(function(x))
#' Suppresses output messages
#' By Hadley Wickham
#' http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
#'
#' @export

quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

get_yaml_params <- function(file){
  lines <- suppressWarnings(quiet(readLines(file)))
  yaml_index <- grep("^---$", lines)
  yaml::read_yaml(text = lines[yaml_index[1]:yaml_index[2]])
}

read_all_funs <- function(path = "R"){
  funs <- list.files("R/", pattern = "*.R", include.dirs = FALSE, full.names = TRUE)
  filor::get_funs(funs)
}

ht <- function(x, n = 5){
  if(is.vector(x)) k <- length(x) else k <- nrow(x)
  if(k < n*2){
    show <- x
  }else{
    idx <- c(1:n, (k - n):k)
    keep <- (1:k) %in% idx
    show <- subset(x, subset = keep)
  }
  if(is.vector(x)) names(show) <- idx
  return(show)
}

get_result <- function(..., lines = NULL, comment = NULL){
  res <- capture.output(...)
  if(!is.null(lines)){
    if(is.character(lines)){
      if(length(lines) == 1) lines[2] <- lines[1]
      start <- grep(lines[1], res)
      end <- grep(lines[2], res)
      res <- res[start:end]
    }else{
      res <- res[lines]
    }
  }
  if(!is.null(comment)){
    res <- paste(comment, res)
  }
  invisible(res)
  cat(res, sep = "\n")
}

as_tex_label <- function(x, pattern){
  x <- factor(x)
  labels <- sprintf(pattern, levels(x))
  factor(x, levels = levels(x), labels = latex2exp::TeX(labels))
}

mtheme <- function(){
  theme_minimal(base_size = 15) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
}