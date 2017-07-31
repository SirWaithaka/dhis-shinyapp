
graphPlot <- function(type = "bar", index, data, dx, bins) {
  
  if (type == "bar") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x=period, y=dx, fill=period)) +
      ggplot2::geom_col(stat = "identity") +
      ggplot2::ylab(dx) +
      ggplot2::ggtitle(paste(dx, "| ", data$name[index])) +
      ggplot2::theme_light()
    
    return(p)

  }
  else if (type == "histo") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x=period, fill=..count..)) +
      ggplot2::geom_histogram(breaks=bins, ggplot2::aes(fill=..count..)) +
      ggplot2::ggtitle(paste(dx, " | ", data$name[index])) +
      ggplot2::theme_light()
    
    return(p)
  }
  else if (type == "line") {
    p <- ggplot2::ggplot(data, ggplot2::aes(x=period, y=dx)) +
      ggplot2::geom_line(color="blue", size=1, group = 1) +
      ggplot2::ylab(dx) +
      ggplot2::ggtitle(paste(dx, " | ", data$name[index])) +
      ggplot2::theme_light()
    
    return(p)
  }
  else if (type == "splot") {
    p <- ggplot(data, ggplot2::aes(x=period, y=dx, color=period)) +
      ggplot2::geom_point(size=4) +
      ggplot2::ylab(dx) +
      ggplot2::ggtitle(paste(dx, " | ", data$name[index])) +
      ggplot2::theme_light()
    
    return(p)
  }

}