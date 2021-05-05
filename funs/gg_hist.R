gg_hist <-  function(data, feature) {
  binwidth = (max(data[[feature]]) - min(data[[feature]]))/8
  ggplot(data, aes(x = .data[[feature]])) +
    geom_histogram(colour = '#048BA8', fill = '#16DB93', alpha = 0.8, binwidth = binwidth) + 
    ggtitle(deparse(substitute(data)) , feature) +
    theme_bw()
}
