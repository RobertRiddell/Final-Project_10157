gg_scatter <-  function(data, feature) {
  ggplot(data, aes(x = .data[[feature]], y = pts)) +
    geom_point(colour = '#048BA8', alpha = 0.5) +
    geom_smooth(method = 'lm', colour = "#16DB93", se= F) +
    ggtitle(deparse(substitute(data)) , feature) +
    theme_bw()
}
