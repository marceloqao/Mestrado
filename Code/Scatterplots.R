ggplot(data=HC_All, aes(x=H, y=C)) + geom_point() + facet_grid(tau ~ D)
