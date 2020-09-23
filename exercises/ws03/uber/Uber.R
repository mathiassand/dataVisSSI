if (! file.exists("nyuber.dat"))
  download.file("http://www.stat.uiowa.edu/~luke/data/nyuber.dat",
                "nyuber.dat")
tu <- read.table("nyuber.dat", head = TRUE)
ggplot(tu, aes(x = uber, y = taxi)) +
  geom_point(aes(size = rides)) +
  geom_abline(slope = -1, linetype = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_size_area()