color1 <- c("#006C67", "#F56258", "#FFB100", "#002A47")
color2 <- c("#3376BC", "#C14D7C", "#F4A82C", "#003015")
color3 <- c("#B35DCC", "#EFCD4A", "#0B7EF3", "#332B02")
color4 <- c("#5ABCC4", "#FFC533", "#FF5452", "#002324")
color5 <- c("#6198FF", "#89B449", "#E69682", "#001F33")
color6 <- c("#FE9A84", "#08808E", "#BEC74B", "#1B0038")
shape1 <- c(16, 17) # circulo grande y triangulo grande
shape2 <- c(16, 4) #circulo mediano y cruz
shape3 <- c(0, 7) #cuadrado y cuadrado tachado
color_eps <- c(color1, color2, color3, color4, color5, color6)
shape_eps <- c(shape1, shape2, shape3)

# save(color_eps, file = "data/colore_eps.rda")
# save(shape_eps, file = "data/shape_eps.rda")

# library(ggplot2)
# ggplot(data.frame(x_axis=1:24, y_axis=rep(1:6, each=4)), aes(x=x_axis, y=y_axis))+
#   geom_point(aes(color= as.factor(1:24), size=20, shape=as.factor(rep(1:6, each=4))))+
#   scale_color_manual(values = color_eps) +
#   scale_shape_manual(values=shape_eps)+
#   theme_minimal()

#cambian los colores entre ggplot y plot de base
