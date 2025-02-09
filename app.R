library(shiny)
library(rgl)
l <- length(col <- hcl.colors(10, "sunset")[c(1:10, 9:1)])
# magick::image_write(magick::image_read(as.raster(col)), "tex.png")
v <- "attribute vec3 aPos; attribute vec4 aCol; uniform vec2 a; varying vec4 v;
void main(void) {vec2 e = a * aCol.a;
  v = vec4((aPos.xy + 1.)/2.*e + aCol.gb-e/2., aCol.r, aPos.z);
  gl_Position = vec4(aPos.xy, 0., 1.);}"
f <- "#ifdef GL_ES
precision highp float;
#endif
uniform sampler2D s; uniform float n,l; varying vec4 v;
float q(float i) {return mod(pow(pow(i,.5)*n,log(v.z)),n);}
vec4 p(float i) {return texture2D(s, vec2(0., -(l-1.)/l*(exp(q(i))-(l+2.)/(l+1.))));}
void main(void) {float x,y,x2,y2 = 0.; float iter, max_i = float(%i);
  for (int i = 0; i < %i; i++) {
    if(x2 + y2 > 4.) {
      float log_zn = log(x*x + y*y) / 2.;
      float nu = log(log_zn / log(2.)) / log(2.);
      iter = float(i) + 1. - nu;
      gl_FragColor = p(iter/max_i); return;
    } y = 2.*x*y + v.y; x = x2 - y2 + v.x; x2 = x*x; y2 = y*y;
  } gl_FragColor = v.w == 0. ? vec4(0,0,0,1) : p(iter/max_i);
}"
m <- mesh3d(vertices = c(-1,-1,0,1,-1,0,1,1,0,-1,1,0), quads = 1:4)
p <- list(seq(0,8,.1), c(-2,.47), c(-1.12,1.12), seq(-12,.81,.01), c(FALSE,TRUE))
q <- lapply(p[c(2,3,5)], function(i) matrix(rep(as.numeric(i), 4), ncol = 4))
e <- lapply(p[c(1,4)], function(i) matrix(rep(exp(i), 4), ncol = 4))
t <- list(s = "tex.png")
u <- list(a = a <- c(2.47/2.24, 1), n = 5, l = l)
b <- function(x, l, u, d) {x <- c(x - (e <- d / 2), x + e)
  round(if(x[1] < l) c(l, l + d) else if(x[2] > u) c(u - d, u) else x, 6)}
w <- function(r, v, a, d, p, i) renderPlaywidget(playwidget("rgl", respondTo = r, vertexControl(
  values = v, attributes = a, objid = d, vertices = 1:4, param = p, interp = i)))
ui <- fluidPage(titlePanel("Mandelbrot Set"), sidebarLayout(sidebarPanel(
  conditionalPanel('false', sliderInput("c", "Color scale:", min = 0, max = 8, value = 0.2, step = .1)),
  sliderInput("x", "Horizontal offset:", min = -2, max = .47, value = -.765, step = .001, ticks = FALSE),
  sliderInput("y", "Vertical offset:", min = -1.12, max = 1.12, value = 0, step = .001, ticks = FALSE),
  sliderInput("e", "Extent:", min = -11, max = .81, value = .81, step = .01, pre = "e^",
              animate = animationOptions(interval = 10)),
  checkboxInput("b", "Blackout", TRUE),
  sliderInput("m", "Max iterations:", min = 1, max = 5, value = 3, step = .5, pre = "10^"),
  actionButton("r", "Reset"),
  playwidgetOutput("cc"), playwidgetOutput("cx"), playwidgetOutput("cy"),
  playwidgetOutput("ce"), playwidgetOutput("cb")
), mainPanel(rglwidgetOutput("rgl", width = "558px", height = "506px"))))
server <- function(input, output, session) {
  save <- options(rgl.inShiny = TRUE, rgl.useNULL = TRUE)
  open3d()
  id <- shade3d(m)
  dev <- cur3d()
  session$onSessionEnded(function() {
    set3d(dev)
    close3d()
    options(save)
  })
  bindEvent(observe({
    updateSliderInput(session, "x", min = -2, max = .47, value = -.765)
    updateSliderInput(session, "y", min = -1.12, max = 1.12, value = 0)
    updateSliderInput(session, "e", value = .81)
    updateSliderInput(session, "b", value = TRUE)
    updateSliderInput(session, "m", value = 3)
  }), input$r)
  bindEvent(observe({
    s <- (d <- a * exp(input$e)) / 100
    x <- b(input$x, -2, .47, d[1])
    y <- b(input$y, -1.12, 1.12, d[2])
    updateSliderInput(session, "x", value = input$x, min = x[1], max = x[2], step = s[1])
    updateSliderInput(session, "y", value = input$y, min = y[1], max = y[2], step = s[2])
  }), input$e)
  x <- reactive({
    updateSliderInput(session, "c")
    updateSliderInput(session, "x")
    updateSliderInput(session, "y")
    updateSliderInput(session, "e")
    updateCheckboxInput(session, "b")
    setUserShaders(id, v, sprintf(f, z <- as.integer(10^input$m), z), textures = t, uniforms = u)
  })
  output$rgl <- renderRglwidget(rglwidget(x(), controllers = c("cc", "cx", "cy", "ce")))
  output$cc <- w("c", e[[1]], "red", id, p[[1]], FALSE)
  output$cx <- w("x", q[[1]], "green", id, p[[2]], TRUE)
  output$cy <- w("y", q[[2]], "blue", id, p[[3]], TRUE)
  output$ce <- w("e", e[[2]], "alpha", id, p[[4]], FALSE)
  output$cb <- w("b", q[[3]], "z", id, p[[5]], FALSE)
}
shinyApp(ui = ui, server = server)
