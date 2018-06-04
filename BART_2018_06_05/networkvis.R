library(visNetwork)

saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  htmlwidgets::saveWidget(widget,file=file,...)
}

visSaveFix <- function (graph, file, selfcontained = TRUE, background = "white") {
  saveWidgetFix(graph, file, selfcontained = selfcontained, background = background)
}

## Populationsgraph: Individuen

palette <- colorRampPalette(colors=c("#000000", "#FFFFFF"))

nodes0 <- tibble::tibble(
  id = 1:20, 
  color.background = palette(length(id)),
  color.border = "black",
  label = id,
  borderWidth = 3,
  shape = "circle",
  level = c(1,1,1,1,2,2,2,3,3,3,1,1,3,3,3,4,4,4,4,4)
)
edges0 <- tibble::tibble()

visNetwork(nodes0, edges0) %>%
  visLayout(randomSeed = 23) %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/01.html", background = "#f1f1f1")

## Populationsgraph: Reproduktion

edges1 <- tibble::tibble(
  from = c(1, 3, 6, 11, 10),
  to =   c(2, 4, 7, 12, 13),
  label = "❤",
  font.size = 30,
  width = 8
)

visNetwork(nodes0, edges1) %>%
  visLayout(randomSeed = 23) %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/02.html", background = "#f1f1f1")

## Populationsgraph: Vertikale Beziehungen

edges2 <- edges1 %>% rbind(
  tibble::tibble(
    from = c(1,2,1,2 , 3,4, 6,7,6,7, 6, 7, 11,12,11,12,11,12, 10,13,10,13,10,13, 10, 10,13),
    to =   c(5,5,6,6 , 7,7, 8,8,9,9,10,10, 13,13,14,14,15,15, 16,16,17,17,18,18, 19, 20,20),
    label = "*",
    font.size = c(30),
    width = 4
  )
)

visNetwork(nodes0, edges2) %>%
  visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/03.html", background = "#f1f1f1")

## Populationsgraph: Horizontale Beziehungen

# set.seed(123)
# additional_relations_small <- dplyr::sample_n(additional_relations, 30)
additional_relations_small <- tibble::tibble(
  from = c(2,  3, 5, 8,  9, 13, 14, 16, 17, 18, 19),
  to =   c(3, 11, 6, 9, 10, 14, 15, 17, 18, 19, 20),
  label = NA,
  font.size = 30,
  width = 1
)

edges4 <- edges2 %>% rbind(additional_relations_small)

visNetwork(nodes0, edges4) %>%
  visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/04.html", background = "#f1f1f1")

## Populationsgraph: Vererbung und Transmission

nodes2 <- nodes0

#1
nodes2$color.background[c(1)] <- "orange"
nodes2$color.background[c(12)] <- "blue"
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/05.html", background = "#f1f1f1")

#2 
nodes2$color.background[c(2, 6)] <- "orange"
nodes2$color.background[c(11, 15, 14)] <- "blue"
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/06.html", background = "#f1f1f1")

#3
nodes2 <- nodes2[-c(1, 12), ]
nodes2[nodes2$id %in% c(3, 8, 9, 10), ]$color.background <- "orange"
nodes2[nodes2$id %in% c(13), ]$color.background <- "blue"
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/07.html", background = "#f1f1f1")

#4
nodes2 <- nodes2[!(nodes2$id %in% c(2, 6, 11, 15, 14)), ]
nodes2[nodes2$id %in% c(16, 17, 18), ]$color.background <- "orange"
nodes2[nodes2$id %in% c(20), ]$color.background <- "blue"
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/08.html", background = "#f1f1f1")

#5
nodes2 <- nodes2[!(nodes2$id %in% c(3, 8, 9, 10, 13)), ]
nodes2[nodes2$id %in% c(19), ]$color.background <- "blue"
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/09.html", background = "#f1f1f1")

#6
nodes2 <- nodes2[!(nodes2$id %in% c(16, 17, 18, 20)), ]
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/10.html", background = "#f1f1f1")

#7 
nodes2 <- nodes2[!(nodes2$id %in% c(19)), ]
visNetwork(nodes2, edges4) %>% visHierarchicalLayout() %>% 
  visSaveFix(file = "BART_2018_06_05/network_figures/11.html", background = "#f1f1f1")


#### transform html to png ####

html_files <- list.files("BART_2018_06_05/network_figures", pattern = ".html$")

for (i in 1:length(html_files)) {
  webshot::webshot(
    file.path("BART_2018_06_05", "network_figures", html_files[i]), 
    file.path("BART_2018_06_05", "network_figures", paste0(formatC(i, width = 2, format = "d", flag = "0"), ".png")), 
    delay = 1,
    vwidth = 1000,
    vheight = 700,
    selector = ".vis-network"
  )
}
