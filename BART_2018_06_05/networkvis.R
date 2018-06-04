library(visNetwork)

## Populationsgraph: Individuen

palette <- colorRampPalette(colors=c("#000000", "#FFFFFF"))

nodes0 <- tibble::tibble(
  id = 1:20, 
  color.background = palette(length(id)),
  color.border = "black",
  #label = id,
  borderWidth = 3,
  shape = "circle",
  level = c(1,1,1,1,2,2,2,3,3,3,1,1,3,3,3,4,4,4,4,4)
)
edges0 <- tibble::tibble()

visNetwork(nodes0, edges0) %>%
  visLayout(randomSeed = 23)

## Populationsgraph: Reproduktion

edges1 <- tibble::tibble(
  from = c(1, 3, 6, 11, 10),
  to =   c(2, 4, 7, 12, 13),
  label = "❤",
  font.size = 30,
  width = 8
)

visNetwork(nodes0, edges1) %>%
  visLayout(randomSeed = 23)

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
  visHierarchicalLayout() 

## Populationsgraph: Vererbung 

nodes1 <- nodes0
nodes1$color.background[c(4, 7, 9, 10, 17, 19)] = "orange"

visNetwork(nodes1, edges2) %>%
  visHierarchicalLayout() 

## Populationsgraph: Horizontale Beziehungen

expand.grid.unique <- function(x, y, include.equals=FALSE) {
  x <- unique(x)
  y <- unique(y)
  g <- function(i) {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  do.call(rbind, lapply(seq_along(x), g)) %>%
    as.data.frame() %>%
    return()
}

all_relations <- expand.grid(nodes0$id, nodes0$id) %>%
  dplyr::rename(
    from = Var1,
    to = Var2
  ) %>% dplyr::filter(
    from != to
  )

additional_relations <- all_relations %>% 
  dplyr::anti_join(
    edges2,
    by = c("from", "to")
  ) %>%
  dplyr::anti_join(
    edges2 %>% dplyr::rename(to = from, from = to),
    by = c("from", "to")
  ) %>%
  dplyr::mutate(
    label = NA,
    font.size = 30,
    width = 1
  )

edges3 <- edges2 %>% rbind(additional_relations)

edges3 %>%
  igraph::graph_from_data_frame(d = ., vertices = nodes0, directed = FALSE) %>%
  visNetwork::visIgraph(layout = "layout_in_circle", idToLabel = FALSE)

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
  visHierarchicalLayout() 

## Populationsgraph: Vererbung und Transmission

nodes2 <- nodes0
nodes2$color.background[c(4, 11, 12, 13, 14, 7, 6, 8, 9, 10, 16, 17, 18, 19)] <- "orange"

visNetwork(nodes2, edges4) %>%
  visHierarchicalLayout() 
