#### Required packages ####
if (!require("gratia")) install.packages("gratia")
if (!require("ggplot2")) install.packages("ggplot2")

#### Required data ####
load("./data/GAMM.Rda") 

#### SI Figure 4 ####
draw(Mod)

# The modified draw.gam function
mydraw.gam <- function (object, parametric = TRUE, select = NULL, 
                        scales = c("free", "fixed"), 
                        align = "hv", axis = "lrtb", n = 100, 
                        unconditional = FALSE, 
                        overall_uncertainty = TRUE, dist = 0.1, ...){
  scales <- match.arg(scales)
  S <- smooths(object)
  select <- gratia:::check_user_select_smooths(smooths = S, select = select)
  d <- gratia:::smooth_dim(object)
  take <- d <= 2L
  select <- select[take]
  S <- S[take]
  d <- d[take]
  is_re <- vapply(object[["smooth"]], gratia:::is_re_smooth, logical(1L))
  is_by <- vapply(object[["smooth"]], gratia:::is_by_smooth, logical(1L))
  if (any(is_by)) {
    S <- vapply(strsplit(S, ":"), `[[`, character(1L), 1L)
  }
  npara <- 0
  nsmooth <- length(S)
  if (isTRUE(parametric)) {
    terms <- parametric_terms(object)
    npara <- length(terms)
    p <- vector("list", length = npara)
  }
  g <- l <- vector("list", length = nsmooth)
  for (i in unique(S)) {
    eS <- evaluate_smooth(object, smooth = i, n = n, unconditional = unconditional, 
                          overall_uncertainty = overall_uncertainty, dist = dist)
    l[S == i] <- split(eS, eS[["smooth"]])
  }
  l <- l[select]
  d <- d[select]
  g <- g[select]
  if (length(g) == 0L) {
    message("Unable to draw any of the model terms.")
    return(invisible(g))
  }
  for (i in seq_along(l)) {
    g[[i]] <- draw(l[[i]])
  }
  if (isTRUE(parametric)) {
    for (i in seq_along(terms)) {
      p[[i]] <- evaluate_parametric_term(object, term = terms[i])
      g[[i + length(g)]] <- draw(p[[i]])
    }
  }
  if (isTRUE(identical(scales, "fixed"))) {
    wrapper <- function(x) {
      range(x[["est"]] + (2 * x[["se"]]), x[["est"]] - 
              (2 * x[["se"]]))
    }
    ylims <- range(unlist(lapply(l, wrapper)))
    if (isTRUE(parametric)) {
      ylims <- range(ylims, unlist(lapply(p, function(x) range(x[["upper"]], 
                                                               x[["lower"]]))))
    }
    gg <- seq_along(g)[c(d == 1L, rep(TRUE, npara))]
    for (i in gg) {
      g[[i]] <- g[[i]] + lims(y = ylims)
    }
  }
  g
}

# Example no. 1
# dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
# mod <- gam(y ~ s(x0),  data = dat, method = "REML")
p <- mydraw.gam(Mod)

SuppFig3A <- p[[1]]+
  labs(title = "Neighbourhood asymmetry",
                            subtitle="Grafted")+
  xlab("Neighbourhood asymmetry")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
      axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))

SuppFig3B <- p[[2]] +
  labs(title = "Neighbourhood asymmetry",
       subtitle="Non-grafted")+
  xlab("Neighbourhood asymmetry")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))


SuppFig3C <- p[[3]] +
  labs(title = "Stem diameter",
       subtitle="Grafted")+
  xlab("Stem diameter (cm)")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))

SuppFig3D <- p[[4]] +
  labs(title = "Stem diameter",
       subtitle="Non-grafted")+
  xlab("Stem diameter (cm)")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))

SuppFig3E <- p[[5]] +
  labs(title = "Condition",
       subtitle="")+
  xlab("Condition")+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))

SuppFig3F <- p[[7]] +
  labs(title = "Salinity",
       subtitle="")+
  xlab("Salinity")+
  ylab("Partial effect of Salinity")+
  scale_x_discrete(labels = c("39.7", "41.6", "45.0", "45.3", "46.9",
                              "56.1", "58.1", "58.6"))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                   size=12, colour = "black"),
        axis.title.x = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))


#### Merge figures ####
Supp_Fig_Fig4 <-ggarrange(SuppFig3A, SuppFig3B, SuppFig3C,
                          SuppFig3D, SuppFig3E, SuppFig3F, 
                          nrow = 2, ncol = 3, align = "hv",
                          labels = c("a)","b)","c)","d)","e)","f)"))

#### Save file ####
tiff("figures/Supp_Inf_Fig4.tiff", height = 4000, width = 7000, res=600)
annotate_figure(Supp_Fig_Fig4,
                top = text_grob(paste0("Supplementary Figure 4. Generalized additive mixed effects model\nshowing the effects of smooth terms on tree height\n"),  color = "black", face = "bold", size = 12, hjust = 0, x=0.01,just="left"))
dev.off()
