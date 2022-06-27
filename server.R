shinyServer(function(input, output) {
  
  # Function for performing Descriptive & Exploratory Analysis for Historical records ----
  HistoricalAnalysisFnct <- function (df = "History") {
    df <- read_excel(paste(df, "xlsx", sep = "."))
    myvars <- names(df) %in% c("ID", "SYNTAX SCORE")
    df <- df %>% mutate_if(is.character, as.factor)
    
    # Descriptive Statistics ----
    dfSubset <- df[!myvars]
    PropTablefnct <- function(Var) {
      PropTable <-
        data.frame(round(prop.table(table(Var, useNA = "always")) * 100, 2))
      return(PropTable)
    }
    is.fact <- sapply(dfSubset, is.factor)
    PropTabledf <-
      do.call(rbind, lapply(dfSubset[is.fact],  PropTablefnct))
    VarNamesdf <-
      data.frame(rep(colnames(dfSubset[is.fact]), each = 3))
    colnames(VarNamesdf) <- "Predictor"
    PropTabledf <-
      data.frame(VarNamesdf, PropTabledf, row.names = NULL)
    Barplots  <-
      ggplot(data = PropTabledf, aes(x = Var, y = Freq)) +
      facet_wrap(~ Predictor, ncol = 3) +
      geom_bar(stat = "identity",
               na.rm = F,
               fill = "white") +
      theme_solarized(light = FALSE) +
      scale_y_continuous(limits = c(0, 130)) +
      geom_text(
        aes(label = Freq),
        vjust = -0.3,
        size = 5,
        fontface = "bold",
        color = "white"
      ) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Percentage",
           x = "Predictor (History Records)")
    is.num <- sapply(dfSubset, is.numeric)
    DescriptiveHistorical <-
      round(data.frame(psych::describe(as.data.frame(dfSubset[is.num]))), 3)
    colnames (DescriptiveHistorical)[1] <- c("Predictor")
    DescriptiveHistorical[1, 1] <- colnames(dfSubset)[is.num]
    DescriptiveTable <-
      DescriptiveHistorical[, c("Predictor", "n", "mean", "sd", "median", "min", "max")]
    HistogramPlots <- dfSubset[is.num] %>%
      gather(key, value, colnames(dfSubset[is.num])) %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "white") +
      facet_wrap( ~ key, scales = "free", ncol = 4) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        )
      ) +
      labs(y = "Density",
           x = "")
    
    # Exploratory Statistics ----
    dfExploratory <-
      tibble(dfSubset[is.fact], df[, c("ID", "SYNTAX SCORE")])
    BoxplotdfLong <-
      reshape2::melt(dfExploratory, id.vars = c("ID", "SYNTAX SCORE"))
    ExploratoryTabledf <-
      describeBy(
        BoxplotdfLong$`SYNTAX SCORE`,
        list(BoxplotdfLong$value, BoxplotdfLong$variable),
        mat = T
      )
    ExploratoryTabledf <-
      ExploratoryTabledf[, c("group2", "group1", "n", "mean", "sd", "median", "min", "max")]
    ExploratoryTabledf[, c("mean", "sd", "median", "min", "max")] <-
      lapply(ExploratoryTabledf[, c("mean", "sd", "median", "min", "max")], round, 3)
    colnames(ExploratoryTabledf)[1:2] <- c("Predictor", "Level")
    ExploratoryTable <- ExploratoryTabledf
    BoxplotdfLongComplete <-
      BoxplotdfLong[complete.cases(BoxplotdfLong), ]
    Boxplots  <-
      ggplot(data = BoxplotdfLongComplete, aes(x = value, y = `SYNTAX SCORE`, fill =
                                                 value)) +
      geom_boxplot() +
      facet_wrap(. ~ variable, ncol = 3) +
      scale_fill_manual(values = c("white", "#FDE725FF")) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      geom_boxplot(
        notch = F,
        outlier.size = -1,
        color = "black",
        lwd = 1.2,
        alpha = 0.7
      ) +
      geom_point(
        shape = 21,
        size = 2,
        position = position_jitterdodge(),
        color = "black",
        alpha = 0.5
      ) +
      geom_violin(
        alpha = 0.25,
        position = position_dodge(width = .75),
        size = 1,
        color = "black"
      ) +
      ggbeeswarm::geom_quasirandom(
        shape = 21,
        size = 2,
        dodge.width = 0.50,
        color = "black",
        alpha = .25,
        show.legend = F
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 1, color =
                                                       "black"))) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Syntax Score", x = "Predictor (History Records)")
    NoCompar <- length(dfSubset[is.fact])
    KruskalWallisResdf <-
      data.frame(matrix(0, nrow = NoCompar, ncol = 5))
    colnames (KruskalWallisResdf) <- c("Comparison",
                                       "Method",
                                       "Statistic",
                                       "df",
                                       "p")
    dfInferential <-
      tibble(dfSubset[is.fact], df[, c("SYNTAX SCORE")])
    formulas.Inferential <-
      paste("`SYNTAX SCORE`", "~", "`", names(dfSubset[is.fact]), "`", sep =
              "")
    for (i in 1:NoCompar) {
      if (nlevels(dfSubset[is.fact] %>% pull(i)) > 2)
      {
        kruskal.test.res <-
          kruskal.test(as.formula(formulas.Inferential[i]), data = dfInferential)
        KruskalWallisResdf [i, 2] <- kruskal.test.res$method
        KruskalWallisResdf [i, 1] <- kruskal.test.res$data.name
        KruskalWallisResdf [i, 3] <-
          round(kruskal.test.res$statistic, 3)
        KruskalWallisResdf [i, 4] <- kruskal.test.res$parameter
        KruskalWallisResdf [i, 5] <-
          round(kruskal.test.res$p.value, 3)
      } else if (nlevels(dfSubset[is.fact] %>% pull(i)) == 2)
      {
        kruskal.test.res <-
          wilcox.test(as.formula(formulas.Inferential[i]), data = dfInferential)
        KruskalWallisResdf [i, 2] <- kruskal.test.res$method
        KruskalWallisResdf [i, 1] <- kruskal.test.res$data.name
        KruskalWallisResdf [i, 3] <-
          round(kruskal.test.res$statistic, 3)
        KruskalWallisResdf [i, 4] <- 1
        KruskalWallisResdf [i, 5] <-
          round(kruskal.test.res$p.value, 3)
      } else {
        KruskalWallisResdf [i, 1] <- c(formulas.History.SS[i])
        KruskalWallisResdf [i, 2] <- c(NA)
        KruskalWallisResdf [i, 3] <- c(NA)
        KruskalWallisResdf [i, 4] <- c(NA)
        KruskalWallisResdf [i, 5] <- c(NA)
      }
    }
    KruskalWallisResdf$Comparison <- colnames(dfSubset[is.fact])
    KruskalWallisResdf$Method <-
      factor(KruskalWallisResdf$Method, labels = c("Mann Whitney Test"))
    is.num <- sapply(df, is.numeric)
    dfCorrelation <- df[is.num]
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }
    res2 <- rcorr(as.matrix(dfCorrelation), type = c("spearman"))
    CorrMat <- flattenCorrMatrix(res2$r, res2$P)
    CorrMat <- CorrMat %>% mutate_if(is.numeric, round, digits = 3)
    CorrMat <-
      data.frame(cbind(
        CorrMat[CorrMat$column == "SYNTAX SCORE", 1],
        rep("Spearman", times = ncol(dfCorrelation) -
              1),
        CorrMat[CorrMat$column == "SYNTAX SCORE", 3],
        rep("", times = ncol(dfCorrelation) - 1),
        
        CorrMat[CorrMat$column == "SYNTAX SCORE", 4]
      ))
    colnames(CorrMat) <- colnames(KruskalWallisResdf)
    HypothesisTestdf <- rbind(KruskalWallisResdf, CorrMat)
    HypothesisTable <- HypothesisTestdf
    list(
      DescriptiveTable = DescriptiveTable,
      Barplots = Barplots,
      HistogramPlots = HistogramPlots,
      ExploratoryTable = ExploratoryTable,
      Boxplots = Boxplots,
      HypothesisTable = HypothesisTable
    )
  }
  
  # End of function ----
  
  # Function for performing Descriptive & Exploratory Analysis for Entry records ----
  EntryAnalysisFnct <- function (df = "Entry") {
    df <- read_excel(paste(df, "xlsx", sep = "."))
    myvars <- names(df) %in% c("ID", "SYNTAX SCORE")
    df <- df %>% mutate_if(is.character, as.factor)
    
    # Descriptive Statistics ----
    dfSubset <- df[!myvars]
    PropTablefnct <- function(Var) {
      PropTable <-
        data.frame(round(prop.table(table(Var, useNA = "always")) * 100, 2))
      return(PropTable)
    }
    is.fact <- sapply(dfSubset, is.factor)
    PropTabledf <-
      do.call(rbind, lapply(dfSubset[is.fact],  PropTablefnct))
    VarNamesdf <-
      data.frame(rep(colnames(dfSubset[is.fact]), each = 3))
    colnames(VarNamesdf) <- "Predictor"
    PropTabledf <-
      data.frame(VarNamesdf, PropTabledf, row.names = NULL)
    Barplots  <-
      ggplot(data = PropTabledf, aes(x = Var, y = Freq)) +
      facet_wrap(~ Predictor, ncol = 3) +
      geom_bar(stat = "identity",
               na.rm = F,
               fill = "white") +
      theme_solarized(light = FALSE) +
      scale_y_continuous(limits = c(0, 130)) +
      geom_text(
        aes(label = Freq),
        vjust = -0.3,
        size = 5,
        fontface = "bold",
        color = "white"
      ) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Percentage",
           x = "Predictor (Entry Records)")
    is.num <- sapply(dfSubset, is.numeric)
    DescriptiveContinuous <-
      round(data.frame(psych::describe(dfSubset[is.num])), 3)
    DescriptiveContinuous <-
      tibble(row.names(DescriptiveContinuous), DescriptiveContinuous)
    colnames (DescriptiveContinuous)[1] <- c("Predictor")
    DescriptiveTable <-
      DescriptiveContinuous[, c("Predictor", "n", "mean", "sd", "median", "min", "max")]
    HistogramPlots <- dfSubset[is.num] %>%
      gather(key, value, colnames(dfSubset[is.num])) %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "white") +
      facet_wrap(~ key, scales = "free", ncol = 4) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        )
      ) +
      labs(y = "Density",
           x = "Predictor (Entry Records)")
    
    # Exploratory Statistics ----
    dfExploratory <-
      tibble(dfSubset[is.fact], df[, c("ID", "SYNTAX SCORE")])
    BoxplotdfLong <-
      reshape2::melt(dfExploratory, id.vars = c("ID", "SYNTAX SCORE"))
    ExploratoryTabledf <-
      describeBy(
        BoxplotdfLong$`SYNTAX SCORE`,
        list(BoxplotdfLong$value, BoxplotdfLong$variable),
        mat = T
      )
    ExploratoryTabledf <-
      ExploratoryTabledf[, c("group2", "group1", "n", "mean", "sd", "median", "min", "max")]
    ExploratoryTabledf[, c("mean", "sd", "median", "min", "max")] <-
      lapply(ExploratoryTabledf[, c("mean", "sd", "median", "min", "max")], round, 3)
    colnames(ExploratoryTabledf)[1:2] <- c("Predictor", "Level")
    ExploratoryTable <- ExploratoryTabledf
    BoxplotdfLongComplete <-
      BoxplotdfLong[complete.cases(BoxplotdfLong), ]
    Boxplots  <-
      ggplot(data = BoxplotdfLongComplete, aes(x = value, y = `SYNTAX SCORE`, fill =
                                                 value)) +
      geom_boxplot() +
      scale_fill_manual(values = c("white", "#FDE725FF")) +
      facet_wrap(. ~ variable, ncol = 3) +
      
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      geom_boxplot(
        notch = F,
        outlier.size = -1,
        color = "black",
        lwd = 1.2,
        alpha = 0.7
      ) +
      geom_point(
        shape = 21,
        size = 2,
        position = position_jitterdodge(),
        color = "black",
        alpha = 0.5
      ) +
      geom_violin(
        alpha = 0.25,
        position = position_dodge(width = .75),
        size = 1,
        color = "black"
      ) +
      ggbeeswarm::geom_quasirandom(
        shape = 21,
        size = 2,
        dodge.width = 0.50,
        color = "black",
        alpha = .25,
        show.legend = F
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 1, color =
                                                       "black"))) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Syntax Score", x = "Predictor (Entry Records)")
    
    NoCompar <- length(dfSubset[is.fact])
    KruskalWallisResdf <-
      data.frame(matrix(0, nrow = NoCompar, ncol = 5))
    colnames (KruskalWallisResdf) <- c("Comparison",
                                       "Method",
                                       "Statistic",
                                       "df",
                                       "p")
    dfInferential <-
      tibble(dfSubset[is.fact], df[, c("SYNTAX SCORE")])
    formulas.Inferential <-
      paste("`SYNTAX SCORE`", "~", "`", names(dfSubset[is.fact]), "`", sep =
              "")
    for (i in 1:NoCompar) {
      if (nlevels(dfSubset[is.fact] %>% pull(i)) > 2)
      {
        kruskal.test.res <-
          kruskal.test(as.formula(formulas.Inferential[i]), data = dfInferential)
        KruskalWallisResdf [i, 2] <- kruskal.test.res$method
        KruskalWallisResdf [i, 1] <- kruskal.test.res$data.name
        KruskalWallisResdf [i, 3] <-
          round(kruskal.test.res$statistic, 3)
        KruskalWallisResdf [i, 4] <- kruskal.test.res$parameter
        KruskalWallisResdf [i, 5] <-
          round(kruskal.test.res$p.value, 3)
      } else if (nlevels(dfSubset[is.fact] %>% pull(i)) == 2)
      {
        kruskal.test.res <-
          wilcox.test(as.formula(formulas.Inferential[i]), data = dfInferential)
        KruskalWallisResdf [i, 2] <- kruskal.test.res$method
        KruskalWallisResdf [i, 1] <- kruskal.test.res$data.name
        KruskalWallisResdf [i, 3] <-
          round(kruskal.test.res$statistic, 3)
        KruskalWallisResdf [i, 4] <- 1
        KruskalWallisResdf [i, 5] <-
          round(kruskal.test.res$p.value, 3)
      } else {
        KruskalWallisResdf [i, 1] <- c(formulas.History.SS[i])
        KruskalWallisResdf [i, 2] <- c(NA)
        KruskalWallisResdf [i, 3] <- c(NA)
        KruskalWallisResdf [i, 4] <- c(NA)
        KruskalWallisResdf [i, 5] <- c(NA)
      }
    }
    KruskalWallisResdf$Comparison <- colnames(dfSubset[is.fact])
    KruskalWallisResdf$Method <-
      factor(KruskalWallisResdf$Method, labels = c("Mann Whitney Test"))
    is.num <- sapply(df, is.numeric)
    dfCorrelation <- df[is.num]
    dfCorrelation <-
      dfCorrelation[, c(ncol(dfCorrelation), 1:(ncol(dfCorrelation) - 1))]
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }
    res2 <- rcorr(as.matrix(dfCorrelation), type = c("spearman"))
    CorrMat <- flattenCorrMatrix(res2$r, res2$P)
    CorrMat2 <- flattenCorrMatrix(res2$r, res2$n)
    CorrMat <- data.frame(CorrMat, CorrMat2[, 4])
    colnames(CorrMat)[5] <- c("df")
    CorrMat <- CorrMat %>% mutate_if(is.numeric, round, digits = 3)
    CorrMat <-
      data.frame(cbind(
        CorrMat[CorrMat$row == "SYNTAX SCORE", 2],
        rep("Spearman", times = ncol(dfCorrelation) -
              1),
        CorrMat[CorrMat$row == "SYNTAX SCORE", 3],
        CorrMat[CorrMat$row == "SYNTAX SCORE", 5],
        CorrMat[CorrMat$row == "SYNTAX SCORE", 4]
      ))
    colnames(CorrMat) <- colnames(KruskalWallisResdf)
    
    CorrMatdf <- CorrMat
    CorrMatdf$Characterization <-
      cut(
        as.numeric(CorrMatdf$Statistic),
        breaks = c(-Inf, 0, +Inf),
        labels = c("Negative", "Positive")
      )
    CorrPlot <-
      ggplot(CorrMatdf,
             aes(
               x = as.numeric(Statistic),
               y = reorder (Comparison, as.numeric(Statistic)),
               group = Characterization
             )) +
      geom_segment(aes(
        xend = 0,
        yend = Comparison,
        color = Characterization
      ),
      size =
        5) +
      geom_vline(xintercept = 0, color = "white") +
      expand_limits(x = c(-max(abs(
        as.numeric(CorrMatdf$Statistic)
      )), max(abs(
        as.numeric(CorrMatdf$Statistic)
      )))) +
      geom_label(aes(label = Statistic)) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      #guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))+
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size =
            11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size =
            11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size =
            18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size =
            18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Predictor (Entry Records)", x = "Spearman Correlation Coefficient")
    
    
    HypothesisTestdf <- rbind(KruskalWallisResdf, CorrMat)
    HypothesisTable <- HypothesisTestdf
    list(
      DescriptiveTable = DescriptiveTable,
      Barplots = Barplots,
      HistogramPlots = HistogramPlots,
      ExploratoryTable = ExploratoryTable,
      Boxplots = Boxplots,
      CorrPlot = CorrPlot,
      HypothesisTable = HypothesisTable
    )
  }
  
  # End of function ----
  
  # Function for performing Descriptive & Exploratory Analysis for Biochemical records ----
  BiochemicalAnalysisFnct <- function (df = "Biochemical") {
    df <- read_excel(paste(df, "xlsx", sep = "."))
    myvars <- names(df) %in% c("ID", "SYNTAX SCORE")
    df <- df %>% mutate_if(is.character, as.factor)
    
    # Descriptive Statistics ----
    dfSubset <- df[!myvars]
    is.num <- sapply(dfSubset, is.numeric)
    DescriptiveContinuous <-
      round(data.frame(psych::describe(dfSubset[is.num])), 3)
    DescriptiveContinuous <-
      tibble(row.names(DescriptiveContinuous), DescriptiveContinuous)
    colnames (DescriptiveContinuous)[1] <- c("Predictor")
    DescriptiveTable <-
      DescriptiveContinuous[, c("Predictor", "n", "mean", "sd", "median", "min", "max")]
    HistogramPlots <- dfSubset[is.num] %>%
      gather(key, value, colnames(dfSubset[is.num])) %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "white") +
      facet_wrap( ~ key, scales = "free", ncol = 4) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 8
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        )
      ) +
      labs(y = "Density",
           x = "Predictor (Biochemical Records)")
    is.num <- sapply(df, is.numeric)
    dfCorrelation <- df[is.num]
    dfCorrelation <-
      dfCorrelation[, c(ncol(dfCorrelation), 1:(ncol(dfCorrelation) - 1))]
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }
    res2 <- rcorr(as.matrix(dfCorrelation), type = c("spearman"))
    CorrMat <- flattenCorrMatrix(res2$r, res2$P)
    CorrMat2 <- flattenCorrMatrix(res2$r, res2$n)
    CorrMat <- data.frame(CorrMat, CorrMat2[, 4])
    colnames(CorrMat)[5] <- c("df")
    CorrMat <- CorrMat %>% mutate_if(is.numeric, round, digits = 3)
    CorrMat <-
      data.frame(cbind(
        CorrMat[CorrMat$row == "SYNTAX SCORE", 2],
        rep("Spearman", times = ncol(dfCorrelation) -
              1),
        CorrMat[CorrMat$row == "SYNTAX SCORE", 3],
        CorrMat[CorrMat$row == "SYNTAX SCORE", 5],
        CorrMat[CorrMat$row == "SYNTAX SCORE", 4]
      ))
    colnames(CorrMat) <-
      c("Comparison", "Method", "Statistic", "df", "p")
    CorrMatdf <- CorrMat
    CorrMatdf$Characterization <-
      cut(
        as.numeric(CorrMatdf$Statistic),
        breaks = c(-Inf, 0, +Inf),
        labels = c("Negative", "Positive")
      )
    CorrPlot <-
      ggplot(CorrMatdf,
             aes(
               x = as.numeric(Statistic),
               y = reorder (Comparison, as.numeric(Statistic)),
               group = Characterization
             )) +
      geom_segment(aes(
        xend = 0,
        yend = Comparison,
        color = Characterization
      ),
      size =
        5) +
      geom_vline(xintercept = 0, color = "white") +
      expand_limits(x = c(-max(abs(
        as.numeric(CorrMatdf$Statistic)
      )), max(abs(
        as.numeric(CorrMatdf$Statistic)
      )))) +
      geom_label(aes(label = Statistic)) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      #guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))+
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size =
            11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size =
            11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size =
            18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size =
            18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Predictor (Biochemical Records)", x = "Spearman Correlation Coefficient")
    HypothesisTestdf <- CorrMat
    HypothesisTable <- HypothesisTestdf
    list(
      DescriptiveTable = DescriptiveTable,
      HistogramPlots = HistogramPlots,
      CorrPlot = CorrPlot,
      HypothesisTable = HypothesisTable
    )
  }
  
  # End of function ----
  
  # Function for performing Descriptive & Exploratory Analysis for Complete Blood Count records ----
  CBCAnalysisFnct <- function (df = "CBC") {
    df <- read_excel(paste(df, "xlsx", sep = "."))
    myvars <- names(df) %in% c("ID", "SYNTAX SCORE")
    df <- df %>% mutate_if(is.character, as.factor)
    
    # Descriptive Statistics ----
    dfSubset <- df[!myvars]
    is.num <- sapply(dfSubset, is.numeric)
    DescriptiveContinuous <-
      round(data.frame(psych::describe(dfSubset[is.num])), 3)
    DescriptiveContinuous <-
      tibble(row.names(DescriptiveContinuous), DescriptiveContinuous)
    colnames (DescriptiveContinuous)[1] <- c("Predictor")
    DescriptiveTable <-
      DescriptiveContinuous[, c("Predictor", "n", "mean", "sd", "median", "min", "max")]
    HistogramPlots <- dfSubset[is.num] %>%
      gather(key, value, colnames(dfSubset[is.num])) %>%
      ggplot(aes(x = value)) +
      geom_density(fill = "white") +
      facet_wrap(~ key, scales = "free", ncol = 4) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        )
      ) +
      labs(y = "Density",
           x = "Predictor (Complete Blood Count Records)")
    is.num <- sapply(df, is.numeric)
    dfCorrelation <- df[is.num]
    dfCorrelation <-
      dfCorrelation[, c(ncol(dfCorrelation), 1:(ncol(dfCorrelation) - 1))]
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }
    res2 <- rcorr(as.matrix(dfCorrelation), type = c("spearman"))
    CorrMat <- flattenCorrMatrix(res2$r, res2$P)
    CorrMat2 <- flattenCorrMatrix(res2$r, res2$n)
    CorrMat <- data.frame(CorrMat, CorrMat2[, 4])
    colnames(CorrMat)[5] <- c("df")
    CorrMat <- CorrMat %>% mutate_if(is.numeric, round, digits = 3)
    CorrMat <-
      data.frame(cbind(
        CorrMat[CorrMat$row == "SYNTAX SCORE", 2],
        rep("Spearman", times = ncol(dfCorrelation) -
              1),
        CorrMat[CorrMat$row == "SYNTAX SCORE", 3],
        CorrMat[CorrMat$row == "SYNTAX SCORE", 5],
        CorrMat[CorrMat$row == "SYNTAX SCORE", 4]
      ))
    
    colnames(CorrMat) <-
      c("Comparison", "Method", "Statistic", "df", "p")
    CorrMatdf <- CorrMat
    CorrMatdf$Characterization <-
      cut(
        as.numeric(CorrMatdf$Statistic),
        breaks = c(-Inf, 0, +Inf),
        labels = c("Negative", "Positive")
      )
    CorrPlot <-
      ggplot(CorrMatdf,
             aes(
               x = as.numeric(Statistic),
               y = reorder (Comparison, as.numeric(Statistic)),
               group = Characterization
             )) +
      geom_segment(aes(
        xend = 0,
        yend = Comparison,
        color = Characterization
      ),
      size =
        5) +
      geom_vline(xintercept = 0, color = "white") +
      expand_limits(x = c(-max(abs(
        as.numeric(CorrMatdf$Statistic)
      )), max(abs(
        as.numeric(CorrMatdf$Statistic)
      )))) +
      geom_label(aes(label = Statistic)) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      #guides(fill = guide_legend(override.aes = list(alpha = 1,color="black")))+
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size =
            11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size =
            11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size =
            18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size =
            18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Predictor (Complete Blood Count Records)", x = "Spearman Correlation Coefficient")
    HypothesisTestdf <- CorrMat
    HypothesisTable <- HypothesisTestdf
    list(
      DescriptiveTable = DescriptiveTable,
      HistogramPlots = HistogramPlots,
      CorrPlot = CorrPlot,
      HypothesisTable = HypothesisTable
    )
  }
  
  # End of function ----
  
  # Function for performing Descriptive & Exploratory Analysis for Differential records ----
  DifferentialAnalysisFnct <- function (df = "Differential") {
    df <- read_excel(paste(df, "xlsx", sep = "."))
    myvars <- names(df) %in% c("ID", "SYNTAX SCORE")
    df <- df %>% mutate_if(is.character, as.factor)
    
    # Descriptive Statistics ----
    dfSubset <- df[!myvars]
    PropTablefnct <- function(Var) {
      PropTable <-
        data.frame(round(prop.table(table(Var, useNA = "always")) * 100, 2))
      return(PropTable)
    }
    is.fact <- sapply(dfSubset, is.factor)
    PropTabledf <-
      do.call(rbind, lapply(dfSubset[is.fact],  PropTablefnct))
    VarNamesdf <-
      data.frame(rep(colnames(dfSubset[is.fact]), each = 3))
    colnames(VarNamesdf) <- "Predictor"
    PropTabledf <-
      data.frame(VarNamesdf, PropTabledf, row.names = NULL)
    Barplots  <-
      ggplot(data = PropTabledf, aes(x = Var, y = Freq)) +
      facet_wrap(~ Predictor, ncol = 3) +
      geom_bar(stat = "identity",
               na.rm = F,
               fill = "white") +
      theme_solarized(light = FALSE) +
      scale_y_continuous(limits = c(0, 130)) +
      geom_text(
        aes(label = Freq),
        vjust = -0.3,
        size = 5,
        fontface = "bold",
        color = "white"
      ) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Percentage",
           x = "Predictor (Differential Records)")
    
    # Exploratory Statistics ----
    colnames(df)
    library(reshape2)
    dfExploratory <-
      tibble(dfSubset[is.fact], df[, c("ID", "SYNTAX SCORE")])
    BoxplotdfLong <-
      reshape2::melt(dfExploratory, id.vars = c("ID", "SYNTAX SCORE"))
    ExploratoryTabledf <-
      describeBy(
        BoxplotdfLong$`SYNTAX SCORE`,
        list(BoxplotdfLong$value, BoxplotdfLong$variable),
        mat = T
      )
    ExploratoryTabledf <-
      ExploratoryTabledf[, c("group2", "group1", "n", "mean", "sd", "median", "min", "max")]
    ExploratoryTabledf[, c("mean", "sd", "median", "min", "max")] <-
      lapply(ExploratoryTabledf[, c("mean", "sd", "median", "min", "max")], round, 3)
    colnames(ExploratoryTabledf)[1:2] <- c("Predictor", "Level")
    ExploratoryTable <- ExploratoryTabledf
    BoxplotdfLongComplete <-
      BoxplotdfLong[complete.cases(BoxplotdfLong),]
    Boxplots  <-
      ggplot(data = BoxplotdfLongComplete, aes(x = value, y = `SYNTAX SCORE`, fill =
                                                 value)) +
      geom_boxplot() +
      scale_fill_manual(values = c("white", "#FDE725FF")) +
      facet_wrap(. ~ variable, ncol = 3) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      geom_boxplot(
        notch = F,
        outlier.size = -1,
        color = "black",
        lwd = 1.2,
        alpha = 0.7
      ) +
      geom_point(
        shape = 21,
        size = 2,
        position = position_jitterdodge(),
        color = "black",
        alpha = 0.5
      ) +
      geom_violin(
        alpha = 0.25,
        position = position_dodge(width = .75),
        size = 1,
        color = "black"
      ) +
      ggbeeswarm::geom_quasirandom(
        shape = 21,
        size = 2,
        dodge.width = 0.50,
        color = "black",
        alpha = .25,
        show.legend = F
      ) +
      guides(fill = guide_legend(override.aes = list(alpha = 1, color =
                                                       "black"))) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(
          size = 12,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 20,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Syntax Score", x = "Predictor (Differential Records)")
    
    NoCompar <- length(dfSubset[is.fact])
    KruskalWallisResdf <-
      data.frame(matrix(0, nrow = NoCompar, ncol = 5))
    colnames (KruskalWallisResdf) <- c("Comparison",
                                       "Method",
                                       "Statistic",
                                       "df",
                                       "p")
    dfInferential <-
      tibble(dfSubset[is.fact], df[, c("SYNTAX SCORE")])
    formulas.Inferential <-
      paste("`SYNTAX SCORE`", "~", "`", names(dfSubset[is.fact]), "`", sep =
              "")
    
    for (i in 1:NoCompar) {
      if (nlevels(dfSubset[is.fact] %>% pull(i)) > 2)
      {
        kruskal.test.res <-
          kruskal.test(as.formula(formulas.Inferential[i]), data = dfInferential)
        KruskalWallisResdf [i, 2] <- kruskal.test.res$method
        KruskalWallisResdf [i, 1] <- kruskal.test.res$data.name
        KruskalWallisResdf [i, 3] <-
          round(kruskal.test.res$statistic, 3)
        KruskalWallisResdf [i, 4] <- kruskal.test.res$parameter
        KruskalWallisResdf [i, 5] <-
          round(kruskal.test.res$p.value, 3)
        
      } else if (nlevels(dfSubset[is.fact] %>% pull(i)) == 2)
      {
        kruskal.test.res <-
          wilcox.test(as.formula(formulas.Inferential[i]), data = dfInferential)
        KruskalWallisResdf [i, 2] <- kruskal.test.res$method
        KruskalWallisResdf [i, 1] <- kruskal.test.res$data.name
        KruskalWallisResdf [i, 3] <-
          round(kruskal.test.res$statistic, 3)
        KruskalWallisResdf [i, 4] <- 1
        KruskalWallisResdf [i, 5] <-
          round(kruskal.test.res$p.value, 3)
      } else {
        KruskalWallisResdf [i, 1] <- c(formulas.History.SS[i])
        KruskalWallisResdf [i, 2] <- c(NA)
        KruskalWallisResdf [i, 3] <- c(NA)
        KruskalWallisResdf [i, 4] <- c(NA)
        KruskalWallisResdf [i, 5] <- c(NA)
      }
    }
    KruskalWallisResdf$Comparison <- colnames(dfSubset[is.fact])
    KruskalWallisResdf$Method <-
      factor(KruskalWallisResdf$Method, labels = c("Mann Whitney Test"))
    HypothesisTestdf <- KruskalWallisResdf
    HypothesisTable <- HypothesisTestdf
    list(
      Barplots = Barplots,
      ExploratoryTable = ExploratoryTable,
      Boxplots = Boxplots,
      #CorrPlot = CorrPlot,
      HypothesisTable = HypothesisTable
    )
  }
  
  # End of function ----
  
  # Function for predicting the probability for the patient to present a non-zero SYNTAX score based on input values of predictors formulating the zero-part model ----
  PredictionBinaryRandomForest <- function (CHEST.PAIN = 1,
                                            STTCHANGES = 1,
                                            GLU = 120,
                                            ATRIALFIBRILLATION = 1,
                                            GENDER = 1,
                                            EASYFATIGUE = 1,
                                            AORTICANEURYSMS = 1,
                                            RATIO1 = 1.4,
                                            NEU = 64.2,
                                            GRACESCORE = 112,
                                            RATIO3 = 0.5,
                                            SGOT = 23,
                                            EOS = 2.8) {
    TrainingData.ColNames <- c(
      "TOTAL.SS.bin",
      "CHEST.PAIN",
      "ST.T.CHANGES",
      "GLU",
      "ATRIAL.FIBRILLATION",
      "GENDER",
      "EASY.FATIGUE",
      "AORTIC.ANEURYSMS",
      "RATIO1",
      "NEU...",
      "GRACE.SCORE",
      "RATIO3",
      "SGOT",
      "EOS..."
    )
    load("FittedBinaryRandomForestObject.RData")
    NewData <- tibble(
      NA,
      factor(
        CHEST.PAIN,
        levels = c(1, 2),
        labels = c("No", "Yes")
      ),
      factor(
        STTCHANGES,
        levels = c(1, 2),
        labels = c("No", "Yes")
      ),
      GLU,
      factor(
        ATRIALFIBRILLATION,
        levels = c(1, 2),
        labels = c("No", "Yes")
      ),
      factor(
        GENDER,
        levels = c(1, 2),
        labels = c("Female", "Male")
      ),
      factor(
        EASYFATIGUE,
        levels = c(1, 2),
        labels = c("No", "Yes")
      ),
      factor(
        AORTICANEURYSMS,
        levels = c(1, 2),
        labels = c("No", "Yes")
      ),
      RATIO1,
      NEU,
      GRACESCORE,
      RATIO3,
      SGOT,
      EOS
    )
    colnames(NewData) <- c(TrainingData.ColNames)
    PredictedValue <-
      predict(fit.random.forest.train, NewData, type = "prob")
    list(PredictedValue = PredictedValue[2])
  }
  
  # End of function ----
  
  # Function for predicting the SYNTAX score of the patient based on input values of predictors formulating the count-part model given that he/she present a non-zero SYNTAX score ----
  PredictionCountRandomForest <- function (CRUSADE.SCORE = 22,
                                           GFR = 98.3,
                                           AGE = 56,
                                           QRS.DURATION.ms = 102,
                                           GLU = 133,
                                           HCT = 35.9,
                                           HGB = 13.2,
                                           DIABETES.MELLITUS = 1,
                                           UREA = 67,
                                           GRACESCORE = 130) {
    TrainingData.ColNames <- c(
      "SYNTAX SCORE",
      "CRUSADE.SCORE",
      "GFR",
      "AGE",
      "QRS.DURATION.ms",
      "GLU",
      "HCT",
      "HGB",
      "UREA",
      "GRACE.SCORE",
      "DIABETES.MELLITUS"
    )
    load("FittedCountRandomForestObject.RData")
    NewData <- tibble(
      NA,
      CRUSADE.SCORE,
      GFR,
      AGE,
      QRS.DURATION.ms,
      GLU,
      HCT,
      HGB,
      UREA,
      GRACESCORE,
      factor(
        DIABETES.MELLITUS,
        levels = c(1, 2),
        labels = c("No", "Yes")
      )
    )
    colnames(NewData) <- c(TrainingData.ColNames)
    PredictedValueCount <-
      round(predict(fit.random.forest.train, NewData), 2)
    list(PredictedValueCount = PredictedValueCount)
  }
  
  # End of function ----
  
  # Function for returning zero SYNTAX score for a patient presenting an estimated probability lower than 0.50 in the zero-part model ----
  ReturnZero <- function () {
    PredictedValueCount <- 0
    list(PredictedValueCount = PredictedValueCount)
  }
  
  # End of function ----
  
  # Function for presenting graphically the importance of predictors for the zero-part of the model ----
  BorutaBinaryImportance <- function () {
    load("BorutaImportanceBinaryPart.RData")
    Boxplots <-
      ggplot(data = BorutaImportanceBinaryPart[BorutaImportanceBinaryPart$Importance >
                                                 1.7,],
             aes(
               x = reorder(Predictor, Importance),
               y = Importance,
               fill = Decision
             )) +
      geom_point(
        shape = 21,
        size = 2,
        position = position_jitterdodge(),
        color = "black",
        alpha = 0.7
      ) +
      ggbeeswarm::geom_quasirandom(
        shape = 21,
        size = 2,
        dodge.width = 0.50,
        color = "black",
        alpha = .25,
        show.legend = F
      ) +
      geom_boxplot() +
      scale_fill_manual(breaks = BorutaImportanceBinaryPart$Decision,
                        values = c("green", "red")) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = c(0.75, 0.25),
        strip.text.x = element_text(
          size = 11,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        title = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 14,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Importance", x = "Predictor",
           title = "Importance of Top Clinical Predictors") +
      coord_flip()
    list(Boxplots = Boxplots)
  }
  
  # End of function ----
  
  # Function for presenting graphically the importance of predictors for the count-part of the model ----
  BorutaCountImportance <- function () {
    load("BorutaImportanceCountPart.RData")
    Boxplots <-
      ggplot(data = BorutaImportanceCountPart[BorutaImportanceCountPart$Importance >
                                                1,],
             aes(
               x = reorder(Predictor, Importance),
               y = Importance,
               fill = Decision
             )) +
      geom_point(
        shape = 21,
        size = 2,
        position = position_jitterdodge(),
        color = "black",
        alpha = 0.7
      ) +
      ggbeeswarm::geom_quasirandom(
        shape = 21,
        size = 2,
        dodge.width = 0.50,
        color = "black",
        alpha = .25,
        show.legend = F
      ) +
      geom_boxplot() +
      scale_fill_manual(values = c("green", "red")) +
      theme_solarized(light = FALSE) +
      removeGrid(x = TRUE, y = TRUE) +
      theme(
        legend.position = c(0.75, 0.25),
        strip.text.x = element_text(
          size = 11,
          hjust = 0.5,
          vjust = 0.5,
          face = 'bold'
        ),
        axis.text.x = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.text.y = element_text(
          face = "bold",
          color = "white",
          size = 11
        ),
        axis.title.x = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        axis.title.y = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        title = element_text(
          face = "bold",
          color = "white",
          size = 18
        ),
        legend.text = element_text(
          size = 14,
          face = 'bold',
          color = "white"
        )
      ) +
      labs(y = "Importance", x = "Predictor",
           title = "Importance of Top Clinical Predictors") +
      coord_flip()
    
    list(Boxplots = Boxplots)
  }
  
  # End of function ----
  
  # Descriptive Item (server-side) ----
  
  # Descriptive results for History records ----
  HistoryDescrResults <- reactive ({
    HistoricalAnalysisFnct()
  })
  
  output$HistoryBarplots <- renderPlot({
    HistoryDescrResults()$Barplots
  })
  
  output$HistoryHistogram <- renderPlot({
    HistoryDescrResults()$HistogramPlot
  })
  
  output$HistoryDescriptiveTable <- renderDataTable(
    HistoryDescrResults()$DescriptiveTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Descriptive results for Entry records ----
  EntryDescrResults <- reactive ({
    EntryAnalysisFnct()
  })
  
  output$EntryBarplots <- renderPlot({
    EntryDescrResults()$Barplots
  })
  
  output$EntryHistogram <- renderPlot({
    EntryDescrResults()$HistogramPlots
  })
  
  output$EntryDescriptiveTable <- renderDataTable(
    EntryDescrResults()$DescriptiveTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Descriptive results for Biochemical records ----
  BiochemicalDescrResults <- reactive ({
    BiochemicalAnalysisFnct()
  })
  
  output$BiochemicalHistogram <- renderPlot({
    BiochemicalDescrResults()$HistogramPlots
  })
  
  output$BiochemicalDescriptiveTable <- renderDataTable(
    BiochemicalDescrResults()$DescriptiveTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Descriptive results for Complete Blood Count records ----
  BloodDescrResults <- reactive ({
    CBCAnalysisFnct()
  })
  
  output$BloodHistogram <- renderPlot({
    BloodDescrResults()$HistogramPlots
  })
  
  output$BloodDescriptiveTable <- renderDataTable(
    BloodDescrResults()$DescriptiveTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Descriptive results for Differential records ----
  DifferentialDescrResults <- reactive ({
    DifferentialAnalysisFnct()
  })
  
  output$DifferentialBarplots <- renderPlot({
    DifferentialDescrResults()$Barplots
  })
  
  # Exploratory Item (server-side) ----
  
  # Exploratory results for History records ----
  output$HistoryExpBoxplots <- renderPlot({
    HistoryDescrResults()$Boxplots
  })
  
  output$HistoryExploratoryTable <- renderDataTable(
    HistoryDescrResults()$ExploratoryTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Exploratory results for Entry records ----
  output$EntryExpBoxplots <- renderPlot({
    EntryDescrResults()$Boxplots
  })
  
  output$EntryExpCorrplot <- renderPlot({
    EntryDescrResults()$CorrPlot
  })
  
  output$EntryExploratoryTable <- renderDataTable(
    EntryDescrResults()$ExploratoryTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Exploratory results for Biochemical records ----
  output$BiochemicalExpCorrplot <- renderPlot({
    BiochemicalDescrResults()$CorrPlot
  })
  
  # Exploratory results for Complete Blood Count records ----
  output$CBCExpCorrplot <- renderPlot({
    BloodDescrResults()$CorrPlot
  })
  
  # Exploratory results for Differential records ----
  output$DifferentialExpBoxplots <- renderPlot({
    DifferentialDescrResults()$Boxplots
  })
  
  output$DifferentialExploratoryTable <- renderDataTable(
    DifferentialDescrResults()$ExploratoryTable,
    rownames = FALSE,
    options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE
    )
  )
  
  # Inferential Item (server-side) ----
  
  # Inferential results for History records ----
  output$HistoryHypTable <- renderDataTable(
    datatable(
      HistoryDescrResults()$HypothesisTable,
      rownames = FALSE,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    ) %>% formatStyle(column = 'p',
                      color = styleInterval(0.05, c("darkorange", NA)))
  )
  
  # Inferential results for Entry records ----
  output$EntryHypTable <- renderDataTable(
    datatable(
      EntryDescrResults()$HypothesisTable,
      rownames = FALSE,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    ) %>%
      formatStyle(column = 'p',
                  color = styleInterval(0.05, c("darkorange", NA)))
  )
  
  # Inferential results for Biochemical records ----
  output$BiochemicalHypTable <- renderDataTable(
    datatable(
      BiochemicalDescrResults()$HypothesisTable,
      rownames = FALSE,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    ) %>%
      formatStyle(column = 'p',
                  color = styleInterval(0.05, c("darkorange", NA)))
  )
  
  # Inferential results for Complete Blood Count records ----
  output$BloodHypTable <- renderDataTable(
    datatable(
      BloodDescrResults()$HypothesisTable,
      rownames = FALSE,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    ) %>%
      formatStyle(column = 'p',
                  color = styleInterval(0.05, c("darkorange", NA)))
  )
  
  # Inferential results for Differential records ----
  output$DifferentialHypTable <- renderDataTable(
    datatable(
      DifferentialDescrResults()$HypothesisTable,
      rownames = FALSE,
      options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    ) %>%
      formatStyle(column = 'p',
                  color = styleInterval(0.05, c("darkorange", NA)))
  )
  
  # Predictive Item (server-side) ----
  
  # Results for selection of predictors for the zero- and count-parts of the model  ----
  BorutaBinaryImportanceResults <- reactive ({
    BorutaBinaryImportance()
  })
  
  output$BorutalFeaturesBinaryBoxplots <- renderPlot({
    BorutaBinaryImportanceResults()$Boxplots
  })
  
  BorutaCountImportanceResults <- reactive ({
    BorutaCountImportance()
  })
  
  output$BorutalFeaturesCountBoxplots <- renderPlot({
    BorutaCountImportanceResults()$Boxplots
  })
  
  # Inputs for selected predictors of the model ----
  observeEvent(input$PREDICTIONRun,
               {
                 # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
                 withBusyIndicatorServer("PREDICTIONRun",
                                         {
                                           Sys.sleep(1)
                                         })
               })
  
  # Zero-part  ----
  PredictionBinaryRFResults <- reactive ({
    input$PREDICTIONRun
    isolate({
      PredictionBinaryRandomForest(
        CHEST.PAIN = as.numeric(input$inCheckboxCHESTPAINZeroPart),
        STTCHANGES = as.numeric(input$inCheckboxSTCHANGESZeroPart),
        GLU = as.numeric(input$inCheckboxGLUBothPart),
        ATRIALFIBRILLATION = as.numeric(input$inCheckboxATRIALFIBRILLATIONZeroPart),
        GENDER = as.numeric(input$inCheckboxGENDERZeroPart),
        EASYFATIGUE = as.numeric(input$inCheckboxEASYFATIGUEZeroPart),
        AORTICANEURYSMS = as.numeric(input$inCheckboxAORTICANEURYSMSZeroPart),
        RATIO1 = as.numeric(input$inCheckboxRATIO1ZeroPart),
        NEU = as.numeric(input$inCheckboxNEUZeroPart),
        GRACESCORE = as.numeric(input$inCheckboxGRACESCOREBothPart),
        RATIO3 = as.numeric(input$inCheckboxRATIO3ZeroPart),
        SGOT = as.numeric(input$inCheckboxSGOTZeroPart),
        EOS = as.numeric(input$inCheckboxEOSZeroPart)
      )
    })
  })
  
  colour_InfoBoxZeroPart <- function(PredictedClass) {
    cut(
      as.numeric(PredictionBinaryRFResults()$PredictedValue),
      breaks = c(-Inf, 0.499, Inf),
      labels = c("green", "orange"),
      right = FALSE
    )
  }
  
  output$PredictedClass <- renderInfoBox({
    validate(
      need(
        input$PREDICTIONRun,
        "Please type in the values of Predictors and then Run button"
      )
    )
    infoBox(
      "Probability of presenting non-zero Syntax Score",
      value = PredictionBinaryRFResults()$PredictedValue ,
      color = colour_InfoBoxZeroPart(),
      icon = icon("info-circle")
    )
  })
  
  # Count Part ----
  PredictionCountRFResults <- reactive ({
    input$PREDICTIONRun
    isolate({
      if (PredictionBinaryRFResults()$PredictedValue <= 0.49999) {
        ReturnZero()
      } else{
        PredictionCountRandomForest(
          CRUSADE.SCORE = as.numeric(input$inCheckboxCRUSADESCORECountPart),
          GFR = as.numeric(input$inCheckboxGFRCountPart),
          AGE = as.numeric(input$inCheckboxGFRCountPart),
          QRS.DURATION.ms = as.numeric(input$inCheckboxQRSDURATIONCountPart),
          GLU = as.numeric(input$inCheckboxGLUBothPart),
          HCT = as.numeric(input$inCheckboxHCTCountPart),
          HGB = as.numeric(input$inCheckboxHGBCountPart),
          DIABETES.MELLITUS = as.numeric(input$inCheckboxDIABETESMELLITUSCountPart),
          UREA = as.numeric(input$inCheckboxUREACountPart),
          GRACESCORE = as.numeric(input$inCheckboxGRACESCOREBothPart)
          
        )
      }
      
    })
  })
  
  output$PredictedCount <- renderInfoBox({
    validate(
      need(
        input$PREDICTIONRun,
        "Please type in the values of Predictors and then Run button"
      )
    )
    infoBox(
      "Total Syntax Score",
      value = PredictionCountRFResults()$PredictedValueCount ,
      color = "blue",
      icon = icon("line-chart")
    )
  })
})

  
  
  
  

