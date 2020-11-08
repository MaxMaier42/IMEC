#Package functions IMEC

#helper function fro set edge matrix
setconnection <- function(m, x1, x2, weight)
{
    m[rownames(m) == x1, colnames(m) == x2] <- weight
    m[rownames(m) == x2, colnames(m) == x1] <- weight
    return(m)
}
#'Initialize the Explanatory Network
#'
#'This function initializes the network in which explanatory relations can be stored later.
#'
#'@param phenomena Vector of phenomena that are explained
#'@param theory1 Vector of propositions included in theory 1
#'@param theory2 Vector of propositions included in theory 2 (only set manually if theory comparison is intended)
#'
#'@return An empty edge matrix (all edges 0).
#'
#'@examples
#'#Comparison of Oxygen and Phlogiston Theory of Combustion
#'Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
#'Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
#'Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
#'evidence <- c(rep(1,8))
#'## oxygen and phlogiston
#'explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
#'#oygen explanations
#'explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
#'explanations <- explain(c("OH1", "OH3"), "E3", explanations)
#'explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E5", explanations)
#'explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E7", explanations)
#'explanations <- explain(c("OH1", "OH6"), "E8", explanations)
#'#phlogiston explanations
#'explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
#'explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
#'explanations <- explain(c("PH5", "PH6"), "E5", explanations)
#'#contradictions
#'explanations <- contradict("PH3", "OH3", explanations)
#'explanations <- contradict("PH6", "OH5", explanations)
#'IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
#'plot(IMEC)
#'summary(IMEC)
#'
#'@export
initializeNetwork <- function(phenomena, theory1, theory2 = character()) {
    propositions <- c(theory1, theory2)
    size <- length(propositions) + length(phenomena)
    edgematrix <- matrix(rep(0, size^2), nrow = size)
    rownames(edgematrix) <- c(propositions, phenomena)
    colnames(edgematrix) <- c(propositions, phenomena)
    return(edgematrix)
}

#'explain
#'
#'Sets an explanatory relation between a set of propositions and a phenomenon.
#'If more than one proposition is used the edge weight will be reduced accordingly.
#'
#'@param Explanation Vector of Explanations that explain the Explanadum
#'@param Explanandum A proposition or phenomenon that is explained
#'@param matrix Matrix of Explanatory relations that is modified
#'@param weight Strength of connection (i.e., quality of explanation)
#'
#'@return Returns the explanatory matrix with the edge weights modified according
#'to the specified explanation
#'@examples
#'#Comparison of Oxygen and Phlogiston Theory of Combustion
#'Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
#'Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
#'Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
#'evidence <- c(rep(1,8))
#'## oxygen and phlogiston
#'explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
#'#oygen explanations
#'explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
#'explanations <- explain(c("OH1", "OH3"), "E3", explanations)
#'explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E5", explanations)
#'explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E7", explanations)
#'explanations <- explain(c("OH1", "OH6"), "E8", explanations)
#'#phlogiston explanations
#'explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
#'explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
#'explanations <- explain(c("PH5", "PH6"), "E5", explanations)
#'#contradictions
#'explanations <- contradict("PH3", "OH3", explanations)
#'explanations <- contradict("PH6", "OH5", explanations)
#'IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
#'plot(IMEC)
#'summary(IMEC)
#'
#'@export
explain <- function(Explanation, Explanandum, matrix, weight = 1){
    if (length(Explanandum) > 1) {
        stop("Only one explanandum allowed")
    }
    if (length(weight) > 1) {
        stop("only one weight allowed")
    }
    weight <- weight/length(Explanation)
    print(paste("The used edge weight is", weight))
    for (i in 1:length(Explanation)) {
        matrix <- setconnection(matrix, Explanation[i], Explanandum, weight)
    }
    return(matrix)
}


#'contradict
#'
#'Sets a contradictory relation between a set of propositions and a phenomenon.
#'If more than one proposition is used the edge weight will be reduced accordingly.
#'
#'@param Explanation Vector of Explanations that explain the Explanadum
#'@param Explanandum A proposition or phenomenon that is explained
#'@param matrix Matrix of Explanatory relations that is modified
#'@param weight Strength of connection (i.e., strength of contradiction)
#'
#'#'@return returns the explantory matrix with the edge weights modified according
#'to the specified contradiction
#'
#'@examples
#'#Comparison of Oxygen and Phlogiston Theory of Combustion
#'Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
#'Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
#'Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
#'evidence <- c(rep(1,8))
#'## oxygen and phlogiston
#'explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
#'#oygen explanations
#'explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
#'explanations <- explain(c("OH1", "OH3"), "E3", explanations)
#'explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E5", explanations)
#'explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E7", explanations)
#'explanations <- explain(c("OH1", "OH6"), "E8", explanations)
#'#phlogiston explanations
#'explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
#'explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
#'explanations <- explain(c("PH5", "PH6"), "E5", explanations)
#'#contradictions
#'explanations <- contradict("PH3", "OH3", explanations)
#'explanations <- contradict("PH6", "OH5", explanations)
#'IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
#'plot(IMEC)
#'summary(IMEC)
#'
#'@export
contradict <- function(Explanation, Explanandum,  matrix, weight = 4){
    if (length(Explanandum) > 1) {
        stop("Only one explanandum allowed")
    }
    if (length(weight) > 1) {
        stop("only one weight allowed")
    }
    weight <- weight/length(Explanation)
    print(paste("The used edge weight is", -weight))
    for (i in 1:length(Explanation)) {
        matrix <- setconnection(matrix, Explanation[i], Explanandum, -weight)
    }
    return(matrix)
}

#'Computes the Ising Model of Explanatory Coherence.
#'
#'Computes IMEC based on previously specified explanatory relations.
#'
#'@param matrix matrix of explanatory relations.
#'@param evidence vector of evidence for phenomena.
#'@param phenomena vector of phenomena should be the same length as evidence.
#'@param theory1 vector of propositions in theory1.
#'@param theory2 vector of propositions in theory2.
#'@param analytic whether the result should be calculated analytically or (for large networks) estimated using
#'Metropolis-Hastings algorithm enhanced with Coupling from the past (Murray, 2007).
#'@param analogy this argument is only for purposes of adding analogy in the future and should currently not be used.
#'@return returns an IMEC object which contains the explanatory coherence of the propositions, the explanatory relations,
#'the evidence, and the phenomena
#'
#'@examples
#'#Comparison of Oxygen and Phlogiston Theory of Combustion
#'Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
#'Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
#'Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
#'evidence <- c(rep(1,8))
#'## oxygen and phlogiston
#'explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
#'#oygen explanations
#'explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
#'explanations <- explain(c("OH1", "OH3"), "E3", explanations)
#'explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E5", explanations)
#'explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E7", explanations)
#'explanations <- explain(c("OH1", "OH6"), "E8", explanations)
#'#phlogiston explanations
#'explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
#'explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
#'explanations <- explain(c("PH5", "PH6"), "E5", explanations)
#'#contradictions
#'explanations <- contradict("PH3", "OH3", explanations)
#'explanations <- contradict("PH6", "OH5", explanations)
#'IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
#'plot(IMEC)
#'summary(IMEC)
#'
#'@export
computeIMEC <- function(matrix, evidence, phenomena, theory1, theory2 = character() ,analytic = T, analogy = numeric())
{
    propositions <- c(theory1, theory2)
    if (length(phenomena) != length(evidence)) {
        stop("Every phenomenon needs some evidence! Make sure the legth of evidence and phenomena is the same.")
    }
    Labels <- c(propositions, phenomena)
    theory <- 1:length(propositions)
    if (length(analogy) == 0)        {
        thresholds <- c(rep(0, length(theory)), evidence) # Thresholds for propositions are zero and thresholds for phenomena specified by use
    }
    else if (length(analogy) == length(theory)){
        thresholds <- c(analogy, evidence) # Thresholds for propositions are zero and thresholds for phenomena specified by use
    }
    else {
        stop("Analaogy must be length zero or length of theory")
    }
    if (analytic == T) {
        res <- IsingSampler::IsingLikelihood(matrix,thresholds, beta = 1, response = c(-1,1))
        coherenceT1 <- numeric()
        for (i in 1:length(theory1)) {
            cp <- subset(res, res[i+1] == 1)
            cp <-sum(cp$Probability)
            coherenceT1 <- c(coherenceT1, cp)
        }
        startP <- length(propositions)+2
        coherencePhenomena <- numeric()
        for (i in startP:ncol(res)) {
            cp <- subset(res, res[i] == 1)
            cp <-sum(cp$Probability)
            coherencePhenomena <- c(coherencePhenomena, cp)
        }
        if (length(theory2) < 1)   {
            results <- list(list(theory1, as.numeric(coherenceT1)), phenomena, coherencePhenomena,evidence, analogy, matrix)
            names(results) <- c("ExplanatoryCoherenceT1", "Phenomena", "CredibilityOfPhenomena", "Evidence", "Analogy", "Explanations")
        } else {
            coherenceT2 <- numeric()
            for (i in (length(theory1)+1):length(propositions)){
                cp <- subset(res, res[i+1] == 1)
                cp <-sum(cp$Probability)
                coherenceT2 <- c(coherenceT2, cp)
            }
            results <- list(list(theory1, as.numeric(coherenceT1)), list(theory2, as.numeric(coherenceT2)), phenomena, coherencePhenomena, evidence,analogy, matrix)
            names(results) <- c("ExplanatoryCoherenceT1","ExplanatoryCoherenceT2", "Phenomena","CredibilityOfPhenomena", "Evidence","Analogy", "Explanations")
        }
    } else {
        res <- IsingSampler::IsingSampler(10000, matrix, thresholds, beta = 1, response = c(-1,1), method ="CFTP")
        coherenceT1 <- numeric()
        for (i in 1:length(theory1)) {
            cp <- subset(res, res[,i] == 1)
            cp <- nrow(cp)/10000
            coherenceT1 <- c(coherenceT1, cp)
        }
        startP <- length(propositions)+1
        coherencePhenomena  <- numeric()
        for (i in startP:ncol(res)) {
            cp <- subset(res, res[,i] == 1)
            cp <- nrow(cp)/10000
            coherencePhenomena <- c(coherencePhenomena, cp)
        }

        if (length(theory2) < 1)   {
            results <- list(list(theory1, as.numeric(coherenceT1)), phenomena, coherencePhenomena, evidence,analogy, matrix)
            names(results) <- c("ExplanatoryCoherenceT1", "Phenomena","CredibilityOfPhenomena", "Evidence","Analogy", "Explanations")
        } else {
            coherenceT2 <- numeric()
            for (i in (length(theory1)+1):length(propositions)){
                cp <- subset(res, res[,i] == 1)
                cp <- nrow(cp)/10000
                coherenceT2 <- c(coherenceT2, cp)
            }
            results <- list(list(theory1, as.numeric(coherenceT1)), list(theory2, as.numeric(coherenceT2)), phenomena, coherencePhenomena, evidence,analogy, matrix)
            names(results) <- c("ExplanatoryCoherenceT1","ExplanatoryCoherenceT2", "Phenomena","CredibilityOfPhenomena", "Evidence","Analogy", "Explanations")
        }
    }

    class(results) <- "IMEC"
    return(results)
}

#'Plots the Explanatory Relations
#'
#'Plot the explanatory relations between data and phenomena. A window will open where you
#'can drag the nodes in the intended position. Then press enter to plot the network.
#'
#'@param x Object of the class IMEC as returned by computeIMEC
#'@param nodesize size of vertices in the plotted network
#'@param ... other parameters passed on to S3 method.
#'@examples
#'#Comparison of Oxygen and Phlogiston Theory of Combustion
#'Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
#'Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
#'Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
#'evidence <- c(rep(1,8))
#'## oxygen and phlogiston
#'explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
#'#oygen explanations
#'explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
#'explanations <- explain(c("OH1", "OH3"), "E3", explanations)
#'explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E5", explanations)
#'explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E7", explanations)
#'explanations <- explain(c("OH1", "OH6"), "E8", explanations)
#'#phlogiston explanations
#'explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
#'explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
#'explanations <- explain(c("PH5", "PH6"), "E5", explanations)
#'#contradictions
#'explanations <- contradict("PH3", "OH3", explanations)
#'explanations <- contradict("PH6", "OH5", explanations)
#'IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
#'plot(IMEC)
#'summary(IMEC)
#'
#'@export
plot.IMEC <- function(x, nodesize = 10,...) {
    IMEC <- x
    matrix <- IMEC$Explanations
    phenomena <- IMEC$Phenomena
    theory1 <- IMEC$ExplanatoryCoherenceT1[[1]]
    if (length(IMEC) > 4) {
        theory2 <- IMEC$ExplanatoryCoherenceT2[[1]]
    } else {
        theory2 <- character()
    }
    propositions  <- c(theory1, theory2)
    g  <- igraph::graph_from_adjacency_matrix(matrix, mode = "undirected", weighted = TRUE)
    colors <- c(rep("yellow", length(theory1)), rep("green", length(theory2)), rep("orangered", length(phenomena)))
    shapes <- c(rep("circle", length(theory1)), rep("circle", length(theory2)), rep("square", length(phenomena)))
    yp <- c(rep(1, length(theory1)), rep(10, length(theory2)), rep(5, length(phenomena)))
    if (length(theory2) > 0) {
        xp <- c(1:length(theory1),1:length(theory2),1:length(phenomena))*20
    } else {
        xp <- c(1:length(theory1),1:length(phenomena))*200
    }
    positions <- as.matrix(cbind(xp, yp))
    tk <- igraph::tkplot(g, vertex.color = colors, canvas.width = 1000)
    igraph::tk_set_coords(tk, positions)
    igraph::tk_center(tk)
    igraph::tk_fit(tk)
    readline(prompt="Press [enter] to continue")
    co <- igraph::tkplot.getcoords(tk)
    qgraph::qgraph(matrix, layout = co, color = colors, shape = shapes, vsize = nodesize, theme = "colorblind")
}

#'Summary of an IMEC object.
#'@param object IMEC object.
#'@param ... other paremeters passed on from S3 method.
#'@examples
#'#Comparison of Oxygen and Phlogiston Theory of Combustion
#'Phenomena <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
#'Oxygen <- c("OH1", "OH2", "OH3", "OH4", "OH5", "OH6")
#'Phlogiston <- c("PH1", "PH2", "PH3", "PH4", "PH5", "PH6")
#'evidence <- c(rep(1,8))
#'## oxygen and phlogiston
#'explanations <- initializeNetwork(Phenomena, Oxygen, Phlogiston)
#'#oygen explanations
#'explanations <- explain(c("OH1", "OH2", "OH3"), "E1", explanations)
#'explanations <- explain(c("OH1", "OH3"), "E3", explanations)
#'explanations <- explain(c("OH1", "OH3", "OH4"), "E4", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E5", explanations)
#'explanations <- explain(c("OH1", "OH4", "OH5"), "E6", explanations)
#'explanations <- explain(c("OH1", "OH5"), "E7", explanations)
#'explanations <- explain(c("OH1", "OH6"), "E8", explanations)
#'#phlogiston explanations
#'explanations <- explain(c("PH1", "PH2", "PH3"), "E1", explanations)
#'explanations <- explain(c("PH1", "PH3", "PH4"), "E2", explanations)
#'explanations <- explain(c("PH5", "PH6"), "E5", explanations)
#'#contradictions
#'explanations <- contradict("PH3", "OH3", explanations)
#'explanations <- contradict("PH6", "OH5", explanations)
#'IMEC <- computeIMEC(explanations,evidence, Phenomena, Oxygen, Phlogiston)
#'plot(IMEC)
#'summary(IMEC)
#'
#'@export
summary.IMEC <- function(object,...) {
    IMEC <- object
    if (length(IMEC$ExplanatoryCoherenceT2) > 0) {
        ret1 <- data.frame(IMEC$ExplanatoryCoherenceT1[[1]], IMEC$ExplanatoryCoherenceT1[[2]])
        names(ret1) <- c("T1", "EC_T1")
        ret2 <- data.frame(IMEC$ExplanatoryCoherenceT2[[1]], IMEC$ExplanatoryCoherenceT2[[2]])
        names(ret2) <- c("T2", "EC_T2")
        return(list(ret1, ret2))
    } else {
        ret <- data.frame(IMEC$ExplanatoryCoherenceT1[[1]], IMEC$ExplanatoryCoherenceT1[[2]])
        names(ret) <- c("T1", "EC_T1")
        return(ret)
    }
}
