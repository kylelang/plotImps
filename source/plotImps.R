### Title:    Create Imputation Diagnostic Plots
### Author:   Kyle M. Lang
### Created:  2015-OCT-02
### Modified: 2017-MAR-30

plotImps <- function(impList, rMat, typeVec, targetVar = NULL, interactive = FALSE)
{
    nImps          <- length(impList)
    missData       <- impList[[1]]
    missData[rMat] <- NA

    if(is.null(targetVar)) {
        levVec <- unlist(
            lapply(missData, function(x) length(unique(na.omit(x))))
            )
        indexVec <- which(colSums(rMat) > 0 & levVec > 1 & typeVec != "drop")
    } else if(is.character(targetVar)) {
        indexVec <- which(colnames(missData) %in% targetVar)
    } else {
        indexVec <- targetVar
    }

    for(i in indexVec) {
        target     <- missData[ , i]
        targetName <- colnames(missData)[i]
        catTarget  <- typeVec[i] %in% c("nominal", "ordinal")

        if(catTarget)
            imps <- lapply(impList,
                           function(x, rMat, index)
                                        #as.numeric(factor(x[rMat[ , index], index])),
                               x[rMat[ , index], index],
                               rMat  = rMat,
                           index = i)
        else
            imps <- lapply(impList,
                           function(x, rMat, index) x[rMat[ , index], index],
                           rMat  = rMat,
                           index = i)

        if(catTarget) {
            hMat <- rbind(table(target), do.call("rbind", lapply(imps, table)))
            yMax <- max(hMat, na.rm = TRUE)

            barplot(height = hMat,
                    beside = TRUE,
                    col    = c("blue", rep("red", nImps)),
                    border = c("blue", rep("red", nImps)),
                    ylim   = c(0, yMax),
                    space  = c(0, nImps * 0.1),
                    width  = c(100, rep(1, nImps)),
                    main   =
                        paste0("Imputed (Red) vs. Observed (Blue) Values\nof ",
                               targetName),
                    ylab   = "Frequency",
                    xlab   = paste0("Level of ", targetName)
                    )
        } else {
            dens1 <- density(target, na.rm = TRUE)
            if(sum(rMat[ , i]) > 1) {
                dens2       <- lapply(imps, density)
                dens2RangeY <- range(lapply(dens2, function(x){x$y}))
                dens2RangeX <- range(lapply(dens2, function(x){x$x}))
            } else {
                dens2       <- imps
                dens2RangeY <- 0
                dens2RangeX <- range(unlist(imps))
            }
            yRange  <- range(dens1$y, dens2RangeY, na.rm = TRUE)
            xRange  <- range(dens1$x, dens2RangeX, na.rm = TRUE)

            plot(NULL,
                 main =
                     paste0("Densities of Imputations (Red) vs.\n",
                            "Observed Data (Blue) for ",
                            targetName),
                 ylab = "Density",
                 xlab = paste0("Value of ", targetName),
                 ylim = yRange,
                 xlim = xRange
                 )
            if(sum(rMat[ , i]) > 1)
                for(m in 1 : length(impList)) lines(dens2[[m]], col = "red")
            else
                for(m in 1 : length(impList))
                    points(y = 0, x = dens2[[m]], col = "red")

            lines(dens1, lwd = 3, col = "blue")
        }
        if(interactive & length(indexVec) > 1)
            readline("Hit any key to generate the next plot... ")
    }
}# END plotImps()

