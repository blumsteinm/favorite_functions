##################################################
## Useful Functions
##################################################

## Running Mean
running_filter <- function(x, y, n = 10, func = "mean"){
  
  ## Order the data based on x so running mean is logical
  df <- data.frame(x = x, y = y)
  df <- df[order(df$x),]
  x <- df$x
  y <- df$y
  
  if(func == "mean"){
    y_out <- c()
    for(i in 1:nrow(df)){
      if(i < n){
        val <- mean(y[1:i], na.rm = T)
      }else{
        val <- mean(y[(i-n):i], na.rm = T)
      }
      y_out <- c(y_out, val)
    }
    output_df <- data.frame(x = x, y = y_out)
    return(output_df)
  }
  if(func == "sd"){
    y_out <- c()
    for(i in 1:nrow(df)){
      if(i < n){
        val <- sd(y[1:i], na.rm = T)
      }else{
        val <- sd(y[(i-n):i], na.rm = T)
      }
      y_out <- c(y_out, val)
    }
    output_df <- data.frame(x = x, y = y_out)
    output_df[1:2,2] <- mean(output_df$y, na.rm = T)
    return(output_df)
  }
  
  
}

## Round DF Function
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, function(x) class(x)) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

## Histograms as Proportions
hist_prop <- function(x, ...){
  h <- hist(x, plot = F)
  vals <- h$counts/sum(h$counts)
  b <- barplot(vals, border = "white", col = "forestgreen", ylim = range(pretty(vals)), ...)
  # axis(side = 1, at = b, h$mids )
  text(b, -0.01, xpd = T,h$mids, srt = 45)
}

## Switch all factors to characters
unfactor_df <- function(x, numeric_or_character = "character", ignore.name = NULL) {
     
    ignore_cols <- which(colnames(x) %in% ignore.name)
    factor_columns <- sapply(x, function(x) class(x)) == 'factor'
    factor_columns[ignore_cols] <- FALSE ## Change columns that are factors to is.factor = F if you want to leave untouched
     
     if(numeric_or_character == "character"){
       x[factor_columns] <-  apply( x[factor_columns], 1:2, function(x) as.character(x) )
     }
     if(numeric_or_character == "numeric"){
       x[factor_columns] <-  apply( x[factor_columns], 1:2, function(x) as.numeric(x) )
     }
     
     return(x)
}

factor_df <- function(x) {
     character_columns <- sapply(x, function(x) class(x)) == 'character'
     x[character_columns] <-  lapply( x[character_columns], as.factor )
     return(x)
}

## Instead of plotting frequency or density, plot the proportion of data in each bin, for data comparison
hist_percentage <- function(data1 = data1, data2 = data2, col1 = col1, border1 = border1, col2 = col2, border2 = border2){
  
  ## Load Data and find densities
  h1 <- hist(data1, plot = F)
  h2 <- hist(data2, plot = F)

  ## Find the distance between breaks to generate the x-length of each bar
  xlen_1 <- mean(diff(h1$breaks))
  xlen_2 <- mean(diff(h2$breaks))
  
  ## If the x-values are on different scales, readjust smaller scale
  if(xlen_1 < xlen_2){
    n_breaks <- length(h1$breaks)
    new_breaks <- seq( h1$breaks[1], h1$breaks[n_breaks], xlen_2)
    n_breaks <- length(new_breaks)
    h1 <- hist(data1, breaks = n_breaks, plot = F)
  }
  if(xlen_2 < xlen_1){
    n_breaks <- length(h2$breaks)
    new_breaks <- seq( h2$breaks[1], h2$breaks[n_breaks], xlen_1)
    n_breaks <- length(new_breaks)
    h2 <- hist(data2, breaks = n_breaks, plot = F)
  }
  
  ## Convert to proportion of data
  h1$density <- h1$counts / sum(h1$counts)
  h2$density <- h2$counts / sum(h2$counts)
  
  ## Recalculate x-bar lengths
  xlen_1 <- mean(diff(h1$breaks))
  xlen_2 <- mean(diff(h2$breaks))
  
  xlen <- max(xlen_1, xlen_2)/2
  
  ## Set up rectangle coordinates
  coord_df_1 <- data.frame(x1 = h1$mids - xlen, 
                           x2 = h1$mids - xlen, 
                           x3 = h1$mids + xlen,
                           x4 = h1$mids + xlen,
                           y1 = 0, y2 = h1$density, y3 = h1$density, y4 = 0)
  coord_df_2 <- data.frame(x1 = h2$mids - xlen, 
                           x2 = h2$mids - xlen, 
                           x3 = h2$mids + xlen,
                           x4 = h2$mids + xlen,
                           y1 = 0, y2 = h2$density, y3 = h2$density, y4 = 0)
  
  ## Draw rectangles
  apply(coord_df_1, 1, function(x) polygon(x[1:4], x[5:8], col = col1, border = border1))
  apply(coord_df_2, 1, function(x) polygon(x[1:4], x[5:8], col = col2, border = border2))
}


## Color Ramp for rasters
clr_ramp <- function(n, plotclr, text, horizontal = F, xl = 280000, xr = 290000, yb =860000 , yt = 920000, type = "categorical"){
  
  if(horizontal == F){
    
    ## Set up Plot Parameters
    par(mar = c(0,0,0,0))
    color_ramp <- colorRampPalette(plotclr)(n)
    
    ## Draw rectangles 
    jump <- (yt - yb) / n
    y_corners <- seq(yb, yt, by = jump)
    if(type == "categorical"){y_midpoints <- y_corners[-(n+1)] + jump/2}
    if(type == "continuous"){y_midpoints <- y_corners}
    for(i in 1:(length(y_corners)-1)){
      rect(xl, y_corners[i], xr, y_corners[i+1], col = color_ramp[i], border = NA)
    }
    
    ## Add Text
    if(length(text) == 2){y_midpoints <- c(y_midpoints[1], y_midpoints[n])}
    text(xr, y_midpoints, text, family = "serif",adj = c(0,0), cex = 1)
  }
  par(mar = c(4.5, 4.5, 0, 0))
}

sunCalc<-function(d,lat,long){
  if(any(is.null(c(d,lat,long)))){
    stop("day, latitude and Longitude must be supplied")
  }
  
  date<-as.Date(d,format="%Y-%m-%d")
  
  decimal.day<-as.numeric(format(date,format="%j"))
  
  ## Function to convert degrees to radians
  rad<-function(x)pi*x/180
  
  ##Radius of the earth (km)
  R=6378
  
  ##Radians between the xy-plane and the ecliptic plane
  epsilon=rad(23.45)
  
  ##Convert observer's latitude to radians
  L=rad(lat)
  
  ## Calculate offset of sunrise based on longitude (min)
  ## If Long is negative, then the mod represents degrees West of
  ## a standard time meridian, so timing of sunrise and sunset should
  ## be made later.
  timezone = -4*(abs(long)%%15)*sign(long)
  
  ## The earth's mean distance from the sun (km)
  r = 149598000
  
  theta = 2*pi/365.25*(decimal.day-80)
  
  z.s = r*sin(theta)*sin(epsilon)
  r.p = sqrt(r^2-z.s^2)
  
  t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))
  
  ##a kludge adjustment for the radius of the sun
  that = t0+5 
  
  ## Adjust "noon" for the fact that the earth's orbit is not circular:
  n = 720-10*sin(4*pi*(decimal.day-80)/365.25)+8*sin(2*pi*decimal.day/365.25)
  
  ## now sunrise and sunset are:
  sunrise = (n-that+timezone)/60
  sunset = (n+that+timezone)/60
  
  srH<-floor(sunrise)
  srM<-floor((sunrise-srH)*60)
  
  ssH<-floor(sunset)
  ssM<-floor((sunset-ssH)*60)
  
  SR<-paste(date,paste(srH, srM,sep=":"),sep=" ")
  SS<-paste(date,paste(ssH, ssM,sep=":"),sep=" ")
  
  return(list("sunrise" = SR,"sunset" = SS, "daylength" = sunset - sunrise))
}

##################################################
## Color Pallettes 
##################################################
choose_colors <- function(alpha){
  
                default_par <- par()
                alpha <- alpha
                
                color_list <- list(maxres(alpha), cinmint(alpha), instaCl(alpha), skittles(alpha), landuse(alpha), porcelin(alpha), castle(alpha),
                                   beachGlass(alpha), gainingHeat(alpha), fishtank(alpha), sunsetCamping(alpha), comics(alpha), 
                                   dark_ocean(alpha), ocean(alpha), long(alpha), latitude_cl(alpha), pistachio(alpha))
                color_nmes <- c("maxres", "cinmint", "instaCl", "skittles", "landuse", "porcelin", "castle", "beachGlass", "gainingHeat", 
                                "fishtank", "sunsetCamping", "comics", "dark_ocean", "ocean", "long", "latitude_cl", "pistachio")
                par(mfrow = c(4, 3), mar = c(0, 0.2, 2, 0.2))
                for(i in 1:length(color_list)){
                  col_subset <- color_list[[i]]
                  barplot(rep(1, length(col_subset)), col = col_subset, border = "white", axes = F, xlim = c(0, (length(col_subset) + 1.5)), ylim = c(-0.5, 1.5))
                  title(main = color_nmes[i])
                  axis(side = 4, lwd = 5, lwd.ticks = 0, labels = F)
                }
                par <- default_par
}
pistachio <- function(alpha){
  col_pal <- c(
    rgb(175, 204, 153, max = 255, alpha = alpha),
    rgb(103, 126, 82, max = 255, alpha = alpha),
    rgb(182, 202, 121, max = 255, alpha = alpha),
    rgb(246, 232, 177, max = 255, alpha = alpha),
    rgb(137, 114, 91, max = 255, alpha = alpha)
  )
  return(col_pal)
}

latitude_cl <- function(alpha){
  alpha <- alpha 
  col_pal <- c(
    rgb(21, 72, 122, max = 255, alpha = alpha),
    rgb(14, 74, 138, max = 255, alpha = alpha),
    rgb(51, 116, 176, max = 255, alpha = alpha),
    rgb(0, 134, 168, max = 255, alpha = alpha),
    rgb(3, 146, 160, max = 255, alpha = alpha),
    rgb(7, 160, 153, max = 255, alpha = alpha),
    rgb(70, 168, 126, max = 255, alpha = alpha),
    rgb(120, 184, 100, max = 255, alpha = alpha),
    rgb(175, 184, 81, max = 255, alpha = alpha),
               rgb(240, 189, 43, max = 255, alpha = alpha),
               rgb(241, 175, 47, max = 255, alpha = alpha),
               rgb(241, 144, 57, max = 255, alpha = alpha),
               rgb(242, 113, 74, max = 255, alpha = alpha),
               rgb(240, 98, 77, max = 255, alpha = alpha),
               rgb(239, 85, 82, max = 255, alpha = alpha),
               rgb(229, 77, 70, max = 255, alpha = alpha),
               rgb(215, 68, 56, max = 255, alpha = alpha),
               rgb(149, 46, 47, max = 255, alpha = alpha),
               rgb(107, 32, 46, max = 255, alpha = alpha))
  return(col_pal)
}




maxres <- function(alpha){
            alpha <- alpha 
            col_pal <- c(rgb(2, 135, 169, max = 255, alpha = alpha),
            rgb(127, 211, 194, max = 255, alpha = alpha),
            rgb(243, 241, 209, max = 255, alpha = alpha),
            rgb(254, 147, 91, max = 255, alpha = alpha),
            rgb(240, 104, 75, max = 255, alpha = alpha))
            return(col_pal)
}
cinmint <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(20, 166, 151, max = 255, alpha = alpha),
               rgb(242, 193, 46, max = 255, alpha = alpha),
               rgb(242, 157, 53, max = 255, alpha = alpha),
               rgb(242, 118, 73, max = 255, alpha = alpha),
               rgb(242, 82, 82, max = 255, alpha = alpha))
  return(col_pal)
}
instaCl <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(37, 39, 42, max = 255, alpha = alpha),
               rgb(23, 90, 139, max = 255, alpha = alpha),
               rgb(102, 147, 178, max = 255, alpha = alpha),
               rgb(198, 200, 201, max = 255, alpha = alpha),
               rgb(240, 240, 241, max = 255, alpha = alpha))
  return(col_pal)
}
skittles <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(207, 66, 50, max = 255, alpha = alpha),
               rgb(235, 127, 35, max = 255, alpha = alpha),
               rgb(250, 192, 35, max = 255, alpha = alpha),
               rgb(6, 134, 117, max = 255, alpha = alpha),
               rgb(107, 32, 46, max = 255, alpha = alpha))
  return(col_pal)
}
landuse <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(162, 0, 1, max = 255, alpha = alpha),
               rgb(198, 76, 37, max = 255, alpha = alpha),
               rgb(249, 239, 74, max = 255, alpha = alpha),
               rgb(75, 119, 68, max = 255, alpha = alpha),
               rgb(29, 54, 74, max = 255, alpha = alpha))
  return(col_pal)
}
porcelin <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(15, 31, 56, max = 255, alpha = alpha), 
               rgb(253, 60, 60, max = 255, alpha = alpha),
               rgb(255, 183, 76, max = 255, alpha = alpha),
               rgb(19, 141, 144, max = 255, alpha = alpha), 
               rgb(253, 246, 246, max = 255, alpha = alpha))
  return(col_pal)
}
castle <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(123, 42, 59, max = 255, alpha = alpha), 
               rgb(229, 118, 97, max = 255, alpha = alpha),
               rgb(248, 197, 140, max = 255, alpha = alpha),
               rgb(248, 231, 162, max = 255, alpha = alpha), 
               rgb(134, 221, 178, max = 255, alpha = alpha))
  return(col_pal)
}
beachGlass <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(255, 246, 201, max = 255, alpha = alpha), 
               rgb(200, 232, 199, max = 255, alpha = alpha),
               rgb(164, 222, 171, max = 255, alpha = alpha),
               rgb(133, 204, 159, max = 255, alpha = alpha), 
               rgb(73, 158, 141, max = 255, alpha = alpha))
  return(col_pal)
}
gainingHeat <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(255, 230, 230, max = 255, alpha = alpha), 
               rgb(255, 172, 172, max = 255, alpha = alpha),
               rgb(255, 115, 115, max = 255, alpha = alpha),
               rgb(255, 58, 58, max = 255, alpha = alpha), 
               rgb(255, 0, 0, max = 255, alpha = alpha))
  return(col_pal)
}
fishtank <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(48, 119, 156, max = 255, alpha = alpha), 
               rgb(97, 232, 215, max = 255, alpha = alpha),
               rgb(104, 170, 71, max = 255, alpha = alpha),
               rgb(255, 220, 89, max = 255, alpha = alpha), 
               rgb(232, 98, 67, max = 255, alpha = alpha))
  return(col_pal)
}
sunsetCamping <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(46, 27, 45, max = 255, alpha = alpha), 
               rgb(84, 0, 50, max = 255, alpha = alpha),
               rgb(130, 3, 51, max = 255, alpha = alpha),
               rgb(201, 40, 62, max = 255, alpha = alpha), 
               rgb(240, 67, 58, max = 255, alpha = alpha))
  return(col_pal)
}
comics <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(191, 48, 86, max = 255, alpha = alpha), 
               rgb(79, 73, 115, max = 255, alpha = alpha),
               rgb(67, 189, 217, max = 255, alpha = alpha),
               rgb(95, 191, 80, max = 255, alpha = alpha), 
               rgb(217, 208, 89, max = 255, alpha = alpha))
  return(col_pal)
}
dark_ocean <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(16, 19, 38, max = 255, alpha = alpha), 
               rgb(43, 53, 65, max = 255, alpha = alpha),
               rgb(84, 102, 114, max = 255, alpha = alpha),
               rgb(148, 167, 163, max = 255, alpha = alpha), 
               rgb(182, 191, 186, max = 255, alpha = alpha))
  return(col_pal)
}
ocean <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(0, 56, 64, max = 255, alpha = alpha), 
               rgb(0, 90, 91, max = 255, alpha = alpha),
               rgb(0, 115, 105, max = 255, alpha = alpha),
               rgb(0, 140, 114, max = 255, alpha = alpha), 
               rgb(2, 166, 118, max = 255, alpha = alpha))
  return(col_pal)
}
long <- function(alpha){
  alpha <- alpha 
  col_pal <- c(rgb(28, 51, 81, max = 255, alpha = alpha), 
               rgb(28, 76, 81, max = 255, alpha = alpha),
               rgb(28, 100, 81, max = 255, alpha = alpha),
               rgb(28, 124, 81, max = 255, alpha = alpha), 
               rgb(148, 160, 81, max = 255, alpha = alpha),
               
               rgb(245, 201, 117, max = 255, alpha = alpha), 
               rgb(245, 168, 96, max = 255, alpha = alpha),
               rgb(245, 136, 88, max = 255, alpha = alpha),
               rgb(245, 94, 74, max = 255, alpha = alpha), 
               rgb(245, 52, 58, max = 255, alpha = alpha),
               
               rgb(180, 46, 65, max = 255, alpha = alpha), 
               rgb(143, 29, 44, max = 255, alpha = alpha),
               rgb(90, 20, 42, max = 255, alpha = alpha),
               rgb(64, 13, 42, max = 255, alpha = alpha), 
               rgb(20, 10, 37, max = 255, alpha = alpha)
               )
  return(col_pal)
}

##################################################
## SNP Subsetting
##################################################

subset_by_SNP <- function(GWAS_file, Nsnps, output_file){
  
  ## Read and oranize GWAS results
  snps <- read.table(GWAS_file)
  colnames(snps) <- c("Chr", "Pos", "P-Val")
  snps <- snps[order(snps$`P-Val`),]
  
  ## Just the number of snps you are iterating over 
  snp_rows <- snps[1:Nsnps,] 
  snp_rows <- snp_rows[order(snp_rows$Chr, as.numeric(snp_rows$Pos)),] ## order by chromosome, then position for efficiency
  # write.csv(snp_rows, snp_info, row.names = F)
  
  ## Save out the values for selecting rows to textfile to pass to bash
  cat("unique_chr=(",paste(unique(snp_rows$Chr), collapse = " "), ")" , file = output_file, sep = "\n")
  cat("snp_chr=(",paste(snp_rows$Chr, collapse = " "), ")"            , file = output_file, sep = "\n", append = T)
  cat("snp_order=(",paste(snp_rows$Order, collapse = " "), ")"        , file = output_file, sep = "\n", append = T)
  cat("snp_row=(",paste(snp_rows$Pos, collapse = " "), ")"            , file = output_file, sep = "\n", append = T)
  
  # system("/Users/Meghs/Dropbox/PhD_Dissertation/Code/Genotype_Predictions/Select_Rows_From_SNPs.txt")
}

pseudo_manhattan_base <- function (tped, correlations, values = "p", ...) {
  require(fields)
  if (is.character(tped)) {
    message("loading tped file")
    tped <- fread(tped, data.table = F)
  }
  snps_analyzed <- which(rownames(correlations$Pvalues) %in% 
                           tped[, 2])
  map <- tped[snps_analyzed, 1:2]
  if (values == "p") {
    likelyhood_sum <- colSums(-log(correlations$Pvalues))
    data_p <- as.data.frame(cbind(likelyhood_sum, c(1:length(likelyhood_sum)),map[, 1]))
    colnames(data_p) <- c("like_sum", "Nvar", "chr")
    chr.color <- color.scale(data_p$chr, col = rainbow(19, alpha = .7))
    par(mar = c(3,3, 0.5, 0.5), mgp = c(1.2,0.5, 0))
    plot(like_sum ~ Nvar, data = data_p, col = chr.color, pch = 16, family = "serif", xlab = "Index", ylab = "Sum of -Log Likelihood", ...)
    
  }
  if (values == "c") {
    
    likelyhood_sum <- colSums(abs(correlations$Coefficients))
    data_p <- as.data.frame(cbind(likelyhood_sum, c(1:length(likelyhood_sum)), map[, 1]))
    colnames(data_p) <- c("like_sum", "Nvar", "chr")
    chr.color <- color.scale(data_p$chr, col = rainbow(19, alpha = .7))
    par(mar = c(3,3, 0.5, 0.5), mgp = c(1.2,0.5, 0))
    plot(like_sum ~ Nvar, data = data_p, col = chr.color, pch = 16, family = "serif", xlab = "Index", ylab = "Sum of Effect Size", ...)
    
  }
}


##################################################
## Plots
##################################################


## Customized Biplot for PCAs
mb_biplot <- function(pca, pcs = 1:2, arrow.color, arrow.width, score_labels = F, labs = "", arrow_labs = "", xlim, ylim, ...){
  
  ## Pull data for scaling
  lam <- pca$sdev[pcs]
  scores <- pca$x
  n   <- nrow(scores)
  lam <- lam / sqrt(n)
  print(paste("Lam is: ", round( lam, 4) ))
  
  ## Scale data by "lam"
  x <- t(t(scores[, pcs]) / lam)
  y <- t(t(pca$rotation[, pcs]) * lam)
  
  ## Determine Axis limits
  rangex1 <- range(x[, 1])
  rangex2 <- range(x[, 2])
  rangey1 <- range(y[, 1])
  rangey2 <- range(y[, 2])
  
  if(missing(xlim) && missing(ylim))
    xlim <- ylim <- rangex1 <- rangex2 <- range(rangex1, rangex2)
  
  # ratio <- max( rangey1/rangex1, rangey2/rangex2)
  ratio <- max( rangey1/xlim[1], rangey2/xlim[2])
  
  ## Plot PC Scores
  plot(x, type = "n", xlim = xlim, ylim = ylim, ...)
  if(score_labels == T) { 
    text(x, as.character( labs ) )
  }
  
  ## Plot Loadings
  par(new = TRUE)
  plot(y, axes = FALSE, type = "n", xlim = xlim*ratio, ylim = ylim*ratio, xlab = "", ylab = "")
  axis(3)
  axis(4)
  box()
  ifelse(length(arrow_labs) > 0, nmes_string <- arrow_labs, nmes_string <- as.character(names(pca$center)))
  text(y, nmes_string)
  arrows(0, 0, y[,1] * 0.8, y[,2] * 0.8, col = arrow.color, length = 0.1, lwd = arrow.width)
  
  ## Go back to scores Plot
  par(new = TRUE)
  plot(x, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "")
  
} 


flip_biplot <- function(pca, pcs = 1:2, ...){
  
  ## Pull data for scaling
  lam <- pca$sdev[pcs]
  scores <- pca$x
  n   <- nrow(scores)
  lam <- lam / sqrt(n)
  print(paste("Lam is: ", round( lam, 4) ))
  
  ## Scale data by "lam"
  x <- t(t(scores[, pcs]) / lam)
  y <- t(t(pca$rotation[, pcs]) * lam)
  
  ## Determine Axis limits
  rangex1 <- range(x[, 1])
  rangex2 <- range(x[, 2])
  rangey1 <- range(y[, 1])
  rangey2 <- range(y[, 2])
  
  xlim <- ylim <- rangex1 <- rangex2 <- range(rangex1, rangex2)

  ratio <- max( rangey1/xlim[1], rangey2/xlim[2])

  ## Plot Loadings
  par(new = TRUE)
  plot(y, axes = FALSE, type = "n", xlim = xlim*ratio, ylim = ylim*ratio, xlab = "", ylab = "")
}

getP <- function(m){
  
  coefs <- summary(m)$coefficients
  pval  <- round(coefs[2,4], 3)
  rval  <- round( summary(m)$r.squared, 2)
  mval  <- round(coefs[2,1], 3)
  
  out.list <- list()
  out.list[["pval"]] <- pval
  out.list[["R2"]]   <- rval
  out.list[["m"]]    <- mval
  
  return(out.list)
  
}
fitLine <- function(xvar, yvar, newdata, xplace, yplace, line_col = maxres(255)[1], polygon_col = maxres(100)[1]){
  
  ## Run Model 
  m <- lm(yvar ~ xvar)
  
  ## Predict with new X-values
  p <- predict(m, newdata = data.frame(xvar = newdata), se.fit = T)
  s <- summary(m)
  
  ## Plot Fit
  polygon(c(newdata, rev(newdata) ), c(p$fit + p$se.fit, rev(p$fit - p$se.fit)), col = polygon_col, border = NA)
  abline(m, col = line_col)
  text(xplace, yplace, paste("R2: ", round( s$r.squared, 2), "\n", "pval: ", round( s$coefficients[2,4],2 )))
  
  
}




##################################################
## PHENOLOGY
##################################################

GuFitDoubleLog <- function (x, t = index(x), tout = t, hessian = FALSE, sf = quantile(x, probs = c(0.05, 0.95), na.rm = TRUE), ...) 
  {
    .normalize <- function(x, sf) (x - sf[1])/(sf[2] - sf[1])
    .backnormalize <- function(x, sf) (x + sf[1]/(sf[2] - sf[1])) * 
      (sf[2] - sf[1])
    if (any(is.na(x))) 
      stop("NA in the time series are not allowed: fill them with e.g. na.approx()")
    if (class(index(x))[1] == "POSIXct") {
      doy.vector <- as.numeric(format(index(x), "%j"))
      index(x) <- doy.vector
      t <- index(x)
      tout <- t
    }
    n <- length(na.omit(x))
    n <- length(x)
    x <- .normalize(x, sf = sf)
    avg <- mean(x, na.rm = TRUE)
    mx <- max(x, na.rm = TRUE)
    mn <- min(x, na.rm = TRUE)
    ampl <- mx - mn
    .doubleLog <- function(par, t) {
      y0 = par[1]
      a1 <- par[2]
      a2 <- par[3]
      t01 <- par[4]
      t02 <- par[5]
      b1 <- par[6]
      b2 <- par[7]
      c1 <- par[8]
      c2 <- par[9]
      xpred <- y0 + (a1/(1 + exp(-(t - t01)/b1))^c1) - (a2/(1 + 
                                                              exp(-(t - t02)/b2))^c2)
      return(xpred)
    }
    .error <- function(par, x, t) {
      if (any(is.infinite(par))) 
        return(99999)
      xpred <- .doubleLog(par, t = t)
      sse <- sum((xpred - x)^2, na.rm = TRUE)
      return(sse)
    }
    doy <- quantile(t, c(0.25, 0.75), na.rm = TRUE)
    y0 <- mn
    a1 <- ampl
    a2 <- ampl
    tmp <- smooth.spline(x, df = 0.5 * length(x))
    doy.max <- which.max(tmp$y)
    t01 <- doy[1] + 0.5 * (doy.max - doy[1])
    t02 <- doy.max + 0.5 * (doy[2] - doy.max)
    b1 <- 10
    b2 <- 10
    c1 <- 1
    c2 <- 1
    prior <- rbind(c(y0, a1, a2, t01, t02, b1, b2, c1, c2), c(y0, 
                                                              a1, a2, t01, t02, b1, b2, 1.2, c2), c(y0, 0.05, 0.05, 
                                                                                                    t01, t02, 0.5, b2, c1, c2), c(y0, a1, a2, doy[1], t02, 
                                                                                                                                  b1, b2, c1, c2), c(y0, a1, a2, t01, doy[2], 5, 5, c1, 
                                                                                                                                                     c2))
    opt.l <- apply(prior, 1, optim, .error, x = x, t = t, method = "Nelder-Mead", 
                   control = list(maxit = 1000), hessian = hessian)
    opt.df <- cbind(cost = unlist(llply(opt.l, function(opt) opt$value)), 
                    convergence = unlist(llply(opt.l, function(opt) opt$convergence)), 
                    ldply(opt.l, function(opt) opt$par))
    best <- which.min(opt.df$cost)
    if (opt.df$convergence[best] == 1) {
      opt <- opt.l[[best]]
      opt <- optim(opt.l[[best]]$par, .error, x = x, t = t, 
                   method = "Nelder-Mead", control = list(maxit = 1500), hessian = hessian)
      prior <- rbind(prior, opt$par)
      xpred <- .doubleLog(opt$par, t)
    }
    else if (opt.df$convergence[best] == 0) {
      opt <- opt.l[[best]]
      prior <- rbind(prior, opt$par)
      xpred <- .doubleLog(opt$par, t)
    }
    if (opt$convergence != 0) {
      opt$par[] <- NA
      xpred <- rep(NA, length(tout))
    }
    else {
      xpred <- .doubleLog(opt$par, tout)
    }
    xpred <- .backnormalize(xpred, sf = sf)
    xpred.out <- zoo(xpred, order.by = t)
    names(opt$par) <- c("y0", "a1", "a2", "t01", "t02", "b1", 
                        "b2", "c1", "c2")
    if (hessian) {
      opt.new <- optim(opt$par, .error, x = x, t = t, method = "Nelder-Mead", 
                       hessian = TRUE)
      .qr.solve <- function(a, b, tol = 1e-07, LAPACK = TRUE) {
        if (!is.qr(a)) 
          a <- qr(a, tol = tol, LAPACK = LAPACK)
        nc <- ncol(a$qr)
        nr <- nrow(a$qr)
        if (a$rank != min(nc, nr)) 
          stop("singular matrix 'a' in solve")
        if (missing(b)) {
          if (nc != nr) 
            stop("only square matrices can be inverted")
          b <- diag(1, nc)
        }
        res <- qr.coef(a, b)
        res[is.na(res)] <- 0
        res
      }
      vc <- .qr.solve(opt$hessian)
      npar <- nrow(vc)
      s2 <- opt.df$cost[best]^2/(n - npar)
      std.errors <- sqrt(diag(vc) * s2)
    }
    fit.formula <- expression(y0 + (a1/(1 + exp(-(t - t01)/b1))^c1) - 
                                (a2/(1 + exp(-(t - t02)/b2))^c2))
    output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula, 
                   sf = sf)
    if (hessian) 
      output <- list(predicted = xpred.out, params = opt$par, 
                     formula = fit.formula, stdError = std.errors, sf = sf)
    return(output)
  }
  
  















