function (n, h = c(261, -360), c = 80, l = c(18, 90), power = 1.5, 
    gamma = NULL, fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, 
    register = NULL, ..., h1, h2, c1, l1, l2, p1, p2, cmax = NULL) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    if (is.character(h)) 
        palette <- h
    pals <- if (!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Diverging", palette = palette)[, 
            2L:11L])[1L, ]
    }
    else {
        structure(c(rep_len(h, 2L), c(c[1L], NA), rep_len(l, 
            2L), if (length(power) < 2L) c(power, NA) else rep_len(power, 
            2L), if (length(c) > 1L) c[2L] else NA, 1), .Names = vars.pal)
    }
    if (!missing(h) && !is.character(h)) {
        h <- rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if (!missing(c)) {
        pals["c1"] <- c[1L]
        if (length(c) > 1L) 
            pals["cmax"] <- c[2L]
    }
    if (!missing(l)) {
        l <- rep_len(l, 2L)
        pals["l1"] <- l[1L]
        pals["l2"] <- l[2L]
    }
    if (!missing(power)) {
        power <- if (length(power) < 2L) 
            c(power, NA)
        else rep_len(power, 2L)
        pals["p1"] <- power[1L]
        pals["p2"] <- power[2L]
    }
    if (!missing(fixup)) 
        pals["fixup"] <- as.logical(fixup)
    if (!missing(h1)) 
        pals["h1"] <- h1
    if (!missing(h2)) 
        pals["h2"] <- h2
    if (!missing(c1)) 
        pals["c1"] <- c1
    if (!missing(l1)) 
        pals["l1"] <- l1
    if (!missing(l2)) 
        pals["l2"] <- l2
    if (!missing(p1)) 
        pals["p1"] <- p1
    if (!missing(p2)) 
        pals["p2"] <- p2
    if (!missing(cmax)) 
        pals["cmax"] <- cmax
    pals["c2"] <- NA
    if (is.character(register) && nchar(register) > 0L) {
        add_hcl_pals(palette = register, type = "Diverging", 
            parameters = pals)
        register <- TRUE
    }
    else {
        register <- FALSE
    }
    if (is.na(pals["p2"])) 
        pals["p2"] <- pals["p1"]
    cmaxat <- 1/(1 + abs(pals["cmax"] - pals["c1"])/pals["cmax"])
    if (!is.na(cmaxat) && (cmaxat <= 0 | cmaxat >= 1)) 
        cmaxat <- NA
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = pals["l2"] - (pals["l2"] - pals["l1"]) * 
        abs(rval)^pals["p2"], C = if (is.na(cmaxat)) {
        pals["c1"] * abs(rval)^pals["p1"]
    }
    else {
        ifelse(abs(rval)^pals["p1"] <= cmaxat, pals["cmax"] * 
            (abs(rval)^pals["p1"])/cmaxat, pals["cmax"] - (pals["cmax"] - 
            pals["c1"]) * ((abs(rval)^pals["p1"] - cmaxat)/(1 - 
            cmaxat)))
    }, H = ifelse(rval > 0, pals["h1"], pals["h2"])), fixup = as.logical(pals["fixup"]), 
        ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- ifelse(is.na(rval), NA, paste(rval, alpha, sep = ""))
    }
    if (rev) 
        rval <- rev(rval)
    if (register) 
        invisible(rval)
    else return(rval)
}
