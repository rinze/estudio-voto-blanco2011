# All D'Hont functions are versions of the code proposed by Carlos Gil Bellosta
# on http://r.789695.n4.nabble.com/D-Hondt-method-td879362.html

# Version 1: according to current Spanish law:
#   1. Blank votes are valid.
#   2. Parties with less than 5 % of valid vote are not taken into account.
computeDHontCurrent <- function(candidates, votes, seats, blanks) {
    votes[(votes / (sum(votes) + blanks)) < 0.05] <- 0
    tmp <- data.frame(
        candidates = rep(candidates, each = seats ),
        scores     = as.vector(sapply(votes, function(x) x / 1:seats))
    )
    dhont <- tmp$candidates[order(-tmp$scores)] [1:seats]
    return(table(dhont))
} 

# Version 2: no blank votes
#   1. Parties with less than 5 % of valid vote are still not taken into account.
computeDHontNoBlanks <- function(candidates, votes, seats, blanks) {
    votes[(votes / sum(votes)) < 0.05] <- 0
    tmp <- data.frame(
        candidates = rep(candidates, each = seats ),
        scores     = as.vector(sapply(votes, function(x) x / 1:seats))
    )
    dhont <- tmp$candidates[order(-tmp$scores)] [1:seats]
    return(table(dhont))
} 

# Version 3: no threshold.
computeDHontNoThreshold <- function(candidates, votes, seats, blanks) {
    tmp <- data.frame(
        candidates = rep(candidates, each = seats ),
        scores     = as.vector(sapply(votes, function(x) x / 1:seats))
    )
    dhont <- tmp$candidates[order(-tmp$scores)] [1:seats]
    return(table(dhont))
}

computeSeats <- function(population) {
    # Number of seats in city halls is computed according to this formula:
    # http://elecciones.mir.es/locales2011/Resuelva_sus_dudas/Elecciones_Locales_22_de_mayo_de_2011.htm
    
    if (population <= 100) {
        return(3)
    } else if (population <= 250) {
        return(5)
    } else if (population <= 1000) {
        return(7) 
    } else if (population <= 2000) {
        return(9)
    } else if (population <= 5000) {
        return(11) 
    } else if (population <= 10000) {
        return(13)
    } else if (population <= 20000) {
        return(17) 
    } else if (population <= 50000) {
        return(21)
    } else if (population <= 100000) {
        return(25)
    } else {
        seats <- 25 + ceiling((population - 100000) / 100000)
        if (seats %% 2 == 0)
            seats <- seats + 1
        return(seats)
    }
}

getTownResults <- function(town_row) {
    # Compute the number of seats obtained by each party according to
    party_limit <- which(names(town_row) == "Votos.nulos") + 1
    votes <- town_row[party_limit:ncol(town_row)]
    votes <- votes[as.numeric(votes) > 0]
    candidates <- names(votes)
    seats <- computeSeats(town_row$Población)
    blanks <- town_row$Votos.en.blanco
    
    # According to current law
    dhont_current <- computeDHontCurrent(candidates, votes, seats, blanks)
    dhont_current <- sort(dhont_current[dhont_current > 0], decreasing = TRUE)
    dhont_current <- data.frame(party = names(dhont_current),
                                seats = as.numeric(dhont_current),
                                method = "current")
    
    # Blank votes don't count
    dhont_blanks <- computeDHontNoBlanks(candidates, votes, seats, blanks)
    dhont_blanks <- sort(dhont_blanks[dhont_blanks > 0], decreasing = TRUE)
    dhont_blanks <- data.frame(party = names(dhont_blanks),
                               seats = as.numeric(dhont_blanks),
                               method = "noblanks")
    
    # No threshold
    dhont_thres <- computeDHontNoThreshold(candidates, votes, seats, blanks)
    dhont_thres <- sort(dhont_thres[dhont_thres > 0], decreasing = TRUE)
    dhont_thres <- data.frame(party = names(dhont_thres),
                               seats = as.numeric(dhont_thres),
                               method = "nothreshold")
    
    return(rbind(dhont_current, dhont_blanks, dhont_thres))
    
}