library(markovchain)
library(stringr)
##this function will generate a transition matrix from agreggating observations of 
##chord sequences from different songs. It requires an NxM character matrix where N is the length of the chord
#progression and M is the number of songs, and each observation denotes the key-agnostic root note of the song
transMatrix<-function(songs){
    uniqueVals<-unique(c(songs))
    nVals<-length(uniqueVals)
    len<-nrow(songs)
    nSongs<-ncol(songs)
    p <- matrix(nrow = nVals, ncol = nVals, dimnames = list(uniqueVals,uniqueVals), 0)
    for (t in 1:(ncol(songs))){
        for(i in 1:len - 1){
            p[songs[i,t], songs[i+1,t]] <- p[songs[i,t], songs[i+1,t]] + 1
        }
    } 
    for (i in 1:nVals){
        #cheating trick, if the sequence always ends on a certain note, we make that an "absorption state" 
        #of the matrix that returns it to the root note
        if(sum(p[i,]) < 1){
            p[i,"1"]<-1.0
            p[i, ] <- p[i, ] / sum(p[i, ])
            }
        else{
            p[i, ] <- p[i, ] / sum(p[i, ])
        }
    }
    p
}

###function for transposing to correct key###
getChords<-function(artist, part, key, nChords){
    #set random seed to ensure our "engine" will be more creative
    #set.seed(runif(1,0,100000000))
    seqs<-matrix(nrow=4, ncol = 0)
    if(artist == "Taylor Swift"){
        if(part == "Verse"){
            seqs<-cbind(seqs,c(1, 6, 4, 5))
            seqs<-cbind(seqs, c(1, 4, 6, 5))
            seqs<-cbind(seqs, c(1, 5, 6, 4))
        }
        else if(part == "Chorus"){
            seqs<-cbind(seqs, c(1, 6, 2, 4))
            seqs<-cbind(seqs, c(1, 5, 6, 4))
            seqs<-cbind(seqs, c(4, 1, 5, 6))
        }
    }
    else if(artist == "Nicki Minaj"){
        if(part == "Verse"){
            seqs<-cbind(seqs, c(1, 2, 6, 4))
            seqs<-cbind(seqs, c(1, 4, 6, 5))
            seqs<-cbind(seqs, c(1,4,6,5))
        }
        else if(part == "Chorus"){
            seqs<-cbind(seqs, c(1, 2, 6, 4))
            seqs<-cbind(seqs, c(1, 4, 6, 5))
            seqs<-cbind(seqs, c(1,4,6,5))
        }
    }
    else if(artist == "Katy Perry"){
        if(part == "Verse"){
            seqs<-cbind(seqs, c(1, 4, 5, 6))
            seqs<-cbind(seqs, c(1, 5, 6, 4))
            seqs<-cbind(seqs, c(1,2,6,4))
        }
        else if(part == "Chorus"){
            seqs<-cbind(seqs, c(1, 4, 5, 6))
            seqs<-cbind(seqs, c(1, 5, 6, 4))
            seqs<-cbind(seqs, c(1,6,4,1))
        }
    }
    #tSwiftChorus2Ton<-c("maj", "maj", "min", "maj")
    seqs<-apply(seqs, c(1,2), as.character)
    tM<-transMatrix(seqs)
    model<-new("markovchain", states = unique(c(seqs)), transitionMatrix = tM)
    seq<-c(1,predict(object = model, newdata = 1, n.ahead = nChords-1))
    seq<-as.integer(seq)
    seq
    
    notes <- c("A","A#","B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
    chordsList<-vector()
    start<-which(notes==key)
    getInt<-function(int){
        if(int == 1){
            int = 0
        }
        else if (int == 2){
            int = 2
        }
        else if (int == 3){
            int = 4
        }
        else if(int == 4){
            int = 5
        }
        else if (int == 5){
            int = 7
        }
        else if (int == 6){
            int = 9
        }
        else if (int == 7){
            int = 11
        }
        int
    }
    #function for deciding major or minor
    minOrMaj<-function(chord){
        if(chord == 2 | chord == 3 | chord == 6){
            tone = "min"
        }
        else if(chord ==7){
            tone = "dim"
        }
        else{
            tone = "maj"
        }
        tone
    }
    for (int in seq){
        newInt<-getInt(int)
        tone = minOrMaj(int)
        ind = start + newInt
        if(ind>12){
            ind = ind%%12
        }
        note = notes[ind]
        note = str_c(note,tone, sep = "")
        chordsList<-c(chordsList,note)
    }
    chordsList
}