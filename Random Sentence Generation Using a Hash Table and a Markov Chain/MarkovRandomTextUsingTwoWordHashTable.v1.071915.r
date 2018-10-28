#########################################################
#########################################################
# R program to use a Markov process to generate randome
# text based on two-word frequencies, using a
# hash table
#
# Version 1 (7/19/2015): Adapting code from the R program
# MarkovFourLetterEnglishUsingHashTable.v1.071915.r

generateFirstWord <- function(twoWordHashTable) {
    # Make a list of all the Keys in the hash table.
    KeyLIST <- keys(twoWordHashTable)

    # Filter the list to contain only keys that start with a
    # capital letter.
    KeyLISTStartWordMatch <- KeyLIST[ grepl("^[A-Z]", KeyLIST) ]

    # Obtain the Values (frequencies) of the Keys in KeyLIST.
    # These will be used as sampling weights.
    ValueLIST <- values(twoWordHashTable[KeyLISTStartWordMatch])

    # Randomly select one of the Keys from KeyLIST,
    # with weighted sampling.
    newKey <- sample(x=KeyLISTStartWordMatch,size=1,prob=ValueLIST)
    return(newKey)
}

generateSentence <- function(twoWordHashTable,nWordLimit) {

#saveSeed <- .Random.seed
#set.seed(saveSeed)

    # Generate first two words. Get the last character of the 2nd word.
    sentence  <- generateFirstWord(twoWordHashTable)
    word1     <- strsplit(sentence," ")[[1]][1]
    word2     <- strsplit(sentence," ")[[1]][2]
    lastChar2 <- substring(word2,nchar(word2),nchar(word2))

    # Make a list of all the Keys in the hash table.
    KeyLIST <- keys(twoWordHashTable)

    # Count the number of peiods, question marks, and exclamation points.
    # These tallies will be used as sampling weights.
    numPeriods      <- sum(as.numeric(grepl('\\.$',KeyLIST)))
    numQuestions    <- sum(as.numeric(grepl('\\?$',KeyLIST)))
    numExclamations <- sum(as.numeric(grepl('\\!$',KeyLIST)))
    sentenceEndWts  <- c(numPeriods,numQuestions,numExclamations)

    # Now keep generating characters, Markov Chain-like, until
    # we hit a period, question mark, exclamation point,
    # or we hit the maximum number of words allowed.
    numWord <- length(strsplit(sentence," ")[[1]])
    while ( ( lastChar2 != '.' )
          & ( lastChar2 != '?' )
          & ( lastChar2 != '!' )
          & ( numWord < nWordLimit ) ) {

        # Filter the list to contain only keys that start with WORD2.
        grepStr <- sprintf('^%s',word2)
        KeyLISTStartWordMatch <- KeyLIST[ grepl(grepStr, KeyLIST) ]

        # If there are no Keys to sample from, randomly select
        # a Key that starts with a small letter.
        if ( length(KeyLISTStartWordMatch) == 0 ) {
            # Filter the list to contain only keys that start with a
            # small letter.
            KeyLISTStartWordMatch <- KeyLIST[ grepl("^[a-z]", KeyLIST) ]

            # Obtain the Values (frequencies) of the Keys in KeyLISTSTARTWORDMATCH.
            # These will be used as sampling weights.
            ValueLIST <- values(twoWordHashTable[KeyLISTStartWordMatch])

            # Randomly select one of the Keys from KeyLIST,
            # with weighted sampling.
            newKey <- sample(x=KeyLISTStartWordMatch,size=1,prob=ValueLIST)
        } else {

            # Obtain the Values (frequencies) of the Keys in KeyLIST.
            # These will be used as sampling weights.
            ValueLIST <- values(twoWordHashTable[KeyLISTStartWordMatch])

            # Randomly select one of the Keys from KeyLIST,
            # with weighted sampling.
            newKey   <- sample(x=KeyLISTStartWordMatch,size=1,prob=ValueLIST)
        }

        # Extract new WORD2 from NEWKEY. Find the last character in WORD2.
        splitNewKey <- strsplit(newKey," ")[[1]]
        word2       <- splitNewKey[2]
        lastChar2   <- substring(word2,nchar(word2),nchar(word2))
#        print(sprintf('WORD2 = %s, LASTCHAR2 = %s',word2,lastChar2))

        # Append the new WORD2 to the sentence.
        sentence  <- sprintf('%s %s',sentence,word2)

        # If the sentence has reached the maximum allowed length,
        # terminate it with a period, question mark, or exclamation point.
        # Then return the sentence.
        numWord <- length(strsplit(sentence," ")[[1]])
        if ( numWord >= nWordLimit ) {
            lastChar2 <- sample(x=c('.','?','!'),size=1,prob=sentenceEndWts)
            sentence  <- sprintf('%s%s',sentence,lastChar2)
        }
#cat(sentence,'\n')
#cat(numWord,'\n')
#cat(word2,'\n')
#cat(lastChar2,'\n')
    }
#cat(sentence,'\n')

    return(sentence)
}

cleanUpCharacterString <- function(charArray) {
    # Loop over undesired characters and remove them.
    for ( theChar in c("\\$",
                       "\\*",
                       "\\+",
                       "\\|",
                       "\\\\",
                       "\\^",
                       "\\[",
                       "\\]",
                       "\\{",
                       "\\}",
                       "\\(",
                       "\\)",
                       '"',
                       '\t',
                       "0","1","2","3","4","5","6","7","8","9") ) {
        while( grepl(theChar,charArray) ) {
            charArray <- sub(theChar,"",charArray)
        }
    }

    # Remove multiple spaces.
    while ( grepl("  ",charArray) ) {
        charArray <- sub("  "," ",charArray)
    }
    return(charArray)
}

buildTwoWordHashTable <- function(fileName) {

    # Read ALL lines from the input text file.
    tryCatch( filePointer <- file(fileName,'r'), finally="filePointer <- NULL" )
    if ( is.null(filePointer) ) {
        stop('Unable to open input file!')
    }
    allLines <- readLines(filePointer)
    close(filePointer)
    numLines <- length(allLines)

    # Initialize hash table that encodes a two-word frequency matrix.
    # The Key will be a two-word string such as "Hello world" which
    # represents two consecutive words that appear at least once in a sample of text.
    # The Value will be the frequency with which that pair of words appears
    # in a given sample of text.
    require("hash")
    twoWordHashTable <- hash()

    # Initialize LASTWORD to NULL.
    # This will hold the trailing word of the previous line.
    lastWord <- NULL

    # Process lines of text, and keep reading lines of text until EOF.
    i <- 1
    while ( i <= numLines ) {
        print(sprintf("Line count = %d of %d",i,numLines))
        flush.console()

        # Get the next line, find the last character].
        nextLine  <- allLines[i]
        lastChar  <- substring(nextLine,nchar(nextLine),nchar(nextLine))

        # If LASTWORD is non-NULL, prepend it to CHARARRAY.
        # If the previous line did NOT end with a hyphen,
        # LASTWORD will end with a blank. Otherwise it will not.
        charArray <- ifelse(! is.null(lastWord),
                            sprintf('%s%s',lastWord,charArray),
                            charArray)

        # Filter CHARRAY to contain only desired characters.
        charArray <- cleanUpCharacterString(nextLine)

        # Now split up CHARARRAY into words.
        wordLIST <- strsplit(charArray," ")[[1]]
        numWords <- length(wordLIST)

        # If the CURRENT line ended with a hyphen OR if WORDLIST does not
        # have at least two words, keep appending lines until we find a line
        # that does NOT end in a hyphen AND WORDLIST has at least two words.
        while ( ( lastChar == '-' ) | ( numWords < 2 ) ) {
            if ( i + 1 <= numLines ) {
                i <- i + 1
                print(sprintf("Line count = %d of %d",i,numLines))
                flush.console()
                nextLine  <- allLines[i]
                lastChar2 <- substring(nextLine,nchar(nextLine),nchar(nextLine))
                charArray <- ifelse(lastChar == '-',
                                    charArray <- sprintf('%s%s', charArray,nextLine),
                                    charArray <- sprintf('%s %s',charArray,nextLine))
                charArray <- cleanUpCharacterString(charArray); # Keep only desired characters.
                wordLIST  <- strsplit(charArray," ")[[1]]
                numWords  <- length(wordLIST)
                lastChar  <- lastChar2
            } else {
                return(twoWordHashTable)
            }
        }

        # Determine first key of the character string CHARARRAY.
        word1 <- wordLIST[1]
        word2 <- wordLIST[2]
        Key   <- sprintf('%s %s',word1,word2)

    splitKey <- strsplit(Key," ")[[1]]
    word3    <- splitKey[2]
    if ( is.na(word3) ) {
        print(sprintf('Key = %s',Key))
        print(sprintf('word1 = %s',word1))
        print(sprintf('word2 = %s',word2))
        stop('odd')
    }
    if ( Key == "noticed -" ) {
        print(sprintf('charArray= %s',charArray))
        print(sprintf('Key = %s',Key))
        print(sprintf('word1 = %s',word1))
        print(sprintf('word2 = %s',word2))
        stop('odd')
    }

        # If this Key does not yet exist in the hash table, set its value to 1.
        # Otherwise, increment its value by 1.
        twoWordHashTable[[Key]] <- ifelse(! has.key(Key,twoWordHashTable),
                                         1,
                                         twoWordHashTable[[Key]] + 1)

        # Loop over remaining words (if any) in the line and update the hash table.
        if ( numWords > 2 ) {
            for ( k in 3:numWords ) {
                # Determine next key of this line.
                word1 <- word2
                word2 <- wordLIST[k]
                Key   <- sprintf('%s %s',word1,word2)
    splitKey <- strsplit(Key," ")[[1]]
    word3    <- splitKey[2]
    if ( is.na(word3) ) {
        print(sprintf('Key = %s',Key))
        print(sprintf('word1 = %s',word1))
        print(sprintf('word2 = %s',word2))
        stop('odd')
    }
    if ( Key == "noticed -" ) {
        print(sprintf('charArray= %s',charArray))
        print(sprintf('Key = %s',Key))
        print(sprintf('word1 = %s',word1))
        print(sprintf('word2 = %s',word2))
        stop('odd')
    }

                # If this Key does not yet exist in the hash table, set its value to 1.
                # Otherwise, increment its value by 1.
                twoWordHashTable[[Key]] <- ifelse(! has.key(Key,twoWordHashTable),
                                                1,
                                                twoWordHashTable[[Key]] + 1)
            }
        }

        # Prepare for next iteration through WHILE loop.
        # If the last character was a hyphen, and the second
        # word itself was not a hyphen, consider it
        # to mean that the word continues to the next line.
        # Otherwise, treat the end of line as if it were
        # simply a space between words.
        lastWord <- ifelse( ( lastChar == '-' ) & ( word2 != '-' ),
                           word2,
                           sprintf('%s ',word2))

        # If there are still lines to process, continue to the next line.
        if ( i + 1 <= numLines ) {
            i <- i + 1
        } else {
            return(twoWordHashTable)
        }

    }; # End WHILE ( i <= numLines )

    return(twoWordHashTable)
}

# Build two-word hash table.
fileName         <- "E:/DATA/Documents and Settings/Joe/PDF Collection/Speculative Fiction/Complete Works of H.P. Lovecraft - UNICODE.txt"
twoWordHashTable <- buildTwoWordHashTable(fileName)

# Save the hash table to disk.
save(twoWordHashTable,file="E:/DATA/Documents and Settings/Joe/PDF Collection/Speculative Fiction/Lovecraft Two-Word HashTable - v1.071915.rdata")

# Generate 10 random sentences.
for ( k in 1:10 ) {
    sentence <- generateSentence(twoWordHashTable,2000)
    cat(sentence,'\n')
    flush.console()
}


