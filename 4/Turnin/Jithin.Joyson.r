
library(textreuse)

files <- list.files("/Users/Jithin/OneDrive - hawk.iit.edu/Lenovo/School/2019 Spring/CS 422/Homework/4/corpus", full.names=T) 

corpus <- TextReuseCorpus(files,tokenizer = tokenize_ngrams, keep_tokens = TRUE)

total = 0
for (i in tokens(corpus)){total <- total + length(i)}
paste('There are',total,'tokens')

paste('There are',length(files),'documents')
paste('There are',total,'shingles')
paste('Thus the dimensions are row x columns',length(files),'X',total)

doc <- corpus[['orig_taske']]

tokens(doc)[1:5]

reduction <- (total - 240)/total *100
paste('There is a',reduction,'% of reduction in size')

signature <- 240
sim <- .23

bands = 0
cur_sim = 1.0
for (i in 1:signature) {
    if (signature %% i == 0){
        check <- lsh_threshold(h=signature, b=i)
        if ((check > sim) & (check < cur_sim)){
            cur_sim = check
            bands = i
        }
    }
}

paste('There should be',bands,'bands')

prob <- lsh_probability(h=signature,  b=bands, s=sim)

paste('The probability is',prob,'for catching similar documents')

pc <- pairwise_candidates(pairwise_compare(corpus, jaccard_similarity))

obs_pc <- length(pc[[1]])

paste('There were',obs_pc,'comparision')

jsim <- subset(pc, score >= .23)
count <- length(jsim[[1]])

paste('There were',count,'observations that had a jacard similarity of atleast',sim)

jsim[order(-jsim$score),]

seed = 100
minhash <- minhash_generator(seed = seed, n = signature)
rm(corpus)
corpus <- TextReuseCorpus(files,tokenizer = tokenize_ngrams, keep_tokens = TRUE, minhash_func = minhash)

ls <- lsh_compare(lsh_candidates(lsh(corpus, bands = bands)), corpus, jaccard_similarity)

obs_ls <- length(ls[[1]])
paste('There were',obs_ls,'comparision')

decrease <- (obs_pc - obs_ls)/obs_pc *100

paste('There was a',decrease,'% decrease')

ls[order(-ls$score),]

jsim1 <- subset(ls, score >= .23)
count <- length(jsim1[[1]])

paste('There were',count,'observations that had a jacard similarity of atleast',sim)

jsim1[order(-jsim1$score),]

table(jsim$a)

table(jsim1$a)

table(jsim$b)

table(jsim1$b)

users <- 671
id <- 20349472

user_Id <- id %% users

library('dplyr')
library('stringr')

df.ratings <- read.csv("ml-latest-small/ratings.csv", sep=",", header=T)

df.movies <- read.csv("ml-latest-small/movies.csv", sep=",", header=T)

user.ratings <- filter(df.ratings, userId == user_Id)

labels <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "IMAX", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western", "(no genres listed)")

user_prof <- merge(user.ratings, df.movies)

l = c()
index <- 1
for (m in user_prof$genres){
    l[[index]] <- strsplit(m,"\\|")
    index = index + 1
}
user_prof$tokens <- l

for (la in labels){
    temp <- c()
    index <- 1
    for (m in user_prof$tokens){
        if(la %in% m[[1]]){
            temp[[index]] <- 1
        }
        else{
            temp[[index]] <- 0
        }
        index = index + 1
    }
    user_prof[get('la')] <- temp
}

user_prof <- user_prof[c(-2,-3,-4,-5,-6,-7)] 

head(user_prof)

user_prof_avg <- c()
index <- 1
for( c in colnames(user_prof)){
    if (c != 'movieId'){
        user_prof_avg[[index]] <- sum(user_prof[get('c')])/nrow(user_prof)
        index = index + 1
    }
}

user_prof_avg

movies <- 10
#set.seed(seed)
movieId <- sample(df.movies$movieId, movies)
movie_prof <- data.frame(movieId)

movie_prof <- merge(movie_prof, df.movies)

title <- movie_prof$title

l = c()
index <- 1
for (m in movie_prof$genres){
    l[[index]] <- strsplit(m,"\\|")
    index = index + 1
}
movie_prof$tokens <- l

for (la in labels){
    temp <- c()
    index <- 1
    for (m in movie_prof$tokens){
        if(la %in% m[[1]]){
            temp[[index]] <- 1
        }
        else{
            temp[[index]] <- 0
        }
        index = index + 1
    }
    movie_prof[get('la')] <- temp
}

movie_prof <- movie_prof[c(-2,-3,-4)]

movie_list <- movie_prof$movieId
movie_prof <- movie_prof[c(-1)]

head(movie_prof)

cosine_sim <- c()
index = 1
for (i in 1:nrow(movie_prof)){
    numerator <- 0
    denomenator_a <- 0
    denomenator_b <- 0
    test_movie <- movie_prof[i,]
    for (d in 1:length(test_movie)){
        numerator <- numerator + test_movie[d][[1]]*user_prof_avg[d]
        denomenator_a <- denomenator_a + test_movie[d][[1]]**2
        denomenator_b <- denomenator_b + user_prof_avg[d]**2
    }
    denomenator <- sqrt(denomenator_a) * sqrt(denomenator_b)
    cosine_sim[[index]] <- numerator/denomenator
    index <- index + 1
}

movie_prof$movieId <- movie_list
movie_prof$cos_sim <- cosine_sim
movie_prof$title <- title

movie_prof <- movie_prof[order(-movie_prof$cos_sim),]

cat(sprintf('User ID %s chose the following %s movies:\n',user_Id,movies))
cat(sprintf('%s',movieId))
cat(sprintf('\n'))
cat(sprintf('Of these, the following 5 movies are recommended: \n'))
cat(sprintf('MovieId\t\t\tMovieName\t\t\t\tSimilarity\n'))
cat(sprintf('-------------------------------------------------------------------------------------------------\n'))
for (i in 1:5){
    cat(sprintf('%s\t\t\t%s\t\t\t%s\n', movie_prof[i,]$movieId, movie_prof[i,]$title, movie_prof[i,]$cos_sim))
}

user_Id <- 191
movies <- c(10,34,47,110,150,153,161,165,185,208,231,292,296,300,318,339,344,349,356,380,434,454,457,480,588,590,592,593,595)
test_Ids <- c(150, 296, 380, 590)
userId <- c(513,317,415,375,64,556,82,225,657,266,568,50)
Jacard_Similarity <- c(0.4358974,0.4033613,0.3255814,0.3049645,0.2753623,0.2727273,0.2527473,0.2420382,0.2262774,0.2216216,0.2105263,0.2009804)
jacard <- data.frame(userId,Jacard_Similarity)

n <- 5
#set.seed(seed)
random_sample <- sample(jacard$userId, n)
user_IDs <- random_sample

user_IDs[[6]] <- user_Id
utility_matrix <- data.frame(userId = user_IDs)

merged <- filter(df.ratings, userId %in% random_sample)

labels <- intersect(movies,merged$movieId)
length(labels)

# making labels optimal length
labels <- movies
length(labels)

merged <- merge(jacard,utility_matrix)
merged <- merged[order(-merged$Jacard_Similarity),]
merged

for (l in labels){
    temp <- c()
    index <- 1
    for (u in utility_matrix$userId){
        if(u == user_Id & l %in% test_Ids){
            temp[[index]] <- NA
        }
        else{
            selection <- filter(df.ratings, userId == u & movieId == l)
            if(nrow(selection) == 1){
                temp[[index]] <- selection$rating
            }
            else{
                temp[[index]] <- NA
            }
        }
        index <- index + 1
    }
    utility_matrix[paste(get('l'))] <- temp
}

utility_matrix

test <- data.frame(test_Ids)
ratings <- c()
index <- 1
neighborhood <- 3
for (t in test_Ids){
    sim_sum <- 0
    rating_sum <- 0
    for (user in 1:neighborhood){
        sim_sum <- sim_sum + merged[user, ]$Jacard_Similarity
        rating_sum <- rating_sum + merged[user, ]$Jacard_Similarity * filter(df.ratings, userId == merged[user, ]$userId & movieId == t)$rating
    }
    ratings[[index]] <- rating_sum/sim_sum
    index <- index + 1
}
test$ratings <- ratings

test

cat(sprintf('User ID %s, %s random user IDs:\n\n',user_Id,n))
cat(sprintf('%s',random_sample))
cat(sprintf('\n\n'))
cat(sprintf('Using user-user similarity, User ID %s will rate the movies as follows:\n\n',user_Id))
for (t in 1:nrow(test)){
    cat(sprintf('%s: %s\n',test[t,]$test_Ids,test[t,]$ratings))
}
RSME <- 0
for (i in 1:nrow(test)){
    RSME <- (test[i,]$ratings - filter(df.ratings, userId == user_Id & movieId == test[i,]$test_Ids)$rating)**2 + RSME
}
RSME <- sqrt(RSME/nrow(test))
cat(sprintf('RSME: %s\n',RSME))

n = 5
#set.seed(seed)
random_sample <- sample(jacard$userId, n)
user_IDs <- random_sample

user_IDs[[6]] <- user_Id

merged <- filter(df.ratings, userId %in% random_sample)

labels <- intersect(movies,merged$movieId)
length(labels)

# making labels optimal length
labels <- movies
length(labels)

utility_matrix <- data.frame(movieId = labels)

for (l in user_IDs){
    temp <- c()
    index <- 1
    for (u in utility_matrix$movieId){
        if(l == user_Id & u %in% test_Ids){
            temp[[index]] <- NA
        }
        else{
            selection <- filter(df.ratings, userId == l & movieId == u)
            if(nrow(selection) == 1){
                temp[[index]] <- selection$rating
            }
            else{
                temp[[index]] <- NA
            }
        }
        index <- index + 1
    }
    utility_matrix[paste(get('l'))] <- temp
}

utility_matrix

test <- data.frame(test_Ids)
neighborhood <- 3

ratings <- c()
index <- 1

for (t in test_Ids){
    movies <- c()
    sim <- c()
    index1 <- 1

    #main mean calculation
    sum <- 0
    counter <- 0
    data <- filter(utility_matrix, movieId == t)[c(-1)][1,]
    for (f in data){
        if (!is.na(f)){
            sum <- f + sum
            counter <- counter + 1
        }
    }
    
    if(counter == 0){
        mean <- 0
    }
    else{
        mean <- sum/counter
    }
    vector_a <- data - mean
    for (um in utility_matrix$movieId){
        if(um != t){
            #secondary means calculation
            sum <- 0
            counter <- 0
            data <- filter(utility_matrix, movieId == um)[c(-1)][1,]
            
            for (f in data){
                if (!is.na(f)){
                    sum <- f + sum
                    counter <- counter + 1
                }
            }
            if(counter == 0){
                mean <- 0
            }
            else{
                mean <- sum/counter
            }
            vector_b <- data - mean
            
            #cosine similarity
            numerator <- 0
            denomenator_a <- 0
            denomenator_b <- 0
            for (d in 1:length(vector_a)){
                if(is.na(vector_a[d])){
                    vector_a[d] <- 0
                }
                if(is.na(vector_b[d])){
                    vector_b[d] <- 0
                }
                numerator <- numerator + vector_a[d]*vector_b[d]
                denomenator_a <- denomenator_a + vector_a[d]**2
                denomenator_b <- denomenator_b + vector_b[d]**2
            }
            denomenator = sqrt(denomenator_a) * sqrt(denomenator_b)
            if(denomenator[[1]] != 0){
                sim[[index1]] = numerator[[1]]/denomenator[[1]]
                movies[[index1]] = um
                index1 <- index1 + 1
            }
        }
    }
    
    test2 <- data.frame(sim)
    test2$movies <- movies
    test2 <- test2[order(-test2$sim),]
    
    sim_sum <- 0
    rating_sum <- 0
    
    #weighted average calculation
    for (user in 1:neighborhood){
        sim_sum <- sim_sum + test2[user, ]$sim
        #rating <- filter(df.ratings, userId == user_Id & movieId == test2[user, ]$movies)
        rating <- filter(utility_matrix, movieId == test2[user, ]$movies)[paste(user_Id)]
        if(!is.na(rating[[1]])){
            rating_sum <- rating_sum + test2[user, ]$sim * rating[[1]]
        }
        else{
            rating_sum <- rating_sum + test2[user, ]$sim * 0
        }
    }
    ratings[[index]] <- rating_sum/sim_sum
    index <- index + 1
}

test$ratings <- ratings

cat(sprintf('User ID %s, %s random user IDs:\n\n',user_Id,n))
cat(sprintf('%s',random_sample))
cat(sprintf('\n\n'))
cat(sprintf('Using item-item similarity, User ID %s will rate the movies as follows:\n\n',user_Id))
for (t in 1:nrow(test)){
    cat(sprintf('%s: %s\n',test[t,]$test_Ids,test[t,]$ratings))
}
RSME <- 0
for (i in 1:nrow(test)){
    RSME <- (test[i,]$ratings - filter(df.ratings, userId == user_Id & movieId == test[i,]$test_Ids)$rating)**2 + RSME
}
RSME <- sqrt(RSME/nrow(test))
cat(sprintf('RSME: %s\n',RSME))
