install.packages(c("devtools", "rjson", "bit64", "httr"))

library(devtools)
install_github("twitteR", username="geoffjentry")
library(twitteR)
library(stringr)
library(ggplot2)
library(plyr)
library(RColorBrewer)

api_key <- "2i96Rb39gwmZKEdf3tSkkFTsH"
api_secret <- "4jkj1GYkxzXCqOH14b51aMpOuO6chjvfrWByCuSXt6yYGai6kU"
access_token <- "508003181-FkCfOvl9FTzpRXRYnEg3QzAmgqzlrDuAZYyKG1tM"
access_token_secret <- "TDXW3yM2silSaA0ocX7sZYhUObPKKJG7wg0y4K3qlBgr3"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Load in Hu & Liu's opinion lexicon of positive and negative words
pos.words <- scan('/Users/Malter/Twitter/positive-words.txt', what='character', comment.char=';')
neg.words <- scan('/Users/Malter/Twitter/negative-words.txt', what='character', comment.char=';')


#Score sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        sentence = tolower(sentence)
        word.list = str_split(sentence, '\\s+')
        words = unlist(word.list)
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, pos.words, neg.words, .progress=.progress )
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

whitesox.tweets <- searchTwitter('#whitesox', n=1500, lang="en")
tigers.tweets <- searchTwitter('#tigers', n=1500, lang="en")
twins.tweets <- searchTwitter('@twins', n=1500, lang="en")
royals.tweets <- searchTwitter('@royals', n=1500, lang="en")
indians.tweets <- searchTwitter('#indians', n=1500, lang="en")

angels.tweets <- searchTwitter('#angels', n=1500, lang="en")
athletics.tweets <- searchTwitter('#athletics', n=1500, lang="en")
mariners.tweets <- searchTwitter('#mariners', n=1500, lang="en")
astros.tweets <- searchTwitter('#astros', n=1500, lang="en")
rangers.tweets <- searchTwitter('rangers + mlb', n=1500, lang="en")

nationals.tweets <- searchTwitter('#nats', n=1500, lang="en")
mets.tweets <- searchTwitter('#mets', n=1500, lang="en")
braves.tweets <- searchTwitter('#braves', n=1500, lang="en")
marlins.tweets <- searchTwitter('#marlins', n=1500, lang="en")
phillies.tweets <- searchTwitter('#phillies', n=1500, lang="en")

cardinals.tweets <- searchTwitter('#stlcards', n=1500, lang="en")
pirates.tweets <- searchTwitter('#pirates', n=1500, lang="en")
brewers.tweets <- searchTwitter('#brewers', n=1500, lang="en")
reds.tweets <- searchTwitter('#reds', n=1500, lang="en")
cubs.tweets <- searchTwitter('#cubs', n=1500, lang="en")

dodgers.tweets <- searchTwitter('#dodgers', n=1500, lang="en")
giants.tweets <- searchTwitter('#giants', n=1500, lang="en")
padres.tweets <- searchTwitter('#padres', n=1500, lang="en")
rockies.tweets <- searchTwitter('#rockies', n=1500, lang="en")
diamondbacks.tweets <- searchTwitter('#dbacks', n=1500, lang="en")

#Create an array of the output text

# AL East
orioles.text = laply(orioles.tweets, function(t) t$getText())
yankees.text = laply(yankees.tweets, function(t) t$getText())
bluejays.text = laply(bluejays.tweets, function(t) t$getText())
rays.text = laply(rays.tweets, function(t) t$getText())
redsox.text = laply(redsox.tweets, function(t) t$getText())

# AL Central
whitesox.text = laply(whitesox.tweets, function(t) t$getText())
tigers.text = laply(tigers.tweets, function(t) t$getText())
twins.text = laply(twins.tweets, function(t) t$getText())
royals.text = laply(royals.tweets, function(t) t$getText())
indians.text = laply(indians.tweets, function(t) t$getText())

# AL West
angels.text = laply(angels.tweets, function(t) t$getText())
athletics.text = laply(athletics.tweets, function(t) t$getText())
mariners.text = laply(mariners.tweets, function(t) t$getText())
astros.text = laply(astros.tweets, function(t) t$getText())
rangers.text = laply(rangers.tweets, function(t) t$getText())

# NL East
nationals.text = laply(nationals.tweets, function(t) t$getText())
mets.text = laply(mets.tweets, function(t) t$getText())
braves.text = laply(braves.tweets, function(t) t$getText())
marlins.text = laply(marlins.tweets, function(t) t$getText())
phillies.text = laply(phillies.tweets, function(t) t$getText())

# NL Central
cardinals.text = laply(cardinals.tweets, function(t) t$getText())
pirates.text = laply(pirates.tweets, function(t) t$getText())
brewers.text = laply(brewers.tweets, function(t) t$getText())
reds.text = laply(reds.tweets, function(t) t$getText())
cubs.text = laply(cubs.tweets, function(t) t$getText())

# NL West
dodgers.text = laply(dodgers.tweets, function(t) t$getText())
giants.text = laply(giants.tweets, function(t) t$getText())
padres.text = laply(padres.tweets, function(t) t$getText())
rockies.text = laply(rockies.tweets, function(t) t$getText())
diamondbacks.text = laply(diamondbacks.tweets, function(t) t$getText())


#R dosen't like funny characters so strip them out

# AL East
orioles.text = gsub("[^[:alnum:]|^[:space:]]", "", orioles.text)
yankees.text = gsub("[^[:alnum:]|^[:space:]]", "", yankees.text)
bluejays.text = gsub("[^[:alnum:]|^[:space:]]", "", bluejays.text)
rays.text = gsub("[^[:alnum:]|^[:space:]]", "", rays.text)
redsox.text = gsub("[^[:alnum:]|^[:space:]]", "", redsox.text)

# AL Central
whitesox.text = gsub("[^[:alnum:]|^[:space:]]", "", whitesox.text)
tigers.text = gsub("[^[:alnum:]|^[:space:]]", "", tigers.text)
twins.text = gsub("[^[:alnum:]|^[:space:]]", "", twins.text)
royals.text = gsub("[^[:alnum:]|^[:space:]]", "", royals.text)
indians.text = gsub("[^[:alnum:]|^[:space:]]", "", indians.text)

# AL West
angels.text = gsub("[^[:alnum:]|^[:space:]]", "", angels.text)
athletics.text = gsub("[^[:alnum:]|^[:space:]]", "", athletics.text)
mariners.text = gsub("[^[:alnum:]|^[:space:]]", "", mariners.text)
astros.text = gsub("[^[:alnum:]|^[:space:]]", "", astros.text)
rangers.text = gsub("[^[:alnum:]|^[:space:]]", "", rangers.text)

# NL East
nationals.text = gsub("[^[:alnum:]|^[:space:]]", "", nationals.text)
mets.text = gsub("[^[:alnum:]|^[:space:]]", "", mets.text)
braves.text = gsub("[^[:alnum:]|^[:space:]]", "", braves.text)
marlins.text = gsub("[^[:alnum:]|^[:space:]]", "", marlins.text)
phillies.text = gsub("[^[:alnum:]|^[:space:]]", "", phillies.text)

# NL Central
cardinals.text = gsub("[^[:alnum:]|^[:space:]]", "", cardinals.text)
pirates.text = gsub("[^[:alnum:]|^[:space:]]", "", pirates.text)
brewers.text = gsub("[^[:alnum:]|^[:space:]]", "", brewers.text)
reds.text = gsub("[^[:alnum:]|^[:space:]]", "", reds.text)
cubs.text = gsub("[^[:alnum:]|^[:space:]]", "", cubs.text)

# NL West
dodgers.text = gsub("[^[:alnum:]|^[:space:]]", "", dodgers.text)
giants.text = gsub("[^[:alnum:]|^[:space:]]", "", giants.text)
padres.text = gsub("[^[:alnum:]|^[:space:]]", "", padres.text)
rockies.text = gsub("[^[:alnum:]|^[:space:]]", "", rockies.text)
diamondbacks.text = gsub("[^[:alnum:]|^[:space:]]", "", diamondbacks.text)


# AL East
orioles.scores <- score.sentiment(orioles.text, pos.words, 
                                   neg.words, .progress='text')
yankees.scores <- score.sentiment(yankees.text, pos.words, 
                                   neg.words, .progress='text')
bluejays.scores <- score.sentiment(bluejays.text, pos.words, 
                                   neg.words, .progress='text')
rays.scores <- score.sentiment(rays.text, pos.words, 
                                   neg.words, .progress='text')
redsox.scores <- score.sentiment(redsox.text, pos.words, 
                                   neg.words, .progress='text')

# AL Central
whitesox.scores <- score.sentiment(whitesox.text, pos.words, 
                                   neg.words, .progress='text')
tigers.scores <- score.sentiment(tigers.text, pos.words, 
                                  neg.words, .progress='text')
twins.scores <- score.sentiment(twins.text, pos.words, 
                                   neg.words, .progress='text')
royals.scores <- score.sentiment(royals.text, pos.words, 
                               neg.words, .progress='text')
indians.scores <- score.sentiment(indians.text, pos.words, 
                                 neg.words, .progress='text')

# AL West
angels.scores <- score.sentiment(angels.text, pos.words, 
                                   neg.words, .progress='text')
athletics.scores <- score.sentiment(athletics.text, pos.words, 
                                 neg.words, .progress='text')
mariners.scores <- score.sentiment(mariners.text, pos.words, 
                                neg.words, .progress='text')
astros.scores <- score.sentiment(astros.text, pos.words, 
                                 neg.words, .progress='text')
rangers.scores <- score.sentiment(rangers.text, pos.words, 
                                  neg.words, .progress='text')

# NL East
nationals.scores <- score.sentiment(nationals.text, pos.words, 
                                 neg.words, .progress='text')
mets.scores <- score.sentiment(mets.text, pos.words, 
                                    neg.words, .progress='text')
braves.scores <- score.sentiment(braves.text, pos.words, 
                                   neg.words, .progress='text')
marlins.scores <- score.sentiment(marlins.text, pos.words, 
                                 neg.words, .progress='text')
phillies.scores <- score.sentiment(phillies.text, pos.words, 
                                  neg.words, .progress='text')

# NL Central
cardinals.scores <- score.sentiment(cardinals.text, pos.words, 
                                    neg.words, .progress='text')
pirates.scores <- score.sentiment(pirates.text, pos.words, 
                               neg.words, .progress='text')
brewers.scores <- score.sentiment(brewers.text, pos.words, 
                                 neg.words, .progress='text')
reds.scores <- score.sentiment(reds.text, pos.words, 
                                  neg.words, .progress='text')
cubs.scores <- score.sentiment(cubs.text, pos.words, 
                                   neg.words, .progress='text')

# NL West
dodgers.scores <- score.sentiment(dodgers.text, pos.words, 
                                    neg.words, .progress='text')
giants.scores <- score.sentiment(giants.text, pos.words, 
                                  neg.words, .progress='text')
padres.scores <- score.sentiment(padres.text, pos.words, 
                                  neg.words, .progress='text')
rockies.scores <- score.sentiment(rockies.text, pos.words, 
                               neg.words, .progress='text')
diamondbacks.scores <- score.sentiment(diamondbacks.text, pos.words, 
                               neg.words, .progress='text')

# AL East
orioles.scores$team = 'Orioles'
orioles.scores$code = 'BAL'
yankees.scores$team = 'Yankees'
yankees.scores$code = 'NYA'
bluejays.scores$team = 'Blue Jays'
bluejays.scores$code = 'TOR'
rays.scores$team = 'Rays'
rays.scores$code = 'TBA'
redsox.scores$team = 'Red Sox'
redsox.scores$code = 'BOS'

# AL Central
whitesox.scores$team = 'White Sox'
whitesox.scores$code = 'CHA'
tigers.scores$team = 'Tigers'
tigers.scores$code = 'DET'
twins.scores$team = 'Twins'
twins.scores$code = 'MIN'
royals.scores$team = 'Royals'
royals.scores$code = 'KCA'
indians.scores$team = 'Indians'
indians.scores$code = 'CLE'

# AL West
angels.scores$team = 'Angels'
angels.scores$code = 'LAA'
athletics.scores$team = "A's"
athletics.scores$code = 'OAK'
mariners.scores$team = 'Marineres'
mariners.scores$code = 'SEA'
astros.scores$team = 'Astros'
astros.scores$code = 'HOU'
rangers.scores$team = 'Rangers'
rangers.scores$code = 'TEX'

# NL East
nationals.scores$team = 'Nationals'
nationals.scores$code = 'WSN'
mets.scores$team = 'Mets'
mets.scores$code = 'NYN'
braves.scores$team = 'Braves'
braves.scores$code = 'ATL'
marlins.scores$team = 'Marlins'
marlins.scores$code = 'MIA'
phillies.scores$team = 'Phillies'
phillies.scores$code = 'PHI'

# NL Central
cardinals.scores$team = 'Cardinals'
cardinals.scores$code = 'STL'
pirates.scores$team = 'Pirates'
pirates.scores$code = 'PIT'
brewers.scores$team = 'Brewers'
brewers.scores$code = 'MIL'
reds.scores$team = 'Reds'
reds.scores$code = 'CIN'
cubs.scores$team = 'Cubs'
cubs.scores$code = 'CHN'

# NL West
dodgers.scores$team = 'Dodgers'
dodgers.scores$code = 'LAD'
giants.scores$team = 'Giants'
giants.scores$code = 'SFN'
padres.scores$team = 'Padres'
padres.scores$code = 'SDN'
rockies.scores$team = 'Rockies'
rockies.scores$code = 'COL'
diamondbacks.scores$team = 'D-backs'
diamondbacks.scores$code = 'ARI'

# AL East
aleast.scores = rbind(orioles.scores, yankees.scores, bluejays.scores, rays.scores, redsox.scores)

# AL Central
alcentral.scores = rbind(whitesox.scores, tigers.scores, twins.scores, royals.scores, indians.scores)

# AL West
alwest.scores = rbind(angels.scores, athletics.scores, mariners.scores, astros.scores, rangers.scores)

# NL East
nleast.scores = rbind(nationals.scores, mets.scores, braves.scores, marlins.scores, phillies.scores)

# NL Central
nlcentral.scores = rbind(cardinals.scores, pirates.scores, brewers.scores, reds.scores, cubs.scores)

# NL West
nlwest.scores = rbind(dodgers.scores, giants.scores, padres.scores, rockies.scores, diamondbacks.scores)


# AL East
ggplot(data=aleast.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer() +
    labs(title="AL East Sentiment")

# AL Central
ggplot(data=alcentral.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer() +
    labs(title="AL Central Sentiment")

# AL West
ggplot(data=aleast.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer() +
    labs(title="AL West Sentiment")
 
# NL East
ggplot(data=nleast.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer() +
    labs(title="NL East Sentiment")

# NL Central
ggplot(data=nlcentral.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer()  +
    labs(title="NL Central Sentiment")

# NL West
ggplot(data=nleast.scores) +
    geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
    facet_grid(team~.) +
    theme_bw() + scale_color_brewer() +
    labs(title="NL West Sentiment")

# boxplot
ggplot(aleast.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("Boxplot - AL East's Sentiment Scores")

ggplot(alcentral.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("Boxplot - AL Central's Sentiment Scores")

ggplot(alwest.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("Boxplot - AL West's Sentiment Scores")

ggplot(nleast.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("Boxplot - NL East's Sentiment Scores")

ggplot(nlcentral.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("Boxplot - NL Central's Sentiment Scores")

ggplot(nlwest.scores, aes(x=team, y=score, group=team)) +
    geom_boxplot(aes(fill=team)) +
    geom_jitter(color="gray40",
                position=position_jitter(width=0.2), alpha=0.3) +
    ggtitle("Boxplot - NL West's Sentiment Scores")

