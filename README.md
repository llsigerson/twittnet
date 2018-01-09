# twittnet
Tools for mapping individual social networks on Twitter with R

These tools are designed to map out local ego networks for Twitter users. These are "1.5 ego" networks, meaning they include the user's connections to their contacts on Twitter, as well as connections among these contacts. 

Currently, the tools focus on reciprocated relationships on Twitter, i.e. users who follow or mention each other. This makes more efficient use of the Twitter API's rate limit, and hopefully focuses on connections that are more likely to be meaningful.

Types of Connections

The tools can map various types of connections among users (AKA "ties"). The two main types of ties are following and mentions. Following ties are dichotomous (two users are either connected or not). They probably represent more stable, long term relationships on Twitter. However, since no effort is required to maintain these relationships, and because of "follow for follow" practices on Twitter, as well as the ability to mute someone that you follow, there's a risk of these networks including meaningless connections.
The functions in this repository that are used to map out reciprocated following ties are: get_followers2, get_friends2, recipfollowers, and recipfollowers_network.

Mention ties are based on reciprocated mentions of each other, which also include retweets and replies. These relationships require active maintenance, and users tend to have fewer of these connections. Since the API only allows access to a user's recent tweets, these ties are less stable. For example, replies between two users in a single thread could look like a very strong relationship, even if they never talk to each other again.  The default for the recipmentioners function is to include all connections that have a minimum of two mentions in the last 60 days, but this is customizable.
The functions in this repository that are used to map out reciprocated mentions ties are get_tweets, get_mentionees, recipmentioners, and recipmentioners_network.

