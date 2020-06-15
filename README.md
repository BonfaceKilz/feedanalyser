### What is Feed Analyser?

This aggregates content from different channels(for now only twitter is properly
supported) and displays them in one place. In most hackathons, organisations and
sometimes events, content is spread over different places e.g. Discord, slack,
IRC, mailing list, issue trackers etc. This project aims to aggregate news from
different places and display them in one place.

For now, the project supports twitter with partial support for GitHub.

It also allows you to vote on the feeds.

You can view a demo [here](https://feed.bonfacemunyoki.com/).

### How does it work?

The program scrapes content from twitter and github with optional filters(at
least for now) and stores the data into redis queues with a score. You can then
read from the redis queue.

There's also an in-built provision for voting on queues, whereby each vote
updates the zscore of an individual tweet thereby increasing or decreasing the
likelihood of it being displayed on a page.


_Why are you storing things to Redis? Why not just generate pages straight from them?__

We'd like to extend the program later so that it can provide a source of
aggregate data to other programs. To achieve this, you'd need an intermediary
data store, and for this, Redis was chosen.

#### How do I run this?

First ensure you have [twint](https://github.com/twintproject/twint/tree/master/twint) installed- this is used to fetch data from twitter without using twitter's restrictive API.

- Start the twitter fetching daemon:

```
racket bin/add-tweets.rkt
```

- To run the server
```
racket bin/polling-server.rkt
```

#### TODO/ Suggestions/ Help Wanted

- Fetch data from Slack
- Come up with an algorithm to sieve out noise(Machine Learning)
- Expose content using an API
- Come up with a sane deployment process

#### RoadMap

- [x] Better Parsing from Twitter
- [ ] Add package and dependencies to Guix
- [ ] Integrate to GN2
- [ ] Fetch Pull Requests and merges from GitHub and display them
- [ ] Fetch Data from Slack
- [ ] Make a lib layer and add project to upstream to racket-lang.org
- [ ] Fetch Data from IRC
- [ ] Add NLP to make more meaningful data
