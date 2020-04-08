### What is Feed Analyser?

This aggregates content from different channels and displays them in one place. In most hackathons, organisations and sometimes events, content is spread over different places e.g. Discord, slack, IRC, mailing list, issue trackers etc. This project aims to aggregate to fetch news from different places and displays them in one place.

For now, the project supports twitter and github.

You can view a demo [here](https://feed.bonfacemunyoki.com/).

### How does it work?

The program scrapes content from twitter and github with optional filters(at least for now) and stores the data into redis queues after which it generates a simple HTML page- our feed- which can you can server using any server of your choice. This page is re-generated every 15 minutes.

_Why are you storing things to Redis? Why not just generate pages straight from them?__

We'd like to extend the program later so that it can provide a source of
aggregate data to other programs. To achieve this, you'd need an intermediary
data store, and for this, Redis was chosen.

#### How do I run this?

First ensure you have [twint](https://github.com/twintproject/twint/tree/master/twint) installed- this is used to fetch data from twitter without using twitter's restrictive API.

- Generate the html feed:

```
racket feed.rkt
```

- To run the "daemon" in the background:
```
bash livefeed &
```

#### TODO/ Suggestions/ Help Wanted

- Fetch data from Slack
- Come up with an algorithm to sieve out noise(Machine Learning)
- Better UI
- Expose content using an API
- Come up with a sane deployment process
