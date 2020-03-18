## Feedback Analyser

To display the tweets:

```
racket feed.rkt -u "AbePalmer" -a "display"
```

To store the tweets:
```
racket feed.rkt -u "AbePalmer" -a "store"
``**

Tweets are stored in a queue as a JSON in the format:

```
{
    'author': 'John Nichols',
    'time': 'Time posted: March 13, 2020 19:20:18 (UTC)',
    'tweet': '<p class=\"timeline-Tweet-text\" lang=\"en\" dir=\"ltr\">Sicily has figured out this whole self-isolation thing.<a href=\"https://twitter.com/hashtag/COVID19?src=hash\" data-query-source=\"hashtag_click\" class=\"PrettyLink hashtag customisable\" dir=\"ltr\" rel=\"tag\" data-scribe=\"element:hashtag\"><span class=\"PrettyLink-prefix\">#</span><span class=\"PrettyLink-value\">COVID19</span></a> <a href=\"https://twitter.com/hashtag/CoronavirusPandemic?src=hash\" data-query-source=\"hashtag_click\" class=\"PrettyLink hashtag customisable\" dir=\"ltr\" rel=\"tag\" data-scribe=\"element:hashtag\"><span class=\"PrettyLink-prefix\">#</span><span class=\"PrettyLink-value\">CoronavirusPandemic</span></a> \n  </p>',
}

```


**TODO:**:
- Add filters
- Fetch followers
- Write more succinct README
- Make redis conf more explicit


