# Laboratory work nr. 2

*YouTube to mp3 converter/downloader*

### How it works

This is a program for finding and downloading mp3 songs. It uses YouTube
and Auderio, a service that converts youtube videos to mp3 songs. When the program
starts, the user has to enter a name of a song or a phrase that is present in the name.
This is used to perform a search query on youtube and fetch top 5 search results.
The user, then, can choose one of the five and by doing so a the conversion process
is initiated and the obtained mp3 file is downloaded.


### HTTP

The purpose of this lab is getting to know HTTP which means that we cannot use any
library that would provide such functionality out of the box and have to implement
something like this ourselves. For the given task it was enough to implement only
a function form making GET requests, however for working with Google APIs it was
necessary to support SSL/TLS encrypted connections. The *HTTP.hs* module contains
the necessary tools for performing a GET request with custom headers, as well
as for parsing the obtained response (status line, headers and response body).
Chunked responses are also supported.


### YouTube

Google provides a great bunch of different APIs for all kind of service, including,
of course, YouTube. In order to use it, all I had to do was to log into my account and
access the developer console in order to generate an API key. As I mentioned above
it is importand to have support for HTTPS requests, because otherwise it just won't work.
I was interested in a particular feature of YouTube API, which is listing a number of
videos as search results for a given search string. This can be done by performing the following request:

```
GET https://www.googleapis.com/youtube/v3/search?part=snippet&type=video&q=QUERY&key=KEY
```
- `QUERY` - the string to lookup (name of the song)
- `KEY` - API key provided by Google

The result of the query is a JSON object containing information such as video ID and title,
and a bunch of other interesting stuff that is not really necessary for this lab work.


### Auderio

If finding the necessary video was pretty straight forward, converting it to mp3
was not a trivial task. After looking up some public APIs and finding little usefull
things I recurred to exploing how some online conversion services work.
- The first hit was youtube-mp3.org, however after 'beautifying' the client Javascript
it was obvious that their developers invested quite some time into making it a lot harder
for other people to use the routes for automated scripts.
- The next choice fell on youtubeinmp3.com who claim to have an API but after several tests
it turned out to be of little use.
- After some more searching, I found out that mashape.com provide an API from auderio.com,
and instead of fetching a credit card, I decided to dig deeper into the client Javascript of auderio.com.
As it turned out it was quite easy to transfer the functionality into my own code.

Auderio supports 3 kinds of requests for converting a youtube video into an mp3 file:
- `GET http://auderio.com/download?url=LINK_TO_VIDEO` - initiates a conversion process
- `GET http://auderio.com/check-download?id=VIDEO_ID` - returns a JSON with the status of conversion
- `GET http://auderio.com/get-cloud-link?id=VIDEO_ID` - returns a JSON which contains a link to the mp3 file

This functionality is extracted into a separate module *Auderio.hs*.


### Conclusion, sort of..

Overall I had fun writing code for this laboratory work, with the exeption of a couple
of hours lost on debugging Haskell and making HTTP work over SSL. It was also
an interesting experience of digging into foreign code in order to to provide similar
features.

