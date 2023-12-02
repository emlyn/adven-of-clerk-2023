# 🎄 Advent of Clerk 2023

My attempt at [Advent of Code](https://adventofcode.com)
with [Clerk](https://clerk.vision).

Uses [Advent of clerk](https://github.com/nextjournal/advent-of-clerk),
but I can't have it as a github fork because I have another copy forked for 2022,
and github won't let me have 2 forks of the same repo...

## View notebook output

You can see the notebook visualisation of the code on
[CLERK.garden](https://github.clerk.garden/emlyn/advent-of-clerk-2023).

## Run locally

Clone this repo, make sure you have [Clojure
installed](https://clojure.org/guides/install_clojure), then run:

``` shell
clj -M:nextjournal/clerk nextjournal.clerk/serve! --watch-paths src --port 7878 --browse
```

This will start the Clerk webserver on port 7878 and watch the `src/`
directory for changes and open Clerk in your browser.

Open one of the files there, e.g. `day_01.clj`, make a change and save
it. You should then see these changes reflected in the browser.

<sup><sub>If Clerk Garden is not up to date, [update](https://github.clerk.garden/emlyn/advent-of-clerk-2023?update=1) it.</sub></sup>
