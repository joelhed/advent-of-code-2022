# advent-of-code-y2022

I plan on solving this year's Advent of Code using Haskell, primary reason being that I find the language interesting and worth practicing.

## Getting started

This uses [Stack](https://docs.haskellstack.org/en/stable/) as a build tool.

```console
$ stack build

# This runs the latest calendar part available.
$ stack exec advent-of-code-y2022-exe

# You can also specify the day. It then runs the latest part for that day.
$ stack exec advent-of-code-y2022-exe 1

# Or you can specify the part as well
$ stack exec advent-of-code-y2022-exe 1 1

# You can also specify an option to download your input from the aoc server
# This requires you to put your session cookie in a file called `session_id`
$ stack exec advent-of-code-y2022-exe --download
```

## Animate day 9

You can watch a console animation of the rope simulation from day 9 part 2 with the
following command.

```console
$ stack exec animate-day9
```
