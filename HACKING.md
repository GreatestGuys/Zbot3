# Hacking Guide

## Building

Building is as simple as running `stack build`.

## Testing Locally

You can test your changes locally in GHCi without having to connect to a real
IRC server using the `runMockBot` utility.

```
$ stack ghci
ghci> :set -XOverloadedStrings
ghci> :m + Zbot.Core.Irc.Types Zbot.Core.Bot.Mock
ghci> let services = registerService_ reputation
ghci> let events = [Shout "#channel" "nick" "!rep"]
ghci> runMockBot "./data-test/" services events
[->IRC] (RealTime) NICK mockBot
[->IRC] (RealTime) USER mockBot 0 * :Zbot v3
[->IRC] (BestEffort) PRIVMSG #channel :Zhenya has 114 rep
[->IRC] (BestEffort) PRIVMSG #channel :CFumo has 94 rep
[->IRC] (BestEffort) PRIVMSG #channel :jesse has 68 rep
[->IRC] (BestEffort) PRIVMSG #channel :will has 58 rep
[->IRC] (BestEffort) PRIVMSG #channel :Graham has 55 rep
[->IRC] (BestEffort) PRIVMSG #channel :zhenya_bot has 14 rep
```

For a service with a history dependency:

```
$ stack ghci
ghci> :set -XOverloadedStrings
ghci> :m + Zbot.Core.Irc.Types Zbot.Core.Bot.Mock
ghci> let historyHandle = history >> registerService
ghci> let services = do{h <-historyHandle; registerService_ $ replace h;}
ghci> let events = [Shout "#channel" "nick" "hello warld", Shout "#channel" "nick" "s/warld/world"]
ghci> runMockBot "./data-test/" services events
[->IRC] (RealTime) NICK mockBot
[->IRC] (RealTime) USER mockBot 0 * :Zbot v3
[->IRC] (BestEffort) PRIVMSG #channel :nick: hello world
```

## Running

Running the zbot binary can also be handled by `stack`:

```
stack exec zbot -- --server irc.myserver.net --channel '#zbot3' --date ./data
```

For a full list of flags and what they do run `stack exec zbot -- --help`.
