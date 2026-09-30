# Feutran
Discord bot in Fortran that interacts through a websocket
This is the public repository for this project

## What to expect

-Minimal but functional implementation of Discord's Gateway API

-Implementation of the Secure WebSocket protocol with fortran-curl enhanced by a custom module

-Improved implementation of Discord's HTTPS API based on skynet-fortran

## What is this bot

Feurtran is an automated implementation of the French "quoi - feur" (and it's variant "quoi - coubeh") joke.
If a message containing the string "quoi" appears in a channel or a server that is monitored, the bot answers to the message or uses
emoji reactions according to what has been setup in the config.json file. It also reacts to the "FEURTRAN HELP" command.

It works right out of the box on servers (reading private messages is possible but not configured in the provided code) as long as
the config file is filled properly (check said file for further details).

Most of the bot logic can be found in the feurtran.f90 file.
To stop the program properly, send a SIGINT signal (by pressing Ctrl+C in your terminal for example).

## Possible issue

fortran_curl_lacune.f90 (curlplus module) represents the curl_off_t type with c_int64_t. This may not be true on 32 bits systems and is just an assumption I made.
Any hindsight on how to handle this peculiar type would be highly appreciated.

## Various projects that made this possible

https://github.com/MrGlockenspiel/skynet-fortran the core of this whole project

https://github.com/jacobwilliams/json-fortran

https://github.com/scivision/fortran-sleep

https://github.com/interkosmos/fortran-curl

https://github.com/wcdawn/ftime

https://github.com/fortran-lang/http-client

