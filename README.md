# tc - A TileCache for OpenStreetMap TileServers

A server caching tiles to reduce trafic to the net and safe resources on the real tile server.

## Usage with Docker

Just start your local tile cache server with

    docker run --rm -p 8008:8008 ratopi/tile-cache

## Usage with rebar3

Just start it

    rebar3 shell
