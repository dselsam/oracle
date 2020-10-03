# oracle

Haskell prototype of the Search Transformer and Universal Oracle.

See [recent talk](https://www.youtube.com/watch?v=GtAo8wqWHHg) for overview and motivation.

## Building

- Install [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- Install [Protocol buffers](https://developers.google.com/protocol-buffers)
- Install [proto-lens](https://github.com/google/proto-lens)
  - `stack install proto-lens-protoc`
  - TODO(dselsam): confirm this is the only variant that needs to be installed
- Bash script to compile the protocol buffers: `protos/build_protos.sh`
- `python3-pip` requirements for learning: `python/requirements.txt`
