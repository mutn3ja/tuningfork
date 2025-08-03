# Tuningfork

A high-performance TCP proxy server built with Erlang/OTP that forwards traffic from clients to configurable upstream hosts.

## Overview

Tuningfork is a lightweight TCP proxy that accepts incoming connections and forwards all traffic to a specified upstream server. It's designed for scenarios where you need to route traffic through an intermediary server.

## Features

- **Protocol Agnostic**: Forwards raw TCP traffic without protocol-specific processing
- **Configurable Upstream**: Set upstream host via environment variable
- **High Concurrency**: Built on Erlang/OTP for excellent concurrent connection handling
- **Connection Management**: Automatic cleanup of client and server connections
- **Timeout Handling**: 60-second timeout for inactive connections

## Configuration

### Environment Variables

- `TUNINGFORK_UPSTREAM_HOST_ADDR`: The upstream host to proxy traffic to (default: `google.com`)

### Server Configuration

- **Listen Port**: 443 (configured in `tuningfork_app.erl:16`)
- **Upstream Port**: 443 (hardcoded in `proxy_protocol.erl:41`)

## Build & Run

### Prerequisites

- Erlang/OTP 27+
- Rebar3

### Building

```bash
$ rebar3 compile
```

### Running in Development

```bash
$ rebar3 shell
```

### Creating a Release

```bash
$ rebar3 as prod release
```

### Running the Release

```bash
$ _build/prod/rel/tuningfork/bin/tuningfork foreground
```

## Container Usage

Build and run using the provided Containerfile:

```bash
$ podman build -t tuningfork .
$ podman run -p 9000:443 -e TUNINGFORK_UPSTREAM_HOST_ADDR=example.com tuningfork
```

Use socat to setup port forwarding:

```bash
$ sudo socat TCP-LISTEN:443,fork TCP:localhost:9000
```

Test the proxy:

```bash
$ curl --resolve 'example.com:443:127.0.0.1' https://www.example.com
```

## Usage Examples

### Basic Proxy Setup

```bash
# Proxy traffic to example.com
$ TUNINGFORK_UPSTREAM_HOST_ADDR=example.com rebar3 shell
```

### Testing the Proxy

```bash
# Connect to the proxy (assuming it's running locally)
$ curl --resolve 'example.com:443:127.0.0.1' https://www.example.com
```

## Architecture

Tuningfork uses the Ranch library for connection handling and implements a `gen_statem` behavior for connection state management:

- **Ranch TCP Listener**: Accepts incoming connections on port 443
- **Proxy Protocol Module**: Handles bidirectional data forwarding
- **State Management**: Maintains client and server socket connections
- **Connection Lifecycle**: Automatic cleanup on connection close or timeout

## License

Apache-2.0
