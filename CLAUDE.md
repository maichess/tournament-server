# Project: NowChess Tournament Server

Standalone tournament server for chess bots. Not part of the maichess microservice ecosystem — no maichess dependencies allowed.

## What It Does

Engines connect via HTTP to play in tournaments. The server handles lifecycle (create/join/start), Swiss pairings, standings, and NDJSON event streaming. It does **not** render UI or manage game move transport — that happens through external board/bot endpoints.

## Stack

- Scala 3.8 / sbt 1.12
- ZIO for effects, concurrency, and streaming
- Target: HTTP server (ZIO HTTP or Tapir + ZIO backend)

## API Contract

`api/openapi.yaml` is the source of truth. Implement endpoints exactly as specified.

Key design points:
- Auth via Bearer JWT on most endpoints; some are public (tournament list, get, results, round pairings, game export)
- NDJSON streaming (`application/x-ndjson`) for tournament events, results, and game export
- Game export supports both PGN (`application/x-chess-pgn`) and NDJSON via `Accept` header
- Tournament status lifecycle: `created` → `started` → `finished`
- Swiss pairing logic runs server-side; actual game creation delegates to `POST /api/board/game` on the external platform

## Bot Flow

```
POST /api/tournament              # director creates
POST /api/tournament/{id}/join    # bots join
POST /api/tournament/{id}/start   # director starts

GET  /api/tournament/{id}/stream  # bot opens NDJSON stream

-- per round, bot receives gameStart event --
GET  /bot/stream/game/{gameId}    # external: stream game state
POST /bot/game/{gameId}/move/{uci} # external: submit moves
-- round ends, next round starts --

GET  /api/tournament/{id}/results # final standings
```

## Code Quality

- Keep modules small and focused — one responsibility per file
- Separate concerns clearly: routes, domain logic, persistence, and external integrations belong in distinct packages
- Domain types (Tournament, Pairing, Result, etc.) go in a dedicated `domain` package with no framework imports
- Use ZIO layers for dependency injection — wire services via `ZLayer`, not hardcoded instantiation
- Keep side effects at the edges; domain logic should be pure functions where possible
- Prefer descriptive names over comments — if a function needs a comment to explain what it does, rename it
- Write tests for domain logic (pairing, scoring, lifecycle transitions) independently of HTTP or persistence

## Testing

- **100% test coverage is mandatory.** Every change must maintain 100% coverage — no exceptions. A change that drops coverage below 100% will not be approved.
- Use `zio-test` as the test framework
- Unit test domain logic (pairing algorithms, scoring, lifecycle state transitions) with pure functions — no ZIO runtime needed for most of these
- Integration test routes by hitting endpoints with a test HTTP client against in-memory service layers
- Test NDJSON streaming endpoints by collecting emitted chunks and asserting on the sequence of events
- Test error paths: invalid state transitions (e.g. starting an already-started tournament), auth failures, missing resources
- Keep tests fast — mock or stub external calls (e.g. `POST /api/board/game`) via test `ZLayer` implementations
- Run `sbt test` before every commit; CI must pass all tests

## What NOT to Do

- Do not add maichess-specific dependencies, proto imports, or coupling
- Do not build rendering or UI — the server is data-only (JSON, NDJSON, PGN)
- Do not modify `api/openapi.yaml` without explicit approval
- Do not deviate from the OpenAPI spec; if something is unimplementable, document it in `CONTRACT_NOTES.md`
