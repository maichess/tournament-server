# Contract Notes

Changes from the original OpenAPI spec v1.0.0 to v2.0.0.

## Self-contained game management

The original spec delegated game management to external endpoints (`POST /api/board/game`, `GET /bot/stream/game/{gameId}`, `POST /bot/game/{gameId}/move/{uci}`). The tournament server now manages games internally.

New endpoints added under `/api/tournament/{id}/game/{gameId}/`:
- `GET .../game/{gameId}` — get game state (public)
- `GET .../game/{gameId}/stream` — NDJSON game event stream (auth)
- `POST .../game/{gameId}/move/{uci}` — submit move (auth, turn-enforced)

New schemas: `GameState`, `GameEvent`.

## Tournament format extensions

`CreateTournamentForm` extended with:
- `format` (enum: swiss, singleElimination, doubleElimination, groupStage, league; default: swiss)
- `startPosition` (FEN string; default: "standard")
- `matchesPerPairing` (integer, min 1; default: 1; odd numbers for best-of-N)
- `groupSize` (integer, required when format=groupStage)

`TournamentInfo` extended with `format`, `matchesPerPairing`, `startPosition`.

`Pairing` extended with `matchesPerPairing` and `matchResults` array for best-of-N tracking.

## Engine identity enforcement

Bot identity is derived exclusively from the JWT `sub` claim. The `POST .../move/{uci}` endpoint enforces that only the bot whose turn it is may submit a move, verified against the token. No request body carries a self-declared bot identity.
