# Public Tournament Analytics Export

Der Public Tournament Server stellt nach einem abgeschlossenen Turnier einen vollständigen Analytics-Export bereit.

Der Server berechnet selbst keine Spark-Analytics. Er liefert nur die Turnierdaten in einem stabilen Format. Jedes Team kann diese Daten abrufen und in der eigenen Plattform analysieren oder direkt in der eigenen UI anzeigen.

## Systemidee

```text
Public Tournament Server
  → spielt Turniere und Games
  → stellt vollständigen Export bereit

Client / Team-Plattform
  → lädt den Export
  → analysiert die Daten selbst
  → zeigt Analytics in der eigenen UI
```

## Export Endpoint

```http
GET /api/tournament/{tournamentId}/analytics-export
```

Der Endpoint ist nur für abgeschlossene Turniere verfügbar.

Mögliche Antworten:

```text
200 OK        → Export verfügbar
404 Not Found → Tournament existiert nicht
409 Conflict  → Tournament ist noch nicht fertig
```

## Enthaltene Daten

Der Export enthält unter anderem:

```text
schemaVersion
tournamentId
format
clock
rated
nbRounds
startedAt
finishedAt
exportedAt
games
standings
```

Pro Game:

```text
gameId
round
whiteBotId
blackBotId
winner
winnerBotId
terminationReason
totalPly
moves
startedAt
endedAt
durationMillis
optional: family, strategyType, engineType, modelVersion
```

Pro Standing:

```text
botId
botName
rank
points
wins
draws
losses
nbGames
tieBreak
optional: family, strategyType, engineType, modelVersion
```

## Integration in eigene Client-UI

Ein Team-Client braucht nur:

```text
Public Server Base URL
tournamentId
analytics-export Endpoint
eigenen Analytics-/Parsing-Code
```

Typischer Ablauf:

```text
1. Tournament auswählen
2. analytics-export laden
3. Daten lokal oder serverseitig analysieren
4. Ergebnisse in der eigenen UI anzeigen
```

Mögliche UI-Ansichten:

```text
Summary
Leaderboard
Bot Statistics
ELO Ratings
Game Results
Head-to-Head
Color Performance
Termination Reasons
Fastest Wins
Medals
```

## Wichtig

Der Public Server stellt dafür einen stabilen Export der Turnier- und Spieldaten bereit. Auf dieser Basis kann jede Team-Plattform eigene Analytics berechnen und in der eigenen UI anzeigen.