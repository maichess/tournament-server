# Tournament Server

Zentraler Turnierserver zur Vermittlung zwischen Schach-Bots, Verwaltung von Turnieren, Matchmaking und Ergebnisspeicherung.

---

## 🚀 API-Spezifikation

Die API ist vollständig als OpenAPI 3.0.3 spezifiziert und im Repository hinterlegt:
👉 **[api/openapi.yaml](./api/openapi.yaml)**

---

## 📊 Analytics Export

Abgeschlossene Turniere können als strukturierter Export abgerufen werden – inklusive aller Games, Standings und Bot-Metadaten:
👉 **[docs/analytics.md](./docs/analytics.md)**

---

## 🏛️ Architektur (Ansatz 1)

* **Zentraler Server:** Einheitliche Plattform, mit der sich alle Bots verbinden.
* **API-Standard:** Schnittstellen orientieren sich an den Vorgaben von *Team Now-Chess*, um Kompatibilität zu gewährleisten.

---

## 🛠️ Entwicklungsregeln

### 1. Branch-Schutz
* Der `main`-Branch ist geschützt.
* Merges auf `main` erfolgen nur, wenn die Version nachweislich lauffähig ist.

### 2. Pull Requests & Reviews
* **Kleine PRs:** Codeänderungen modular halten und auf mehrere Dateien verteilen, um Merge-Konflikte zu minimieren.
* **Schnelle Reviews:** PRs zeitnah prüfen und mergen, um Backlogs zu vermeiden.

### 3. Code-Disziplin
* Trotz KI-Unterstützung ist hohe Disziplin bei den Code-Standards erforderlich.

---

## 🔑 Repository-Zugriff

* Einladungen werden durch **@Floo** verwaltet. 
* Fehlende Zugriffe per GitHub-Username an ihn melden.
