package nowchess.tournament.http.routes

import zio.*
import zio.http.*
import zio.json.*
import zio.stream.ZStream

/** Shared assembly for the NDJSON (`application/x-ndjson`) event-stream responses.
  * Each element is encoded as one complete JSON object per line (per the API
  * contract), with a JSON heartbeat line interleaved to keep idle connections
  * alive. The heartbeat is a real JSON object (`{"type":"heartbeat"}`), never a
  * blank line, so strict line-by-line JSON consumers never choke on it.
  */
object NdjsonStream:

  /** A valid NDJSON line clients can recognise by `type` and ignore. */
  val heartbeatLine: String = """{"type":"heartbeat"}"""

  private val headers: Headers = Headers(
    Header.ContentType(MediaType("application", "x-ndjson")),
    Header.Custom("Cache-Control", "no-cache"),
  )

  private val heartbeat: ZStream[Any, Nothing, String] =
    ZStream.tick(10.seconds).as(heartbeatLine)

  /** Builds a chunked NDJSON streaming response from `events`.
    *
    * When `closeWhen` is provided, the response completes right after the first
    * event matching it (inclusive) and the heartbeat stops with it — used for
    * the game stream, which the contract says closes when the game ends. When
    * omitted the response stays open for the lifetime of `events` plus the
    * heartbeat — used for the tournament stream.
    */
  def response[A: JsonEncoder](
    events: ZStream[Any, Nothing, A],
    closeWhen: Option[A => Boolean] = None,
  ): Response =
    val content = closeWhen.fold(events)(events.takeUntil)
    val lines   = content.map(_.toJson)
    val merged  = closeWhen match
      case Some(_) => lines.merge(heartbeat, ZStream.HaltStrategy.Left)
      case None    => lines.merge(heartbeat)
    val body = Body.fromStreamChunked(
      merged.mapConcatChunk(s => Chunk.fromArray((s + "\n").getBytes))
    )
    Response(status = Status.Ok, headers = headers, body = body)
