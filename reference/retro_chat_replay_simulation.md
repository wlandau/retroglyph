# Replay chat constructor

Create an \`ellmer\` chat object that replays a fixed, real tool-call
transcript instead of talking to a model. Useful for exercising
\[retro_agent()\]/\[retro_app()\] end to end, deterministically and
without a network call.

## Usage

``` r
retro_chat_replay_simulation()
```

## Value

An \`ellmer\` chat object.

## Details

The returned chat is only good for one image: the
\`inst/simulation.png\` example shipped with the package. Its
\`chat()\`/\`stream_async()\` methods replay the exact tool-call
sequence a live model once took to reconstruct that specific image -
quantize, label, distill, then data, each called with the arguments that
model read off \`inst/simulation.png\` - then return a canned summary of
the result. \`chat()\` returns that summary directly, and
\`stream_async()\` returns a promise of it, so \[retro_app()\] streams
the replay the same way it streams a real model's response. Because the
replayed calls run the real tools \[retro_agent()\] registers (not
canned tool results), the state ends up populated exactly as it would
from a real reconstruction of \`inst/simulation.png\`, but any other
image will not track this chat's hard-coded axis calibration and risk
table, so results are only sensible for \`inst/simulation.png\`.

## See also

Other chats:
[`retro_chat_mock()`](https://wlandau.github.io/retroglyph/reference/retro_chat_mock.md)

## Examples

``` r
  if (identical(Sys.getenv("RETROGLYPH_EXAMPLES"), "true")) {
    chat <- retro_chat_replay_simulation()
    agent <- retro_agent(chat)
    agent$register(system.file("simulation.png", package = "retroglyph"))
    agent$chat$chat("Reconstruct the data from the registered plot.")
    agent$data()
    if (interactive()) {
      agent$compare()
    }
    # Run the Shiny app against the replay chat instead of a real model:
    shiny::runApp(retro_chat_replay_simulation())
  }
```
