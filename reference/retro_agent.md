# \`retroglyph\` agent constructor.

Create a \`retroglyph\` agent object.

## Usage

``` r
retro_agent(chat, state = new.env(parent = emptyenv()))
```

## Arguments

- chat:

  An \`ellmer\` chat object with no registered tools. It is best to use
  a pragmatic low-cost model (e.g. \`"sonnet"\`) for the chat, since
  sending images to a frontier model can be expensive.

- state:

  An empty environment or empty Shiny \`reactiveValues\` list to hold
  agent state. The state must be completely empty when passed to
  \`retro_agent()\`, and it should be reserved for the agent's use only.

## Value

A \`retro_agent\` \`R6\` object.

## Examples

``` r
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    chat <- ellmer::chat_anthropic()
    agent <- retro_agent(chat)
    agent$chat$chat("ping")
  }
```
