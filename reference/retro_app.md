# Launch a \`retroglyph\` Shiny app.

Create and return a Shiny app that wraps a \[retro_agent()\] in a
browser-based UI for uploading Kaplan-Meier images, chatting with the
model, and reviewing the reconstructed image, risk table, hazard ratios,
and survival quantiles/probabilities.

## Usage

``` r
retro_app(
  chat,
  theme = bslib::bs_theme(version = 5, preset = "flatly", primary = "#2C3E50"),
  authentication_ui = NULL,
  authentication_server = NULL,
  onStart = NULL,
  options = list()
)
```

## Arguments

- chat:

  An \`ellmer\` chat object with no registered tools, as in
  \[retro_agent()\]. Cloned once per session.

- theme:

  A \`bslib\` theme object from \`bslib::bs_theme()\`, passed to
  \`bslib::page_sidebar()\`. The default is the Bootstrap 5 \`"flatly"\`
  preset with a dark navy primary color. The app puts a light/dark mode
  toggle (\`bslib::input_dark_mode()\`) in the top right of the main
  panel and starts in light mode. Bootstrap 5 color modes mean a single
  theme covers both modes.

- authentication_ui:

  \`NULL\`, or a Shiny tag/taglist to insert into the app's UI, outside
  the main navigation panels. Use this to layer on authentication UI
  (e.g. an SSO login gate) without \`retroglyph\` itself depending on
  any authentication package.

- authentication_server:

  \`NULL\`, or a function with arguments \`input\`, \`output\`,
  \`session\`, called first inside the app's server function. Use this
  together with \`authentication_ui\` to layer on server-side
  authentication logic.

- onStart:

  See \`shiny::shinyApp()\`.

- options:

  See \`shiny::shinyApp()\`.

## Value

A Shiny app object from \`shiny::shinyApp()\`.

## Details

\`chat\` is cloned once per Shiny session (via \`chat\$clone(deep =
TRUE)\`), so the same \`chat\` object can be reused across multiple
concurrent app sessions without one session's conversation or registered
tools leaking into another's.

## Examples

``` r
  if (identical(Sys.getenv("RETROGLYPH_EXAMPLES"), "true")) {
    retro_app(retro_chat_replay_simulation())
 }
```
