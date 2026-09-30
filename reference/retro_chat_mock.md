# Mock chat constructor

Create an \`ellmer\` chat object that does not authenticate or connect
to any real provider. Useful for testing and development without network
access.

## Usage

``` r
retro_chat_mock()
```

## Value

An \`ellmer\` chat object.

## Details

The returned chat supports registering tools, setting system prompts,
and all other local operations. It will error only if you attempt to
send a message to the model.

## See also

Other chats:
[`retro_chat_replay_simulation()`](https://wlandau.github.io/retroglyph/reference/retro_chat_replay_simulation.md)

## Examples

``` r
  chat <- retro_chat_mock()
  chat$get_system_prompt()
#> NULL
```
