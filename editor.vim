:command! -nargs=0 GHCid :terminal! ghcid --command="cabal repl" --test ":main source.srt rule.srt result.srt"

:argadd Main.hs
