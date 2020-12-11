stack build
stack exec -- clash -isrc -outputdir _build/ --verilog src/Board.hs \
      -fclash-debug DebugSilent \
      -fclash-inline-limit=100 \
      -fclash-spec-limit=30 \
      2>&1 | tee log.txt
