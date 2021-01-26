stack build && \
stack exec -- clash -isrc -outputdir _build/ --verilog src/Board.hs \
      -fclash-inline-limit=40 \
      -fclash-spec-limit=32 \
      2>&1 | tee log.txt
