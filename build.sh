stack build && \
stack exec -- clash -isrc -outputdir _build/ --verilog src/Board.hs \
      -fclash-inline-limit=30 \
      2>&1 | tee log.txt
