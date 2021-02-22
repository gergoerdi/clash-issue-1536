stack build && \
stack exec -- clash -isrc -outputdir _build/ --verilog src/Compucolor2.hs \
      -fclash-inline-limit=100 \
      -fclash-spec-limit=100 \
      2>&1 | tee log.txt
