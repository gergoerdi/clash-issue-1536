stack build && \
stack exec -- clash -isrc -outputdir _build/ --verilog src/SpaceInvaders.hs \
      -fclash-inline-limit=60 \
      -fclash-spec-limit=40 \
      2>&1 | tee log.txt
