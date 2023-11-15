*-- Author :
C  New common block to allow GSKING, GSSTAK to allow storage of user
C  words for secondaries  SRTonse. Before calling GSKING, set NUBUF to the
C  number of user words (up to 10) to be written into KINE. Fill the words
C  themselves into UBUF. GSSTAK will take care of the rest. If GSKING is called
C  with an argument 0 then all secondaries will have identical UBUF words.
      INTEGER NUBUF
      REAL UBUF
      COMMON/SECUBUF/NUBUF,UBUF(10)
