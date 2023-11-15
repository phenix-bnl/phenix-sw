C
C  NTRU_EVT   is the event number from the particle generator Monte Carlo,
C  NSUB_EVT   is current sub-event number in the true event,
C  NSUBEVENTS is the total number of subevents in this event
C  END_EVTFLG is true for the last sub-event in a true event.
C  POPSUB     is specified in input data cards, & is the # of
C               particles to define a sub-event. SRTonse  1-SEP-1992
C  IEVPRT     is the index of the next particle to be fetched from input event
C  T0START    is the start time (seconds) from T0INIT
C
      INTEGER NTRU_EVT, NSUB_EVT, IPOPSUB, IEVPRT, NSUBEVENTS
      LOGICAL END_EVTFLG
      REAL T0START
 
      COMMON /SUBEVT/ NTRU_EVT, NSUB_EVT, END_EVTFLG, IPOPSUB,
     +                IEVPRT, NSUBEVENTS, T0START
 
