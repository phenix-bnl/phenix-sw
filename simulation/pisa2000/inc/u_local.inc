c
c   Include file for user sorting parameters.
c
c        n_events_to_sort  : integer; Number of events to sort
c        use_p4vec          : logical; if true, p4vec(1..4,i) is filled,
c                                otherwise px, py, pz and energy are filled.
c
        integer*4 n_events_to_sort
        logical*4 use_p4vec
c
        common /u_local/ n_events_to_sort, use_p4vec
c
