c     File:- EventCode.Inc
c
c	Orignal Author: Charles F. Maguire
c	Creation date: February, 1999
c
c	Purpose: set up event codes for use in e_put_dst header writing
c
c	Revision History
c       Name                    Date        Comment
c       C.F. Maguire            6/15/99     Added rv_chi code
c       C.F. Maguire            6/23/99     Added vni code
c	C.F. Maguire            1/05/2001   Added nexus code
c	C.F. Maguire            5/27/2002   Added forced acceptance of neutrals
c
c     ====================
        integer hijet, venus, fritiof, luciae, hijing, rqmd, nexus
	integer	pythia, rv_phi, rv_jpsi, rv_chi, vni, sngl_jpsi, sngl_phi
	integer text_file, sngl_neutral
        parameter (hijet   = 1)
        parameter (venus   = 2)
        parameter (fritiof = 3)
        parameter (luciae  = 4)
        parameter (hijing  = 5) 
        parameter (rqmd  = 6) 
	parameter (pythia = 7)
	parameter (vni = 8)
	parameter (nexus = 9)
        parameter (rv_phi = 101)
        parameter (rv_jpsi = 102)
        parameter (rv_chi = 103)
        parameter (sngl_jpsi = 104)
        parameter (sngl_phi = 105)
	parameter (text_file = 106)  
	parameter (sngl_neutral = 107)  
