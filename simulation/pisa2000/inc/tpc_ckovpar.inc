
	integer Nck
	parameter ( Nck = 45 )      ! # of momentum bins for Cerenkov photons

	common/ckovpar/ 
     > 	ppckov(Nck), 
     > 	rindex_dummy(Nck), 
     >  rindex_gas(Nck),  rindex_ch4(Nck), rindex_cf4(Nck), 
     >	absco_gri(Nck),   absco_csi(Nck),  absco_mylar(Nck),
     >	absco_gas(Nck),   absco_ch4(Nck),  absco_cf4(Nck),		
     >	effic_dummy(Nck), effic_csi(Nck)

	real ppckov, 
     >  rindex_dummy, 
     >	rindex_gas,  rindex_ch4, rindex_cf4,
     >	absco_gri,   absco_csi,  absco_mylar,
     >  absco_gas,   absco_ch4,  absco_cf4, 
     >  effic_dummy, effic_csi
