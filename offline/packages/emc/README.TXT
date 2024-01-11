
30/08/2007 jozsef imrek <mazsi@bnl.gov>:



the huge emcal library has been split into smaller libraries.  the list of the 
new libraries:


libemc-common.so:	services used by the other libemc-* libraries. you
			probably do not want to link your code directly with
			this library.


libemc-datatypes.so:	library containing all code that is necessary to read
			emc data structures.  if you only read the DST files
			(is do not recalibrate, etc) then you should link your
			code with this library.


libemc-calibration.so:	this library contains the [re]calibrators for the data
			structures in libemc-datatypes.so.


libemc-reconstruction.so: this library contains code for reconstruction, ie to 
			fill emcClusterContainers from emcTowerContainer.  you
			probably do not want to use this library directly.


libemc-simulation.so:	code for pisa -> DST processing.  link your pisaToDST
			related code to this library instead of libemc.so, it
			should contain everything you need.


libemc-realdata.so:	code for PRDFF -> DST processing.  link your real data
			processing code to this library instead of libemc.so, 
			it should contain everything you need.


libemc.so is provided for compatibility reasons for a transition period.


