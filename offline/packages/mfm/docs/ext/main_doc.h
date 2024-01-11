/*! 

	\mainpage 
	
	<p>libmfm is the library that handles the PHENIX muon and central arms magnetic field maps inside PISA (the PHENIX detector GEANT3 implementation). It is responsible to read the various magnetic field maps that have been generated over time to match the real-life field configuration, and give the magnetic field for any given point inside PHENIX spectrometer, by means of interpolations between the recorded points. 

  <p>libmfm is also used by the offline muon arm reconstruction (mutoo) to perform the same job. A independent implementation of the magnetic field map reading is used for the central arm reconstruction.
 
  <p>In PISA the magnetic field is selected by changing the value of the relevant parameter in the pisa.kumac file. 
  <p>For the offline reconstruction, by default the relevant magnetic field map is selected at run-time based on the run number that is being analysed. This map can also be selected manually by using the mMfmMT interface class. The selection uses the mMfmMT::MapFileFlags enumeration, but raw integers are also supported 

*/
