/*
  DST evaluation macro
  Author: Barbara Truett, Charles Maguire, Hugo Valle
  Creation Date: July 18, 2001

  Purpose: process real PRDFs for the evaluation of the Pad Chamber performance

  Revision History
  C.F. Maguire     July 19, 2001  Add simulation flag option, and debug flag option

*/

void pRecoPadReal(const Int_t maxEvents=1, const char *prdfIFile="realData.prdf",
                  const Int_t runNumber=10545, const Int_t simFlag=0, const Int_t debugFlag=0,
		  const char *dstOFile="dstOut.root") {

  gSystem->Load("libpreco.so");
  if(simFlag)
    setSimulationFlag();

  pfileopen(prdfIFile);
  poutfileopen(dstOFile);
  setRunNumber(runNumber);

  Int_t iTest;
  if(debugFlag) {
    //
    // Debug pause for doing attach gdb
    //
    cout << "\n debug pause " << endl;
    cin >> iTest;
    cout << "\n iTest = " << iTest << endl;
  }

  precoSelect::setMVD();  // 0 means the system is OFF
  precoSelect::setZDC();  
  precoSelect::setBBC();   // default is to have the system ON
  precoSelect::setDCH(0);
  precoSelect::setPAD();
  precoSelect::setCRK();
  precoSelect::setTEC();
  precoSelect::setTOF();
  precoSelect::setEMC();
  precoSelect::setCGL(0);
  precoSelect::setMOM(0);
  precoSelect::setMUTR(0);
  precoSelect::setMUID(0);

  prun(maxEvents);
  pend();
  pexit();

}
