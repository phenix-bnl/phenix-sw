
void run_qa(const char* dstname="data.dst" , const char* qaoutname="qaout.root", const int nevts = 10)
{
// Load/intialize DST QA library
  gSystem->Load("libfun4all.so");
  gSystem->Load("libdstqa.so");
  gSystem->Load("libqasummary.so");
  Fun4AllServer *se = Fun4AllServer::instance();
	
//----------------------------------------------------------------------
/** 
    QAReco(char *QAoutputFile, char *option)
    option = 
    L -- Forward Calorimeter
    Z -- Zero Degree Calorimeter
    B -- Beam-Beam Counter
    N -- NTC Counter ( Run2 p-p )
    O -- NTCP Counter ( Run 3) 
    S -- T-zero Counter
    V -- Multiplicity Vertex Detector
    D -- Drift Chamber
    P -- Pad Chamber
    T -- Time Expansion Chamber
    F -- Time of Flight Detector
    E -- Electromagnetic Calorimeter
    C -- Ring Imaging Cherenkov Counter
    M -- Muon Tracker
    U -- Muon Identifier
    R -- EMC-RICH (ERT) trigger 
    electron -- EWG
    min_bias -- loop only into Minimum Bias Events
    
the default option is "ZBNSVDPTFECMUR"
*/

  //SubsysReco *qa = new QAReco(qaoutname,"LZBOSVDPTFECMURelectron");
  SubsysReco *qa = new QAReco(qaoutname,"BMU");
  se->registerSubsystem(qa);
  dfileopen(dstname);
  drun(nevts);
  se->EndRun();
}
