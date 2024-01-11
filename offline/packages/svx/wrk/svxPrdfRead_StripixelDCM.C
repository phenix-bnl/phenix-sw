#include <string>
using namespace std;

void svxPrdfRead_StripixelDCM(int nEvents = 0, char *filein="/direct/phenix+zdata03/phnxreco/VTX/prdf/run11/eventdata/EVENTDATA_P00-0000349667-0000.PRDFF", int fRunFlag=11)

//void svxPrdfRead_StripixelDCM(int nEvents = 0, char *filein="/phenix/scratch/rnouicer/Run12/pp200GeV/EVENTDATA_P00-0000363092-0031.PRDFF", int fRunFlag=12)
{
  //cout << "Loading libraries...\n";
  
  //gSystem->Load("libsvx");
  gSystem->Load("~/offline/packages/svx/install/lib/libsvx");

  gSystem->Load("libEvent.so");
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");

  
  char DSTname[200];
  char Mapname[200];
  size_t find_s;
  string sfilein = filein;

  /*
  find_s=sfilein.find("EVENTDATA");
  if (find_s!=string::npos){
    string runnumber;
    runnumber=sfilein.substr(find_s+14,15);
    cout<<"runnumber and segment: "<<runnumber<<endl;
    sprintf(Mapname,"map_%s.txt",runnumber.c_str());
  }
  else {
    cout<<"Check input file name"<<endl;
    return;
  }
  */

  //---------------------------------------------
 if (find_s!=string::npos){
    string runnumber;
    runnumber=sfilein.substr(find_s+71,81);
    cout<<"runnumber and segment: "<<runnumber<<endl;
   int irun = atoi(runnumber.c_str());
      cout<<"runnum: "<<irun<<endl;

     string segnumber;
    segnumber=sfilein.substr(find_s+82,86);
    cout<<"segment: "<<segnumber<<endl;
   int iseg = atoi(segnumber.c_str());
      cout<<"segnum: "<<iseg<<endl;

    sprintf(Mapname,"map_%s.txt",runnumber.c_str());
  }
  else {
    cout<<"Check input file name"<<endl;
    return;
  }
 //----------------------------------------------------

  Fun4AllServer *se = Fun4AllServer::instance();
  se->Verbosity(0);
  recoConsts *rc = recoConsts::instance();

  SubsysReco *svxpar = new SvxParManager();
  se->registerSubsystem(svxpar);

  SubsysReco *svxdecode = new SvxDecode();
  (dynamic_cast<SvxDecode*>svxdecode)->setAdcOffset(24);
  (dynamic_cast<SvxDecode*>svxdecode)->setAdcCutoff(0);
  se->registerSubsystem(svxdecode);

  SubsysReco *ana = new SvxStripFindHotDead();
  if(fRunFlag==11) {
    (dynamic_cast<SvxStripFindHotDead *>ana)->setRunFlag(SvxStripFindHotDead::Run11); }
  else if(fRunFlag==12) {
    (dynamic_cast<SvxStripFindHotDead *>ana)->setRunFlag(SvxStripFindHotDead::Run12); }
  else {
    cout << "unknown run flag ..." << endl; return; }

  (dynamic_cast<SvxStripFindHotDead *>ana)->Load_ThresholdFile("threshold.h"); // set threshold file

  //Output file as Ntuple 
  TString fName = TString("/phenix/scratch/rnouicer/Run11DCMtest100PRDFs/test2/ROOT/VtxStripDeadChannels_")+long(irun)+"--"+long(iseg)+".root";  (dynamic_cast<SvxStripFindHotDead *>ana)->setOutputName(fName);

  //Output file as txt 
  //fName = TString("/phenix/scratch/rnouicer/Run11DCMtest100PRDFs/test2/TXT/VtxStripDeadChannels_")+long(irun)+"--"+long(iseg)+".txt";
  //(dynamic_cast<SvxStripFindHotDead *>ana)->set_OutputFileName(fName.Data());// map name

  (dynamic_cast<SvxStripFindHotDead *>ana)->set_MarkType(1);// mark type n bad modules
  (dynamic_cast<SvxStripFindHotDead *>ana)->set_ThresholdShift(24);//have to be same as (dynamic_cast<SvxDecode*>svxdecode)->setAdcOffset(24);
  (dynamic_cast<SvxStripFindHotDead *>ana)->Verbosity(0);
  se->registerSubsystem(ana);


  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(filein);
  se->registerInputManager(in);

  se->run(nEvents); //se->run(0) to process all events 
  se->End();

  cout<<"End of Processing of Dead Channel map"<<endl; 

}
