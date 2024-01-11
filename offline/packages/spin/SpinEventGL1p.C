#include <RunHeader.h>
#include <PHIODataNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh> 
#include <PdbSpinGL1p.hh>
#include <SpinEventGL1p.h>

#include <cstdlib>

ClassImp(SpinEventGL1p)
//==========================================================
SpinEventGL1p::SpinEventGL1p(PHCompositeNode* topNode) {
  Reset();
  if( topNode )
    initialize(topNode);
};
//==========================================================
void SpinEventGL1p::Reset(){
  _runnum = -1;
  _stat_offset_beamx = false;
  _offset_beamx = -999;
};

//==========================================================
void SpinEventGL1p::initialize(PHCompositeNode *topNode)
{
  Reset();
  // First find the run header and get the run number...
  RunHeader *d_runhdr;
  PHTypedNodeIterator<RunHeader> iRUN(topNode);
  PHIODataNode<RunHeader> *RUN = iRUN.find("RunHeader");
  if (!RUN)
    {
      std::cout << PHWHERE << "SpinEventGL1p::no RunHeader Node...cancelling corrections" << std::endl;
      return ;
    }
  d_runhdr = (RunHeader*) RUN->getData();
  if (!d_runhdr)
    {
      std::cout << PHWHERE << "SpinEventGL1p::no runheader...cancelling corrections" << std::endl;
      return ;
    }
  int RunNumber = d_runhdr->get_RunNumber();
  std::cout << " SpinEventGL1p::initialize() Getting calibration adjustments for run #" << RunNumber << std::endl;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);
  PdbCalBank *spinbank;
  PdbSpinGL1p* pdbspingl1p = NULL;

  if (application->startRead())
    {
      const char* calibname = "calib.spin.gl1p";
      //
#ifdef TEST_FDB

      std::cout << " SpinEventGL1p:: -------------------------------------------- " << std::endl;
      std::cout << " SpinEventGL1p:: !!!! This is for a private fedrated DB. " << std::endl;
      std::cout << " SpinEventGL1p:: -------------------------------------------- " << std::endl;
      PHTimeStamp searchtime(2000, 1, 1, 0, 0, 0);
      spinbank = bankManager->fetchBank("PdbSpinGL1pBank", bankID, calibname, searchtime);
#else

      spinbank = bankManager->fetchBank("PdbSpinGL1pBank", bankID, calibname, RunNumber);
#endif
      //
      if (spinbank)
        {
          pdbspingl1p = (PdbSpinGL1p*) & (spinbank->getEntry(0));
          _stat_offset_beamx = pdbspingl1p->getStatOffsetBeamX();
          _offset_beamx = pdbspingl1p->getOffsetBeamX();
          while ( _offset_beamx < 0 )
            {
              _offset_beamx += nCrossing;
            }
          _offset_beamx = _offset_beamx % nCrossing;
          _runnum = RunNumber;
          spinbank->print();
          pdbspingl1p->print();
          if ( _runnum != pdbspingl1p->getRunNum() )
            {
              std::cout << PHWHERE << " error in run number of DB content." << std::endl;
              exit(0);
            }
          std::cout << " SpinEventGL1p::initialize() Bunch shift is " << _offset_beamx << std::endl;
          delete spinbank;
        }
      else
        {
          std::cout << PHWHERE << " warning in getting database entry." << std::endl;
        }
      application->commit();
    }
  else
    {
      std::cout << PHWHERE << " error accessing database" << std::endl;
      exit(0);
    }
  //
};

//==========================================================
void SpinEventGL1p::event(PHCompositeNode *dstNode){
  int iboard;

  PHTypedNodeIterator<SpinDataEventOut> iter(dstNode);
  PHIODataNode<SpinDataEventOut> *spindataeventout_node = iter.find("SpinDataEventOut");
  if (!spindataeventout_node) {
    std::cout << "SpinEventGL1p::no SpinDataEventOut node in dst/udst" << std::endl;
    return;
  }
  SpinDataEventOut* spindataeventout;
  spindataeventout = dynamic_cast<SpinDataEventOut*>(spindataeventout_node->getData());

  //std::cout<<" SpinEventGL1p::event() Offset of GL1p bunch crossing is "<<_offset_beamx<<". The status="<<_stat_offset_beamx<<std::std::endl;
  if( _stat_offset_beamx ){
    int GL1_Crossing_ID_corrected = (spindataeventout->GetGL1CrossingID() + _offset_beamx ) % nCrossing;
    spindataeventout->SetSpinGL1CrossingID(GL1_Crossing_ID_corrected);
    
    iboard = nGL1PBoard;
    while(iboard--){
      int GL1P_Crossing_ID_corrected = (spindataeventout->GetGL1PCrossingID(iboard) + _offset_beamx ) % nCrossing;
      spindataeventout->SetSpinGL1PCrossingID(iboard, GL1P_Crossing_ID_corrected);
    }
    
    int GL1PSum_Crossing_ID_corrected = (spindataeventout->GetGL1PSumCrossingID() + _offset_beamx ) % nCrossing;
    spindataeventout->SetSpinGL1PSumCrossingID(GL1PSum_Crossing_ID_corrected);
  } else {
    spindataeventout->SetSpinGL1CrossingID(-999);
    iboard = nGL1PBoard;
    while(iboard--)
      spindataeventout->SetSpinGL1PCrossingID(iboard, -999);
    spindataeventout->SetSpinGL1PSumCrossingID(-999);

  }


};
//==========================================================
