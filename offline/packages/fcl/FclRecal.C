#include "FclRecal.h"

//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"

#include "FclCalib.h"
//INCLUDECHECKER: Removed this line: #include "FclConsts.h"
//INCLUDECHECKER: Removed this line: #include "FclRaw.h"
#include "FclOutv1.h"
#include "FclIndexer.h"
#include "recoConsts.h"

#include "PHGlobal.h"
#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHONode_t;

FclRecal::FclRecal(const char *name)
  : southCal(0), northCal(0)
{
  ThisName = name;
  return;
}

FclRecal::~FclRecal()
{
  if (southCal!=0) delete southCal;
  if (northCal!=0) delete northCal;
}

int FclRecal::Init(PHCompositeNode *topNode)
{
   return 0;
}

int FclRecal::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  PHTimeStamp TimeStp = rc->get_TimeStamp();

  topNode->print();
  
  if(southCal!=0) delete southCal;
  southCal = new FclCalib;
  southCal->setSide(FCALSOUTH);
  southCal->getDatabaseInfo(TimeStp);

  if(northCal!=0) delete northCal;
  northCal = new FclCalib;
  northCal->setSide(FCALNORTH);
  northCal->getDatabaseInfo(TimeStp);

  CreateNodeTree(topNode);
  
  return 0;
}

int FclRecal::CreateNodeTree(PHCompositeNode *topNode){
   
   PHNodeIterator iter(topNode);
   PHTypedNodeIterator<FclOut> fclOutiter(topNode);
   PHIODataNode<FclOut> *fclOutNorthNode = fclOutiter.find("fclOutNorth");
   PHIODataNode<FclOut> *fclOutSouthNode = fclOutiter.find("fclOutSouth");
   
   PHCompositeNode* dstNode = 0;
   dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
   if(!dstNode) return -1;
   
   if(!fclOutNorthNode)
   {
      FclOut* fclNorth = new FclOutv1();
      
      dstNode->addNode( new PHIODataNode <PHObject>(fclNorth, "fclOutNorth", "PHObject")
                        );      
   }
   
   if(!fclOutSouthNode)
   {
      FclOut* fclSouth = new FclOutv1();
      dstNode->addNode( new PHIODataNode <PHObject>(fclSouth, "fclOutSouth", "PHObject")
                        );
   }
   
   
   return 0;
}

int FclRecal::process_event(PHCompositeNode *topNode){

  //Ensure Calibrations are initialized
  if(southCal == 0) InitRun(topNode);

  // First setup the node structure and get your objects in order:

  FclRaw* fclRawNorth = 0;
  FclRaw* fclRawSouth = 0;
  FclOut* fclOutNorth = 0;
  FclOut* fclOutSouth = 0;
  
  PHTypedNodeIterator<FclRaw> fcliter(topNode);
  PHIODataNode<FclRaw> *fclRawNorthNode = fcliter.find("fclRawNorth");
  PHIODataNode<FclRaw> *fclRawSouthNode = fcliter.find("fclRawSouth");

  PHTypedNodeIterator<FclOut> fclOutiter(topNode);
  PHIODataNode<FclOut> *fclOutNorthNode = fclOutiter.find("fclOutNorth");
  PHIODataNode<FclOut> *fclOutSouthNode = fclOutiter.find("fclOutSouth");
  
  // Now get the raw data tables:
  if (fclRawNorthNode){
    fclRawNorth = fclRawNorthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclRawNorth node missing..." << endl;
    return 0;
  }

  if (fclRawSouthNode){
    fclRawSouth = fclRawSouthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclRawSouth node missing." << endl;
    return 0;
  }

  if (fclOutNorthNode){
    fclOutNorth = fclOutNorthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclOutNorth node missing..." << endl;
    return 0;
  }

  if (fclOutSouthNode){
    fclOutSouth = fclOutSouthNode->getData();
  }
  else{
    cout << PHWHERE
	 << "WARNING: FclOutSouth node missing." << endl;
    return 0;
  }



  float zdceN = FCL_INVALID_FLOAT;
  float zdceS = FCL_INVALID_FLOAT;


  // If PHGlobal Present update sums

  PHTypedNodeIterator<PHGlobal> globaliter(topNode);
  PHIODataNode<PHGlobal> *globalNode = globaliter.find("PHGlobal");

  PHGlobal* fGlobal = 0;
  static bool noglobalerr = true;
  if(globalNode!=0)
    fGlobal = globalNode->getData();    
  if( fGlobal )
  {
    if(fGlobal->getZdcEnergyN() >=0)
      zdceN = fGlobal->getZdcEnergyN();
    if(fGlobal->getZdcEnergyS() >=0 )
      zdceS = fGlobal->getZdcEnergyS();
  }

  //fill processed output objects
  fclOutNorth->readData(fclRawNorth);
  fclOutSouth->readData(fclRawSouth);

  //calibrate
  calibrateData(fclOutNorth,zdceN);
  calibrateData(fclOutSouth,zdceS);
  
  fclOutNorth->computeSums();
  fclOutSouth->computeSums();
  
  if(fGlobal!=0){

    fGlobal->set_FclTotlN(fclOutNorth->getSumAll() );
    fGlobal->set_FclGreyN(fclOutNorth->getSumGrey() );
    fGlobal->set_FclTotlS(fclOutSouth->getSumAll() );
    fGlobal->set_FclGreyS(fclOutSouth->getSumGrey() );

  }else if(noglobalerr) {
    std::cout<<PHWHERE<<" PHGlobal not found and not updated!\n";
    std::cout<<PHWHERE<<" Previous Message will not be repeated!\n";
    noglobalerr = false;
 }

  return 0;

}

void FclRecal::calibrateData(FclOut* fclO, float zdce)
{
  FclIndexer* indexer = FclIndexer::Instance();

  int whichSide = fclO->getSide();
  float yint = 0.0, yint_err=0.0, slope=0.0, slope_err=0.0;

  FclCalib* calib = (whichSide==FCALNORTH) ? northCal: southCal; 

  for (int channel = 0; channel < CHANTOT; channel++){
    int row = indexer->getRow(whichSide,channel);
    int col = indexer->getColumn(whichSide,channel);
    if(row>-1 &&
       row<ROWUSE &&
       col>-1 &&
       col<COLUSE)
      {

	if(whichSide==FCALSOUTH && zdce>0)
	{
	  //Subtract the ZDC crosstalk
	  calib->getZdcXtalk(row,col,yint,slope,yint_err,slope_err);
	  //	  if(slope>slope_err)
	  //	  cout<<"DEBUG: "<<col<<":"<<row<<":"<<fclO->getLowGain(channel);
	  fclO->setLowGain(channel, fclO->getLowGain(channel)-slope*zdce);
	  //	  cout<<":"<<slope<<":"<<zdce<<":"<<fclO->getLowGain(channel)<<endl;
	}
        if(calib->getCalib(channel)<=0)
          {
            cout<<"ERROR:: calibration problem "<<calib->getCalib(channel);
            cout<<" "<<channel<<" "<<indexer->getRow(whichSide,channel);
            cout<<" "<<indexer->getColumn(whichSide,channel) <<PHWHERE<<endl;
          }
        fclO->setLowGain(channel, fclO->getLowGain(channel)/calib->getCalib(channel));
      }
  }


}
