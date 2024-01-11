#include <RecoverSvxHits.h>
#include "setIntflag.h"

#include <SvxHitMapEntry.h>
#include <SvxHitMap.h>
#include <SvxTrackMapEntry.h>
#include <SvxTrackMap.h>
#include <SvxCentralTrackMapEntry.h>
#include <SvxCentralTrackMap.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <useInt.h>

#include <cstdlib>
#include <sstream>
#include <fstream>

using namespace std;

//---------------------------------------------------------------------------------------

RecoverSvxHits::RecoverSvxHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoversvxhits.dump");
#endif

  return;
}

//---------------------------------------------------------------------------------------

int RecoverSvxHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
#ifdef useIntflag
  if(verbosity>0) cout << "RecoverSvxHits::InitRun using INT flag." << endl;
  VariableArrayInt *svxhit    = findNode::getClass<VariableArrayInt>(topNode, "SvxHit_VarArray");
  VariableArrayInt *svxhitall = findNode::getClass<VariableArrayInt>(topNode, "SvxHitAll_VarArray");
  VariableArrayInt *svxtrack  = findNode::getClass<VariableArrayInt>(topNode, "SvxTrack_VarArray");
  VariableArrayInt *svxcenttrack = findNode::getClass<VariableArrayInt>(topNode, "SvxCentralTrack_VarArray");
  VariableArrayInt *svxcenttrackbg = findNode::getClass<VariableArrayInt>(topNode, "SvxCentralTrackBG_VarArray");
#else
  VariableArray *svxhit    = findNode::getClass<VariableArray>(topNode, "SvxHit_VarArray");
  VariableArray *svxhitall = findNode::getClass<VariableArray>(topNode, "SvxHitAll_VarArray");
  VariableArray *svxtrack  = findNode::getClass<VariableArray>(topNode, "SvxTrack_VarArray");
  VariableArray *svxcenttrack = findNode::getClass<VariableArray>(topNode, "SvxCentralTrack_VarArray");
  VariableArray *svxcenttrackbg = findNode::getClass<VariableArray>(topNode, "SvxCentralTrackBG_VarArray");
#endif

  if(verbosity>0) cout << "RecoverSvxHits::InitRun creating SvxComp_comp node..." << endl;
  SvxHitMap *svxmap=NULL;
  if (svxhit||svxhitall)
    {
      svxmap  = findNode::getClass<SvxHitMap>(topNode, "SvxHit_comp");
      if (!svxmap)
        {
          svxmap = new SvxHitMap();
	  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxmap, "SvxHit_comp" , "PHObject");
          dstNode->addNode(PHObjectIONode);
        }
      //svxhit->identify();
    }

  if(verbosity>0) cout << "RecoverSvxHits::InitRun creating SvxTrack_comp node..." << endl;
  SvxTrackMap *svxtrackmap=NULL;
  if (svxtrack)
    {
      svxtrackmap  = findNode::getClass<SvxTrackMap>(topNode, "SvxTrack_comp");
      if (!svxtrackmap)
        {
          svxtrackmap = new SvxTrackMap();
          PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxtrackmap, "SvxTrack_comp" , "PHObject");
          dstNode->addNode(PHObjectIONode);
        }
      //svxtrack->identify();
    }

  if(verbosity>0) cout << "RecoverSvxHits::InitRun creating SvxCentralTrack_comp node..." << endl;
  SvxCentralTrackMap *svxcenttrackmap=NULL;
  if (svxcenttrack)
    {
      svxcenttrackmap  = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrack_comp");
      if (!svxcenttrackmap)
        {
          svxcenttrackmap = new SvxCentralTrackMap();
          PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxcenttrackmap, "SvxCentralTrack_comp" , "PHObject");
          dstNode->addNode(PHObjectIONode);
          if(verbosity>0) cout << "RecoverSvxHits::InitRun SvxCentralTrack_comp node added to DST node." << endl;
        }
      //svxcenttrack->identify();
    }

  if(verbosity>0) cout << "RecoverSvxHits::InitRun creating SvxCentralTrackBG_comp node..." << endl;
  SvxCentralTrackMap *svxcenttrackmapbg=NULL;
  if (svxcenttrackbg)
    {
      svxcenttrackmapbg  = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrackBG_comp");
      if (!svxcenttrackmapbg)
        {
          svxcenttrackmapbg = new SvxCentralTrackMap();
          PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxcenttrackmapbg, "SvxCentralTrackBG_comp" , "PHObject");
          dstNode->addNode(PHObjectIONode);
          if(verbosity>0) cout << "RecoverSvxHits::InitRun SvxCentralTrackBG_comp node added to DST node." << endl;
        }
    }

  // schema version
  if(verbosity>0)
    {
      cout<<"RecoverSvxHits schema version"<<endl;
      if(svxhit        !=NULL) cout<<"  Cluster             Schema value : "<<svxhit->Id()        <<endl;        
      if(svxtrack      !=NULL) cout<<"  Segment             Schema value : "<<svxtrack->Id()      <<endl;
      if(svxcenttrack  !=NULL) cout<<"  SvxCentralTrack     Schema value : "<<svxcenttrack->Id()  <<endl;
      if(svxcenttrackbg!=NULL) cout<<"  SvxCentralTrack(BG) Schema value : "<<svxcenttrackbg->Id()<<endl;
    }
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------

int RecoverSvxHits::process_event(PHCompositeNode *topNode)
{
  int irecover=0;
  int irecovertrk=0;
  int irecovercenttrk=0;
  int irecovercenttrkbg=0;
  if(verbosity>0) cout << "=============== RecoverSvxHits::process_event() started. =======================" << endl;

#ifdef useIntflag
  VariableArrayInt *hitarray    = findNode::getClass<VariableArrayInt>(topNode, "SvxHit_VarArray");
  VariableArrayInt *hitallarray = findNode::getClass<VariableArrayInt>(topNode, "SvxHitAll_VarArray");
  VariableArrayInt *trackarray  = findNode::getClass<VariableArrayInt>(topNode, "SvxTrack_VarArray");
  VariableArrayInt *centtrackarray = findNode::getClass<VariableArrayInt>(topNode, "SvxCentralTrack_VarArray");
  VariableArrayInt *centtrackarraybg = findNode::getClass<VariableArrayInt>(topNode, "SvxCentralTrackBG_VarArray");
#else
  VariableArray *hitarray    = findNode::getClass<VariableArray>(topNode, "SvxHit_VarArray");
  VariableArray *hitallarray = findNode::getClass<VariableArray>(topNode, "SvxHitAll_VarArray");
  VariableArray *trackarray  = findNode::getClass<VariableArray>(topNode, "SvxTrack_VarArray");
  VariableArray *centtrackarray = findNode::getClass<VariableArray>(topNode, "SvxCentralTrack_VarArray");
  VariableArray *centtrackarraybg = findNode::getClass<VariableArray>(topNode, "SvxCentralTrackBG_VarArray");
#endif

  static int init_comment=0;
  if(hitallarray!=NULL){
    hitarray = hitallarray;

    if(init_comment<2){
      cout<<"SvxHitAll_VarArray is used" <<endl;
    }

    init_comment++;
  }

// first unpack clusters

  int hitarrayID = 1000;
  if (hitarray)
    {
      hitarrayID = hitarray->Id();
      SvxHitMap *svxmap  = findNode::getClass<SvxHitMap>(topNode, "SvxHit_comp");
      if (!svxmap)
        {
          cerr << PHWHERE << "ERROR: Cannot locate SvxHit_comp node." << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
      if(verbosity>0) std::cout << "RecoverSvxHits::process_event() starting with cluster array size = " << size << std::endl;

#ifdef useIntflag
      const int *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();

#endif
      
      if(array == NULL)
	{
	  //cerr << PHWHERE << "ERROR: Cannot locate SvxHit array." <<endl;
	}
      else{
	
	float mkm = 1.;
	float xfval = -9999.;
	float yfval = -9999.;
	float zfval = -9999.;
	
	short xival = -9999;
	short yival = -9999;
	short zival = -9999;

	SvxHitMapEntry svxentry;
	while (size > 0)
	  {
	    svxentry.set_id(*array++);
	    size--;
	    svxentry.set_adcandsize(*array++);
	    size--;
      if(hitarrayID == 10001)
      {
        svxentry.set_nhot(*array++);
        size--;
        svxentry.set_ncold(*array++);
        size--;
      }


	    //---	    
	    xival = *array++;
	    xfval = xival/mkm;
	    svxentry.set_x(xfval);
	    size--;

	    yival = *array++;
	    yfval = yival/mkm;
	    svxentry.set_y(yfval);
	    size--;
	    
	    zival = *array++;
	    zfval = zival/mkm;
	    svxentry.set_z(zfval);
	    size--;


	    //---
// 	    svxentry.set_x(*array++);
// 	    size--;
// 	    svxentry.set_y(*array++);
// 	    size--;
// 	    svxentry.set_z(*array++);
// 	    size--;
	
    
	    if(verbosity>1) {
	      cout << "   Svx cluster: " << svxentry.get_id() << " "
		   << svxentry.get_adcandsize() << " "
		   << svxentry.get_x() << " "
		   << svxentry.get_y() << " "
		   << svxentry.get_z() << " "
		   << endl;
	    }
	    svxmap->AddHit(irecover, svxentry);
	    irecover++;
	  }//while (size >0 )
#ifdef DUMP
	svxmap->identify(dumprecover);
#endif
      }//if(array == NULL)

      if(verbosity>0) std::cout << "RecoverSvxHits::process_event() recovered " << irecover << " clusters." << std::endl;
    }//if(hitarray)
// svx standalone tracks now
  
  int trackarrayID = 1000;
  if (trackarray)
    {
      trackarrayID = trackarray->Id();
      SvxTrackMap *svxtrackmap  = findNode::getClass<SvxTrackMap>(topNode, "SvxTrack_comp");
      if (!svxtrackmap)
        {
          cerr << PHWHERE << "ERROR: Cannot locate SvxTrack_comp node." << endl;
          exit(1);
        }
      unsigned int size = trackarray->get_array_size();
      //if(verbosity>0) std::cout << "RecoverSvxHits::process_event() starting with track array size = " << size << std::endl;

#ifdef useIntflag
      const int *array = trackarray->get_array();
#else
      const short int *array = trackarray->get_array();
#endif

      SvxTrackMapEntry svxtrkentry;
      while (size > 0)
        {
          svxtrkentry.set_charge(*array++);
          size--;
          svxtrkentry.set_nhits(*array++);
          size--;
          svxtrkentry.set_quality(*array++);
          size--;
          svxtrkentry.set_dca2d(*array++);
          size--;
          svxtrkentry.set_dca3d(*array++);
          size--;
          svxtrkentry.set_x(*array++);
          size--;
          svxtrkentry.set_y(*array++);
          size--;
          svxtrkentry.set_z(*array++);
          size--;
          svxtrkentry.set_px(*array++);
          size--;
          svxtrkentry.set_py(*array++);
          size--;
          svxtrkentry.set_pz(*array++);
          size--;
          if(trackarrayID == 1001 || trackarrayID == 1002)
          {
          svxtrkentry.set_livePercentage(0, *array++);
          size--;
          svxtrkentry.set_livePercentage(1, *array++);
          size--;
          svxtrkentry.set_livePercentage(2, *array++);
          size--;
          svxtrkentry.set_livePercentage(3, *array++);
          size--;
          svxtrkentry.set_segmentQuality(*array++);
          size--;
          svxtrkentry.set_segmentScore(*array++);
          size--;
          if( trackarrayID == 1002)
          {
          svxtrkentry.set_dEdX1(*array++);
          size--;
          svxtrkentry.set_dEdX2(*array++);
          size--;
          for(int ilay=0; ilay<4; ilay++) {
            for(int ihit=0; ihit<2; ihit++) {
              svxtrkentry.set_clusterGoodFraction(ilay, ihit, *array++);
              size--;
            }
          }
          } // id= 1002

          } // id=1001 or 1002

          if(verbosity>1) {
            cout << "   Svx standalone track: " << svxtrkentry.get_charge() << " "
                                            << svxtrkentry.get_nhits() << " "
                                            << svxtrkentry.get_px() << " "
                                            << svxtrkentry.get_py() << " "
                                            << svxtrkentry.get_pz() << " "
                                            << endl;
          }
          svxtrackmap->AddHit(irecovertrk, svxtrkentry);
          irecovertrk++;
        }
#ifdef DUMP
      svxtrackmap->identify(dumprecover);
#endif
    }
      if(verbosity>0) std::cout << "RecoverSvxHits::process_event() recovered " << irecovertrk << " tracks." << std::endl;

// SvxCentral tracks -------------------------------------------------------------------------

  int centtrackarrayID = 1000;
  if (centtrackarray)
    {
      centtrackarrayID = centtrackarray->Id();
      SvxCentralTrackMap *svxcenttrackmap  = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrack_comp");
      if (!svxcenttrackmap) { cerr << PHWHERE << "ERROR: Cannot locate SvxCentralTrack_comp node." << endl; exit(1); }

      unsigned int size = centtrackarray->get_array_size();
      if(verbosity>0) std::cout << "RecoverSvxHits::process_event() starting with SvxCentral track array size = " << size << std::endl;

#ifdef useIntflag
      const int *array = centtrackarray->get_array();
#else
      const short int *array = centtrackarray->get_array();
#endif

      SvxCentralTrackMapEntry svxcententry;
      while (size > 0)
        {
          svxcententry.set_DchIndex(*array++);
          size--;
          svxcententry.set_HitPattern(*array++);
          size--;
          svxcententry.set_Unique(*array++);
          size--;
          svxcententry.set_DCA2D(*array++);
          size--;
          svxcententry.set_DCAZ(*array++);
          size--;
          svxcententry.set_ClosestApproachX(*array++);
          size--;
          svxcententry.set_ClosestApproachY(*array++);
          size--;
          svxcententry.set_ClosestApproachZ(*array++);
          size--;
          svxcententry.set_ChisquarePhi(*array++);
          size--;
          svxcententry.set_ChisquareZ(*array++);
          size--;
          svxcententry.set_Chisquare(*array++);
          size--;
          svxcententry.set_Chisquare2(*array++);
          size--;
          short int NClusters = *array++;
          svxcententry.set_NClusters(NClusters);
          size--;
          if(centtrackarrayID == 1001 || centtrackarrayID == 1002 || centtrackarrayID == 1003)
          {
            for (int ilayer=0; ilayer<4; ilayer++) 
              svxcententry.set_LivePercent(ilayer, *array++);
            size -= 4;
            svxcententry.set_LinkQuality(*array++);
            size--;
            svxcententry.set_LinkScore(*array++);
            size--;

            if(centtrackarrayID == 1002 || centtrackarrayID == 1003)
            {
              svxcententry.set_MomentumX(*array++);
              size--;
              svxcententry.set_MomentumY(*array++);
              size--;
              svxcententry.set_MomentumZ(*array++);
              size--;
            }
          }
          if(centtrackarrayID == 1003)
          {
            svxcententry.set_DCA2Dprimary(*array++);
            size--;
            svxcententry.set_DCAZprimary(*array++);
            size--;

            //cout<<"recover : "
            //    <<svxcententry.get_DCA2Dprimary()<<" "
            //    <<svxcententry.get_DCAZprimary()<<endl;
          }

          for(int i=0; i<NClusters; i++) {
            svxcententry.set_ClusterID(i, *array++);
            size--;
            svxcententry.set_ClusterDPhi(i, *array++);
            size--;
            svxcententry.set_ClusterDZ(i, *array++);
            size--;
          }

          if(verbosity>1) {
            cout << "   SvxCentral track: " << svxcententry.get_DchIndex() << " "
                                            << svxcententry.get_DCA2D() << " "
                                            << svxcententry.get_ChisquarePhi() << " "
                                            << svxcententry.get_NClusters() << " "
                                            << endl;
          }
          svxcenttrackmap->AddHit(irecovercenttrk, svxcententry);
          irecovercenttrk++;
        }
    }
    if(verbosity>0) std::cout << "RecoverSvxHits::process_event() recovered " << irecovercenttrk << " SvxCentral tracks." << std::endl;

// SvxCentral Background tracks -------------------------------------------------------------------------

  int centtrackarraybgID = 1000;
  if (centtrackarraybg)
    {
      centtrackarraybgID = centtrackarraybg->Id();
      SvxCentralTrackMap *svxcenttrackmapbg  = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrackBG_comp");
      if (!svxcenttrackmapbg) { cerr << PHWHERE << "ERROR: Cannot locate SvxCentralTrackBG_comp node." << endl; exit(1); }

      unsigned int size = centtrackarraybg->get_array_size();
      if(verbosity>0) std::cout << "RecoverSvxHits::process_event() starting with SvxCentral Background track array size = " << size << std::endl;

#ifdef useIntflag
      const int *array = centtrackarraybg->get_array();
#else
      const short int *array = centtrackarraybg->get_array();
#endif

      SvxCentralTrackMapEntry svxcententrybg;
      while (size > 0)
        {
          svxcententrybg.set_DchIndex(*array++);
          size--;
          svxcententrybg.set_HitPattern(*array++);
          size--;
          svxcententrybg.set_Unique(*array++);
          size--;
          svxcententrybg.set_DCA2D(*array++);
          size--;
          svxcententrybg.set_DCAZ(*array++);
          size--;
          svxcententrybg.set_ClosestApproachX(*array++);
          size--;
          svxcententrybg.set_ClosestApproachY(*array++);
          size--;
          svxcententrybg.set_ClosestApproachZ(*array++);
          size--;
          svxcententrybg.set_ChisquarePhi(*array++);
          size--;
          svxcententrybg.set_ChisquareZ(*array++);
          size--;
          svxcententrybg.set_Chisquare(*array++);
          size--;
          svxcententrybg.set_Chisquare2(*array++);
          size--;
          short int NClusters = *array++;
          svxcententrybg.set_NClusters(NClusters);
          size--;

          if( centtrackarraybgID == 1002 || centtrackarraybgID == 1003 )
          {
            for (int ilayer=0; ilayer<4; ilayer++) 
              svxcententrybg.set_LivePercent(ilayer, *array++);
            size -= 4;
            svxcententrybg.set_LinkQuality(*array++);
            size--;
            svxcententrybg.set_LinkScore(*array++);
            size--;
            svxcententrybg.set_MomentumX(*array++);
            size--;
            svxcententrybg.set_MomentumY(*array++);
            size--;
            svxcententrybg.set_MomentumZ(*array++);
            size--;
          }
          if(centtrackarrayID == 1003)
          {
            svxcententrybg.set_DCA2Dprimary(*array++);
            size--;
            svxcententrybg.set_DCAZprimary(*array++);
            size--;
            //cout<<"recoverbg : "
            //    <<svxcententrybg.get_DCA2Dprimary()<<" "
            //    <<svxcententrybg.get_DCAZprimary()<<endl;
          }

          for(int i=0; i<NClusters; i++) {
            svxcententrybg.set_ClusterID(i, *array++);
            size--;
            svxcententrybg.set_ClusterDPhi(i, *array++);
            size--;
            svxcententrybg.set_ClusterDZ(i, *array++);
            size--;
          }

          if(verbosity>1) {
            cout << "   SvxCentral Background track: " << svxcententrybg.get_DchIndex() << " "
                                            << svxcententrybg.get_DCA2D() << " "
                                            << svxcententrybg.get_ChisquarePhi() << " "
                                            << svxcententrybg.get_NClusters() << " "
                                            << endl;
          }
          svxcenttrackmapbg->AddHit(irecovercenttrkbg, svxcententrybg);
          irecovercenttrkbg++;
        }
    }
    if(verbosity>0) std::cout << "RecoverSvxHits::process_event() recovered " << irecovercenttrkbg << " SvxCentral Background tracks." << std::endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------

int RecoverSvxHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}


