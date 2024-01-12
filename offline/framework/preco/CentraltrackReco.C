#include <PHCompositeNode.h>
#include <CentraltrackReco.h>

#include <phool.h>

#include <BbcOut.h>
#include <BbcRaw.h>
#include <CglTrack.h>  
#include <PHTrackOut.h>
#include <PHDchTrackOut.h>
#include <DchTrack.h>
#include <EmcClusterLocalExt.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <emcTowerContainer.h>
#include <emcTowerContent.h>
#include <EmcIndexer.h>
#include <EventHeader.h>
#include <RunHeader.h>
#include <Lvl2DecisionOut.h>
#include <PadCluster.h>
#include <TecOut.hh>
#include <TofOut.h>
#include <TrigLvl1.h>
#include <VtxOut.h>
#include <ZdcOut.h>
#include <CrkRing.h>
#include <CrkHit.h>
#include <T0Out.h>
#include <ErtOut.h>
#include <Event.h>
#include <AccRaw.h>
#include <AccProj.h>
#include <HbdMiniCellList.h>
#include <HbdCellList.h>
#include <TofwHit.h>

#include <PHCentralTrackv22.h>
#include <PHCentralTrackv21.h>
#include <PHCentralTrackv20.h>
#include <PHCentralTrackv19.h>
#include <PHCentralTrackv18.h>
#include <PHCentralTrackv17.h>
#include <PHCentralTrackv16.h>
#include <PHCentralTrackv14.h>
#include <PHCentralTrackv13.h>
#include <PHCentralTrackv12.h>
#include <PHCentralTrackv11.h>
#include <PHCentralTrackv10.h>
#include <PHCentralTrackv9.h>
#include <PHCentralTrackv6.h>
#include <PHCentralTrackv5.h>
#include <PHCentralTrackv4.h>

#include <utiMatch.h>
#include <gsl/gsl_const.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;
typedef PHIODataNode <PHCentralTrack> PHParticleNode_t;

typedef PHIODataNode<BbcOut> BbcOutNode_t;
typedef PHIODataNode<BbcRaw> BbcRawNode_t;
typedef PHIODataNode<CglTrack> CglTrackNode_t;
typedef PHIODataNode<PHTrackOut> PHTrackOutNode_t;
typedef PHIODataNode<PHDchTrackOut> PHDchTrackOutNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;
typedef PHIODataNode<EmcClusterLocalExt> EmcClusterLocalExtNode_t;
typedef PHIODataNode<emcClusterContainer> emcClusterContainerNode_t;
typedef PHIODataNode<emcTowerContainer> emcTowerContainerNode_t;
typedef PHIODataNode<EventHeader> EventHeaderNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHIODataNode<Lvl2DecisionOut> Lvl2DecisionOutNode_t;
typedef PHIODataNode<PadCluster> PadClusterNode_t;
typedef PHIODataNode<TecOut> TecOutNode_t;
typedef PHIODataNode<TofOut> TofOutNode_t;
typedef PHIODataNode<TrigLvl1> TrigLvl1Node_t;
typedef PHIODataNode<VtxOut> VtxOutNode_t;
typedef PHIODataNode<ZdcOut> ZdcOutNode_t;
typedef PHIODataNode<CrkRing> CrkRingNode_t;
typedef PHIODataNode<CrkHit> CrkHitNode_t;
typedef PHIODataNode<T0Out> T0OutNode_t;
typedef PHIODataNode<ErtOut> ErtOutNode_t;
typedef PHDataNode<Event> EventNode_t;
typedef PHIODataNode <AccRaw>  AccRawNode_t;
typedef PHIODataNode<HbdMiniCellList>  HbdMiniCellListNode_t;
typedef PHIODataNode<HbdCellList>  HbdCellListNode_t;
typedef PHIODataNode<TofwHit>  TofwHitNode_t;

//Matching Correctors -- ala J. Jia...
static utiMatch *m;
static bool RUN3MATCH=False;
static const float lightSpeed = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns


CentraltrackReco::CentraltrackReco(int v, const char * name):SubsysReco(name)
{
  version = v;
  FieldOnFlag = 1;
  // Zero out all the data array pointers.
  particle  = 0;     
  d_runhdr  = 0;  
  d_bbc     = 0;     
  d_vtx     = 0;     
  d_dctrk   = 0;   
  d_pc1     = 0;     
  d_pc2     = 0;     
  d_pc3     = 0;     
  d_tecout  = 0;  
  d_emc     = 0;     
  d_twr     = 0;     
  d_emcOLD  = 0;     
  d_tof     = 0;     
  d_cgl     = 0;     
  d_proj    = 0;    
  d_crkring = 0;
  d_t0      = 0;      
  d_evt     = 0;     
  d_model   = 0;   
  d_scrkring= 0;   
  d_scgl    = 0;   
  d_sproj   = 0;   
  d_accraw  = 0;   
  d_accproj = 0;   
  d_hbd     = 0;
  d_tofw    = 0;

  // define all the logical flags for the chosen data type.
  Common        = True;
  Indices       = False;
  AbsolutePos   = False;
  AbsoluteDelta = False;
  Lvl1          = False;
  PadWidths     = False;
  TrackModel    = False;
  Swapped       = False;
  Run1pp        = False;
  EmcE          = False;
  Acc           = False;
  TOFPH         = False;
  TOFTDC        = False;
  TECEXTRA      = False;
  TofwTrack     = False;
  HbdTrack      = False;
  fillExtraSwap = False;

  switch(version)
    {
    case 4: // Run 2 Au Au for QM 2002
      TrackModel = True;
      Swapped    = True;
      break;

    case 5:  // Run1 pp
      TrackModel = True;
      Swapped    = True;
      Run1pp     = True;
      break;

    case 6: 
      TrackModel = True;
      Swapped    = True;
      Run1pp     = True;
      Indices    = True;
      PadWidths  = True;
      EmcE       = True;
      break;
      
    case 9:
      Indices       = True;
      AbsolutePos   = True;
      AbsoluteDelta = True;
      Lvl1          = True;
      EmcE          = True;
      break;
      
    case 10:  // Run3 dAu Test Production and beyond
      Indices       = True;
      AbsolutePos   = True;
      AbsoluteDelta = True;
      Lvl1          = True;
      EmcE          = True;
      break;
      
    case 11:  // Run3 Production
      Indices       = True;
      AbsolutePos   = False;
      AbsoluteDelta = True;
      Lvl1          = False;
      EmcE          = True;
      break;
      
    case 12: // Run 4 Au Au
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      break;

    case 13: // Run 4 Au Au
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TECEXTRA      = True;
      break;

    case 14: // Run 4 Au Au
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TECEXTRA      = True;
      break;

    case 16: // Run 4 Au Au
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TOFTDC        = True;
      TECEXTRA      = False;
      break;

    case 17: // Run 5 Cu Cu
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TOFTDC        = True;
      TECEXTRA      = False;
      break;

    case 18: // Run-05 p+p, Run-08 d+Au and p+p
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TOFTDC        = True;
      TECEXTRA      = False;
      TofwTrack     = True;  // Add TOF.W for Run-08 reconstruction
      break;

    case 19: // Run 5 Cu Cu
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TOFTDC        = True;
      TECEXTRA      = False;
      break;

    case 21: // Run 7 Au Au
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TOFTDC        = True;
      TECEXTRA      = False;
      TofwTrack     = True;
      HbdTrack      = True;
      break;

    case 22: // Run 7 Au Au
      Indices       = True;
      AbsoluteDelta = True;
      TrackModel    = True;
      Swapped       = True;
      EmcE          = True;
      Acc           = True;
      TOFPH         = True;
      TOFTDC        = True;
      TECEXTRA      = False;
      TofwTrack     = True;
      HbdTrack      = True;
      fillExtraSwap = True;
      break;

    default:
      cout << PHWHERE << "WARNING::CentraltrackReco...Unknown Track Version Requested: " << version <<endl;
      cout << PHWHERE << "WARNING::CentraltrackReco...You will receive version 11 Tracks instead." << endl;
      cout << PHWHERE << "WARNING::CentraltrackReco...I hope you like them." << endl;
      Indices       = True;
      AbsolutePos   = False;
      AbsoluteDelta = True;
      Lvl1          = False;
      EmcE          = True;
      break;

    }


  return ;
}


int CentraltrackReco::Init(PHCompositeNode *topNode)
{
  topNode->print();

  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  cout << "Initialize the Central Track Analysis..." << endl;
  PHCentralTrack *particle=0;
  switch (version)
    {
    case 4:
      particle = new PHCentralTrackv4();
      break;

    case 5:
      particle = new PHCentralTrackv5();
      break;

    case 6:
      particle = new PHCentralTrackv6();
      break;

    case 9:
      particle = new PHCentralTrackv9();
      break;

    case 10:
      particle = new PHCentralTrackv10();
      break;

    case 11:
      particle = new PHCentralTrackv11();
      break;

    case 12:
      particle = new PHCentralTrackv12();
      break;

    case 13:
      particle = new PHCentralTrackv13();
      break;

    case 14:
      particle = new PHCentralTrackv14();
      break;

    case 16:
      particle = new PHCentralTrackv16();
      break;

    case 17:
      particle = new PHCentralTrackv17();
      break;

    case 18:
      particle = new PHCentralTrackv18();
      break;

    case 19:
      particle = new PHCentralTrackv19();
      break;

    case 21:
      particle = new PHCentralTrackv21();
      break;

    case 22:
      particle = new PHCentralTrackv22();
      break;
  
    default:
      cout << PHWHERE << "WARNING::CentraltrackReco...Unknown Track Version Requested: " << version <<endl;
      cout << PHWHERE << "WARNING::CentraltrackReco...You will receive version 11 Tracks instead." << endl;
      cout << PHWHERE << "WARNING::CentraltrackReco...You poor bastard." << endl;
      particle = new PHCentralTrackv11();
      break;

    }

  PHObjectNode_t *PHParticleNode  = 
    new PHIODataNode<PHObject>(particle,"PHCentralTrack","PHObject");  
  dstNode->addNode(PHParticleNode);
  
  m = new utiMatch();
  
  return 0;
}

int CentraltrackReco::InitRun(PHCompositeNode *topNode)
{

  d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  int runnumber = d_runhdr->get_RunNumber();
  int currentCentral = d_runhdr->get_currentCentral();

  if( abs(currentCentral) < 100)
    {
      FieldOnFlag = 0;
    }
  else
    {
      FieldOnFlag = 1;
    }
  
  RUN3MATCH=False;
  if( runnumber>65000)
    {
      RUN3MATCH=True;
    }

  
  return 0;
}

//_____________________________________________________________________________
int CentraltrackReco::process_event(PHCompositeNode *topNode)
{

  //  Zero out all the data array pointers.
  //  This allows any of our routines to check for 
  //  null pointer to know if the necessary pointer has
  //  already been found or not.
  particle  = 0;     
  d_runhdr  = 0;  
  d_bbc     = 0;     
  d_vtx     = 0;     
  d_dctrk   = 0;   
  d_pc1     = 0;     
  d_pc2     = 0;     
  d_pc3     = 0;     
  d_tecout  = 0;  
  d_emc     = 0;     
  d_twr     = 0;     
  d_emcOLD  = 0;     
  d_tof     = 0;     
  d_cgl     = 0;     
  d_proj    = 0;    
  d_crkring = 0;
  d_t0      = 0;      
  d_evt     = 0;     
  d_model   = 0;   
  d_scrkring= 0;   
  d_scgl    = 0;   
  d_sproj   = 0;   
  d_accraw  = 0;   
  d_accproj = 0;   
  d_tofw    = 0;
  d_hbd     = 0;

  //Fill the fields common to all types of particles.
  FillCommon    (topNode);  

  //  Fill any and all additional blocks that you need.
  if( Indices      )  FillIndices      (topNode);
  if( AbsolutePos  )  FillAbsolutePos  (topNode);
  if( AbsoluteDelta)  FillAbsoluteDelta(topNode);
  if( Lvl1         )  FillLvl1         (topNode);
  if( PadWidths    )  FillPadWidths    (topNode);
  if( TrackModel   )  FillTrackModel   (topNode);
  if( Swapped      )  FillSwapped      (topNode);
  if( EmcE         )  FillEmcE         (topNode);
  if( Acc          )  FillAcc          (topNode);
  if( TOFPH        )  FillTOFPH        (topNode);
  if( TOFTDC       )  FillTOFTDC       (topNode);
  if( TECEXTRA     )  FillTECEXTRA     (topNode);
  if( HbdTrack     )  FillHbdTrack     (topNode);
  if( TofwTrack    )  FillTofwTrack    (topNode);
  if( fillExtraSwap)  FillExtraSwap    (topNode);

  return 0;
}


int CentraltrackReco::FillCommon(PHCompositeNode *topNode)
{

  // This routine is used to fill those variables that are common to 
  // all track version types.  Along the way it sets the pointers for
  // a number of different input data blocks.  Since FillCommon will always be
  // called, we don't need to check if the pointers are already references, 
  // they are.

  // Particle
  PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
  PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
  if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
  if( !particle) cout << PHWHERE << "CentraltrackReco:: PHparticle not in Node Tree" << endl;

  //Run Header
  PHTypedNodeIterator<RunHeader> iRUN(topNode);
  RunHeaderNode_t *RUN = iRUN.find("RunHeader");
  if( RUN) d_runhdr = RUN->getData();
  if( !d_runhdr) cout << PHWHERE << "CentraltrackReco:: runhdr not in Node Tree" << endl;
  
  // BBC
  PHTypedNodeIterator<BbcOut> iBBC(topNode);
  BbcOutNode_t *BBC = iBBC.find("BbcOut");
  if(BBC) d_bbc = BBC->getData();
  if( !d_bbc) cout << PHWHERE << "CentraltrackReco:: bbc not in Node Tree" << endl;
  
  // VTX
  PHTypedNodeIterator<VtxOut> iVTX(topNode);
  VtxOutNode_t *VTX = iVTX.find("VtxOut");
  if(VTX) d_vtx = VTX->getData();
  if( !d_vtx) cout << PHWHERE << "CentraltrackReco:: vtx not in Node Tree" << endl;
  
  // DC
  PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
  DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
  if(DCTRACK) d_dctrk  = DCTRACK->getData ();
  if( !d_dctrk) cout << PHWHERE << "CentraltrackReco:: dctrk not in Node Tree" << endl;
  
  // PC1 
  PHTypedNodeIterator<PadCluster> iPC1(topNode);
  PadClusterNode_t *PC1 = iPC1.find("Pc1Cluster");
  if(PC1) d_pc1  = PC1->getData ();
  if( !d_pc1) cout << PHWHERE << "CentraltrackReco:: pc1 not in Node Tree" << endl;
  
  // PC2
  PHTypedNodeIterator<PadCluster> iPC2(topNode);
  PadClusterNode_t *PC2 = iPC2.find("Pc2Cluster");
  if(PC2) d_pc2  = PC2->getData ();
  if( !d_pc2) cout << PHWHERE << "CentraltrackReco:: pc2 not in Node Tree" << endl;
  
  // PC3
  PHTypedNodeIterator<PadCluster> iPC3(topNode);
  PadClusterNode_t *PC3 = iPC3.find("Pc3Cluster");
  if(PC3) d_pc3  = PC3->getData ();
  if( !d_pc3) cout << PHWHERE << "CentraltrackReco:: pc3 not in Node Tree" << endl;  
  
  // TEC
  PHTypedNodeIterator<TecOut> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOut");
  if(TecOutNode) d_tecout = TecOutNode->getData();
  //if( !d_tecout) cout << PHWHERE << "CentraltrackReco:: tec not in Node Tree" << endl;
  
  // EMC
  PHTypedNodeIterator<emcClusterContainer> iEMC(topNode);
  emcClusterContainerNode_t* EMC = iEMC.find("emcClusterContainer");
  if(EMC) d_emc = EMC->getData();
  
  // EMC OLD FORMAT...
  PHTypedNodeIterator<EmcClusterLocalExt> iEMCOLD(topNode);
  EmcClusterLocalExtNode_t *EMCOLD = iEMCOLD.find("EmcClusterLocalExt");
  if(EMCOLD) d_emcOLD  = EMCOLD->getData ();
  if( !d_emc && !d_emcOLD) cout << PHWHERE << "CentraltrackReco:: EMC (neither old nor new) not in Node Tree" << endl;
  
  // EMC Tower
  PHTypedNodeIterator<emcTowerContainer> iTWR(topNode);
  emcTowerContainerNode_t* TWR = iTWR.find("emcTowerContainer");
  if(TWR) d_twr = TWR->getData();
  if( !d_twr) cout << PHWHERE << "CentraltrackReco:: emc tower not in Node Tree" << endl;
  
  // TOF
  PHTypedNodeIterator<TofOut> iTOF(topNode);
  TofOutNode_t *TOF = iTOF.find("TofOut");
  if(TOF) d_tof  = TOF->getData ();
  if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  
  // global tracking
  PHTypedNodeIterator<CglTrack> iCGL(topNode);
  CglTrackNode_t *CGL = iCGL.find("CglTrack");
  if(CGL) d_cgl  = CGL->getData ();
  if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  
  // Projections
  PHTypedNodeIterator<PHTrackOut> iPHTrackOut(topNode);
  PHTrackOutNode_t *PHTrackOutNode = iPHTrackOut.find("PHTrackOut");
  if(PHTrackOutNode) d_proj = PHTrackOutNode->getData();
  if( !d_proj) cout << PHWHERE << "CentraltrackReco:: projections not in Node Tree" << endl;
  
  //RICH RINGS
  PHTypedNodeIterator<CrkRing>    crkringmicroiter(topNode);
  CrkRingNode_t *CrkRingMicroNode = crkringmicroiter.find("CrkRing");
  if(CrkRingMicroNode) d_crkring = CrkRingMicroNode->getData();
  if( !d_crkring) cout << PHWHERE << "CentraltrackReco:: ring not in Node Tree" << endl;
  
  // T0
  PHTypedNodeIterator<T0Out> iT0(topNode);
  T0OutNode_t *T0 = iT0.find("T0Out");
  if(T0) d_t0  = T0->getData ();
  //if( !d_t0) cout << PHWHERE << "CentraltrackReco:: T0 not in Node Tree" << endl;
  
  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_cgl && particle && d_dctrk)
    {
      particle->Reset();
      particle->set_npart           ( d_cgl->get_CglNTrack() ) ;
      particle->set_TClonesArraySize( d_cgl->get_CglNTrack() );
      

      float TimeZero = 0.;
      if( d_bbc && d_bbc->isValid()) {
	TimeZero = d_bbc->get_TimeZero();
      }
      else if( d_t0 && d_t0->isValid()) {
	TimeZero = d_t0->get_T0();
      }
      else {
	cout << PHWHERE << "No Time Zero available" << endl;
      }
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  particle->AddPHParticle(itrk);
	  
	  //  Set up all the pointers to other structures...
	  int dchid = d_cgl->get_dctracksid(icgl);
	  int ipc1  = d_cgl->get_pc1clusid (icgl);	
	  int ipc2  = d_cgl->get_pc2clusid (icgl);	
	  int ipc3  = d_cgl->get_pc3clusid (icgl);
	  int iemc  = d_cgl->get_emcclusid (icgl);
	  int itof  = d_cgl->get_tofrecid  (icgl);
	  int icrk  = d_cgl->get_richringid(icgl);
	  int itec  = d_cgl->get_tectrackid(icgl);
	  // info from the DCH object...
          particle->set_quality (itrk, d_dctrk->get_quality (dchid)); 
          particle->set_zed     (itrk, d_dctrk->get_zed     (dchid)); 
          particle->set_phi     (itrk, d_dctrk->get_phi     (dchid)); 
          particle->set_alpha   (itrk, d_dctrk->get_alpha   (dchid)); 
          particle->set_beta    (itrk, d_dctrk->get_beta    (dchid)); 
          particle->set_phi0    (itrk, d_dctrk->get_phi0    (dchid)); 
          particle->set_the0    (itrk, d_dctrk->get_theta0  (dchid)); 
          particle->set_mom     (itrk, d_dctrk->get_momentum(dchid));
	  particle->set_nx1hits (itrk, d_dctrk->get_nx1hits (dchid)); 
	  particle->set_nx2hits (itrk, d_dctrk->get_nx2hits (dchid)); 
          particle->set_dcarm   (itrk, d_dctrk->get_arm     (dchid)); 
          particle->set_dcside  (itrk, d_dctrk->get_side    (dchid)); 

	  // set the charge
	  // first check if B=0 
	  int runnumber = 0;
	  if( d_runhdr)
	    {
	      int currentC = d_runhdr->get_currentCentral();
	      runnumber = d_runhdr->get_RunNumber();
	      if( currentC)
		{ // central magnet field is ON
		  // check run number
		  // NOTE:  In future runs, the current of the B-field will
		  // be signed and used to determine the charge...
		  if( currentC < 0) // reverse field
		    {
		      if( particle->get_alpha(itrk) > 0)
			{
			  particle->set_charge(itrk, 1);
			}
		      else if( particle->get_alpha(itrk) < 0)
			{
			  particle->set_charge(itrk, -1);
			}
		      else
			{
			  particle->set_charge(itrk, 0); // Means ambiguous charge
			}
		    }
		  else
		    { // full field normal polarity
		      if( particle->get_alpha(itrk) > 0)
			{
		          particle->set_charge(itrk, -1);
			}
		      else if( particle->get_alpha(itrk) < 0)
			{
			  particle->set_charge(itrk, 1);
			}
		      else
			{
                          particle->set_charge(itrk, 0);
			}
		    }
		}
              else
                { // B=0
                  particle->set_charge(itrk, -999);
                }
	    }

	  if(!runnumber) //Runheader information is wrong, hence no valid current value
	    {
	      cout << PHWHERE << "Could not find a valid run number, exiting now" << endl;
	      exit(1);
	    }


	  // particle projections...
          particle->set_ppc1x (itrk, d_proj->get_projectionPc1(dchid, 0)); 
          particle->set_ppc1y (itrk, d_proj->get_projectionPc1(dchid, 1)); 
          particle->set_ppc1z (itrk, d_proj->get_projectionPc1(dchid, 2)); 
          particle->set_ppc2x (itrk, d_proj->get_projectionPc2(dchid, 0)); 
          particle->set_ppc2y (itrk, d_proj->get_projectionPc2(dchid, 1)); 
          particle->set_ppc2z (itrk, d_proj->get_projectionPc2(dchid, 2)); 
          particle->set_ptecx (itrk, d_proj->get_projectionTec(dchid, 0)); 
          particle->set_ptecy (itrk, d_proj->get_projectionTec(dchid, 1)); 
          particle->set_ptecz (itrk, d_proj->get_projectionTec(dchid, 2)); 
          particle->set_ppc3x (itrk, d_proj->get_projectionPc3(dchid, 0)); 
          particle->set_ppc3y (itrk, d_proj->get_projectionPc3(dchid, 1)); 
          particle->set_ppc3z (itrk, d_proj->get_projectionPc3(dchid, 2)); 
          particle->set_pemcx (itrk, d_proj->get_projectionEmc(dchid, 0)); 
          particle->set_pemcy (itrk, d_proj->get_projectionEmc(dchid, 1)); 
          particle->set_pemcz (itrk, d_proj->get_projectionEmc(dchid, 2)); 
          particle->set_ptofx (itrk, d_proj->get_projectionTof(dchid, 0)); 
          particle->set_ptofy (itrk, d_proj->get_projectionTof(dchid, 1)); 
          particle->set_ptofz (itrk, d_proj->get_projectionTof(dchid, 2)); 
          particle->set_pltof (itrk, d_proj->get_tofPathLength(dchid)); 
          particle->set_plemc (itrk, d_proj->get_emcPathLength(dchid)); 

	  if( ipc1>=0) {
	    particle->set_pc1sect (itrk, d_pc1->get_sector(ipc1));
	  }
	  else {
	    particle->set_pc1sect (itrk, -9999);
	  }


	  // Here we fill the emc stuff.....
	  // Default to not found and then fill with best available.
	  particle->set_sect  (itrk, -9999);
	  particle->set_ysect (itrk, -9999);
	  particle->set_zsect (itrk, -9999);
	  particle->set_ecore (itrk, -9999);

	  if( version<13) 
	    {
	      particle->set_ecorr (itrk, -9999);
	    }
	  if( version>=13)
	    {
	      particle->set_emcdispy (itrk, -9999);
	      particle->set_emcdispz (itrk, -9999);
	    }

	  particle->set_temc  (itrk, -9999);
	  particle->set_prob  (itrk, -9999);
	  particle->set_m2emc (itrk, -9999);
	  particle->set_ecent  (itrk, -9999);
	  particle->set_twrhit (itrk, -9999);
	  particle->set_e9     (itrk, -9999);
	  particle->set_re9    (itrk, -9999);
	  particle->set_emcchi2(itrk, -9999);
	  particle->set_deadmap(itrk, -9999);
	  particle->set_warnmap(itrk, -9999);

	  // SECTION for NEW EMC variable types...
	  if( iemc>=0 && d_emc!=0) {

	    emcClusterContent* clus = d_emc->getCluster(iemc);
	    
	    particle->set_sect   (itrk, clus->sector ());
	    particle->set_ysect  (itrk, clus->iypos());
	    particle->set_zsect  (itrk, clus->izpos());
	    if( version<13) 
	      {
		particle->set_ecorr  (itrk, clus->ecore());  //error???
	      }
	    if( version>=13)
	      {
		particle->set_emcdispy  (itrk, clus->dispy());
		particle->set_emcdispz  (itrk, clus->dispz());
		
		if( d_twr)
		  {
		    int emcarm = clus->arm   ();
		    int emcsec = clus->sector();
		    int emciy  = clus->iypos ();
		    int emciz  = clus->izpos ();
		    int TowerID = EmcIndexer::TowerID(emcarm,emcsec,emciy,emciz);
		    emcTowerContent* twr = d_twr->findTower(TowerID);
		    if( twr)
		      {
			if( !(twr->isSimulated())) {
			  particle->set_emcrawtdc  (itrk, twr->TDC());
			  particle->set_emcrawadc  (itrk, twr->HGPP());
			  particle->set_emcrawadclg(itrk, twr->LGPP());
			}
		      }
		  }		

	      }
	    particle->set_ecore  (itrk, clus->ecore());
	    particle->set_temc   (itrk, clus->tofcorr() - TimeZero);
	    particle->set_prob   (itrk, clus->prob_photon());
	    particle->set_ecent  (itrk, clus->ecent());
	    particle->set_twrhit (itrk, clus->twrhit());
	    particle->set_e9     (itrk, clus->e9());
	    particle->set_re9    (itrk, 0);//clus->re9());
	    particle->set_emcchi2(itrk, clus->chi2());
	    particle->set_deadmap(itrk, clus->deadmap());
	    particle->set_warnmap(itrk, clus->warnmap());
	    
	    // NOTE:  The EMC timing is calibrated to have a peak at zero.
	    // this means that if one wants to use the time on an absolute scale, 
	    // one needs to the "photon flash" time.  We will take that 
	    // flight time as the time for a v=c particle along a straight line
	    // from the measured vertex.
	    //                          TKH 5-13-2002
	    float Zvtx = d_vtx->get_ZVertex();
	    float Xemc = clus->x();
	    float Yemc = clus->y();
	    float Zemc = clus->z();
	    float dist = sqrt(Xemc*Xemc + Yemc*Yemc + (Zemc-Zvtx)*(Zemc-Zvtx));
	    particle->set_temc(itrk, (particle->get_temc(itrk)+dist/lightSpeed) );
	    
	  }

	  // SECTION for OLD EMC variable types... (only if new type not available)
	  if( iemc>=0 && d_emc==0 && d_emcOLD!=0) {

	    particle->set_sect   (itrk, d_emcOLD->get_sector(iemc));
	    int emcindex =  d_emcOLD->get_index(iemc);
	    particle->set_ysect  (itrk, (emcindex/100)%100);
	    particle->set_zsect  (itrk,  emcindex%100);
	    if( version<13) 
	      {
		particle->set_ecorr  (itrk, d_emcOLD->get_ecorr(iemc));
	      }
	    particle->set_ecore  (itrk, d_emcOLD->get_ecore(iemc));
	    particle->set_temc   (itrk, d_emcOLD->get_tofcorr(iemc) - TimeZero);
	    particle->set_prob   (itrk, d_emcOLD->get_prob_photon(iemc));
	    particle->set_ecent  (itrk, d_emcOLD->get_ecent(iemc));
	    particle->set_twrhit (itrk, d_emcOLD->get_twrhit(iemc));
	    particle->set_e9     (itrk, d_emcOLD->get_e9(iemc));
	    particle->set_re9    (itrk, d_emcOLD->get_re9(iemc));
	    particle->set_emcchi2(itrk, d_emcOLD->get_chi2(iemc));
	    particle->set_deadmap(itrk, d_emcOLD->get_deadmap(iemc));
	    particle->set_warnmap(itrk, d_emcOLD->get_warnmap(iemc));
	    
	    // NOTE:  The EMC timing is calibrated to have a peak at zero.
	    // this means that if one wants to use the time on an absolute scale, 
	    // one needs to the "photon flash" time.  We will take that 
	    // flight time as the time for a v=c particle along a straight line
	    // from the measured vertex.
	    //                          TKH 5-13-2002
	    float Zvtx = d_vtx->get_ZVertex();
	    float Xemc = d_emcOLD->get_xyz(iemc,0);
	    float Yemc = d_emcOLD->get_xyz(iemc,1);
	    float Zemc = d_emcOLD->get_xyz(iemc,2);
	    float dist = sqrt(Xemc*Xemc + Yemc*Yemc + (Zemc-Zvtx)*(Zemc-Zvtx));
	    particle->set_temc(itrk, (particle->get_temc(itrk)+dist/lightSpeed) );
	    
	  }

	  // Here we fill the tof stuff.....
	  if( itof>=0) {

	    particle->set_etof (itrk, d_tof->get_eloss(itof));


	    particle->set_slat (itrk, d_tof->get_slatid(itof));
	    particle->set_ttof (itrk, d_tof->get_tof   (itof) - TimeZero);

	    particle->set_m2tof(itrk, -9999.);
	    particle->set_isPi (itrk, -9999.);
	    particle->set_isK  (itrk, -9999.);
	    particle->set_isP  (itrk, -9999.);
	  }
	  else {
	    particle->set_slat (itrk, -9999);
	    particle->set_ttof (itrk, -9999);
	    particle->set_etof (itrk, -9999);
	    particle->set_m2tof(itrk, -9999.);
	    particle->set_isPi (itrk, -9999.);
	    particle->set_isK  (itrk, -9999.);
	    particle->set_isP  (itrk, -9999.);
	  }
	  
	  // Here we fill the RICH stuff...
	  if(icrk>-1) {
	    particle->set_n0   (itrk, d_crkring->get_npmt0(icrk));
	    particle->set_npe0 (itrk, d_crkring->get_npe0 (icrk));
	    particle->set_n1   (itrk, d_crkring->get_npmt1(icrk));
	    particle->set_npe1 (itrk, d_crkring->get_npe1 (icrk));
	    particle->set_n2   (itrk, d_crkring->get_npmt2(icrk));
	    particle->set_npe2 (itrk, d_crkring->get_npe2 (icrk));
	    particle->set_n3   (itrk, d_crkring->get_npmt3(icrk));
	    particle->set_npe3 (itrk, d_crkring->get_npe3 (icrk));
	    particle->set_chi2 (itrk, d_crkring->get_chi2 (icrk));
	    particle->set_disp (itrk, d_crkring->get_disp (icrk));
	    particle->set_tcrk (itrk, d_crkring->get_tcrk (icrk));
	    particle->set_cross_phi (itrk, d_crkring->get_cross_phi (icrk));
	    particle->set_cross_z   (itrk, d_crkring->get_cross_z   (icrk));
	    particle->set_center_phi(itrk, d_crkring->get_center_phi(icrk));
	    particle->set_center_z  (itrk, d_crkring->get_center_z  (icrk));
	  }
	  else {
	    particle->set_n0   (itrk, -9999);
	    particle->set_npe0 (itrk, -9999);
	    particle->set_n1   (itrk, -9999);
	    particle->set_npe1 (itrk, -9999);
	    particle->set_n2   (itrk, -9999);
	    particle->set_npe2 (itrk, -9999);
	    particle->set_n3   (itrk, -9999);
	    particle->set_npe3 (itrk, -9999);
	    particle->set_chi2 (itrk, -9999);
	    particle->set_disp (itrk, -9999);
	    particle->set_tcrk (itrk, -9999);
	    particle->set_cross_phi (itrk, -9999);
	    particle->set_cross_z   (itrk, -9999);
	    particle->set_center_phi(itrk, -9999);
	    particle->set_center_z  (itrk, -9999);
	  }
	  
	  // Here we fill the tec dE/dX
	  if( TECEXTRA)
	    {
	      if( itec>=0) {
		particle->set_tecnhit     (itrk, d_tecout->get_nhits(itec));
		if( version<13)
		  {
		    particle->set_tecdedx1 (itrk, d_tecout->get_dEdx1(itec));
		    particle->set_tecdedx2 (itrk, d_tecout->get_dEdx2(itec));
		  }
	      }
	      else {
		particle->set_tecnhit     (itrk, -9999);
		if( version<13)
		  {
		    particle->set_tecdedx1 (itrk, -9999.);
		    particle->set_tecdedx2 (itrk, -9999.);
		  }
	      }
	    }
	  // Here we fill the sdphi and sdz variables...
	  // These are harder since they require that we perform calculations...
	  //  First we calculate the absolute dphi and dz,
	  //  then we feed these values to the matching cuts so
	  //  that the result is presented in sigmas!!!
	  float momch = 100.0;  // Use some large momentum (>10) as default in case of zero field runs...
	  float mom = particle->get_mom(itrk);
	  float alpha = particle->get_alpha(itrk);
	  int alphasign = alpha>0? 1:-1;
	  momch = -mom*alphasign;
	  PHAngle rawdphi(0);
	  float   rawdz, beta,zed;
	  beta    = d_dctrk->get_beta(dchid);
	  zed     = d_dctrk->get_zed(dchid);
	  // PC2 sdphi, sdz
	  if( ipc2>=0 && particle->get_ppc2x(itrk) > -999.) {
	    PHAngle phiM( atan2( d_pc2->get_xyz(ipc2,1), d_pc2->get_xyz(ipc2,0)) ) ;
	    PHAngle phiP( atan2( particle->get_ppc2y(itrk), particle->get_ppc2x(itrk)) );
	    rawdphi =  phiM-phiP;
	    float zM = d_pc2->get_xyz(ipc2,2);
	    float zP = particle->get_ppc2z(itrk);
	    rawdz      = zM-zP;
	    particle->set_pc2sect  (itrk, d_pc2->get_sector(ipc2));
	    if(RUN3MATCH){
	      particle->set_pc2sdphi(itrk, m->d_PC2_phi_match(momch,zed,rawdphi.getPhi()) );
	      particle->set_pc2sdz  (itrk, m->d_PC2_z_match(momch,zed,rawdz));
	    }else{
	      particle->set_pc2sdphi(itrk, m->d_PC2_phi_match(momch,rawdphi.getPhi()) );
	      particle->set_pc2sdz  (itrk, m->d_PC2_z_match(momch,rawdz));
	    }
	  }
	  else {
	    particle->set_pc2sdphi(itrk, -9999);
	    particle->set_pc2sdz  (itrk, -9999);
	    particle->set_pc2sect  (itrk, -9999);
	  }
	  
	  // PC3 sdphi, sdz
	  if( ipc3>=0 && particle->get_ppc3x(itrk) > -999.) {
	    PHAngle phiM( atan2( d_pc3->get_xyz(ipc3,1), d_pc3->get_xyz(ipc3,0)) );
	    PHAngle phiP( atan2( particle->get_ppc3y(itrk), particle->get_ppc3x(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = d_pc3->get_xyz(ipc3,2);
	    float zP = particle->get_ppc3z(itrk);
	    rawdz      = zM-zP;
	    if(RUN3MATCH){
	      if( particle->get_ppc3x(itrk)>0) { //West arm
		particle->set_pc3sdphi(itrk, m->d_PC3w_phi_match(momch,zed,rawdphi.getPhi()));
		particle->set_pc3sdz  (itrk, m->d_PC3w_z_match(momch,zed,rawdz));
	      }
	      else {
		particle->set_pc3sdphi(itrk, m->d_PC3e_phi_match(momch,zed,rawdphi.getPhi()));
		particle->set_pc3sdz  (itrk, m->d_PC3e_z_match(momch,zed,rawdz));
	      }
	    }else{
	      if( particle->get_ppc3x(itrk)>0) { //West arm
		particle->set_pc3sdphi(itrk, m->d_PC3w_phi_match(momch,rawdphi.getPhi()));
		particle->set_pc3sdz  (itrk, m->d_PC3w_z_match(momch,rawdz));
	      }
	      else {
		particle->set_pc3sdphi(itrk, m->d_PC3e_phi_match(momch,rawdphi.getPhi()));
		particle->set_pc3sdz  (itrk, m->d_PC3e_z_match(momch,rawdz));
	      }
	    }
	    particle->set_pc3sect  (itrk, d_pc3->get_sector(ipc3));
	  }
	  else {
	    particle->set_pc3sdphi(itrk, -9999);
	    particle->set_pc3sdz  (itrk, -9999);
	    particle->set_pc3sect  (itrk, -9999);
	  }
	  

	  // EMC--default to not found, then fill with best available.
	  particle->set_emcsdphi(itrk, -9999);
	  particle->set_emcsdz  (itrk, -9999);
	  particle->set_emcsdphi_e(itrk, -9999);
	  particle->set_emcsdz_e  (itrk, -9999);

	  // EMC sdphi, sdz NEW FORMAT........
	  if( iemc>=0 && particle->get_pemcx(itrk) > -999. && d_emc!=0) {

	    PHAngle phiM;
	    emcClusterContent* cluster=0;
	    cluster = d_emc->getCluster(iemc);
	    phiM = PHAngle( atan2( cluster->y(), cluster->x() ));

	    PHAngle phiP( atan2( particle->get_pemcy(itrk), 
				 particle->get_pemcx(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = cluster->z();
	    float zP = particle->get_pemcz(itrk);
	    rawdz      = zM-zP;

	    int NUCL =3; // True according to utiMatch
	    int ELEC =1;
	    if( particle->get_dcarm(itrk)==1) { //West arm
	      particle->set_emcsdphi(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),NUCL));
	      particle->set_emcsdz  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,NUCL));
	      particle->set_emcsdphi_e(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),ELEC));
	      particle->set_emcsdz_e  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,ELEC));
	    }
	    else {
	      if( particle->get_dcarm(itrk)==0 && particle->get_sect(itrk)>1) {
		particle->set_emcsdphi(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_emcsdz  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,NUCL));
		particle->set_emcsdphi_e(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_emcsdz_e  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,ELEC));
	      }
	      else {
		particle->set_emcsdphi(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_emcsdz  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,NUCL));
		particle->set_emcsdphi_e(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_emcsdz_e  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,ELEC));
	      }	      
	    }
	  }
	  
	  // EMC sdphi, sdz OLD FORMAT........
	  if( iemc>=0 && particle->get_pemcx(itrk) > -999. && d_emc==0 && d_emcOLD!=0) {

	    PHAngle phiM;
	    phiM = PHAngle( atan2( d_emcOLD->get_xyz(iemc,1), d_emcOLD->get_xyz(iemc,0) ));

	    PHAngle phiP( atan2( particle->get_pemcy(itrk), 
				 particle->get_pemcx(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = d_emcOLD->get_xyz(iemc,2);
	    float zP = particle->get_pemcz(itrk);
	    rawdz      = zM-zP;

	    int NUCL =3; // True according to utiMatch
	    int ELEC =1;
	    if( particle->get_dcarm(itrk)==1) { //West arm
	      particle->set_emcsdphi(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),NUCL));
	      particle->set_emcsdz  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,NUCL));
	      particle->set_emcsdphi_e(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),ELEC));
	      particle->set_emcsdz_e  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,ELEC));
	    }
	    else {
	      if( particle->get_dcarm(itrk)==0 && particle->get_sect(itrk)>1) {
		particle->set_emcsdphi(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_emcsdz  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,NUCL));
		particle->set_emcsdphi_e(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_emcsdz_e  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,ELEC));
	      }
	      else {
		particle->set_emcsdphi(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_emcsdz  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,NUCL));
		particle->set_emcsdphi_e(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_emcsdz_e  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,ELEC));
	      }	      
	    }
	  }

	  // TOF sdphi, sdz
	  if( itof>=0 && particle->get_ptofx(itrk) > -999.) {
	    PHAngle phiM( atan2( d_tof->get_xtof(itof,1), d_tof->get_xtof(itof,0)) );
	    PHAngle phiP( atan2( particle->get_ptofy(itrk), particle->get_ptofx(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = d_tof->get_xtof(itof,2);
	    float zP = particle->get_ptofz(itrk);
	    rawdz      = zM-zP;
	    if(RUN3MATCH){
	      particle->set_tofsdphi(itrk, m->d_TOF_phi_match(momch,zed,rawdphi.getPhi()));
	      particle->set_tofsdz  (itrk, m->d_TOF_z_match(momch,zed,rawdz));
	    }else{
	      particle->set_tofsdphi(itrk, m->d_TOF_phi_match(momch,rawdphi.getPhi()));
	      particle->set_tofsdz  (itrk, m->d_TOF_z_match(momch,rawdz));
	    }
	  }
	  else {
	    particle->set_tofsdphi(itrk, -9999);
	    particle->set_tofsdz  (itrk, -9999);
	  }
	  
	  // TEC sdphi, sdalpha
	  if( itec>=0 && particle->get_ptecx(itrk) > -999.) {
	    PHAngle phiM( d_tecout->get_phi(itec) );
	    PHAngle phiP( atan2( particle->get_ptecy(itrk), particle->get_ptecx(itrk)) ); 
	    rawdphi = phiM-phiP;
	    PHAngle aM( d_tecout->get_alpha(itec) );
	    PHAngle aP( particle->get_alpha(itrk) );
	    rawdz      = aM.getPhi()-aP.getPhi()*0.534;  // Factor 0.534 from X. Li
	    particle->set_tecsdphi(itrk,    m->d_TEC_phi_match(momch,rawdphi.getPhi()));
	    particle->set_tecsdalpha(itrk,  m->d_TEC_alpha_match(momch, rawdz));
	  }
	  else {
	    particle->set_tecsdphi  (itrk, -9999);
	    particle->set_tecsdalpha(itrk, -9999);
	  }
	  
	  itrk++;  // Increment the track if particle OK...
	}
      
      particle->set_npart(itrk);
      
    }

  return 0;

}



int CentraltrackReco::FillIndices(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //  Begin the fill...

  if( d_cgl && particle)
    {
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  //  Set up all the pointers to other structures...
	  int ipc1  = d_cgl->get_pc1clusid (icgl);	
	  int ipc2  = d_cgl->get_pc2clusid (icgl);	
	  int ipc3  = d_cgl->get_pc3clusid (icgl);
	  int iemc  = d_cgl->get_emcclusid (icgl);
	  int itof  = d_cgl->get_tofrecid  (icgl);
	  int icrk  = d_cgl->get_richringid(icgl);
	  int itec  = d_cgl->get_tectrackid(icgl);
	  int dchid = d_cgl->get_dctracksid(icgl);
	  
	  particle->set_pc1id (itrk, ipc1); 
	  particle->set_pc2id (itrk, ipc2); 
          particle->set_pc3id (itrk, ipc3); 
          particle->set_emcid (itrk, iemc); 
          particle->set_tofid (itrk, itof); 
          particle->set_tecid (itrk, itec); 
          particle->set_ring  (itrk, icrk); 
	  if( version>=13)
	    {
	      particle->set_dchid(itrk, dchid);
	    }

	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;

}

int CentraltrackReco::FillAbsolutePos(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // DC
  if( !d_dctrk) {
    PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
    DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
    if(DCTRACK) d_dctrk  = DCTRACK->getData ();
    if( !d_dctrk) cout << PHWHERE << "CentraltrackReco:: dctrk not in Node Tree" << endl;
  }

  // PC1 
  if( !d_pc1) {
    PHTypedNodeIterator<PadCluster> iPC1(topNode);
    PadClusterNode_t *PC1 = iPC1.find("Pc1Cluster");
    if(PC1) d_pc1  = PC1->getData ();
    if( !d_pc1) cout << PHWHERE << "CentraltrackReco:: pc1 not in Node Tree" << endl;
  }

  // PC2
  if( !d_pc2) {
    PHTypedNodeIterator<PadCluster> iPC2(topNode);
    PadClusterNode_t *PC2 = iPC2.find("Pc2Cluster");
    if(PC2) d_pc2  = PC2->getData ();
    if( !d_pc2) cout << PHWHERE << "CentraltrackReco:: pc2 not in Node Tree" << endl;
  }

  // PC3
  if( !d_pc3) {
    PHTypedNodeIterator<PadCluster> iPC3(topNode);
    PadClusterNode_t *PC3 = iPC3.find("Pc3Cluster");
    if(PC3) d_pc3  = PC3->getData ();
    if( !d_pc3) cout << PHWHERE << "CentraltrackReco:: pc3 not in Node Tree" << endl;
  }
  
  // TEC
  if( !d_tecout) {
    PHTypedNodeIterator<TecOut> teciter(topNode);
    TecOutNode_t *TecOutNode = teciter.find("TecOut");
    if(TecOutNode) d_tecout = TecOutNode->getData();
    //if( !d_tecout) cout << PHWHERE << "CentraltrackReco:: tec not in Node Tree" << endl;
  }

  // EMC
  if( !d_emc) {
    PHTypedNodeIterator<emcClusterContainer> iEMC(topNode);
    emcClusterContainerNode_t* EMC = iEMC.find("emcClusterContainer");
    if(EMC) d_emc = EMC->getData();
  }

  // EMC OLD FORMAT...
  if( !d_emcOLD) {
    PHTypedNodeIterator<EmcClusterLocalExt> iEMCOLD(topNode);
    EmcClusterLocalExtNode_t *EMCOLD = iEMCOLD.find("EmcClusterLocalExt");
    if(EMCOLD) d_emcOLD  = EMCOLD->getData ();
  }
  if( !d_emc && !d_emcOLD) cout << PHWHERE << "CentraltrackReco:: EMC (old or new) not in Node Tree" << endl;

  // TOF
  if( !d_tof) {
    PHTypedNodeIterator<TofOut> iTOF(topNode);
    TofOutNode_t *TOF = iTOF.find("TofOut");
    if(TOF) d_tof  = TOF->getData ();
    if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_cgl && particle && d_dctrk)
    {

      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  //  Set up all the pointers to other structures...
	  int ipc2  = d_cgl->get_pc2clusid (icgl);	
	  int ipc3  = d_cgl->get_pc3clusid (icgl);
	  int iemc  = d_cgl->get_emcclusid (icgl);
	  int itof  = d_cgl->get_tofrecid  (icgl);
	  int itec  = d_cgl->get_tectrackid(icgl);

	  // PC2 sdphi, sdz
	  if( ipc2>=0 && particle->get_ppc2x(itrk) > -999.) {
	    PHAngle phiM( atan2( d_pc2->get_xyz(ipc2,1), d_pc2->get_xyz(ipc2,0)) ) ;
	    float zM = d_pc2->get_xyz(ipc2,2);
	    particle->set_pc2phi  (itrk, phiM );
	    particle->set_pc2z    (itrk, zM   );
	  }
	  else {
	    particle->set_pc2phi  (itrk, -9999);
	    particle->set_pc2z    (itrk, -9999);
	  }
	  
	  // PC3 sdphi, sdz
	  if( ipc3>=0 && particle->get_ppc3x(itrk) > -999.) {
	    PHAngle phiM( atan2( d_pc3->get_xyz(ipc3,1), d_pc3->get_xyz(ipc3,0)) );
	    float zM = d_pc3->get_xyz(ipc3,2);
	    particle->set_pc3phi  (itrk, phiM );
	    particle->set_pc3z    (itrk, zM   );
	  }
	  else {
	    particle->set_pc3phi  (itrk, -9999);
	    particle->set_pc3z    (itrk, -9999);
	  }
	  
	  // EMC--default with not found then fill best available
	  particle->set_emcphi  (itrk, -9999);
	  particle->set_emcz    (itrk, -9999);

	  // EMC sdphi, sdz
	  if( iemc>=0 && particle->get_pemcx(itrk) > -999. && d_emc!=0) {

	    PHAngle phiM;
	    emcClusterContent* cluster=0;
	    cluster = d_emc->getCluster(iemc);

	    phiM = PHAngle( atan2( cluster->y(), cluster->x() ));
	    float zM = cluster->z();
	    particle->set_emcphi  (itrk, phiM );
	    particle->set_emcz    (itrk, zM   );
	  }
	  
	  // EMC sdphi, sdz OLD FORMAT
	  if( iemc>=0 && particle->get_pemcx(itrk) > -999. && d_emc==0 && d_emcOLD!=0) {
	    PHAngle phiM;
	    phiM = PHAngle( atan2( d_emcOLD->get_xyz(iemc,1), d_emcOLD->get_xyz(iemc,0) ));
	    float zM = d_emcOLD->get_xyz(iemc,2);
	    particle->set_emcphi  (itrk, phiM );
	    particle->set_emcz    (itrk, zM   );
	  }
	  
	  // TOF sdphi, sdz
	  if( itof>=0 && particle->get_ptofx(itrk) > -999.) {
	    PHAngle phiM( atan2( d_tof->get_xtof(itof,1), d_tof->get_xtof(itof,0)) );
	    float zM = d_tof->get_xtof(itof,2);
	    particle->set_tofphi  (itrk, phiM );
	    particle->set_tofz    (itrk, zM   );
	  }
	  else {
	    particle->set_tofphi  (itrk, -9999);
	    particle->set_tofz    (itrk, -9999);
	  }
	  
	  // TEC sdphi, sdalpha
	  if( itec>=0 && particle->get_ptecx(itrk) > -999.) {
	    PHAngle phiM( d_tecout->get_phi(itec) );
	    PHAngle aM( d_tecout->get_alpha(itec) );
	    particle->set_tecphi   (itrk, phiM );
	    particle->set_tecalpha (itrk, aM   );
	  }
	  else {
	    particle->set_tecphi    (itrk, -9999);
	    particle->set_tecalpha  (itrk, -9999);
	  }
	  
	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;

}

int CentraltrackReco::FillAbsoluteDelta(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // DC
  if( !d_dctrk) {
    PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
    DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
    if(DCTRACK) d_dctrk  = DCTRACK->getData ();
    if( !d_dctrk) cout << PHWHERE << "CentraltrackReco:: dctrk not in Node Tree" << endl;
  }

  // PC1 
  if( !d_pc1) {
    PHTypedNodeIterator<PadCluster> iPC1(topNode);
    PadClusterNode_t *PC1 = iPC1.find("Pc1Cluster");
    if(PC1) d_pc1  = PC1->getData ();
    if( !d_pc1) cout << PHWHERE << "CentraltrackReco:: pc1 not in Node Tree" << endl;
  }

  // PC2
  if( !d_pc2) {
    PHTypedNodeIterator<PadCluster> iPC2(topNode);
    PadClusterNode_t *PC2 = iPC2.find("Pc2Cluster");
    if(PC2) d_pc2  = PC2->getData ();
    if( !d_pc2) cout << PHWHERE << "CentraltrackReco:: pc2 not in Node Tree" << endl;
  }

  // PC3
  if( !d_pc3) {
    PHTypedNodeIterator<PadCluster> iPC3(topNode);
    PadClusterNode_t *PC3 = iPC3.find("Pc3Cluster");
    if(PC3) d_pc3  = PC3->getData ();
    if( !d_pc3) cout << PHWHERE << "CentraltrackReco:: pc3 not in Node Tree" << endl;
  }
  
  // TEC
  if( !d_tecout) {
    PHTypedNodeIterator<TecOut> teciter(topNode);
    TecOutNode_t *TecOutNode = teciter.find("TecOut");
    if(TecOutNode) d_tecout = TecOutNode->getData();
    //if( !d_tecout) cout << PHWHERE << "CentraltrackReco:: tec not in Node Tree" << endl;
  }

  // EMC
  if( !d_emc) {
    PHTypedNodeIterator<emcClusterContainer> iEMC(topNode);
    emcClusterContainerNode_t* EMC = iEMC.find("emcClusterContainer");
    if(EMC) d_emc = EMC->getData();
  }

  // EMC OLD FORMAT...
  if( !d_emcOLD) {
    PHTypedNodeIterator<EmcClusterLocalExt> iEMCOLD(topNode);
    EmcClusterLocalExtNode_t *EMCOLD = iEMCOLD.find("EmcClusterLocalExt");
    if(EMCOLD) d_emcOLD  = EMCOLD->getData ();
  }
  if( !d_emc && !d_emcOLD) cout << PHWHERE << "CentraltrackReco:: EMC (old or new) not in Node Tree" << endl;

  // TOF
  if( !d_tof) {
    PHTypedNodeIterator<TofOut> iTOF(topNode);
    TofOutNode_t *TOF = iTOF.find("TofOut");
    if(TOF) d_tof  = TOF->getData ();
    if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_cgl && particle && d_dctrk)
    {

      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  //  Set up all the pointers to other structures...
	  int ipc2  = d_cgl->get_pc2clusid (icgl);	
	  int ipc3  = d_cgl->get_pc3clusid (icgl);
	  int iemc  = d_cgl->get_emcclusid (icgl);
	  int itof  = d_cgl->get_tofrecid  (icgl);
	  int itec  = d_cgl->get_tectrackid(icgl);

	  // Here we fill the absolute phi, z, dphi, and dz variables...
	  // These are harder since they require that we perform calculations...
	  PHAngle rawdphi(0);
	  float   rawdz;
	  
	  // PC2 sdphi, sdz
	  if( ipc2>=0 && particle->get_ppc2x(itrk) > -999.) {
	    PHAngle phiM( atan2( d_pc2->get_xyz(ipc2,1), d_pc2->get_xyz(ipc2,0)) ) ;
	    PHAngle phiP( atan2( particle->get_ppc2y(itrk), particle->get_ppc2x(itrk)) );
	    rawdphi =  phiM-phiP;
	    float zM = d_pc2->get_xyz(ipc2,2);
	    float zP = particle->get_ppc2z(itrk);
	    rawdz      = zM-zP;
	    particle->set_pc2dphi (itrk, rawdphi );
	    particle->set_pc2dz   (itrk, rawdz   );
	  }
	  else {
	    particle->set_pc2dphi (itrk, -9999);
	    particle->set_pc2dz   (itrk, -9999);
	  }
	  
	  // PC3 sdphi, sdz
	  if( ipc3>=0 && particle->get_ppc3x(itrk) > -999.) {
	    PHAngle phiM( atan2( d_pc3->get_xyz(ipc3,1), d_pc3->get_xyz(ipc3,0)) );
	    PHAngle phiP( atan2( particle->get_ppc3y(itrk), particle->get_ppc3x(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = d_pc3->get_xyz(ipc3,2);
	    float zP = particle->get_ppc3z(itrk);
	    rawdz      = zM-zP;
	    particle->set_pc3dphi (itrk, rawdphi );
	    particle->set_pc3dz   (itrk, rawdz   );
	  }
	  else {
	    particle->set_pc3dphi (itrk, -9999);
	    particle->set_pc3dz   (itrk, -9999);
	  }
	  
	  // EMC--default with not found then fill best available
	  particle->set_emcdphi (itrk, -9999);
	  particle->set_emcdz   (itrk, -9999);

	  // EMC sdphi, sdz
	  if( iemc>=0 && particle->get_pemcx(itrk) > -999. && d_emc!=0) {

	    PHAngle phiM;
	    emcClusterContent* cluster=0;
	    cluster = d_emc->getCluster(iemc);
	    phiM = PHAngle( atan2( cluster->y(), cluster->x() ));

	    PHAngle phiP( atan2( particle->get_pemcy(itrk), 
				 particle->get_pemcx(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = cluster->z();
	    float zP = particle->get_pemcz(itrk);
	    rawdz      = zM-zP;

	    particle->set_emcdphi (itrk, rawdphi );
	    particle->set_emcdz   (itrk, rawdz   );
	  }
	  
	  // EMC sdphi, sdz OLD FORMAT
	  if( iemc>=0 && particle->get_pemcx(itrk) > -999. && d_emc==0 && d_emcOLD!=0) {

	    PHAngle phiM;
	    phiM = PHAngle( atan2( d_emcOLD->get_xyz(iemc,1), d_emcOLD->get_xyz(iemc,0) ));

	    PHAngle phiP( atan2( particle->get_pemcy(itrk), 
				 particle->get_pemcx(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = d_emcOLD->get_xyz(iemc,2);
	    float zP = particle->get_pemcz(itrk);
	    rawdz      = zM-zP;

	    particle->set_emcdphi (itrk, rawdphi );
	    particle->set_emcdz   (itrk, rawdz   );
	  }
	  
	  // TOF sdphi, sdz
	  if( itof>=0 && particle->get_ptofx(itrk) > -999.) {
	    PHAngle phiM( atan2( d_tof->get_xtof(itof,1), d_tof->get_xtof(itof,0)) );
	    PHAngle phiP( atan2( particle->get_ptofy(itrk), particle->get_ptofx(itrk)) );
	    rawdphi = phiM-phiP;
	    float zM = d_tof->get_xtof(itof,2);
	    float zP = particle->get_ptofz(itrk);
	    rawdz      = zM-zP;
	    particle->set_tofdphi (itrk, rawdphi );
	    particle->set_tofdz   (itrk, rawdz   );
	  }
	  else {
	    particle->set_tofdphi (itrk, -9999);
	    particle->set_tofdz   (itrk, -9999);
	  }
	  
	  // TEC sdphi, sdalpha
	  if( itec>=0 && particle->get_ptecx(itrk) > -999.) {
	    PHAngle phiM( d_tecout->get_phi(itec) );
	    PHAngle phiP( atan2( particle->get_ptecy(itrk), particle->get_ptecx(itrk)) ); 
	    rawdphi = phiM-phiP;
	    PHAngle aM( d_tecout->get_alpha(itec) );
	    PHAngle aP( particle->get_alpha(itrk) );
	    rawdz      = aM.getPhi()-aP.getPhi()*0.534;  // Factor 0.534 from X. Li
	    particle->set_tecdphi  (itrk, rawdphi );
	    particle->set_tecdalpha(itrk, rawdz   );
	  }
	  else {
	    particle->set_tecdphi   (itrk, -9999);
	    particle->set_tecdalpha (itrk, -9999);
	  }
	  
	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;

}

int CentraltrackReco::FillLvl1(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // The Raw Event...
  PHNodeIterator iter(topNode);
  PHNode *n1 = iter.findFirst("PHDataNode","PRDF");
  if( n1)
    {
      EventNode_t *EVT = static_cast<EventNode_t *>(n1);
      d_evt = EVT->getData();
    }
  if( !d_evt) cout << PHWHERE << "CentraltrackReco:: evt not in Node Tree" << endl;

  //-----------------------------------------------------------------
  //  Begin the fill...

  if( d_evt && particle)
    {
      int rawtrigword    ;
      int livetrigword   ;
      int scaledtrigword ;

      // Get the trigger words from the proper packet...
      Packet *p = d_evt->getPacket(14001);
      if( p) {
	rawtrigword    = p->iValue(0, "RAWTRIG");
	livetrigword   = p->iValue(0, "LIVETRIG");
	scaledtrigword = p->iValue(0, "SCALEDTRIG");
	delete p;
      }
      else {
	rawtrigword    = -9999;
	livetrigword   = -9999;
	scaledtrigword = -9999;
      }
	
      // Put them in the tracks...
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  particle->set_RawL1( itrk, rawtrigword);
	  particle->set_LivL1( itrk, livetrigword);
	  particle->set_SclL1( itrk, scaledtrigword);
	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;
  
}


int CentraltrackReco::FillPadWidths(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: particle not in Node Tree" << endl;
  }

  // PC1 
  if( !d_pc1) {
    PHTypedNodeIterator<PadCluster> iPC1(topNode);
    PadClusterNode_t *PC1 = iPC1.find("Pc1Cluster");
    if(PC1) d_pc1  = PC1->getData ();
    if( !d_pc1) cout << PHWHERE << "CentraltrackReco:: pc1 not in Node Tree" << endl;
  }
  
  // PC2
  if( !d_pc2) {
    PHTypedNodeIterator<PadCluster> iPC2(topNode);
    PadClusterNode_t *PC2 = iPC2.find("Pc2Cluster");
    if(PC2) d_pc2  = PC2->getData ();
    if( !d_pc2) cout << PHWHERE << "CentraltrackReco:: pc2 not in Node Tree" << endl;
  }
  
  // PC3
  if( !d_pc3) {
    PHTypedNodeIterator<PadCluster> iPC3(topNode);
    PadClusterNode_t *PC3 = iPC3.find("Pc3Cluster");
    if(PC3) d_pc3  = PC3->getData ();
    if( !d_pc3) cout << PHWHERE << "CentraltrackReco:: pc3 not in Node Tree" << endl;
  }
  
  
  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //  Begin the fill...

  if( d_cgl && d_pc1 && d_pc2 && d_pc3 && particle)
    {
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  //  Set up all the pointers to other structures...
	  int ipc1  = d_cgl->get_pc1clusid (icgl);	
	  int ipc2  = d_cgl->get_pc2clusid (icgl);	
	  int ipc3  = d_cgl->get_pc3clusid (icgl);

	  if( ipc1>=0) particle->set_pc1wid( itrk, d_pc1->get_type(ipc1) );
	  if( ipc1>=0) particle->set_pc2wid( itrk, d_pc2->get_type(ipc2) );
	  if( ipc1>=0) particle->set_pc3wid( itrk, d_pc3->get_type(ipc3) );

	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;

}


int CentraltrackReco::FillTrackModel(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: particle not in Node Tree" << endl;
  }

  // Track Model
  if( !d_model) {
    PHTypedNodeIterator<PHDchTrackOut> iPHDchTrackOut(topNode);
    PHDchTrackOutNode_t *PHDchTrackOutNode = iPHDchTrackOut.find("PHDchTrackOut");
    if(PHDchTrackOutNode) d_model = PHDchTrackOutNode->getData();
    if( !d_model) cout << PHWHERE << "CentraltrackReco:: model not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //  Begin the fill...

  if( d_cgl && d_dctrk && particle)
    {
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  //  Set up all the pointers to other structures...
	  int dchid = d_cgl->get_dctracksid(icgl);

	  // info from the track model...
          particle->set_status  (itrk, d_dctrk->get_status  (dchid)); 
          particle->set_alpha1  (itrk, d_dctrk->get_alpha1  (dchid));
          particle->set_alpha2  (itrk, d_dctrk->get_alpha2  (dchid));
          particle->set_mx1dist (itrk, d_dctrk->get_dist1   (dchid)); 
          particle->set_mx2dist (itrk, d_dctrk->get_dist2   (dchid)); 
	  
	  if( d_model) {

	    if( FieldOnFlag==1) {
	      if( version<13) 
		{
		  particle->set_nx1x2fit (itrk, d_model->get_numberOfX1X2hitsFitted(dchid)); 
		  particle->set_alphaf   (itrk, d_model->get_fittedAlpha           (dchid)); 
		}
	      
	      particle->set_mchi2    (itrk, d_model->get_chi2                  (dchid)); 
	    }
	  }
	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;

}




int CentraltrackReco::FillSwapped(PHCompositeNode *topNode)
{

  // This routine is used to fill those variables that are common to 
  // all track version types.  Along the way it sets the pointers for
  // a number of different input data blocks.  Since FillCommon will always be
  // called, we don't need to check if the pointers are already references, 
  // they are.

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: particle not in Node Tree" << endl;
  }

  // DC
  if( !d_dctrk) {
    PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
    DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
    if(DCTRACK) d_dctrk  = DCTRACK->getData ();
    if( !d_dctrk) cout << PHWHERE << "CentraltrackReco:: dctrk not in Node Tree" << endl;
  }
  
  // PC1 
  if( !d_pc1) {
    PHTypedNodeIterator<PadCluster> iPC1(topNode);
    PadClusterNode_t *PC1 = iPC1.find("Pc1Cluster");
    if(PC1) d_pc1  = PC1->getData ();
    if( !d_pc1) cout << PHWHERE << "CentraltrackReco:: pc1 not in Node Tree" << endl;
  }
  
  // PC2
  if( !d_pc2) {
    PHTypedNodeIterator<PadCluster> iPC2(topNode);
    PadClusterNode_t *PC2 = iPC2.find("Pc2Cluster");
    if(PC2) d_pc2  = PC2->getData ();
    if( !d_pc2) cout << PHWHERE << "CentraltrackReco:: pc2 not in Node Tree" << endl;
  }
  
  // PC3
  if( !d_pc3) {
    PHTypedNodeIterator<PadCluster> iPC3(topNode);
    PadClusterNode_t *PC3 = iPC3.find("Pc3Cluster");
    if(PC3) d_pc3  = PC3->getData ();
    if( !d_pc3) cout << PHWHERE << "CentraltrackReco:: pc3 not in Node Tree" << endl;
  }
  
  
  // TEC
  if( !d_tecout) {
    PHTypedNodeIterator<TecOut> teciter(topNode);
    TecOutNode_t *TecOutNode = teciter.find("TecOut");
    if(TecOutNode) d_tecout = TecOutNode->getData();
    //if( !d_tecout) cout << PHWHERE << "CentraltrackReco:: tec not in Node Tree" << endl;
  }
  
  // EMC
  if( !d_emc) {
    PHTypedNodeIterator<emcClusterContainer> iEMC(topNode);
    emcClusterContainerNode_t* EMC = iEMC.find("emcClusterContainer");
    if(EMC) d_emc = EMC->getData();
  }
  
  // EMC OLD FORMAT...
  if( !d_emcOLD) {
    PHTypedNodeIterator<EmcClusterLocalExt> iEMCOLD(topNode);
    EmcClusterLocalExtNode_t *EMCOLD = iEMCOLD.find("EmcClusterLocalExt");
    if(EMCOLD) d_emcOLD  = EMCOLD->getData ();
  }
  if( !d_emc && !d_emcOLD) cout << PHWHERE << "CentraltrackReco:: EMC (neither old nor new) not in Node Tree" << endl;

  // TOF
  if( !d_tof) {
    PHTypedNodeIterator<TofOut> iTOF(topNode);
    TofOutNode_t *TOF = iTOF.find("TofOut");
    if(TOF) d_tof  = TOF->getData ();
    if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  }
  
  //RICH BGD RINGS
  if( !d_scrkring) {
    PHTypedNodeIterator<CrkRing>    crkringmicroBGiter(topNode);
    CrkRingNode_t *CrkRingMicroBGNode = crkringmicroBGiter.find("CrkRingBack");
    if( CrkRingMicroBGNode) d_scrkring = CrkRingMicroBGNode->getData();
    if( !d_scrkring) cout << PHWHERE << "CentraltrackReco:: Swapped Rings not in Node Tree" << endl;
  }
  
  //Swapped global tracking
  if( !d_scgl) {
    PHTypedNodeIterator<CglTrack> iSCGL(topNode);
    CglTrackNode_t *SCGL = iSCGL.find("CglTrackBack");
    if(SCGL) d_scgl  = SCGL->getData ();
    if( !d_scgl) cout << PHWHERE << "CentraltrackReco:: Swapped cgl not in Node Tree" << endl;
  }

  // Swapped projection
  if( !d_sproj) {
    PHTypedNodeIterator<PHTrackOut> iPHTrackOutBack(topNode);
    PHTrackOutNode_t *PHTrackOutBackNode= iPHTrackOutBack.find("PHTrackOutBack");
    if(PHTrackOutBackNode) d_sproj = PHTrackOutBackNode->getData();
    if( !d_sproj) cout << PHWHERE << "CentraltrackReco:: Swapped Projections not in Node Tree" << endl;
  }

  
  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_scgl && particle && d_dctrk)
  {
    
    float TimeZero = 0.;
    if( d_bbc && d_bbc->isValid()) {
      TimeZero = d_bbc->get_TimeZero();
    }
    else if( d_t0 && d_t0->isValid()) {
      TimeZero = d_t0->get_T0();
    }
    else {
      cout << PHWHERE << "No Time Zero available" << endl;
    }
      
    int itrk=0;
    for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
    {
      //  Set up all the pointers to other structures...
	  int dchid = d_cgl->get_dctracksid(icgl);

	  int ispc2;
	  int ispc3;
	  int isemc;
	  int istof;
	  int iscrk;
	  int istec;

	  if( d_scgl) { 
      
	    ispc2 = d_scgl->get_pc2clusid(icgl);
	    ispc3 = d_scgl->get_pc3clusid(icgl);
	    isemc = d_scgl->get_emcclusid(icgl);
	    istof = d_scgl->get_tofrecid(icgl);
	    iscrk = d_scgl->get_richringid(icgl);
	    istec = d_scgl->get_tectrackid(icgl);
	  }
	  else {
	    ispc2 = -1;
	    ispc3 = -1;
	    isemc = -1;
	    istof = -1;
	    iscrk = -1;
	    istec = -1;
	  }

	  // Here we fill the sdphi and sdz variables...
	  // These are harder since they require that we perform calculations...
	  //  First we calculate the absolute dphi and dz,
	  //  then we feed these values to the matching cuts so
	  //  that the result is presented in sigmas!!!
	  float momch = 100.0;  // Use some large momentum (>10) as default in case of zero field runs...
	  float mom = particle->get_mom(itrk);
	  float alpha = particle->get_alpha(itrk);
	  int alphasign = alpha>0? 1:-1;
	  momch = -mom*alphasign;
	  PHAngle rawdphi(0);
	  float   rawdz, beta,zed;
	  beta    = d_dctrk->get_beta(dchid);
	  zed     = d_dctrk->get_zed(dchid);

	  // Calculate the dphi and dz for the flip and slides....
	  // Swapped PC2 sdphi, sdz
	  if( ispc2>=0 && d_sproj->get_projectionPc2(dchid,0) > -999.) {
	    PHAngle phiM( atan2( d_pc2->get_xyz(ispc2,1), d_pc2->get_xyz(ispc2,0)) );
	    PHAngle phiP( atan2( d_sproj->get_projectionPc2(dchid,1) , d_sproj->get_projectionPc2(dchid,0) ) );
	    rawdphi = phiM-phiP;
	    float zM = d_pc2->get_xyz(ispc2,2);
	    float zP = d_sproj->get_projectionPc2(dchid,2); 
	    rawdz      = zM-zP;
	    if(RUN3MATCH){
	      particle->set_spc2sdphi(itrk, m->d_PC2_phi_match(momch,zed,rawdphi.getPhi()));
	      particle->set_spc2sdz  (itrk, m->d_PC2_z_match(momch,zed,rawdz));
	    }else{
	      particle->set_spc2sdphi(itrk, m->d_PC2_phi_match(momch,rawdphi.getPhi()));
	      particle->set_spc2sdz  (itrk, m->d_PC2_z_match(momch,rawdz));
	    }
	  }
	  else {
	    particle->set_spc2sdphi(itrk, -9999);
	    particle->set_spc2sdz  (itrk, -9999);
	  }
	  
	  // Swapped PC3 sdphi, sdz
	  if( ispc3>=0 && d_sproj->get_projectionPc3(dchid,0) > -999.) {
	    PHAngle phiM( atan2( d_pc3->get_xyz(ispc3,1), d_pc3->get_xyz(ispc3,0)) );
	    PHAngle phiP( atan2( d_sproj->get_projectionPc3(dchid,1) , d_sproj->get_projectionPc3(dchid,0) ) );
	    rawdphi = phiM-phiP;
	    float zM = d_pc3->get_xyz(ispc3,2);
	    float zP = d_sproj->get_projectionPc3(dchid,2);
	    rawdz      = zM-zP;
	    if(RUN3MATCH){
	      if( particle->get_ppc3x(itrk)>0) { //West arm
		particle->set_spc3sdphi(itrk, m->d_PC3w_phi_match(momch,zed,rawdphi.getPhi()));
		particle->set_spc3sdz  (itrk, m->d_PC3w_z_match(momch,zed,rawdz));
	      }
	      else {
		particle->set_spc3sdphi(itrk, m->d_PC3e_phi_match(momch,rawdphi.getPhi()));
		particle->set_spc3sdz  (itrk, m->d_PC3e_z_match(momch,rawdz));
	      }
	    }else{
	      if( particle->get_ppc3x(itrk)>0) { //West arm
		particle->set_spc3sdphi(itrk, m->d_PC3w_phi_match(momch,rawdphi.getPhi()));
		particle->set_spc3sdz  (itrk, m->d_PC3w_z_match(momch,rawdz));
	      }
	      else {
		particle->set_spc3sdphi(itrk, m->d_PC3e_phi_match(momch,rawdphi.getPhi()));
		particle->set_spc3sdz  (itrk, m->d_PC3e_z_match(momch,rawdz));
	      }
	    }
	  }
	  else {
	    particle->set_spc3sdphi(itrk, -9999);
	    particle->set_spc3sdz  (itrk, -9999);
	  }
	  
	  //EMC--default with not found then fill with best available
	  particle->set_semcsdphi  (itrk, -9999);
	  particle->set_semcsdz    (itrk, -9999);
	  particle->set_semcsdphi_e(itrk, -9999);
	  particle->set_semcsdz_e  (itrk, -9999);
	  if( version<13) particle->set_secorr(itrk, -9999);
	  particle->set_secore     (itrk, -9999);
	  particle->set_stemc      (itrk, -9999);
	  particle->set_sprob      (itrk, -9999);
	  //particle->set_secent     (itrk, -9999);
	  particle->set_stwrhit    (itrk, -9999);
	  particle->set_semcchi2   (itrk, -9999);


	  // Swapped EMC sdphi, sdz  New Format
	  if( isemc>=0 && d_sproj->get_projectionEmc(dchid,0) > -999. && d_emc!=0) {

	    emcClusterContent* cluster=0;
	    cluster = d_emc->getCluster(isemc);

	    //fill the other emc swap variables
	    particle->set_semce    (itrk, cluster->e()    );
	    particle->set_secore   (itrk, cluster->ecore());
	    particle->set_se9      (itrk, cluster->e9()   );
	    particle->set_secent   (itrk, cluster->ecent());
	    particle->set_stemc    (itrk, cluster->tofcorr() - TimeZero);
	    particle->set_sprob    (itrk, cluster->prob_photon());
	    particle->set_semcchi2 (itrk, cluster->chi2() );
	    particle->set_semcdispy(itrk, cluster->dispy());
	    particle->set_semcdispz(itrk, cluster->dispz());
	    particle->set_stwrhit  (itrk, cluster->twrhit());

	    // NOTE:  The EMC timing is calibrated to have a peak at zero.
	    // this means that if one wants to use the time on an absolute scale, 
	    // one needs to the "photon flash" time.  We will take that 
	    // flight time as the time for a v=c particle along a straight line
	    // from the measured vertex.
	    //                          TKH 5-13-2002
	    float Zvtx = d_vtx->get_ZVertex();
	    float Xemc = cluster->x();
	    float Yemc = cluster->y();
	    float Zemc = cluster->z();
	    float dist = sqrt(Xemc*Xemc + Yemc*Yemc + (Zemc-Zvtx)*(Zemc-Zvtx));
	    particle->set_stemc(itrk, (particle->get_stemc(itrk)+dist/lightSpeed) );	

	    
	    PHAngle phiM;
	    phiM = PHAngle( atan2( cluster->y(), cluster->x() ));

	    PHAngle phiP( atan2( d_sproj->get_projectionEmc(dchid,1) , d_sproj->get_projectionEmc(dchid,0) ) );
	    rawdphi = phiM-phiP;
	    float zM = cluster->z();
	    float zP = d_sproj->get_projectionEmc(dchid,2);
	    rawdz      = zM-zP;
	    
	    int NUCL =3; // True according to utiMatch
	    int ELEC =1;
	    if( particle->get_dcarm(itrk)==1) { //West arm
	      particle->set_semcsdphi(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),NUCL));
	      particle->set_semcsdz  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,NUCL));
	      particle->set_semcsdphi_e(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),ELEC));
	      particle->set_semcsdz_e  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,ELEC));
	    }
	    else {
	      if( particle->get_dcarm(itrk)==0 && particle->get_sect(itrk)>1) {
		particle->set_semcsdphi(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_semcsdz  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,NUCL));
		particle->set_semcsdphi_e(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_semcsdz_e  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,ELEC));
	      }
	      else {
		particle->set_semcsdphi(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_semcsdz  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,NUCL));
		particle->set_semcsdphi_e(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_semcsdz_e  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,ELEC));
	      }	      
	    }
	    
	  }
	  
	  // Swapped EMC sdphi, sdz  OLD Format
	  if( isemc>=0 && d_sproj->get_projectionEmc(dchid,0) > -999. && d_emc==0 && d_emcOLD!=0) {

	    particle->set_secore  (itrk,d_emcOLD->get_ecore      (isemc));
	    particle->set_se9     (itrk,d_emcOLD->get_e9         (isemc));
	    particle->set_secent  (itrk,d_emcOLD->get_ecent      (isemc));
	    particle->set_stemc   (itrk,d_emcOLD->get_tof        (isemc));
	    particle->set_sprob   (itrk,d_emcOLD->get_prob_photon(isemc));
	    particle->set_semcchi2(itrk,d_emcOLD->get_chi2       (isemc));
	    particle->set_stwrhit (itrk,d_emcOLD->get_twrhit     (isemc));

	    // NOTE:  The EMC timing is calibrated to have a peak at zero.
	    // this means that if one wants to use the time on an absolute scale, 
	    // one needs to the "photon flash" time.  We will take that 
	    // flight time as the time for a v=c particle along a straight line
	    // from the measured vertex.
	    //                          TKH 5-13-2002
	    float Zvtx = d_vtx->get_ZVertex();
	    float Xemc = d_emcOLD->get_xyz(isemc,0);
	    float Yemc = d_emcOLD->get_xyz(isemc,1);
	    float Zemc = d_emcOLD->get_xyz(isemc,2);
	    float dist = sqrt(Xemc*Xemc + Yemc*Yemc + (Zemc-Zvtx)*(Zemc-Zvtx));
	    particle->set_stemc(itrk, (particle->get_stemc(itrk)+dist/lightSpeed) );
	    

	    PHAngle phiM( atan2( d_emcOLD->get_xyz(isemc,1), d_emcOLD->get_xyz(isemc,0)) );
	    PHAngle phiP( atan2( d_sproj->get_projectionEmc(dchid,1) , d_sproj->get_projectionEmc(dchid,0) ) );
	    rawdphi = phiM-phiP;
	    float zM = d_emcOLD->get_xyz(isemc,2);
	    float zP = d_sproj->get_projectionEmc(dchid,2);
	    rawdz      = zM-zP;
	    
	    int NUCL =3; // True according to utiMatch
	    int ELEC =1;
	    if( particle->get_dcarm(itrk)==1) { //West arm
	      particle->set_semcsdphi(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),NUCL));
	      particle->set_semcsdz  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,NUCL));
	      particle->set_semcsdphi_e(itrk, m->d_PBSCw_phi_match(momch,rawdphi.getPhi(),ELEC));
	      particle->set_semcsdz_e  (itrk, m->d_PBSCw_z_match(momch,beta,rawdz,ELEC));
	    }
	    else {
	      if( particle->get_dcarm(itrk)==0 && particle->get_sect(itrk)>1) {
		particle->set_semcsdphi(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_semcsdz  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,NUCL));
		particle->set_semcsdphi_e(itrk, m->d_PBSCe_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_semcsdz_e  (itrk, m->d_PBSCe_z_match(momch,beta,rawdz,ELEC));
	      }
	      else {
		particle->set_semcsdphi(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),NUCL));
		particle->set_semcsdz  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,NUCL));
		particle->set_semcsdphi_e(itrk, m->d_PBGL_phi_match(momch,rawdphi.getPhi(),ELEC));
		particle->set_semcsdz_e  (itrk, m->d_PBGL_z_match(momch,beta,rawdz,ELEC));
	      }	      
	    }

	  }

	  // Swapped TOF sdphi, sdz
	  if( istof>=0 && d_sproj->get_projectionTof(dchid,0) > -999.) {
	    // Fill setof and sttof
            particle->set_setof(itrk, d_tof->get_eloss(istof) );
            particle->set_sttof(itrk, d_tof->get_tof(istof) );

	    PHAngle phiM( atan2( d_tof->get_xtof(istof,1), d_tof->get_xtof(istof,0)) );
	    PHAngle phiP( atan2( d_sproj->get_projectionTof(dchid,1) , d_sproj->get_projectionTof(dchid,0) ) );
	    rawdphi = phiM-phiP;
	    float zM = d_tof->get_xtof(istof,2);
	    float zP = d_sproj->get_projectionTof(dchid,2);
	    rawdz      = zM-zP;
	    if(RUN3MATCH){
	      particle->set_stofsdphi(itrk, m->d_TOF_phi_match(momch,zed,rawdphi.getPhi()));
	      particle->set_stofsdz  (itrk, m->d_TOF_z_match(momch,zed,rawdz));
	    }else{
	      particle->set_stofsdphi(itrk, m->d_TOF_phi_match(momch,rawdphi.getPhi()));
	      particle->set_stofsdz  (itrk, m->d_TOF_z_match(momch,rawdz));
	    }
	  }
	  else {
	    particle->set_setof(itrk, -9999);
	    particle->set_sttof(itrk, -9999);
	    particle->set_stofsdphi(itrk, -9999);
	    particle->set_stofsdz  (itrk, -9999);
	  }
	  
	  
	  // Swapped TEC sdphi, sdalpha
	  if( istec>=0 && d_sproj->get_projectionTec(dchid,0) > -999.) {
	    PHAngle phiM( d_tecout->get_phi(istec) );
	    PHAngle phiP( atan2( d_sproj->get_projectionTec(dchid,1) , d_sproj->get_projectionTec(dchid,0) ) );
	    rawdphi =  phiM-phiP;
	    PHAngle aM( d_tecout->get_alpha(istec) );
	    PHAngle aP( particle->get_alpha(itrk)  );
	    rawdz      = aM.getPhi()-aP.getPhi()*0.534;
	    particle->set_stecid(itrk, istec);
	    particle->set_stecsdphi(itrk,   m->d_TEC_phi_match(momch, rawdphi.getPhi()));
	    particle->set_stecsdalpha(itrk, m->d_TEC_alpha_match(momch, rawdz));  // Factor 0.534 from X. Li
	  }
	  else {
	    particle->set_stecid(itrk, -1);
	    particle->set_stecsdphi  (itrk, -9999);
	    particle->set_stecsdalpha(itrk, -9999);
	  }

	  // Swapped Cherenkov Rings...different technique.
	  if(iscrk>-1) {
	    particle->set_sn0   (itrk, d_scrkring->get_npmt0(iscrk));
	    particle->set_snpe0 (itrk, d_scrkring->get_npe0 (iscrk));
	    particle->set_sn1   (itrk, d_scrkring->get_npmt1(iscrk));
	    particle->set_snpe1 (itrk, d_scrkring->get_npe1 (iscrk));
	    particle->set_sn2   (itrk, d_scrkring->get_npmt2(iscrk));
	    particle->set_snpe2 (itrk, d_scrkring->get_npe2 (iscrk));
	    particle->set_sn3   (itrk, d_scrkring->get_npmt3(iscrk));
	    particle->set_snpe3 (itrk, d_scrkring->get_npe3 (iscrk));
	    particle->set_schi2 (itrk, d_scrkring->get_chi2 (iscrk));
	    particle->set_sdisp (itrk, d_scrkring->get_disp (iscrk));
	    particle->set_stcrk (itrk, d_scrkring->get_tcrk (iscrk));
	  }
	  else {
	    particle->set_sn0   (itrk, -9999);
	    particle->set_snpe0 (itrk, -9999);
	    particle->set_sn1   (itrk, -9999);
	    particle->set_snpe1 (itrk, -9999);
	    particle->set_sn2   (itrk, -9999);
	    particle->set_snpe2 (itrk, -9999);
	    particle->set_sn3   (itrk, -9999);
	    particle->set_snpe3 (itrk, -9999);
	    particle->set_schi2 (itrk, -9999);
	    particle->set_sdisp (itrk, -9999);
	    particle->set_stcrk (itrk, -9999);
	  }

	  itrk++;  // Increment the track if particle OK...
	}
    }
  
  return 0;

}

int CentraltrackReco::FillEmcE(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // EMC
  if( !d_emc) {
    PHTypedNodeIterator<emcClusterContainer> iEMC(topNode);
    emcClusterContainerNode_t* EMC = iEMC.find("emcClusterContainer");
    if(EMC) d_emc = EMC->getData();
  }

  // EMC OLD FORMAT...
  if( !d_emcOLD) {
    PHTypedNodeIterator<EmcClusterLocalExt> iEMCOLD(topNode);
    EmcClusterLocalExtNode_t *EMCOLD = iEMCOLD.find("EmcClusterLocalExt");
    if(EMCOLD) d_emcOLD  = EMCOLD->getData ();
  }
  if( !d_emc && !d_emcOLD) cout << PHWHERE << "CentraltrackReco:: EMC (old or new) not in Node Tree" << endl;

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_cgl && particle && d_dctrk)
    {

      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  //  Set up all the pointers to other structures...
	  int iemc  = d_cgl->get_emcclusid (icgl);

	  // EMC--default with not found then fill best available
	  particle->set_emce(itrk, -9999);

	  // EMC
	  if( iemc>=0 && d_emc!=0) {
	    emcClusterContent* cluster=0;
	    cluster = d_emc->getCluster(iemc);
	    particle->set_emce  (itrk, cluster->e() );
	  }
	  
	  // EMC OLD FORMAT
	  if( iemc>=0 && d_emc==0 && d_emcOLD!=0) {
	    particle->set_emce  (itrk, d_emcOLD->get_e(iemc) );
	  }
	  
	  itrk++;  // Increment the track if particle OK...
	}
      
    }
  
  return 0;

}

int CentraltrackReco::FillAcc(PHCompositeNode *topNode)
{
  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  //Swapped global tracking
  if( !d_scgl) {
    PHTypedNodeIterator<CglTrack> iSCGL(topNode);
    CglTrackNode_t *SCGL = iSCGL.find("CglTrackBack");
    if(SCGL) d_scgl  = SCGL->getData ();
    if( !d_scgl) cout << PHWHERE << "CentraltrackReco:: Swapped cgl not in Node Tree" << endl;
  }

  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...
  if( d_cgl && d_scgl && particle)
    {    
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  //  Set up all the pointers to other structures...
	  int  iacc =   d_cgl->get_accrecid(icgl);
	  int isacc =  d_scgl->get_accrecid(icgl);

	  particle->set_aerindex (icgl, iacc);
	  if( version>13) particle->set_aersindex (icgl, isacc);
	}
    }
  
  return 0;
}

int CentraltrackReco::FillTOFPH(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  // TOF
  if( !d_tof) {
    PHTypedNodeIterator<TofOut> iTOF(topNode);
    TofOutNode_t *TOF = iTOF.find("TofOut");
    if(TOF) d_tof  = TOF->getData ();
    if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  }

  //  Ummm...fill this in soon please...
  if( d_cgl && particle && d_tof)
    {
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  //  Set up all the pointers to other structures...
	  int itof  = d_cgl->get_tofrecid (icgl);
	  
	  // ACC
	  if( itof>=0 && d_tof!=0) {
	    particle->set_tofph1  (itrk, d_tof->get_qvc(itof, 0));
	    particle->set_tofph2  (itrk, d_tof->get_qvc(itof, 1));
	  }
	  else {
	    particle->set_tofph1  (itrk, -9999);
	    particle->set_tofph2  (itrk, -9999);
	  }

	  itrk++;
	}
    }
  

  return 0;

}


int CentraltrackReco::FillTOFTDC(PHCompositeNode *topNode)
{
  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  // TOF
  if( !d_tof) {
    PHTypedNodeIterator<TofOut> iTOF(topNode);
    TofOutNode_t *TOF = iTOF.find("TofOut");
    if(TOF) d_tof  = TOF->getData ();
    if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  }

  if( d_cgl && particle && d_tof)
    {
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  //  Set up all the pointers to other structures...
	  int itof  = d_cgl->get_tofrecid (icgl);
	  
	  // ACC
	  if( itof>=0 && d_tof!=0) {
	    particle->set_toftdc1  (itrk, d_tof->get_tvc(itof, 0));
	    particle->set_toftdc2  (itrk, d_tof->get_tvc(itof, 1));
	  }
	  else {
	    particle->set_toftdc1  (itrk, -9999);
	    particle->set_toftdc2  (itrk, -9999);
	  }

	  itrk++;
	}
    }
  

  return 0;

}


int CentraltrackReco::FillTECEXTRA(PHCompositeNode *topNode)
{

  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: Particle not in Node Tree" << endl;
  }

  // global tracking
  if( !d_cgl) {
    PHTypedNodeIterator<CglTrack> iCGL(topNode);
    CglTrackNode_t *CGL = iCGL.find("CglTrack");
    if(CGL) d_cgl  = CGL->getData ();
    if( !d_cgl) cout << PHWHERE << "CentraltrackReco:: cgl not in Node Tree" << endl;
  }

  // TEC
  if( !d_tecout) {
    PHTypedNodeIterator<TecOut> teciter(topNode);
    TecOutNode_t *TecOutNode = teciter.find("TecOut");
    if(TecOutNode) d_tecout = TecOutNode->getData();
    //if( !d_tecout) cout << PHWHERE << "CentraltrackReco:: tec not in Node Tree" << endl;
  }

  if( d_cgl && particle && d_tecout)
    {
      
      int itrk=0;
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{
	  
	  //  Set up all the pointers to other structures...
	  int itec  = d_cgl->get_tectrackid (icgl);
	  
	  // Here we fill the tec dE/dX
	  if( itec>=0) {
	    particle->set_tectrklen   (itrk, d_tecout->getTrackLength(itec));
	    particle->set_tecnde      (itrk, d_tecout->getTrackNdEdXbins(itec));
	    particle->set_tecde       (itrk, d_tecout->getTrackdEdX(itec));
	    particle->set_tecde06     (itrk, d_tecout->getTrackdEdX06(itec));
	    particle->set_tecnhit100  (itrk, d_tecout->getTrackNhits100(itec));
	    particle->set_tecnhit200  (itrk, d_tecout->getTrackNhits200(itec));
	    particle->set_tecnhit50   (itrk, d_tecout->getTrackNhits50(itec));
	    particle->set_tecwtb      (itrk, d_tecout->getTrackWeightedTimeBin(itec));
	    particle->set_tecwtbsq    (itrk, d_tecout->getTrackWeightedTimeBinSq(itec));
	  }
	  else {
	    particle->set_tectrklen   (itrk, -9999.);
	    particle->set_tecnde      (itrk, -9999);
	    particle->set_tecde       (itrk, -9999.);
	    particle->set_tecde06     (itrk, -9999.);
	    particle->set_tecnhit100  (itrk, -9999);
	    particle->set_tecnhit200  (itrk, -9999);
	    particle->set_tecnhit50   (itrk, -9999);
	    particle->set_tecwtb      (itrk, -9999.);
	    particle->set_tecwtbsq    (itrk, -9999.);
	  }
	  
	  itrk++;
	}
    }
  
  
  return 0;
  
}

//________________________________________________________________________________
int CentraltrackReco::FillHbdTrack(PHCompositeNode *topNode)
{

  // error count
  static unsigned int error_count( 0 );
  enum { max_error = 10 };
  bool has_error( false );
  
  // Particle
  PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
  PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
  if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
  if( !particle ) 
  {
    has_error = true; 
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillHbdTrack PHparticle not in Node Tree" << endl;
  }
  
  // DC
  PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
  DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
  if( DCTRACK) d_dctrk  = DCTRACK->getData ();
  if( !d_dctrk ) 
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillHbdTrack dctrk not in Node Tree" << endl;
  }
  
  // HBD 
  PHTypedNodeIterator<HbdMiniCellList> iHBDMINI(topNode);
  HbdMiniCellListNode_t *HBDMINI = iHBDMINI.find("HbdMiniCellList");
  PHTypedNodeIterator<HbdCellList> iHBD(topNode);
  HbdCellListNode_t *HBD = iHBD.find("HbdCellList");

  if( HBDMINI) d_hbdmini  = HBDMINI->getData ();
  if( HBD && !HBDMINI)  d_hbd  = HBD->getData ();
  if( !d_hbd && !d_hbdmini) {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillHbdTrack hbd not in Node Tree" << endl;
  }


 
  // global tracking
  PHTypedNodeIterator<CglTrack> iCGL(topNode);
  CglTrackNode_t *CGL = iCGL.find("CglTrack");
  if( CGL ) d_cgl  = CGL->getData ();
  if( !d_cgl ) 
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillHbdTrack cgl not in Node Tree" << endl;
  }
  
  // increment error count
  if( has_error )
  {
    if( error_count < max_error )
    {
      error_count++;
      if( error_count == max_error )
      { cout << PHWHERE << "CentraltrackReco::FillHbdTrack - message disabled" << endl; }
    }
    
    return 0;
  }
  
  // Projections
  d_proj = findNode::getClass<PHTrackOut>(topNode,"PHTrackOut");
  if( !d_proj) 
    {
      cout << PHWHERE << "CentraltrackReco:: projections not in Node Tree" << endl;
      return ABORTEVENT;
    }
  
  
  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_cgl && particle && d_dctrk && (d_hbd || d_hbdmini))
  {
    int itrk=0;
    for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
    {
      //  Set up all the pointers to other structures...
      int dchid = d_cgl->get_dctracksid(icgl);
//      int ihbd = d_cgl->get_hbdblobid(icgl);
      
      // Particle projections

      particle->set_phbdx (itrk, d_proj->get_projectionHbd(dchid, 0)); 
      particle->set_phbdy (itrk, d_proj->get_projectionHbd(dchid, 1)); 
      particle->set_phbdz (itrk, d_proj->get_projectionHbd(dchid, 2)); 
      
/*
      if(ihbd>-1){
        
        particle->set_hbdid(itrk,d_hbd->get_blob(ihbd)->get_id());
        particle->set_hbdsector(itrk,d_hbd->get_blob(ihbd)->get_sector());
        particle->set_hbdsize(itrk,d_hbd->get_blob(ihbd)->get_size());
        particle->set_hbdcharge(itrk,d_hbd->get_blob(ihbd)->get_charge());
        particle->set_hbdx(itrk,d_hbd->get_blob(ihbd)->get_blobx());
        particle->set_hbdy(itrk,d_hbd->get_blob(ihbd)->get_bloby());
        particle->set_hbdz(itrk,d_hbd->get_blob(ihbd)->get_blobz());
        PHAngle phiM( atan2( d_hbd->get_blob(ihbd)->get_bloby(), d_hbd->get_blob(ihbd)->get_blobx()) );
        PHAngle phiP( atan2( particle->get_phbdy(itrk), particle->get_phbdx(itrk)) );
        PHAngle rawdphi = phiM-phiP;
        float zM = d_hbd->get_blob(ihbd)->get_blobz();
        float zP = particle->get_phbdz(itrk);
        float rawdz      = zM-zP;
        particle->set_hbddphi (itrk, rawdphi );
        particle->set_hbddz   (itrk, rawdz   );
      }
*/
      itrk++;
      
    }	  
  }
  return 0;
}

//__________________________________________________________
int CentraltrackReco::FillTofwTrack(PHCompositeNode *topNode)
{

  // error count
  static unsigned int error_count( 0 );
  enum { max_error = 10 };
  bool has_error( false );
  
  // Particle
  PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
  PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
  if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
  if( !particle )
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillTofwTrack - PHparticle not in Node Tree" << endl;
  }
  
  // BBC
  PHTypedNodeIterator<BbcOut> iBBC(topNode);
  BbcOutNode_t *BBC = iBBC.find("BbcOut");
  if( BBC) d_bbc = BBC->getData();
  if( !d_bbc ) 
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillTofwTrack - bbc not in Node Tree" << endl;
  }
  
  // DC
  PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
  DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
  if( DCTRACK) d_dctrk  = DCTRACK->getData ();
  if( !d_dctrk  ) 
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillTofwTrack - dctrk not in Node Tree" << endl;
  }
  
  // TOFW
  PHTypedNodeIterator<TofwHit> iTOFW(topNode);
  TofwHitNode_t *TOFW = iTOFW.find("TofwHit");
  if( TOFW) d_tofw  = TOFW->getData ();
  if( !d_tofw )
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillTofwTrack - tofw not in Node Tree" << endl;
  }
  // global tracking
  PHTypedNodeIterator<CglTrack> iCGL(topNode);
  CglTrackNode_t *CGL = iCGL.find("CglTrack");
  if( CGL ) d_cgl  = CGL->getData ();
  if( !d_cgl ) 
  {
    has_error = true;
    if( error_count < max_error )
    cout << PHWHERE << "CentraltrackReco::FillTofwTrack - cgl not in Node Tree" << endl;
  }
  
  
  // increment error count
  if( has_error )
  {
    if( error_count < max_error )
    {
      error_count++;
      if( error_count == max_error )
      { cout << PHWHERE << "CentraltrackReco::FillTofwTrack - message disabled" << endl; }
    }
    
    return 0;
  }  
  
  // Projections
  PHTypedNodeIterator<PHTrackOut> iPHTrackOut(topNode);
  PHTrackOutNode_t *PHTrackOutNode = iPHTrackOut.find("PHTrackOut");
  if( PHTrackOutNode) d_proj = PHTrackOutNode->getData();
  if( !d_proj) cout << PHWHERE << "CentraltrackReco:: projections not in Node Tree" << endl;
  
  // T0
  PHTypedNodeIterator<T0Out> iT0(topNode);
  T0OutNode_t *T0 = iT0.find("T0Out");
  if( T0) d_t0  = T0->getData ();
  
  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...

  if( d_cgl && particle && d_dctrk)
  {
    
    float TimeZero = 0.0;
    if( d_bbc && d_bbc->isValid()) 
    {
      TimeZero = d_bbc->get_TimeZero();
    } else if( d_t0 && d_t0->isValid()) {
      TimeZero = d_t0->get_T0();
    }
    else 
    {
      cout << PHWHERE << "No Time Zero available" << endl;
    }
    
    int itrk=0;
    for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
    {
      int dcarm = particle->get_dcarm(itrk);
      if(dcarm==1){
        int itofw  = d_cgl->get_tofwrecid (icgl);
        int dchid = d_cgl->get_dctracksid(icgl);
        
        //  Set up all the pointers to other structures...
        // Particle projections
        
        particle->set_ptofwx (itrk, d_proj->get_projectionTofw(dchid, 0)); 
        particle->set_ptofwy (itrk, d_proj->get_projectionTofw(dchid, 1)); 
        particle->set_ptofwz (itrk, d_proj->get_projectionTofw(dchid, 2)); 
        particle->set_pltofw (itrk, d_proj->get_tofwPathLength(dchid)); 	  
        
        // Indecies
        particle->set_tofwid (itrk, itofw); 
        
        
        // Absolute Delta
        PHAngle rawdphi(0);
        float   rawdz;
        
        if( itofw>=0 && particle->get_ptofwx(itrk) > -999.) 
        {
          PHAngle phiM( atan2( d_tofw->get_xyz(itofw,1), d_tofw->get_xyz(itofw,0)) );
          PHAngle phiP( atan2( particle->get_ptofwy(itrk), particle->get_ptofwx(itrk)) );
          rawdphi = phiM-phiP;
          float zM = d_tofw->get_xyz(itofw,2);
          float zP = particle->get_ptofwz(itrk);
          rawdz      = zM-zP;
          particle->set_tofwdphi (itrk, rawdphi );
          particle->set_tofwdz   (itrk, rawdz   );
        }
        else 
        {
          particle->set_tofwdphi (itrk, -9999);
          particle->set_tofwdz   (itrk, -9999);
        }
        // Tofw stuff
        //int itrk_tofw = particle->get_idtrk_tofw(itrk);
        
        if(itrk>=0)
        {
          if( itofw>=0 && d_tofw!=0) 
          {
            particle->set_striptofw  (itrk, d_tofw->get_stripid(itofw));
            //particle->set_tofwx      (itrk, d_tofw->get_xyz(itofw,0));
            //particle->set_tofwy      (itrk, d_tofw->get_xyz(itofw,1));
            //particle->set_tofwz      (itrk, d_tofw->get_xyz(itofw,2));
            particle->set_ttofw      (itrk, d_tofw->get_time(itofw) - TimeZero);
            particle->set_qtofw      (itrk, d_tofw->get_charge(itofw));
            particle->set_tofwadcup  (itrk, d_tofw->get_rawadc(itofw,1));
            particle->set_tofwadcdw  (itrk, d_tofw->get_rawadc(itofw,0));
            particle->set_tofwtdcup  (itrk, d_tofw->get_rawtdc(itofw,1));
            particle->set_tofwtdcdw  (itrk, d_tofw->get_rawtdc(itofw,0));
          }
          else 
          {
            particle->set_striptofw  (itrk, -9999);
            //particle->set_tofwx      (itrk, -9999);
            //particle->set_tofwy      (itrk, -9999);
            //particle->set_tofwz      (itrk, -9999);
            particle->set_ttofw      (itrk, -9999);
            particle->set_qtofw      (itrk, -9999);
            particle->set_tofwadcup  (itrk, -9999);
            particle->set_tofwadcdw  (itrk, -9999);
            particle->set_tofwtdcup  (itrk, -9999);
            particle->set_tofwtdcdw  (itrk, -9999);
          }
        }
      } 
      itrk++;
    }
  }
  
  return 0;
}

//________________________________________________________________________________
int CentraltrackReco::FillExtraSwap(PHCompositeNode *topNode)
{
  
  // This routine is used to fill those variables that are common to 
  // all track version types.  Along the way it sets the pointers for
  // a number of different input data blocks.  Since FillCommon will always be
  // called, we don't need to check if the pointers are already references, 
  // they are.
  
  // Particle
  if( !particle) {
    PHTypedNodeIterator<PHCentralTrack> particlenanoiter(topNode);
    PHParticleNode_t *PHParticleNanoNode = particlenanoiter.find("PHCentralTrack");
    if( PHParticleNanoNode) particle = PHParticleNanoNode->getData();
    if( !particle) cout << PHWHERE << "CentraltrackReco:: particle not in Node Tree" << endl;
  }
  
  // DC
  if( !d_dctrk) {
    PHTypedNodeIterator<DchTrack> iDCTRACK(topNode);
    DchTrackNode_t *DCTRACK = iDCTRACK.find("DchTrack");
    if(DCTRACK) d_dctrk  = DCTRACK->getData ();
    if( !d_dctrk) cout << PHWHERE << "CentraltrackReco:: dctrk not in Node Tree" << endl;
  }
  
  
  // PC2
  if( !d_pc2) {
    PHTypedNodeIterator<PadCluster> iPC2(topNode);
    PadClusterNode_t *PC2 = iPC2.find("Pc2Cluster");
    if(PC2) d_pc2  = PC2->getData ();
    if( !d_pc2) cout << PHWHERE << "CentraltrackReco:: pc2 not in Node Tree" << endl;
  }
  
  // PC3
  if( !d_pc3) {
    PHTypedNodeIterator<PadCluster> iPC3(topNode);
    PadClusterNode_t *PC3 = iPC3.find("Pc3Cluster");
    if(PC3) d_pc3  = PC3->getData ();
    if( !d_pc3) cout << PHWHERE << "CentraltrackReco:: pc3 not in Node Tree" << endl;
  }
  
  // TOF
  if( !d_tof) {
    PHTypedNodeIterator<TofOut> iTOF(topNode);
    TofOutNode_t *TOF = iTOF.find("TofOut");
    if(TOF) d_tof  = TOF->getData ();
    if( !d_tof) cout << PHWHERE << "CentraltrackReco:: tof not in Node Tree" << endl;
  }
  
  // TOFW
  PHTypedNodeIterator<TofwHit> iTOFW(topNode);
  TofwHitNode_t *TOFW = iTOFW.find("TofwHit");
  if( TOFW) d_tofw  = TOFW->getData ();
  
  // it is not usefull to put the warning here again, since it was already 
  // made in FillTofWTrack
  // if( !d_tofw) cout << PHWHERE << "CentraltrackReco:: tofw not in Node Tree" << endl;
  
  // EMC
  if( !d_emc) {
    PHTypedNodeIterator<emcClusterContainer> iEMC(topNode);
    emcClusterContainerNode_t* EMC = iEMC.find("emcClusterContainer");
    if(EMC) d_emc = EMC->getData();
  }
  
  //Swapped global tracking
  if( !d_scgl) {
    PHTypedNodeIterator<CglTrack> iSCGL(topNode);
    CglTrackNode_t *SCGL = iSCGL.find("CglTrackBack");
    if(SCGL) d_scgl  = SCGL->getData ();
    if( !d_scgl) cout << PHWHERE << "CentraltrackReco:: Swapped cgl not in Node Tree" << endl;
  }
  
  
  // Swapped projection
  if( !d_sproj) {
    PHTypedNodeIterator<PHTrackOut> iPHTrackOutBack(topNode);
    PHTrackOutNode_t *PHTrackOutBackNode= iPHTrackOutBack.find("PHTrackOutBack");
    if(PHTrackOutBackNode) d_sproj = PHTrackOutBackNode->getData();
    if( !d_sproj) cout << PHWHERE << "CentraltrackReco:: Swapped Projections not in Node Tree" << endl;
  }
  
  
  //-----------------------------------------------------------------
  //Get a hold of our particle in the nanoDST node, and then fill it
  //  Begin the fill...
  
  if( d_scgl && particle && d_dctrk)
  {
    
    int itrk=0;
    for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
    {
      //  Set up all the pointers to other structures...
      int dchid = d_cgl->get_dctracksid(icgl);
      
      int ispc2;
      int ispc3;
      int istof;
      int isemc;
      
      if( d_scgl) { 
        ispc2 = d_scgl->get_pc2clusid (icgl);
        ispc3 = d_scgl->get_pc3clusid (icgl);
        istof = d_scgl->get_tofrecid  (icgl);
        isemc = d_scgl->get_emcclusid (icgl);
      } else {
        ispc2 = -1;
        ispc3 = -1;
        istof = -1;
        isemc = -1;
      }
      
      //  First we calculate the absolute dphi and dz,
      
      PHAngle rawdphi(0);
      float   rawdz;
      
      // Calculate the dphi and dz for the flip and slides....
      // Swapped PC2 sdphi, sdz
      if( ispc2>=0 && d_sproj->get_projectionPc2(dchid,0) > -999.) {
        PHAngle phiM( atan2( d_pc2->get_xyz(ispc2,1), d_pc2->get_xyz(ispc2,0)) );
        PHAngle phiP( atan2( d_sproj->get_projectionPc2(dchid,1) , d_sproj->get_projectionPc2(dchid,0) ) );
        rawdphi = phiM-phiP;
        float zM = d_pc2->get_xyz(ispc2,2);
        float zP = d_sproj->get_projectionPc2(dchid,2); 
        rawdz      = zM-zP;
        
        particle->set_spc2dphi(itrk, rawdphi.getPhi());
        particle->set_spc2dz  (itrk, rawdz);
      } else {
        particle->set_spc2dphi(itrk, -9999);
        particle->set_spc2dz  (itrk, -9999);
      }
      
      // Swapped PC3 sdphi, sdz
      if( ispc3>=0 && d_sproj->get_projectionPc3(dchid,0) > -999.) {
        PHAngle phiM( atan2( d_pc3->get_xyz(ispc3,1), d_pc3->get_xyz(ispc3,0)) );
        PHAngle phiP( atan2( d_sproj->get_projectionPc3(dchid,1) , d_sproj->get_projectionPc3(dchid,0) ) );
        rawdphi = phiM-phiP;
        float zM = d_pc3->get_xyz(ispc3,2);
        float zP = d_sproj->get_projectionPc3(dchid,2);
        rawdz      = zM-zP;
        
        particle->set_spc3dphi(itrk, rawdphi.getPhi());
        particle->set_spc3dz  (itrk, rawdz);
      } else {
        particle->set_spc3dphi(itrk, -9999);
        particle->set_spc3dz  (itrk, -9999);
      }
      
      // Swapped EMC sdphi, sdz  New Format
      if( isemc>=0 && d_sproj->get_projectionEmc(dchid,0) > -999. && d_emc!=0) {
        
        emcClusterContent* cluster=0;
        cluster = d_emc->getCluster(isemc);
        
        // NOTE:  The EMC timing is calibrated to have a peak at zero.
        // this means that if one wants to use the time on an absolute scale, 
        // one needs to the "photon flash" time.  We will take that 
        // flight time as the time for a v=c particle along a straight line
        // from the measured vertex.
        //                          TKH 5-13-2002
        
        
        PHAngle phiM;
        phiM = PHAngle( atan2( cluster->y(), cluster->x() ));
        
        PHAngle phiP( atan2( d_sproj->get_projectionEmc(dchid,1) , d_sproj->get_projectionEmc(dchid,0) ) );
        rawdphi = phiM-phiP;
        float zM = cluster->z();
        float zP = d_sproj->get_projectionEmc(dchid,2);
        rawdz      = zM-zP;
        
        
        particle->set_semcdphi(itrk, rawdphi.getPhi());
        particle->set_semcdz  (itrk, rawdz);
        
      }
      
      
      
      // Swapped TOF sdphi, sdz
      if( istof>=0 && d_sproj->get_projectionTof(dchid,0) > -999.) 
      {
        
        // Fill setof and sttof
        particle->set_setof(itrk, d_tof->get_eloss(istof) );
        particle->set_sttof(itrk, d_tof->get_tof(istof) );
        
        PHAngle phiM( atan2( d_tof->get_xtof(istof,1), d_tof->get_xtof(istof,0)) );
        PHAngle phiP( atan2( d_sproj->get_projectionTof(dchid,1) , d_sproj->get_projectionTof(dchid,0) ) );
        rawdphi = phiM-phiP;
        float zM = d_tof->get_xtof(istof,2);
        float zP = d_sproj->get_projectionTof(dchid,2);
        rawdz      = zM-zP;
        particle->set_stofdphi(itrk, rawdphi.getPhi());
        particle->set_stofdz  (itrk, rawdz);
      } else {
        particle->set_stofdphi(itrk, -9999);
        particle->set_stofdz  (itrk, -9999);
      }
      
      
      // Swapped TOFW sdphi, sdz
      int dcarm = particle->get_dcarm(itrk);
      if(dcarm==1 && d_tofw )
      {
        
        // check that tofw is there
        int itofw  = d_cgl->get_tofwrecid (icgl);
        
        // Swapped TOFW sdphi, sdz
        if( itofw>=0 && d_sproj->get_projectionTofw(dchid,0) > -999.) 
        {
          
          PHAngle phiM( atan2( d_tofw->get_xyz(itofw,1), d_tofw->get_xyz(itofw,0)) );
          PHAngle phiP( atan2( d_sproj->get_projectionTofw(dchid,1) , d_sproj->get_projectionTofw(dchid,0) ) );
          rawdphi = phiM-phiP;
          float zM = d_tofw->get_xyz(itofw,2);
          float zP = d_sproj->get_projectionTofw(dchid,2);
          rawdz      = zM-zP;
          particle->set_stofwdphi(itrk, rawdphi.getPhi());
          particle->set_stofwdz  (itrk, rawdz);
        } else {
          particle->set_stofwdphi(itrk, -9999);
          particle->set_stofwdz  (itrk, -9999);
        }
        
        
      } 
      // Fill TOFW Swapped value only if dcarm==1, the west arm.  H.Pei July 6, 2007
      
      
      
      
      itrk++;  
      // Increment the track if particle OK...
    }
  }

  return 0;

}
