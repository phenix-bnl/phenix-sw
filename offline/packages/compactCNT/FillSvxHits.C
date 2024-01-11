#include <FillSvxHits.h>
#include "setIntflag.h"

#include <SvxClusterInfo.h>
#include <SvxCentralTrack.h>
#include <SvxCentralTrackList.h>
#include <SvxCluster.h>
#include <SvxClusterList.h>
#include <SvxSegment.h>
#include <SvxSegmentList.h>
#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>
#include <id_detector.h>
#include <CglTrack.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <RunHeader.h>
#include <getClass.h>

#include <fstream>
#include <set>
#include <sstream>
#include <string>

#include <half/half.h>
#include <useInt.h>

#include <cmath>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillSvxHits::FillSvxHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillsvxhits.dump");
#endif
  SaveOnlyAssociatedHits=false;
  SaveOnlySelectedHits=false;
  m_saveHitsToAllNode=false;

  return;
}

//-------------------------------------------------------------------------------------------

int FillSvxHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

# ifdef useIntflag
  VariableArrayInt *svxhit = new VariableArrayInt(10001);
  VariableArrayInt *svxtrack = new VariableArrayInt(1002);
  VariableArrayInt *svxcentraltrack = new VariableArrayInt(1003);   // 1003 from 2021.1.6 T. Hachiya
  VariableArrayInt *svxcentraltrackbg = new VariableArrayInt(1003);
#else
  VariableArray *svxhit = new VariableArray(10001);
  VariableArray *svxtrack = new VariableArray(1002);
  VariableArray *svxcentraltrack = new VariableArray(1003);
  VariableArray *svxcentraltrackbg = new VariableArray(1003);
#endif

  std::string sHitNode = (m_saveHitsToAllNode) ? "SvxHitAll_VarArray" : "SvxHit_VarArray";

  //PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxhit, "SvxHit_VarArray", "PHObject");
  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxhit, sHitNode.c_str(), "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  PHIODataNode<PHObject> *PHObjectIONodeTr = new PHIODataNode<PHObject>(svxtrack, "SvxTrack_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONodeTr);
  
  PHIODataNode<PHObject> *PHObjectIONodeCentTr = new PHIODataNode<PHObject>(svxcentraltrack, "SvxCentralTrack_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONodeCentTr);
  
  PHIODataNode<PHObject> *PHObjectIONodeCentTrBg = new PHIODataNode<PHObject>(svxcentraltrackbg, "SvxCentralTrackBG_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONodeCentTrBg);
  
  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

int FillSvxHits::process_event(PHCompositeNode *topNode)
{
  int isave=0;
  int isavetrk=0;
  int isavecenttrk=0;
  int isavecenttrkbg=0;

  // Created in InitRun
  std::string sHitNode = (m_saveHitsToAllNode) ? "SvxHitAll_VarArray" : "SvxHit_VarArray";

#ifdef useIntflag
  //VariableArrayInt *svxarray = findNode::getClass<VariableArrayInt>(topNode, "SvxHit_VarArray");
  VariableArrayInt *svxarray = findNode::getClass<VariableArrayInt>(topNode, sHitNode.c_str());
  vector<int> savethis;
  VariableArrayInt *svxtrackarray = findNode::getClass<VariableArrayInt>(topNode, "SvxTrack_VarArray");
  vector<int> savethistracks;
  VariableArrayInt *svxcenttrackarray = findNode::getClass<VariableArrayInt>(topNode, "SvxCentralTrack_VarArray");
  vector<int> savethiscenttracks;
  VariableArrayInt *svxcenttrackarraybg = findNode::getClass<VariableArrayInt>(topNode, "SvxCentralTrackBG_VarArray");
  vector<int> savethiscenttracksbg;
#else
  //VariableArray *svxarray = findNode::getClass<VariableArray>(topNode, "SvxHit_VarArray");
  VariableArray *svxarray = findNode::getClass<VariableArray>(topNode, sHitNode.c_str());
  vector<short int> savethis;
  VariableArray *svxtrackarray = findNode::getClass<VariableArray>(topNode, "SvxTrack_VarArray");
  vector<short int> savethistracks;
  VariableArray *svxcenttrackarray = findNode::getClass<VariableArray>(topNode, "SvxCentralTrack_VarArray");
  vector<short int> savethiscenttracks;
  VariableArray *svxcenttrackarraybg = findNode::getClass<VariableArray>(topNode, "SvxCentralTrackBG_VarArray");
  vector<short int> savethiscenttracksbg;
#endif

  if (!svxarray || !svxtrackarray || !svxcenttrackarray || !svxcenttrackarraybg) return EVENT_OK;

  SvxClusterList* d_svx = NULL;
  if(SaveOnlySelectedHits) {
    d_svx = findNode::getClass<SvxClusterList> (topNode, "SvxSelectedClusterList");
    if(verbosity>0) {cout << "FillSvxHits::process_event(): Saving only SELECTED clusters." << endl;}
  } else {
    d_svx = findNode::getClass<SvxClusterList> (topNode, "SvxClusterList");
    if(verbosity>0) {cout << "FillSvxHits::process_event(): Saving ALL clusters." << endl;}
  }
  int nclusters = (d_svx!=NULL) ? d_svx->get_nClusters() : 0;

  SvxSegmentList* d_svxtrk = findNode::getClass<SvxSegmentList> (topNode, "SvxSegmentList");
  int ntracks = (d_svxtrk!=NULL) ? d_svxtrk->get_nSegments() : 0;

  SvxCentralTrackList* d_svxcenttrk = findNode::getClass<SvxCentralTrackList> (topNode, "SvxCentralTrackList");
  int ncenttracks = (d_svxcenttrk!=NULL) ? d_svxcenttrk->get_nCentralTracks() : 0;

  SvxCentralTrackList* d_svxcenttrkbg = findNode::getClass<SvxCentralTrackList> (topNode, "SvxCentralTrackBackList");
  int ncenttracksbg = 0;
  if(d_svxcenttrkbg) { ncenttracksbg = d_svxcenttrkbg->get_nCentralTracks(); }

  if(verbosity>0) {
    std::cout << "FillSvxHits::process_event(): Number of SVX clusters = " << nclusters << std::endl;
    std::cout << "FillSvxHits::process_event(): Number of SVX standalone tracks = " << ntracks << std::endl;
    std::cout << "FillSvxHits::process_event(): Number of SvxCentral tracks = " << ncenttracks << std::endl;
    std::cout << "FillSvxHits::process_event(): Number of SvxCentral BG tracks = " << ncenttracksbg << std::endl;
  }
  int nassoc=0;


  /////////////////////////////////////////
  // Process on SvxCluster

  if(d_svx){

#ifdef DUMP
    dumpfile << "SvxClusterList has nClusters = " <<  nclusters << endl;
#endif

    for(int iclus=0; iclus<nclusters; iclus++)
      {
  
        SvxCluster *sngl = d_svx->get_Cluster(iclus);
  
        short int hitid = sngl->get_hitID();
  
        //      bool isassociated=false;
        //      // save only clusters associated with standalone tracks
        //      if(SaveOnlyAssociatedHits) {
        //        for(int itrk=0; itrk<ntracks; itrk++) {
        //          SvxSegment *sngl = d_svxtrk->get_segment(itrk);
        //          int clusterid0 = sngl->getClusterID(0);
        //          int clusterid1 = sngl->getClusterID(1);
        //          int clusterid2 = sngl->getClusterID(2);
        //          int clusterid3 = sngl->getClusterID(3);
        //          if(hitid==clusterid0 || hitid==clusterid1 || hitid==clusterid2 || hitid==clusterid3) {
        //            isassociated=true; break;
        //          }
        //        }
        //        if(!isassociated) continue;
        //      }
  
        // Each individual cluster is loaded into the array in the format that SvxHitMapEntry 
        // will expect on readback. The index isave keeps track of the entry number
  
        nassoc++;
  
        savethis.push_back( hitid );
  
#ifdef DUMP
        dumpfile << "SvxHitMapEntry # " << isave;
        dumpfile << "  Cluster " << iclus << " has hitid = " << hitid << endl;
#endif
  
        short int adcandsize = sngl->get_size();
        int layer = sngl->get_layer();
        if(layer>1 && layer<4) {
          short int adcx = sngl->get_adc(0); if(adcx>255) {adcx=255;}
          short int adcu = sngl->get_adc(1); if(adcu>255) {adcu=255;}
          adcandsize = adcx + 256*adcu;
        }  
  
        short int x = (short)lrint( check_range( sngl->get_xyz_global(0), -32, 32)*1000. ); //convert from cm to 10mkm
        short int y = (short)lrint( check_range( sngl->get_xyz_global(1), -32, 32)*1000. );
        short int z = (short)lrint( check_range( sngl->get_xyz_global(2), -32, 32)*1000. );
  
        savethis.push_back(adcandsize);
        savethis.push_back(sngl->get_Nhot());
        savethis.push_back(sngl->get_Ncold());
        savethis.push_back(x);
        savethis.push_back(y);
        savethis.push_back(z);
  
        if(verbosity==10){
          cout<<"cluster : "<<iclus<<" : ";
          cout<<sngl->get_xyz_global(0)<<" ";
          cout<<sngl->get_xyz_global(1)<<" ";
          cout<<sngl->get_xyz_global(2)<<" ";
          cout<<" : "<<x<<" "<<y<<" "<<z<<" : "<<sngl->get_layer()<<" "<<sngl->get_ladder()<<endl;
        }
        
        isave++;      
      }
  
    svxarray->set_val(savethis);
  
  }

  //if(verbosity>0) std::cout << "Number of SVX clusters associated with tracks = " << nassoc << std::endl;

  // SVX standalone tracks ------------------------------------------------------------------

  if(d_svxtrk!=NULL){
  
#ifdef DUMP
    dumpfile << "SvxSegmentList has nSegments = " <<  ntracks << endl;
#endif
  
    for(int itrk=0; itrk<ntracks; itrk++)
      {
        SvxSegment *sngl = d_svxtrk->get_segment(itrk);
  
        // Each individual svx standalone track is loaded into the array in the format that SvxTrackMapEntry
        // will expect on readback. The index isavetrk keeps track of the entry number.
  
#ifdef DUMP
        dumpfile << "SvxTrackMapEntry # " << isavetrk;
#endif
  
        short int charge = -1; if(sngl->IsPositive()) { charge = 1; }
        short int nhits  = sngl->getNhits(0)+sngl->getNhits(1)+sngl->getNhits(2)+sngl->getNhits(3);
        short int quality = lrint( check_range(sngl->getQuality(), -320., 320.) * 100.);
        short int dca2d   = lrint( check_range(sngl->getDCA2D(),   -3.2,  3.2) * 10000.); // convert to usnits of 1mkm
        short int dca3d   = lrint( check_range(sngl->getDCA(),     -3.2,  3.2) * 10000.); // convert to usnits of 1mkm
        short int x       = lrint( check_range(sngl->getClosestApproach(0), -3.2, 3.2) * 10000.); // convert to usnits of 1mkm
        short int y       = lrint( check_range(sngl->getClosestApproach(1), -3.2, 3.2) * 10000.); // convert to usnits of 1mkm
        short int z       = lrint( check_range(sngl->getClosestApproach(2), -32, 32)   * 1000.); // convert to usnits of 10mkm
        short int px      = lrint( check_range(sngl->get3MomentumAtPrimaryVertex(0), -32, 32) * 1000.); // convert to MeV
        short int py      = lrint( check_range(sngl->get3MomentumAtPrimaryVertex(1), -32, 32) * 1000.);
        short int pz      = lrint( check_range(sngl->get3MomentumAtPrimaryVertex(2), -32, 32) * 1000.);

        // Added Oct 2013
        short int LivePercent0 = (short) lrint( check_range(sngl->getLivePercentage(0), -320, 320) * 100.); // 0-100
        short int LivePercent1 = (short) lrint( check_range(sngl->getLivePercentage(1), -320, 320) * 100.); // 0-100
        short int LivePercent2 = (short) lrint( check_range(sngl->getLivePercentage(2), -320, 320) * 100.); // 0-100
        short int LivePercent3 = (short) lrint( check_range(sngl->getLivePercentage(3), -320, 320) * 100.); // 0-100
        short int SegQuality   = (short) lrint( check_range(sngl->getSegmentQuality(),  -3.2, 3.2) * 10000);
        short int SegScore     = (short) lrint( check_range(sngl->getSegmentScore(),    -320, 320) * 100.); // 0-100
        short int dedx1        = (short) lrint( check_range(sngl->get_dEdX1(),    -3200, 3200) * 10.); // 0-100
        short int dedx2        = (short) lrint( check_range(sngl->get_dEdX2(),    -3200, 3200) * 10.); // 0-100

        //float quality = sngl->getQuality() * 100.;
        //float dca2d = sngl->getDCA2D() * 10000.; // convert to usnits of 1mkm
        //float dca3d = sngl->getDCA()   * 10000.; // convert to usnits of 1mkm
        //float x = sngl->getClosestApproach(0) * 10000.; // convert to usnits of 1mkm
        //float y = sngl->getClosestApproach(1) * 10000.; // convert to usnits of 1mkm
        //float z = sngl->getClosestApproach(2) * 1000.; // convert to usnits of 10mkm
        //float px = sngl->get3MomentumAtPrimaryVertex(0) * 1000.; // convert to MeV
        //float py = sngl->get3MomentumAtPrimaryVertex(1) * 1000.;
        //float pz = sngl->get3MomentumAtPrimaryVertex(2) * 1000.;
  
        savethistracks.push_back(charge);
        savethistracks.push_back(nhits);
        savethistracks.push_back(quality);
        savethistracks.push_back(dca2d);
        savethistracks.push_back(dca3d);
        savethistracks.push_back(x);
        savethistracks.push_back(y);
        savethistracks.push_back(z);
        savethistracks.push_back(px);
        savethistracks.push_back(py);
        savethistracks.push_back(pz);
        savethistracks.push_back(LivePercent0);
        savethistracks.push_back(LivePercent1);
        savethistracks.push_back(LivePercent2);
        savethistracks.push_back(LivePercent3);
        savethistracks.push_back(SegQuality);
        savethistracks.push_back(SegScore);
        savethistracks.push_back(dedx1);
        savethistracks.push_back(dedx2);

        // Add 2011.Nov.24
        for(int ilay=0; ilay<4; ilay++) {
          for(int ihit=0; ihit<2; ihit++) {
            short int goodflac  = (short) lrint( check_range(sngl->getClusterGoodFraction(ilay, ihit), -3.2, 3.2) *10000.);
            savethistracks.push_back(goodflac);
          }
        }

        //savethistracks.push_back(FloatToInt(quality));
        //savethistracks.push_back(FloatToInt(dca2d));
        //savethistracks.push_back(FloatToInt(dca3d));
        //savethistracks.push_back(FloatToInt(x));
        //savethistracks.push_back(FloatToInt(y));
        //savethistracks.push_back(FloatToInt(z));
        //savethistracks.push_back(FloatToInt(px));
        //savethistracks.push_back(FloatToInt(py));
        //savethistracks.push_back(FloatToInt(pz));
  
        if(verbosity==11){
          cout<<"segment : "<<itrk<<" : ";
          cout<<sngl->getQuality()<<" ";
          cout<<sngl->getDCA2D()  <<" ";
          cout<<sngl->getDCA()    <<" ";
          cout<<sngl->getClosestApproach(0)<<" ";
          cout<<sngl->getClosestApproach(1)<<" ";
          cout<<sngl->getClosestApproach(2)<<" ";
          cout<<sngl->get3MomentumAtPrimaryVertex(0)<<" ";
          cout<<sngl->get3MomentumAtPrimaryVertex(1)<<" ";
          cout<<sngl->get3MomentumAtPrimaryVertex(2)<<" ";
          cout<<sngl->getLivePercentage(0)<<" ";
          cout<<sngl->getLivePercentage(1)<<" ";
          cout<<sngl->getLivePercentage(2)<<" ";
          cout<<sngl->getLivePercentage(3)<<" ";
          cout<<sngl->getSegmentQuality()    <<" ";
          cout<<sngl->getSegmentScore()      <<" ";
          cout<<" : ";
          cout<<quality<<" ";
          cout<<dca2d  <<" "; 
          cout<<dca3d  <<" ";
          cout<<x      <<" ";
          cout<<y      <<" ";
          cout<<z      <<" ";
          cout<<px     <<" ";
          cout<<py     <<" ";
          cout<<pz     <<" ";
          cout<<LivePercent0<<" ";
          cout<<LivePercent1<<" ";
          cout<<LivePercent2<<" ";
          cout<<LivePercent3<<" ";
          cout<<SegQuality <<" ";
          cout<<SegScore   <<" ";
          cout<<endl;
        }

        isavetrk++;
      }
  
    svxtrackarray->set_val(savethistracks);
  }


  // SvxCentral tracks ------------------------------------------------------------------------------

  if(d_svxcenttrk!=NULL)
    {
      for(int itrk=0; itrk<ncenttracks; itrk++)
        {
          SvxCentralTrack *sngl = d_svxcenttrk->getCentralTrack(itrk);
  
          short int DchIndex = sngl->getDchIndex();
          //char cHitPattern = sngl->getHitPattern();
          //short int sHitPattern = sngl->getHitPattern();
          //if(HitPattern<0) {HitPattern = 128 - HitPattern;}
          short int tmpHitPattern = sngl->getHitPattern();
          short int HitPattern = tmpHitPattern&0xff;
          short int Unique = sngl->getUnique();
          short int DCA2D  = (short) lrint( check_range(sngl->getDCA2D(), -3.2, 3.2) * 10000.);  // convert to 1mkm units
          short int DCAZ   = (short) lrint( check_range(sngl->getDCAZ(),  -3.2, 3.2) * 10000.);  // convert to 1mkm units
          short int ClosestApproachX = (short) lrint( check_range(sngl->getClosestApproach(0),-3.2, 3.2) * 10000.); // convert to 1mkm units
          short int ClosestApproachY = (short) lrint( check_range(sngl->getClosestApproach(1),-3.2, 3.2) * 10000.); // convert to 1mkm units
          short int ClosestApproachZ = (short) lrint( check_range(sngl->getClosestApproach(2),-32, 32)   * 1000.);  // convert to 10mkm units
          short int ChisquarePhi     = (short) lrint( check_range(sngl->getChiSquareDPHI(), -320, 320) * 100.); // AMA: keep?
          short int ChisquareZ       = (short) lrint( check_range(sngl->getChiSquareDZ(),   -320, 320) * 100.); // AMA: keep?
          short int Chisquare        = (short) lrint( check_range(sngl->getChiSquare(),     -320, 320) * 100.);
          short int Chisquare2       = (short) lrint( check_range(sngl->getChiSquare2(),    -320, 320) * 100.); // AMA: keep?
          short int NClusters = sngl->getNhits();

          // Added Oct 2013
          short int LivePercent0     = (short) lrint( check_range(sngl->getLivePercentage(0), -320, 320) * 100.);
          short int LivePercent1     = (short) lrint( check_range(sngl->getLivePercentage(1), -320, 320) * 100.);
          short int LivePercent2     = (short) lrint( check_range(sngl->getLivePercentage(2), -320, 320) * 100.);
          short int LivePercent3     = (short) lrint( check_range(sngl->getLivePercentage(3), -320, 320) * 100.);
          short int LinkQuality      = (short) lrint( check_range(sngl->getLinkQuality(),     -3.2, 3.2) * 10000.);
          short int LinkScore        = (short) lrint( check_range(sngl->getLinkScore(),       -320, 320) * 100.);

          // Added Dec 2013 (schema 1002)
          short int MomentumX = (short) lrint( check_range(sngl->get3MomentumAtPrimaryVertex(0),-32., 32.) * 1000.); // convert to 1Mev/c units
          short int MomentumY = (short) lrint( check_range(sngl->get3MomentumAtPrimaryVertex(1),-32., 32.) * 1000.); // convert to 1Mev/c units
          short int MomentumZ = (short) lrint( check_range(sngl->get3MomentumAtPrimaryVertex(2),-32., 32.) * 1000.); // convert to 1Mev/c units

          // Added Jan 2021 (schema 1003)
          short int DCA2Dprimary = (short) lrint( check_range(sngl->getDCA2Dprimary(), -3.2, 3.2) * 10000.);  // convert to 1mkm units
          short int DCAZprimary  = (short) lrint( check_range(sngl->getDCAZprimary(),  -3.2, 3.2) * 10000.);  // convert to 1mkm units
  
          savethiscenttracks.push_back(DchIndex);
          savethiscenttracks.push_back(HitPattern);
          savethiscenttracks.push_back(Unique);
          savethiscenttracks.push_back(DCA2D);
          savethiscenttracks.push_back(DCAZ);
          savethiscenttracks.push_back(ClosestApproachX);
          savethiscenttracks.push_back(ClosestApproachY);
          savethiscenttracks.push_back(ClosestApproachZ);
          savethiscenttracks.push_back(ChisquarePhi); // Keep?
          savethiscenttracks.push_back(ChisquareZ);   // Keep?
          savethiscenttracks.push_back(Chisquare);
          savethiscenttracks.push_back(Chisquare2);   // Keep?
          savethiscenttracks.push_back(NClusters);
          // added in Schema 1001
          savethiscenttracks.push_back(LivePercent0);
          savethiscenttracks.push_back(LivePercent1);
          savethiscenttracks.push_back(LivePercent2);
          savethiscenttracks.push_back(LivePercent3);
          savethiscenttracks.push_back(LinkQuality );
          savethiscenttracks.push_back(LinkScore   );
          // added in Schema 1002
          savethiscenttracks.push_back(MomentumX);
          savethiscenttracks.push_back(MomentumY);
          savethiscenttracks.push_back(MomentumZ);
          // added in Schema 1003
          savethiscenttracks.push_back(DCA2Dprimary);
          savethiscenttracks.push_back(DCAZprimary);

          //cout<<"fill : "<<DCA2Dprimary<<" "<<DCAZprimary<<endl;

          for(int i=0; i<NClusters; i++) {
            SvxClusterInfo* clust = sngl->getClusterInfo(i);
            short int ClusterID   = clust->getClusterId();
            short int DPhi        = lrint( check_range(clust->getdphi(), -3.2, 3.2) * 10000.); // convert to 1mkm units
            short int DZ          = lrint( check_range(clust->getdz(),   -3.2, 3.2) * 10000.);
            savethiscenttracks.push_back(ClusterID);
            savethiscenttracks.push_back(DPhi);
            savethiscenttracks.push_back(DZ);
          }


          ////////////////
  
          if(verbosity==12){
            cout<<"svxcnt : "<<itrk<<" : ";
            cout<<sngl->getDCA2D()<<" ";
            cout<<sngl->getDCAZ() <<" ";
            cout<<sngl->getClosestApproach(0)<<" ";
            cout<<sngl->getClosestApproach(1)<<" ";
            cout<<sngl->getClosestApproach(2)<<" ";
            cout<<sngl->getChiSquareDPHI()<<" ";
            cout<<sngl->getChiSquareDZ()  <<" ";
            cout<<sngl->getChiSquare()    <<" ";
            cout<<sngl->getChiSquare2()   ;
            cout<<endl<<"     ";
            cout<<DCA2D            <<" ";
            cout<<DCAZ             <<" ";
            cout<<ClosestApproachX <<" ";
            cout<<ClosestApproachY <<" ";
            cout<<ClosestApproachZ <<" ";
            cout<<ChisquarePhi     <<" ";
            cout<<ChisquareZ       <<" ";
            cout<<Chisquare        <<" ";
            cout<<Chisquare2       <<endl;
          }
  
          isavecenttrk++;
        }
  
      svxcenttrackarray->set_val(savethiscenttracks);
    }


  // SvxCentral background tracks ------------------------------------------------------------------------------

  if(d_svxcenttrkbg!=NULL)
    {
      for(int itrk=0; itrk<ncenttracksbg; itrk++)
        {
  
          SvxCentralTrack *sngl = d_svxcenttrkbg->getCentralTrack(itrk);
  
          short int DchIndex = sngl->getDchIndex();
          short int tmpHitPattern = sngl->getHitPattern();
          short int HitPattern = tmpHitPattern&0xff;
          short int Unique = sngl->getUnique();
          short int DCA2D  = (short) lrint( check_range(sngl->getDCA2D(), -3.2, 3.2) * 10000.);  // convert to 1mkm units
          short int DCAZ   = (short) lrint( check_range(sngl->getDCAZ(),  -3.2, 3.2) * 10000.);  // convert to 1mkm units
          short int ClosestApproachX = (short) lrint( check_range(sngl->getClosestApproach(0), -3.2, 3.2) * 10000.); // convert to 1mkm units
          short int ClosestApproachY = (short) lrint( check_range(sngl->getClosestApproach(1), -3.2, 3.2) * 10000.); // convert to 1mkm units
          short int ClosestApproachZ = (short) lrint( check_range(sngl->getClosestApproach(2), -32, 32)   * 1000.);  // convert to 10mkm units
          short int ChisquarePhi     = (short) lrint( check_range(sngl->getChiSquareDPHI(), -320, 320) * 100.);
          short int ChisquareZ       = (short) lrint( check_range(sngl->getChiSquareDZ(),   -320, 320) * 100.);
          short int Chisquare        = (short) lrint( check_range(sngl->getChiSquare(),     -320, 320) * 100.);
          short int Chisquare2       = (short) lrint( check_range(sngl->getChiSquare2(),    -320, 320) * 100.);
          short int NClusters = sngl->getNhits();

          // Added Dec 2013 (schema 1002)
          short int LivePercent0     = (short) lrint( check_range(sngl->getLivePercentage(0), -320, 320) * 100.);
          short int LivePercent1     = (short) lrint( check_range(sngl->getLivePercentage(1), -320, 320) * 100.);
          short int LivePercent2     = (short) lrint( check_range(sngl->getLivePercentage(2), -320, 320) * 100.);
          short int LivePercent3     = (short) lrint( check_range(sngl->getLivePercentage(3), -320, 320) * 100.);
          short int LinkQuality      = (short) lrint( check_range(sngl->getLinkQuality(),     -3.2, 3.2) * 10000.);
          short int LinkScore        = (short) lrint( check_range(sngl->getLinkScore(),       -320, 320) * 100.);
          short int MomentumX = (short) lrint( check_range(sngl->get3MomentumAtPrimaryVertex(0),-32., 32.) * 1000.); // convert to 1Mev/c units
          short int MomentumY = (short) lrint( check_range(sngl->get3MomentumAtPrimaryVertex(1),-32., 32.) * 1000.); // convert to 1Mev/c units
          short int MomentumZ = (short) lrint( check_range(sngl->get3MomentumAtPrimaryVertex(2),-32., 32.) * 1000.); // convert to 1Mev/c units
          
          // Added Jan 2021 (schema 1003)
          short int DCA2Dprimary = (short) lrint( check_range(sngl->getDCA2Dprimary(), -3.2, 3.2) * 10000.);  // convert to 1mkm units
          short int DCAZprimary  = (short) lrint( check_range(sngl->getDCAZprimary(),  -3.2, 3.2) * 10000.);  // convert to 1mkm units
  
  
          savethiscenttracksbg.push_back(DchIndex);
          savethiscenttracksbg.push_back(HitPattern);
          savethiscenttracksbg.push_back(Unique);
          savethiscenttracksbg.push_back(DCA2D);
          savethiscenttracksbg.push_back(DCAZ);
          savethiscenttracksbg.push_back(ClosestApproachX);
          savethiscenttracksbg.push_back(ClosestApproachY);
          savethiscenttracksbg.push_back(ClosestApproachZ);
          savethiscenttracksbg.push_back(ChisquarePhi);
          savethiscenttracksbg.push_back(ChisquareZ);
          savethiscenttracksbg.push_back(Chisquare);
          savethiscenttracksbg.push_back(Chisquare2);
          savethiscenttracksbg.push_back(NClusters);
          // added in Schema 1001
          savethiscenttracksbg.push_back(LivePercent0);
          savethiscenttracksbg.push_back(LivePercent1);
          savethiscenttracksbg.push_back(LivePercent2);
          savethiscenttracksbg.push_back(LivePercent3);
          savethiscenttracksbg.push_back(LinkQuality );
          savethiscenttracksbg.push_back(LinkScore   );
          // added in Schema 1002
          savethiscenttracksbg.push_back(MomentumX);
          savethiscenttracksbg.push_back(MomentumY);
          savethiscenttracksbg.push_back(MomentumZ);
          // added in Schema 1003
          savethiscenttracksbg.push_back(DCA2Dprimary);
          savethiscenttracksbg.push_back(DCAZprimary);

          //cout<<"fillbg : "<<DCA2Dprimary<<" "<<DCAZprimary<<endl;

          for(int i=0; i<NClusters; i++) {
            SvxClusterInfo* clust = sngl->getClusterInfo(i);
            short int ClusterID   = clust->getClusterId();
            short int DPhi        = lrint( check_range(clust->getdphi(), -3.2, 3.2) * 10000.); // convert to 1mkm units
            short int DZ          = lrint( check_range(clust->getdz(),   -3.2, 3.2) * 10000.);
            savethiscenttracksbg.push_back(ClusterID);
            savethiscenttracksbg.push_back(DPhi);
            savethiscenttracksbg.push_back(DZ);
          }
  
          if(verbosity==13){
            cout<<"svxcntbg : "<<itrk<<" : ";
            cout<<sngl->getDCA2D()<<" ";
            cout<<sngl->getDCAZ() <<" ";
            cout<<sngl->getClosestApproach(0)<<" ";
            cout<<sngl->getClosestApproach(1)<<" ";
            cout<<sngl->getClosestApproach(2)<<" ";
            cout<<sngl->getChiSquareDPHI()<<" ";
            cout<<sngl->getChiSquareDZ()  <<" ";
            cout<<sngl->getChiSquare()    <<" ";
            cout<<sngl->getChiSquare2()   ;
            cout<<endl<<"      "<<endl;
            cout<<DCA2D            <<" ";
            cout<<DCAZ             <<" ";
            cout<<ClosestApproachX <<" ";
            cout<<ClosestApproachY <<" ";
            cout<<ClosestApproachZ <<" ";
            cout<<ChisquarePhi     <<" ";
            cout<<ChisquareZ       <<" ";
            cout<<Chisquare        <<" ";
            cout<<Chisquare2       <<endl;
          }
  
          isavecenttrkbg++;
        }
  
      svxcenttrackarraybg->set_val(savethiscenttracksbg);
    }

  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

int FillSvxHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return EVENT_OK;
}

#ifdef useIntflag
int FillSvxHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int FillSvxHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif


