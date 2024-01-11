#include "dPadRawWrapper.h"
#include "PadRaw.h"
#include "dPadClusterWrapper.h"
#include "PadCluster.h"
#include "PHIODataNode.h"
#include "PHEmbedStat.h"
#include "PadMixer.hh"
#include <iostream>
#include "Fun4AllReturnCodes.h"

using namespace std;

PadMixer::PadMixer(){
  verbose = 0;
}
PadMixer::~PadMixer(){
}
int PadMixer::InitRun(PHCompositeNode* sngl,
		      PHCompositeNode* real,
		      PHCompositeNode* merged){
  if((!sngl)||(!real)||(!merged)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }

  node1  = sngl;
  node2  = real;
  node3  = merged;
  return EVENT_OK;
}
int PadMixer::merge(){
  if((!node1)||(!node2)||(!node3)){
    cout<< "one of the TopNode trees not exist"<<endl;
    return ABORTEVENT;
  }
  PHNodeIterator iter1(node1);
  PHNodeIterator iter2(node2);
  PHNodeIterator iter3(node3);  

  PHCompositeNode* embedNode = 
    static_cast<PHCompositeNode*>(iter1.findFirst("PHCompositeNode","EMBED"));
  if(!embedNode){
    embedNode = new PHCompositeNode("EMBED");
    node1->addNode(embedNode);
  }
  PHDataNode<PHEmbedStat> *embedStatNode = 
    (PHDataNode<PHEmbedStat>*)iter1.findFirst("PHDataNode","PHEmbedStat");
  if(!embedStatNode){
    PHEmbedStat *stat = new PHEmbedStat;
    embedStatNode = new PHDataNode<PHEmbedStat>(stat,"PHEmbedStat");
    embedNode->addNode(embedStatNode);
  }
  embedStat = embedStatNode->getData();

  //add few tables that is needed for reconstruction

  PHCompositeNode*  dstNode = 
    static_cast<PHCompositeNode*>(iter3.findFirst("PHCompositeNode", "DST"));

  //reset higher level tables
  PHIODataNode<PadCluster> *clusnode;
  PadCluster*clus;
  PHTypedNodeIterator<PadCluster> clus3(node3);
  clusnode    = clus3.find("Pc1Cluster");
  if(clusnode){
    clus        = clusnode->getData();
    clus->Reset();
  }
  clusnode    = clus3.find("Pc2Cluster");
  if(clusnode){
    clus        = clusnode->getData();
    clus->Reset();
  }
  clusnode    = clus3.find("Pc3Cluster");
  if(clusnode){
    clus        = clusnode->getData();
    clus->Reset();
  }

  PHIODataNode<PadRaw> *rawnode;
  PadRaw*raw;
  PHTypedNodeIterator<PadRaw> raw3(node3);
  rawnode    = raw3.find("Pc1Raw");
  if(rawnode){
    raw        = rawnode->getData();
    raw->Reset();
  }
  rawnode    = raw3.find("Pc2Raw");
  if(rawnode){
    raw        = rawnode->getData();
    raw->Reset();
  }
  rawnode    = raw3.find("Pc3Raw");
  if(rawnode){
    raw        = rawnode->getData();
    raw->Reset();
  }

  PHIODataNode<dPadClusterWrapper> *dclusnode;
  dPadClusterWrapper*dclus;
  PHTypedNodeIterator<dPadClusterWrapper> dclus3(node3);
  dclusnode    = dclus3.find("dPc1Cluster");
  if(dclusnode){
    dclus        = dclusnode->getData();
    dclus->SetRowCount(0);
  }else{
    dPadClusterWrapper* cluster = new dPadClusterWrapper("dPc1Cluster", 10000);
    PHIODataNode<PHTable>* dPc1ClusterNode = new PHIODataNode<PHTable>(cluster, "dPc1Cluster");
    dstNode->addNode(dPc1ClusterNode);
  }
  dclusnode    = dclus3.find("dPc2Cluster");
  if(dclusnode){
    dclus        = dclusnode->getData();
    dclus->SetRowCount(0);
  }else{
    dPadClusterWrapper* cluster = new dPadClusterWrapper("dPc2Cluster", 10000);
    PHIODataNode<PHTable>* dPc2ClusterNode = new PHIODataNode<PHTable>(cluster, "dPc2Cluster");
    dstNode->addNode(dPc2ClusterNode);
  }
  dclusnode    = dclus3.find("dPc3Cluster");
  if(dclusnode){
    dclus        = dclusnode->getData();
    dclus->SetRowCount(0);
  }else{
    dPadClusterWrapper* cluster = new dPadClusterWrapper("dPc3Cluster", 10000);
    PHIODataNode<PHTable>* dPc3ClusterNode = new PHIODataNode<PHTable>(cluster, "dPc3Cluster");
    dstNode->addNode(dPc3ClusterNode);
  }

  PHIODataNode<dPadRawWrapper> *drawnode;
  dPadRawWrapper*draw;
  PHTypedNodeIterator<dPadRawWrapper> draw3(node3);
  drawnode    = draw3.find("dPc1Raw");
  if(drawnode){
    draw        = drawnode->getData();
    draw->SetRowCount(0);
  }else{
    dPadRawWrapper* raw = new dPadRawWrapper("dPc1Raw", 10000);
    PHIODataNode<PHTable>* dPc1RawNode = new PHIODataNode<PHTable>(raw, "dPc1Raw");
    dstNode->addNode(dPc1RawNode);
  }
  drawnode    = draw3.find("dPc2Raw");
  if(drawnode){
    draw        = drawnode->getData();
    draw->SetRowCount(0);
  }else{
    dPadRawWrapper* raw = new dPadRawWrapper("dPc2Raw", 10000);
    PHIODataNode<PHTable>* dPc2RawNode = new PHIODataNode<PHTable>(raw, "dPc2Raw");
    dstNode->addNode(dPc2RawNode);
  }
  drawnode    = draw3.find("dPc3Raw");
  if(drawnode){
    draw        = drawnode->getData();
    draw->SetRowCount(0);
  }else{
    dPadRawWrapper* raw = new dPadRawWrapper("dPc3Raw", 10000);
    PHIODataNode<PHTable>* dPc3RawNode = new PHIODataNode<PHTable>(raw, "dPc3Raw");
    dstNode->addNode(dPc3RawNode);
  }

  mergePCRaw(1);
  mergePCRaw(2);
  mergePCRaw(3);
  return EVENT_OK;
}

// When does this function get called?
// It doesn't.
int PadMixer::mergePCCluster(int pc){
  if(!node1||!node2||!node3){
    cout<<"PadMixer:: missing top nodes"<<endl;
    return ABORTEVENT;
  }

  PadCluster *CLUSTER1;
  PadCluster *CLUSTER2;
  PadCluster *CLUSTER3;
  vector<int> null;
  vector<int>&      pcxclusterE    = null;
  vector<int>&      pcxclusterStat = null;
  PHIODataNode<PadCluster> *clusnode;
  PHTypedNodeIterator<PadCluster> clus1(node1);
  PHTypedNodeIterator<PadCluster> clus2(node2);
  PHTypedNodeIterator<PadCluster> clus3(node3);
  if(pc==1){
    clusnode        = clus1.find("Pc1Cluster");
    CLUSTER1        = clusnode->getData();
    clusnode        = clus2.find("Pc1Cluster");
    CLUSTER2        = clusnode->getData();
    clusnode        = clus3.find("Pc1Cluster");
    CLUSTER3        = clusnode->getData();
    pcxclusterE     = embedStat->get_pc1clusterEmbed();
    pcxclusterStat  = embedStat->get_pc1clusterEmbedStat();
  }else if(pc == 2){
    clusnode        = clus1.find("Pc2Cluster");
    CLUSTER1        = clusnode->getData();
    clusnode        = clus2.find("Pc2Cluster");
    CLUSTER2        = clusnode->getData();
    clusnode        = clus3.find("Pc2Cluster");
    CLUSTER3        = clusnode->getData();
    pcxclusterE     = embedStat->get_pc2clusterEmbed();
    pcxclusterStat  = embedStat->get_pc2clusterEmbedStat();
  }else if(pc ==3){
    clusnode        = clus1.find("Pc3Cluster");
    CLUSTER1        = clusnode->getData();
    clusnode        = clus2.find("Pc3Cluster");
    CLUSTER2        = clusnode->getData();
    clusnode        = clus3.find("Pc3Cluster");
    CLUSTER3        = clusnode->getData();
    pcxclusterE     = embedStat->get_pc3clusterEmbed();
    pcxclusterStat  = embedStat->get_pc3clusterEmbedStat();
  }else{
    cout<<"Wrong Pc number"<<endl;
    return 0;
  }
  int numOfCluster1 = CLUSTER1->get_PadNCluster();
  int numOfCluster2 = CLUSTER2->get_PadNCluster();
  int numOfCluster3 = CLUSTER3->get_PadNCluster();

  if(verbose>6) {
    cout << "PC" << pc << " before embedding: "
	 << numOfCluster1 <<" MC, "  
	 << numOfCluster2 <<" RD, "  
	 << numOfCluster3 <<" EMB clusters." << endl;  
  }

  if(verbose>6) for(int i=0;i<numOfCluster3;i++){
    if(CLUSTER3->get_id(i)!=i) 
      cout<<"cluster table not consistent "<<i<<" "<<CLUSTER3->get_id(i)<<endl;
  }

  CLUSTER3->set_TClonesArraySize(numOfCluster3 + numOfCluster1);//simply add to cluster table
  CLUSTER3->set_PadNCluster(numOfCluster3 + numOfCluster1);
  for(int i=0;i<numOfCluster1;i++){
    CLUSTER3->AddPadCluster(numOfCluster3);
    CLUSTER3->set_arm(numOfCluster3,      CLUSTER1->get_arm(i));
    CLUSTER3->set_cell(numOfCluster3,     CLUSTER1->get_cell(i));
    CLUSTER3->set_id(numOfCluster3,       CLUSTER1->get_id(i));
    CLUSTER3->set_sector(numOfCluster3,   CLUSTER1->get_sector(i));
    CLUSTER3->set_type(numOfCluster3,     CLUSTER1->get_type(i));
    CLUSTER3->set_wire(numOfCluster3,     CLUSTER1->get_wire(i));
    for (short j = 0;j<3;j++){
      CLUSTER3->set_dxyz(numOfCluster3,j, CLUSTER1->get_dxyz(i,j));
      CLUSTER3->set_xyz(numOfCluster3,j,  CLUSTER1->get_xyz(i,j));
    }
    //cluster3[numOfCluster3]    =  cluster1[i];
    //cluster3[numOfCluster3].id =  numOfCluster3;
    pcxclusterE[i]             =  numOfCluster3;
    pcxclusterStat[i]          =  0;
    numOfCluster3++;
  }

  if(verbose>6) {
    cout << "PC" << pc << " after embedding: "
	 << numOfCluster1 <<" MC, "  
	 << numOfCluster2 <<" RD, "  
	 << numOfCluster3 <<" EMB clusters." << endl;  
  }

  return EVENT_OK;
}

int PadMixer::mergePCRaw(int pc){
  if(!node1||!node2||!node3){
    cout<<"PadMixer:: missing top nodes"<<endl;
    return ABORTEVENT;
  }

  //  static int allpc1=0, overpc1=0, allpc2=0, overpc2=0, allpc3=0, overpc3=0;
  int allpc1=0, overpc1=0, allpc2=0, overpc2=0, allpc3=0, overpc3=0;
  PadRaw *RAW1;
  PadRaw *RAW2;
  PadRaw *RAW3;
  /*
    vector<int> null;
    vector<int>&      pcxrawE    = null;
    vector<int>&      pcxrawStat = null;
  */
  PHIODataNode<PadRaw> *rawnode;
  PHTypedNodeIterator<PadRaw> rawi1(node1);
  PHTypedNodeIterator<PadRaw> rawi2(node2);
  PHTypedNodeIterator<PadRaw> rawi3(node3);
  if(pc==1){
    rawnode     = rawi1.find("Pc1Raw");
    RAW1        = rawnode->getData();
    rawnode     = rawi2.find("Pc1Raw");
    RAW2        = rawnode->getData();
    rawnode     = rawi3.find("Pc1Raw");
    RAW3        = rawnode->getData();
  }else if( pc == 2){
    rawnode     = rawi1.find("Pc2Raw");
    RAW1        = rawnode->getData();
    rawnode     = rawi2.find("Pc2Raw");
    RAW2        = rawnode->getData();
    rawnode     = rawi3.find("Pc2Raw");
    RAW3        = rawnode->getData();
  }else if( pc == 3){
    rawnode     = rawi1.find("Pc3Raw");
    RAW1        = rawnode->getData();
    rawnode     = rawi2.find("Pc3Raw");
    RAW2        = rawnode->getData();
    rawnode     = rawi3.find("Pc3Raw");
    RAW3        = rawnode->getData();
  }else {
    cout<<"Wrong Pc number"<<endl;
    return 0;
  }

  int numOfRaw1 = RAW1->get_PadNRaw();
  int numOfRaw2 = RAW2->get_PadNRaw();
  if(verbose>60) for(int i=0;i<numOfRaw2;i++){
    if(RAW2->get_id(i)!=i) 
      cout<<"raw table not consistent "<<i<<" "<<RAW2->get_id(i)<<endl;
  }
  for(int i =0 ; i<numOfRaw2;i++) RAW2->set_id(i,i);
  //copy from RAW2 to RAW3
  RAW3->Reset();
  for(int i=0; i<numOfRaw2; i++) {
    RAW3->AddPadRaw(i);
    RAW3->set_id(i,     i);		      
    RAW3->set_arm(i,    RAW2->get_arm(i));    
    RAW3->set_side(i,   RAW2->get_side(i));   
    RAW3->set_sector(i, RAW2->get_sector(i)); 
    RAW3->set_padx(i,   RAW2->get_padx(i));   
    RAW3->set_padz(i,   RAW2->get_padz(i));   
    RAW3->set_padtype(i,RAW2->get_padtype(i));

    if (verbose > 100) {
      cout << "RAW2 " << i << " PadNRaw " << RAW2->get_PadNRaw() << flush;
      cout << " RAW3 " << i << " PadNRaw " << RAW3->get_PadNRaw() << endl;

      cout << "RAW2 "     << i << endl;
      cout << " arm "     << RAW2->get_arm(i) << flush;    
      cout << " side "    << RAW2->get_side(i) << flush;   
      cout << " sector "  << RAW2->get_sector(i) << flush; 
      cout << " padx "    << RAW2->get_padx(i) << flush;   
      cout << " padz "    << RAW2->get_padz(i) << flush;   
      cout << " padtype " << RAW2->get_padtype(i) << endl;

      cout << "RAW3 "     << i << endl;
      cout << " arm "     << RAW3->get_arm(i) << flush;    
      cout << " side "    << RAW3->get_side(i) << flush;   
      cout << " sector "  << RAW3->get_sector(i) << flush; 
      cout << " padx "    << RAW3->get_padx(i) << flush;   
      cout << " padz "    << RAW3->get_padz(i) << flush;   
      cout << " padtype " << RAW3->get_padtype(i) << endl;
    }
    
  }

  // AMA 11/10/2008:
  // I verified that the RAW2 variables have been correctly copied to
  // RAW3. However, RAW3->get_PadNRaw() returns 0 here because
  // RAW3->set_PadNRaw() has not been called!
  //  int numOfRaw3 = RAW3->get_PadNRaw();

  // Here I will fix it:
  RAW3->set_PadNRaw(RAW2->get_PadNRaw());
  int numOfRaw3 = RAW3->get_PadNRaw();

  if (verbose > 30) {
    cout << "PadMixer::MergePCRaw(" << pc << "): After RAW2->RAW3 copy:"
	 << " "  << RAW1->get_PadNRaw() << " RAW1 (MC)" 
	 << ", " << RAW2->get_PadNRaw() << " RAW2 (RD)"
	 << ", " << RAW3->get_PadNRaw() << " RAW3 (MC+RD)" << endl;    

  }

  // At this point, the raw pad hits from real data (RAW2) have been
  // copied to table RAW3. Now the single MC RAW1 pads will be merged
  // into RAW3 if RAW3 does not already contain them (overlap==0). If
  // the raw pad is in both tables, the overlap counter is
  // incremented. In this way duplicate entries are avoided.
  
  //now we merge two tables
  RAW3->set_TClonesArraySize(numOfRaw3 + numOfRaw1);//simply add two raw table
  int total = numOfRaw3;  // total serves as value of last index
  int overlap = 0;
  for(int i = 0; i < numOfRaw1; i++){
    overlap = 0;
    for(int j = 0; j < numOfRaw3; j++){
      // Check if MC and merged tables contain any of the same pads
      if(RAW1->get_arm(i)    == RAW3->get_arm(j) && 
	 RAW1->get_side(i)   == RAW3->get_side(j) &&
	 RAW1->get_sector(i) == RAW3->get_sector(j) &&
	 RAW1->get_padx(i)   == RAW3->get_padx(j) &&
	 RAW1->get_padz(i)   == RAW3->get_padz(j)) { // overlapped
	if(verbose>6) 
	  cout << "---OVERLAP IN PC" << pc <<"---" << endl;
	overlap = 1;
	break;
      }
    }

    if(!overlap){
      RAW3->AddPadRaw(total);
      RAW3->set_id(total,     total);
      RAW3->set_arm(total,    RAW1->get_arm(i));
      RAW3->set_side(total,   RAW1->get_side(i));
      RAW3->set_sector(total, RAW1->get_sector(i));
      RAW3->set_padx(total,   RAW1->get_padx(i));
      RAW3->set_padz(total,   RAW1->get_padz(i));
      RAW3->set_padtype(total,RAW1->get_padtype(i));
      //raw3[total]         = raw1[i];
      //raw3[total].id      = total;
      total++;
    }

    if(verbose>10) {
      if(pc==1) {
	allpc1++;
	if(overlap) overpc1++;
      }
      else if(pc==2) {
	allpc2++;
	if(overlap) overpc2++;
      }
      else if(pc==3) {
	allpc3++;
	if(overlap) overpc3++;
      }
    }

  } // end loop over MC pads
  RAW3->set_PadNRaw(total);
  //  RAW3->SetRowCount(total);

  if(verbose > 10){

    if (pc == 1)      cout <<"Raw hit pads in  PC1: "<<allpc1<<" total, "
			   <<overpc1<<" overlapped.   " << std::flush;
    else if (pc == 2) cout <<"Raw hit pads in  PC2: "<<allpc2<<" total, "
			   <<overpc2<<" overlapped.   " << std::flush;
    else if (pc == 3) cout <<"Raw hit pads in  PC3: "<<allpc3<<" total, "
			   <<overpc3<<" overlapped.   " << std::flush;

    cout << " "  << RAW1->get_PadNRaw() << " RAW1 (MC)" << std::flush;    
    cout << ", " << RAW2->get_PadNRaw() << " RAW2 (RD)" << std::flush;    
    cout << ", " << RAW3->get_PadNRaw() << " RAW3 (MC+RD)" << endl;    

  }

  // Make a copy to old dPadRaw table because PadRec needs it
  dPadRawWrapper *RAWW3=0;
  PHIODataNode<dPadRawWrapper> *rawwnode;
  PHTypedNodeIterator<dPadRawWrapper> raww(node3);
  if(pc==1){
    rawwnode     = raww.find("dPc1Raw");
    RAWW3        = rawwnode->getData();
  }else if( pc == 2){
    rawwnode     = raww.find("dPc2Raw");
    RAWW3        = rawwnode->getData();
  }else if( pc == 3){
    rawwnode     = raww.find("dPc3Raw");
    RAWW3        = rawwnode->getData();
  }
  RAWW3->SetRowCount(total);
  DPADRAW_ST* raw3 = RAWW3->TableData();
  for(int i=0;i<total;i++){
    raw3[i].id      = i;
    raw3[i].arm     = RAW3->get_arm(i);
    raw3[i].side    = RAW3->get_side(i);
    raw3[i].sector  = RAW3->get_sector(i);
    raw3[i].padx    = RAW3->get_padx(i);
    raw3[i].padz    = RAW3->get_padz(i);
    raw3[i].padtype = RAW3->get_padtype(i);
  }

  if(verbose > 10){
    cout << "Copied to dPadRawWrapper: " << RAWW3->RowCount() << " in RAWW3" << endl;    
  }

  return EVENT_OK;
}
