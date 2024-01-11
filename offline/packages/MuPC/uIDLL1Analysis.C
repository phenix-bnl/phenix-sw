//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"

#include "uIDLL1Analysis.h"
#include "uIDLL1Road.h"
#include "uIDLL1SnglRoad.h"

//INCLUDECHECKER: Removed this line: #include <TMutNode.h>
#include <TMuiHitMapO.h>

#include <iostream>

using namespace std;

void MuiMapInit_Trigger(hashVector <TMuiChannelId, int> &map);

typedef PHIODataNode <PHObject>   PHObjectNode_t;
typedef PHIODataNode <uIDLL1Road>     uIDLL1RoadNode_t;
//_____________________________________________
uIDLL1Analysis::uIDLL1Analysis()
{
  clear();

  FILE* fp;
  //.. symset_slope.txt copy from: 
  //lvl2_distribution/offline/RUN3_REAL/L2MuiIdxXoZ.adb
  if(!(fp = fopen("symset_slope.txt", "r"))) {
      cout<<" can not find symset_slope.txt, exit"<<endl;
      exit(9);
  }
  while (!feof(fp)) {
     int arm, orient, ihitid;
     float slope;

     fscanf(fp, "%d%d%d%f", &ihitid, &arm, &orient, &slope);

     idxXoZ[arm][orient][ihitid] = slope;
  }

  // Array to hold gap number for ith symset member.
  // ===============================================
  static int symsetGap[LogicalsPerSymset] =
  {                         0,0,0,0,0, 
                      1,1,1,1,1,1,1,1,1,1,1,
                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
          3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4};
  // Array to hold offset to obtain logical tube number of ith symset member
  // from the symset id.
  // =======================================================================
  static int symsetLogicalOffset[LogicalsPerSymset] =
  {			        -2,-1,0,1,2,
   		       -5,-4,-3,-2,-1,0,1,2,3,4,5,
   	      -8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,
   -11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,
   -14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14};

  bool tmp = false;
  //... Point triggerSignal to signal for the convenience of calculation...
  for (int  iarm = 0;  iarm <  NARMS;  iarm++) 
      for (int orient = 0; orient <  NOrent; orient++) 
	for (int symset = 0; symset <  TwoPacksPerGap; symset++) 
	  for (int i = 0; i < LogicalsPerSymset; i++) {
	    int logical = symset + symsetLogicalOffset[i];
	    if (logical >= 0 && logical < TwoPacksPerGap) {
	      int gap = symsetGap[i];
	      triggerSignal[iarm][orient][symset][i]=
		            &(signal[iarm][orient][logical][gap]);
	    } else {
		triggerSignal[iarm][orient][symset][i] = &tmp;
            }		    
	  }
}

//______________________________________________
int uIDLL1Analysis::Init(PHCompositeNode *topNode)
{
  return 0;
}

//______________________________________________
int uIDLL1Analysis::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

//______________________________________________
int uIDLL1Analysis::Reset(PHCompositeNode *topNode)
{
  clear();
  uidll1road->Reset();
  return 0;
}


//______________________________________________
int uIDLL1Analysis::Event(PHCompositeNode *topNode)
{
   GetNodes(topNode);

   TMuiHitMapO* _hit_map = TMutNode<TMuiHitMapO>::find_node(topNode,"TMuiHitMapO");
   calculate(_hit_map);
   fillRoad();

  cout<<" analyzing uIDLL1Road "<<endl;

  return 0;
}


//__________________________________________________________________
void uIDLL1Analysis::calculate(TMuiHitMapO* _hit_map)
{	
  static hashVector<TMuiChannelId, int> 
	  map(TMuiChannelId::kTwoPacksMaxTotal, &TwoPackHash);
  static bool firstCall = false;

  if(!firstCall) {
     MuiMapInit_Trigger(map);
     firstCall = true;
  }

  clear();

  TMuiHitMapO::const_iterator hit_iter = _hit_map->range();
  while(TMuiHitMapO::const_pointer hit_ptr = hit_iter.next()){
    int arm     = hit_ptr->get()->get_arm();
    int gap     = hit_ptr->get()->get_plane();
    int panel   = hit_ptr->get()->get_panel();
    int orient  = hit_ptr->get()->get_orientation();
    int twopack = hit_ptr->get()->get_twopack();
 
    int logical = map[TMuiChannelId(arm, gap, panel, (EOrient_t) orient, twopack)];
    signal[arm][orient][logical][gap]=true;
  }

  // Synset algorithm to find a road. From Vince C.
  for (int arm = 0; arm < NARMS; arm++) 
    for (int orient = 0; orient < NOrent; orient++) 
      for (int symset = 0; symset < TwoPacksPerGap; symset++) {
	     SymsetAlgorithm(triggerSignal[arm][orient][symset], 
		   idxXoZ[arm][orient][symset], arm, orient, symset);
      }
}

//___________________________________________
void uIDLL1Analysis::clear()
{
  nRoad_deep = 0;

  //... Point triggerSignal to signal for the convenience of calculation...
  for (int  iarm = 0;  iarm <  NARMS;  iarm++) 
      for (int orient = 0; orient <  NOrent; orient++) 
	for (int logical = 0; logical <  TwoPacksPerGap; logical++) 
	  for (int gap = 0; gap < GapPerArm; gap++) {
	     signal[iarm][orient][logical][gap] = false;
	  }

  for (int symset = 0; symset < TwoPacksPerGap; symset++) 
  {
      uRoad_deep[symset].cos = 0;
      uRoad_deep[symset].depth = 0;

      arm_deep[symset] = -1;
      orient_deep[symset] = -1;
      symsetID_deep[symset] = -1;
  }
}
//___________________________________________________________________
void uIDLL1Analysis::SymsetAlgorithm(bool **p, float slope, int arm, 
		             int orient, int symsetID)
{
  /* From Vince C. and John Lajor
   * Deep symset algorithm can be defined as follows:
   *
   *      1/a 2/a 3/a 4/a  
   *  0/b 1/b 2/b 3/b 4/b     (note: this is one symsets according to Vince)
   *      1/c 2/c 3/c 4/c
   *
   *  where the number is the gap and the letter is the logical index.
   *
   *  define 1/OR = 1/a || 1/b || 1/c, etc and similar for 2/OR, 3/OR and 4/OR.
   *
   *  The symset algorithm is:
   *  (0/b || 1/b) && (3/OR || 4/OR) && SUM(0/b, 1/OR, 2/OR, 3/OR, 4/OR) >= 3)
   *
   * Shallow symset algorithm can be defined as follows:
   * from: WWW/p/draft/lajoie/MuID_LL1_Status_Report_17Jun03.pdf
   *
   *  (0/b || 1/b) && SUM(0/b, 1/OR, 2/OR)>=2;
   *
   */

   int bit_gap0 = 0; 
   int bit_gap1 = 0; 
   int bit_gap2 = 0; 
   int bit_gap3 = 0; 
   int bit_gap4 = 0;

   if(*p[2]) //.. 0/b
      bit_gap0 = 1;
   if(*p[9] || *p[10] || *p[11]) //.. 1/OR
      bit_gap1 = 1;
   if(*p[23] || *p[24] || *p[25]) //.. 2/OR
      bit_gap2 = 1;
   if(*p[43] || *p[44] || *p[45]) //.. 3/OR
      bit_gap3 = 1;
   if(*p[69] || *p[70] || *p[71]) //.. 4/OR
      bit_gap4 = 1;

   //  Deep symset algorithm 
   //.. have not clean the neighbourhood symsets yet. 
   //Not clearly how to do. Need to consult vince.
   //but it should have a small effect on rejection factors
   if((*p[2] || *p[10])&&(bit_gap3==1 || bit_gap4==1) && 
     (bit_gap0+bit_gap1+bit_gap2+bit_gap3+bit_gap4)>=3) {
	  uRoad_deep[nRoad_deep].cos = slope;
	  if(bit_gap3==1)
	     uRoad_deep[nRoad_deep].depth = 3;
	  else if(bit_gap4==1) 
	     uRoad_deep[nRoad_deep].depth = 4;

	  uRoad_deep[nRoad_deep].arm = arm;
	  uRoad_deep[nRoad_deep].orient = orient;
	  uRoad_deep[nRoad_deep].symsetID = symsetID;

	  arm_deep[nRoad_deep] = arm;
	  orient_deep[nRoad_deep] = orient;
	  symsetID_deep[nRoad_deep] = symsetID;

	  nRoad_deep++;
   }
}

//_________________________________________
void uIDLL1Analysis::fillRoad()
{
   for(int i =0; i<nRoad_deep; i++) {
      uidll1road->AddDeepRoad(i);
      uidll1road->set_nRoad_deep(i+1); 
      uidll1road->get_deep_road(i)->set_arm(arm_deep[i]);
      uidll1road->get_deep_road(i)->set_orient(orient_deep[i]);
      uidll1road->get_deep_road(i)->set_symset(symsetID_deep[i]);
   }
}

//_____________________________________
int uIDLL1Analysis::GetNodes(PHCompositeNode *topNode)
{
  uidll1road = 0;

  PHTypedNodeIterator<uIDLL1Road> iROAD(topNode);
  uIDLL1RoadNode_t *ROAD = iROAD.find("uIDLL1Road");
  if(ROAD) uidll1road = ROAD->getData();
  if (!uidll1road) cout << PHWHERE << "uIDAnalysis:: uIDLL1Road data not in Node Tree" << endl;

  return 0;
}
