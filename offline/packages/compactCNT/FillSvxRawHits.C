#include "FillSvxRawHits.h"

#include <setIntflag.h>

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>

#include <phool.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <Fun4AllReturnCodes.h>
#include <RunHeader.h>

#include <getClass.h>

#include <SvxRawhit.h>
#include <SvxRawhitList.h>

//#include <fstream>
//#include <set>
//#include <sstream>

#include <half/half.h>
#include <useInt.h>

#include <cmath>

using namespace std;

union floatint
{
  float    f32;
  int      i32;
};

FillSvxRawHits::FillSvxRawHits(const std::string &name): SubsysReco(name)
{
#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillsvxhits.dump");
#endif

  cout<<"------------------------------------------"<<endl<<endl;
  cout<<"FillSvxRawHits:: this only works for pixel"<<endl<<endl;
  cout<<"------------------------------------------"<<endl;

  return;
}

//-------------------------------------------------------------------------------------------

int FillSvxRawHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

# ifdef useIntflag
  VariableArrayInt *svxrawhit = new VariableArrayInt(1000);
#else
  VariableArray *svxrawhit = new VariableArray(1000);
#endif

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxrawhit, "SvxRawHit_VarArray", "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  
  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

int FillSvxRawHits::process_event(PHCompositeNode *topNode)
{
  // Created in InitRun

#ifdef useIntflag
  VariableArrayInt *svxarray = findNode::getClass<VariableArrayInt>(topNode, "SvxRawHit_VarArray");
  vector<int> savethis;
  vector<int> vChipHit;
#else
  VariableArray *svxarray = findNode::getClass<VariableArray>(topNode, "SvxRawHit_VarArray");
  vector<short int> savethis;
  vector<short int> vChipHit;
#endif

  if ( !svxarray ) return EVENT_OK;

  SvxRawhitList* d_svx = findNode::getClass<SvxRawhitList> (topNode, "SvxRawhitList");
  if(verbosity>0) {cout << "FillSvxRawHits::process_event(): " << endl;}

  int nrawhits = (d_svx!=NULL) ? d_svx->get_nRawhits() : 0;


  if(verbosity>0) {
    std::cout << "FillSvxRawHits::process_event(): Number of SVX rawhits = " << nrawhits << std::endl;
  }
  int nassoc=0;


  /////////////////////////////////////////
  // Process on SvxCluster

  if(d_svx){

#ifdef DUMP
    dumpfile << "SvxClusterList has nClusters = " <<  nclusters << endl;
#endif

    // vChipHit is used to keep hit word for one chip
    int prev_pixmod = -1, pixmod=-1;
    unsigned int npixelhit=0;
    //--int vectoradd=0;
    for(int ihit=0; ihit<nrawhits; ihit++)
      {
  
        SvxRawhit *sngl = d_svx->get_Rawhit(ihit);
  
        short int hitid = sngl->get_hitID();
  
        // Each individual cluster is loaded into the array in the format that SvxRawHitMapEntry 
        // will expect on readback. The index isave keeps track of the entry number
  
        nassoc++;

        int layer  = sngl->get_layer();
        if(layer>=2) continue;

        int ladder = sngl->get_ladder();
        int sensor = sngl->get_sensor();

        int sensec = sngl->get_sensorSection();
        int senro  = sngl->get_sensorReadout();
        int ch     = sngl->get_channel();
            pixmod = sngl->get_pixelModule();
        int pixroc = sngl->get_pixelROC();

        int word = ((pixroc&0x7)<<13 | (ch&0x1FFF) );

        if(pixmod != prev_pixmod ){
          fillChipRawhit(prev_pixmod, vChipHit, savethis);
          //--vectoradd+=vChipHit.size();
          //--cout<<"         size adding : "<<npixelhit<<" "<<vectoradd<<endl;

          prev_pixmod = pixmod;
          vChipHit.clear();
        }

        vChipHit.push_back(word&0xFFFF); // fill a hit
        npixelhit++;


        if(verbosity==10){
          cout<<"rawhit : "<<ihit<<" : "<<hitid;
          cout<<" | "<<layer<<" "<<ladder<<" "<<sensor;
          cout<<" | "<<sensec<<" "<<senro<<" "<<ch<<"("<<hex<<ch<<dec<<")";
          cout<<" | "<<pixmod<<" "<<pixroc;
          cout<<" | "<<hex<<"0x"<<word<<dec;
          cout<<endl;
        }
        
      }
      { // add hits in last module
        fillChipRawhit(pixmod, vChipHit, savethis);
        //--vectoradd+=vChipHit.size();
        //--cout<<"         size adding : "<<npixelhit<<" "<<vectoradd<<endl;
        if(npixelhit>0xFFFF){
          cout<<"rawhit too large : "<<npixelhit<<" something happen"<<endl;
        }
      }
  
    if(verbosity>0){
      cout<<"FillSvxRawhit : Npixel hit : "<<npixelhit<<endl; //--" "<<vectoradd<<endl;
    }

    int total_msb16 = (npixelhit >> 16) &0xFFFF;
    int total_lsb16 = (npixelhit      ) &0xFFFF;
    savethis.push_back(total_msb16);
    savethis.push_back(total_lsb16);
  
    svxarray->set_val(savethis);
  
  }


  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

int FillSvxRawHits::End(PHCompositeNode *topNode)
{
#ifdef DUMP
  dumpfile.close();
#endif

  return EVENT_OK;
}

#ifdef useIntflag
int FillSvxRawHits::FloatToInt(const float rval) const
{
  floatint fi;
  fi.f32 = rval;
  return fi.i32;
}
#else
short int FillSvxRawHits::FloatToInt(const float rval) const
{
  half ftoi(rval);
  return ftoi.bits();
}
#endif


#ifdef useIntflag
void FillSvxRawHits::fillChipRawhit(int module, vector<int>& vChipHit, vector<int>& savethis)
#else
void FillSvxRawHits::fillChipRawhit(int module, vector<short int>& vChipHit, vector<short int>& savethis)
#endif
{
  if(vChipHit.size()>0){
    unsigned int size = vChipHit.size();
    if(vChipHit.size()>0xFFFF){
      cerr<<"-------------------"<<endl;
      cerr<<"  FillSvxRawHits : size error : exceed max, size="<<size<<endl;
      cerr<<"-------------------"<<endl;
    }

    int size_lsb16 = (size)    &0xFFFF;
    int size_msb16 = (size>>16)&0xFFFF;

    savethis.push_back((size_msb16<<8) | (module&0x3F));
    savethis.push_back( size_lsb16 );

    savethis.insert(savethis.end(), vChipHit.begin(), vChipHit.end());

    //--cout<<"module : "<<module<<", size : "<<vChipHit.size()<<", "<<savethis.size()<<endl;

  }
}
