#include <RecoverSvxRawHits.h>
#include "setIntflag.h"

#include <phool.h>
#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>

#include <vararray/VariableArray.h>
#include <VariableArrayInt.h>

#include <SvxRawhitListv4.h>
#include <SvxRawhit.h>

#include <svxAddress.hh>


#include <useInt.h>

#include <cstdlib>
//#include <fstream>

using namespace std;
using namespace findNode;

//---------------------------------------------------------------------------------------

RecoverSvxRawHits::RecoverSvxRawHits(const std::string &name): 
SubsysReco(name),
m_test(false),
m_print(false)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/recoversvxhits.dump");
#endif

  return;
}

//---------------------------------------------------------------------------------------

int RecoverSvxRawHits::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
#ifdef useIntflag
  if(verbosity>0) cout << "RecoverSvxRawHits::InitRun using INT flag." << endl;
  VariableArrayInt *svxhit = getClass<VariableArrayInt>(topNode, "SvxRawHit_VarArray");
#else
  VariableArray    *svxhit = getClass<VariableArray>(   topNode, "SvxRawHit_VarArray");
#endif


  if(verbosity>0) cout << "RecoverSvxRawHits::InitRun creating SvxRawhitList node..." << endl;
  if (svxhit)
    {
      string snode = (m_test) ? "SvxRawhitListTest" : "SvxRawhitList";
      SvxRawhitList *svxraw = getClass<SvxRawhitList>(topNode, snode.c_str());
      if (!svxraw)
        {
          svxraw = new SvxRawhitListv4();
          PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(svxraw, snode.c_str(), "PHObject");
          dstNode->addNode(PHObjectIONode);
        }
      //svxhit->identify();
    }

  // schema version
  if(verbosity>0)
    {
      cout<<"RecoverSvxHits schema version"<<endl;
      if(svxhit        !=NULL) cout<<"  Rawhit             Schema value : "<<svxhit->Id()        <<endl;        
    }
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------

int RecoverSvxRawHits::process_event(PHCompositeNode *topNode)
{
  if(verbosity>0) cout << "=============== RecoverSvxRawHits::process_event() started. =======================" << endl;
  int status = recoverSvxRawhit(topNode);

  if(m_test){
    CompareSvxRawList(topNode);
  }

  return status;
}

int RecoverSvxRawHits::recoverSvxRawhit(PHCompositeNode *topNode)
{
#ifdef useIntflag
  VariableArrayInt *hitarray = getClass<VariableArrayInt>(topNode, "SvxRawHit_VarArray");
#else
  VariableArray    *hitarray = getClass<VariableArray>(   topNode, "SvxRawHit_VarArray");
#endif

  svxAddress* address = getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) 
    {
      if(verbosity>0) { cout << PHWHERE  <<  "Can't find svxAddress. " << endl; }
      return ABORTRUN;
    }



// first unpack clusters

//  int hitarrayID = 1000;
  if (hitarray)
    {
      //      hitarrayID = hitarray->Id();

      string snode = (m_test) ? "SvxRawhitListTest" : "SvxRawhitList";
      SvxRawhitList *rawlist  = getClass<SvxRawhitList>(topNode, snode.c_str());
      if (!rawlist)
        {
          cerr << PHWHERE << "ERROR: Cannot locate SvxRawhitList node." << endl;
          exit(1);
        }
      unsigned int size = hitarray->get_array_size();
      if(verbosity>0) std::cout << "RecoverSvxRawHits::process_event() starting with rawhit array size = " << size << std::endl;

#ifdef useIntflag
      const int       *array = hitarray->get_array();
#else
      const short int *array = hitarray->get_array();

#endif
      
      if(array == NULL)
        {
          //cerr << PHWHERE << "ERROR: Cannot locate SvxRawHit array." <<endl;
        }
      else {

        int nt_msb = array[size-2]&0xFFFF; 
        int nt_lsb = array[size-1]&0xFFFF; 
        int nt     = ( (nt_msb<<16) | (nt_lsb) );

        int ntotal = 0, idx=0;
        while (size-2 > 0)
          {
            int data0  = *array++; size--;
            int data1  = *array++; size--;

            int pixmod = data0&0x3F;
            int nhits  = ((((data0>>8)&0xFF)<<16) | (data1&0xFFFF));
            ntotal += nhits;

            for(int ihit=0; ihit<nhits; ihit++){
              int data2  = *array++; size--;

              int chip = (data2>>13)&0x7;
              int ch   = (data2    )&0x1FFF;

              int layer   = address->getPixelLayer(pixmod);
              int ladder  = address->getPixelLadder(pixmod);
              int sensor  = address->getPixelSensor(pixmod, chip);
              int iz      = address->getPixelSensorIZ0(chip, ch);
              int section = address->getPixelSensorSection(iz);


              SvxRawhit* tmphit = rawlist->addRawhit();
              //std::cout << "raw added..." << std::endl;
              tmphit->set_svxSection(0);
              tmphit->set_layer(layer);
              tmphit->set_ladder(ladder);
              tmphit->set_sensor(sensor);
              tmphit->set_sensorSection(section);
              tmphit->set_sensorReadout(0);
              tmphit->set_adc(1);
              tmphit->set_channel(ch);
              tmphit->set_pixelROC(chip);
              tmphit->set_pixelModule(pixmod);

              if(verbosity==10) {
                cout << "   Svx rawhit: " << idx 
                     << " | " << layer << " " << ladder << " " << sensor
                     << " | " << section<<" 0 "<<ch
                     << " | " << pixmod << " " <<chip
                     << endl;
              }
                      
              idx++;
            }



          } //while (size-2 >0 )
          if(nt != ntotal){
            cout<<" error : nTotal is different with recorded value: "<<nt<<" "<<ntotal<<endl;
          }
          if(verbosity>1) cout<<" ntotal : "<<nt<<" "<<ntotal<<endl;
      }//if(array == NULL)

      if(verbosity>0) cout << "RecoverSvxHits::recoverSvxRawhit() recovered " << endl;
    }//if(hitarray)
// svx standalone tracks now
  
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------

int RecoverSvxRawHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}


//---------------------------------------------------------------------------------------
bool RecoverSvxRawHits::CompareSvxRawList(PHCompositeNode *topNode)
{
  string snode = "SvxRawhitList";
  SvxRawhitList *orglist = getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  SvxRawhitList *newlist = getClass<SvxRawhitList>(topNode, "SvxRawhitListTest");
  if(orglist == NULL)
    {
      cerr << PHWHERE << " Error no org SvxRawhitList" << endl;
      return false;
    }
  if(newlist == NULL)
    {
      cerr << PHWHERE << " Error no new SvxRawhitListTest" << endl;
      return false;
    }

  int n_org = orglist->get_nRawhits();
  int n_new = newlist->get_nRawhits();

  bool result = true; //(n_org == n_new);
  if(!result)
    {
      cerr << PHWHERE << " Inconsistent Nrawhits : " << n_org << "!=" << n_new << endl;
    }
  else
    {

      for(int ihit = 0; ihit < n_org; ihit++)
        {
          SvxRawhit *raw_org = orglist->get_Rawhit(ihit);
          SvxRawhit *raw_new = newlist->get_Rawhit(ihit);

          int layer = raw_org->get_layer();
          if(layer>=2) continue;

          result &= CompareSvxRaw(raw_org, raw_new);
          if(!result)
            {
              cerr << PHWHERE << " Inconsistent RawhitID : " << ihit << endl;
              break;
            }
        }
    }

  cout << "Checked RecoverSvxRawhit::CompareSvxRawList Nrawhit : " << n_org << " " << flush;
  cout << (result ? "OK" : "FAILED") << endl;
  cout << endl;

  return result;
}

bool RecoverSvxRawHits::CompareSvxRaw(SvxRawhit *sorg, SvxRawhit* snew)
{
  if(sorg == NULL || snew == NULL)
    {
      cerr << PHWHERE << endl;
      return false;
    }

  bool result = true;

  if(sorg->get_layer() < 2) { // size check for layer 0,1
    result &= compareInt(sorg->get_hitID(),      snew->get_hitID(),      "hitID");
    result &= compareInt(sorg->get_layer()     , snew->get_layer()     , "layer");
    result &= compareInt(sorg->get_ladder()    , snew->get_ladder()    , "ladder");
    result &= compareInt(sorg->get_sensor()    , snew->get_sensor()    , "sensor");
    result &= compareInt(sorg->get_sensorSection(), 
                                                 snew->get_sensorSection(), "sensorSection");
    result &= compareInt(sorg->get_channel()   , snew->get_channel()   , "channel");
    result &= compareInt(sorg->get_pixelROC()  , snew->get_pixelROC()  , "pixelROC");
    result &= compareInt(sorg->get_pixelModule(),snew->get_pixelModule(),"pixelModule");
  }



  if(!result){
    cout<<"   failed info: "<<endl;
    cout<<"   -- org --"<<endl;
    cout<<"         "<<sorg->get_layer()<<" "<<sorg->get_ladder()<<" "<<sorg->get_sensor()<<" : ";
    cout<<sorg->get_channel()<<" "<<sorg->get_pixelROC()<<" "<<sorg->get_pixelModule();
    cout<<endl;
    cout<<"   -- new --"<<endl;
    cout<<"         "<<snew->get_layer()<<" "<<snew->get_ladder()<<" "<<snew->get_sensor()<<" : ";
    cout<<snew->get_channel()<<" "<<snew->get_pixelROC()<<" "<<snew->get_pixelModule();
    cout<<endl;
  }

  return result;
  
}

bool RecoverSvxRawHits::compareInt(int orgval, int newval, const char *err)
{
  bool result = (orgval == newval);
  if(!result)
      {
          cout << "Failed : " << err << " " << orgval << "!=" << newval << endl;
      }
  if(m_print)
      {
          cout << err << "  " << orgval << "==" << newval << "  " << (result ? "OK" : "FAIL") << endl;
      }
  return result;
}
