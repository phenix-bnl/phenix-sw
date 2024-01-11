#include <SvxStripRawHitList.h>

#include <SvxRawhitList.h>
#include <SvxRawhit.h>
#include <svxAddress.hh>

using namespace std;

ClassImp(SvxStripRawHitList)


//============================================================================//
void SvxStripRawHitList::Reset()
{
  // clear the map
  m_chiphits.clear();
}
//============================================================================//


//============================================================================//
void SvxStripRawHitList::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxStripRawHitList object:" << std::endl;
  map<short, std::vector<short> >::const_iterator iter;
  cout << "chipmap entries: " << m_chiphits.size() << endl;
  for (iter = m_chiphits.begin(); iter !=  m_chiphits.end(); ++iter)
    {
      cout << "chip id " << iter->first << ", entries " << iter->second.size() << endl;
    } 
}
//============================================================================//


//============================================================================//
void SvxStripRawHitList::add_hit(SvxRawhit *hit, svxAddress* address)
{
  // make sure the hit pointer is valid
  if (!hit)
    return;

  // get the parameters of the hit
  int lyr     = hit->get_layer();
  int ldr     = hit->get_ladder();
  int sen     = hit->get_sensor();
  int section = hit->get_sensorSection();
  int readout = hit->get_sensorReadout();
  int chn     = hit->get_channel();
  int adc     = hit->get_adc();
  
  int roc   = address->getStripRoc(section, readout, chn); // need svxAddress
  int rocch = address->getStripRocChannel(section, readout, chn); // need svxAddress
  short data = (((rocch&0x7F)<<8) | (adc&0xFF));

  // get the chip index
  short chipID = get_chipID(lyr, ldr, sen, roc);

  // make sure it's a valid chip ID (i.e. pixel hit) then insert it
  if (chipID >= 0)
    add_hit(chipID, data);
  else {
    cout<<" chipid: "<<chipID<<" "<<lyr<<" "<<ldr<<" "<<sen<<" "<<section<<" "<<readout<<" "<<chn<<" "<<adc<<endl;
  }

}
//============================================================================//


//============================================================================//
void SvxStripRawHitList::add_hit(short chipID, short channel)
{

  // check if chip already exists and add it
  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    it->second.push_back(channel);
  }
  // else create and insert it
  else
  {
    std::vector<short> v;
    v.push_back(channel);
    m_chiphits.insert( std::pair< short, std::vector<short> >(chipID, v) );
  }
}
//============================================================================//


//============================================================================//
void SvxStripRawHitList::add_chipHits(short chipID, std::vector<short> *hits)
{

  // check if the chip already exists and append
  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    it->second.insert( it->second.end(), (*hits).begin(), (*hits).end() );
  }
  // else insert it
  else
  {
    m_chiphits.insert( std::pair< short, std::vector<short> >(chipID, *hits) );
  }

}
//============================================================================//


//============================================================================//
std::vector<short> SvxStripRawHitList::get_hits(short chipID) const
{

  // return hits if chip exists
  std::map<short, std::vector<short> >::const_iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    return it->second;
  }
  else
  {
    // return empty vector
    std::vector<short> v;
    return v;
  }
}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::get_nhits(short chipID)
{
  // return hits if chip exists
  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    return it->second.size();
  }
  else
  {
    return 0;
  }

}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::get_nhits()
{
  // iterate over all chips
  int nhits = 0;
  std::map<short, std::vector<short> >::iterator it;
  for ( it = m_chiphits.begin(); it != m_chiphits.end(); ++it)
  {
    nhits += it->second.size();
  }
  return nhits;
}
//============================================================================//


//============================================================================//
void SvxStripRawHitList::reset_chip(short chipID)
{

  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
    {
     it->second.clear();
     m_chiphits.erase(it);
    }
}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::fill_rawhitlist(SvxRawhitList *hitlist,
                                        svxAddress *svx, const int adcoffset)
{

  // get around svxAddress problem by passing in a pointer to topNode,
  // and just require that svxAddress be retrieved from topNode?

  // make sure the input is a valid pointer
  if (!hitlist || !svx)
    return -1;

  svxAddress* address = svx;

  const int maxadc = 255-adcoffset;

  // iterate over all chips
  int nhits = 0;
  std::map<short, std::vector<short> >::iterator it;
  for ( it = m_chiphits.begin(); it != m_chiphits.end(); ++it)
  {

    short chipID = it->first;
    int lyr = get_layer(chipID);
    int ldr = get_ladder(chipID);
    int sen = get_sensor(chipID);
    int roc = get_stripROC(chipID);
    int sensec  = address->getStripSensorSection(roc);
    int readout = address->getStripSensorReadout(roc);
    int module  = address->getStripModuleID(lyr, ldr);

    // iterate over all hit channels for this chip
    for (unsigned int ic = 0; ic < it->second.size(); ic++)
    {
      short data  = it->second.at(ic);
      short rocch = ((data>>8)&0x7F);
      short adc   = ((data)   &0xFF);
      if(adc>maxadc) adc-=256;

      short chn   = address->getStripSensorChannel(roc, rocch);

      // make the rawhit
      SvxRawhit *tmphit = hitlist->addRawhit();
      tmphit->set_svxSection(0);
      tmphit->set_layer( lyr );
      tmphit->set_ladder( ldr );
      tmphit->set_sensor( sen );
      tmphit->set_sensorSection( sensec );
      tmphit->set_sensorReadout( readout);
      tmphit->set_adc(adc);
      tmphit->set_channel( chn );
      //--tmphit->set_pixelROC( 0 );
      tmphit->set_pixelModule( module );

      nhits++;
    }
  }

  return nhits;
}
//============================================================================//


//============================================================================//
short SvxStripRawHitList::get_chipID(int lyr, int ldr, int sen, int roc) const
{
  // double check input validity
  if (lyr < 2 || lyr > 3 || ldr > 23 || sen > 5 || roc > 11)
    return -1;

  int lyr_data = (lyr == 2) ? 0 : 1;
  int ldr_data = ldr&0x1F;
  int sen_data = sen&0x7;
  int roc_data = roc&0xF;

  short data = (lyr_data<<12) | (ldr_data<<7) | (sen_data<<4) | roc_data;

  return data;
}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::get_layer(short chipID) const
{
  int lyr = (chipID>>12)&0x1;

  return lyr+2;
}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::get_ladder(short chipID) const
{
  int ldr = (chipID>>7) & 0x1F;

  return ldr;
}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::get_sensor(short chipID) const
{
  // check the input validity
  int sen = (chipID>>4) & 0x7;

  return sen;
}
//============================================================================//


//============================================================================//
int SvxStripRawHitList::get_stripROC(short chipID) const
{
  // check the input validity
  int roc = (chipID&0xF);

  return roc;
}
//============================================================================//

void
SvxStripRawHitList::Print(const short chipID) const
{
  vector<short> hits = get_hits(chipID);
  cout << "chipID: " << chipID << " hist: " << hits.size() << endl;
  if (m_chiphits.find(chipID) != m_chiphits.end())
    {
      cout << "direct access: " << m_chiphits.find(chipID)->second.size() << endl;
    }
  return;
}
