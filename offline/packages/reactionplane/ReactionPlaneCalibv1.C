
#include "ReactionPlaneCalibv1.h"
#include <PdbBankManager.hh>
#include <recoConsts.h>
#include <PdbApplication.hh>
#include <PdbParameter.hh>
#include <PdbCalBank.hh>
#include <RunToTime.hh>
#include "PHTimeStamp.h"
#include <RunNumberRanges.h>
#include <utiCentrality.h>
#include <PHGlobal.h>
#include <BbcOut.h>
#include <VtxOut.h>
#include <RunHeader.h>
#include <getClass.h>

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>

//using namespace RP;
using namespace std;
using namespace findNode;


//////////////////////////////
// RpCalibData
void ReactionPlaneCalibv1::RpCalibData::init(){
//  cout<<"init "<<m_name.c_str()<<endl;
  fill_n(&m_used[0], sizeof(m_used)/sizeof(bool), true);
  reset();
}
void ReactionPlaneCalibv1::RpCalibData::reset(){
//  cout<<"reset"<<endl;

  memset(m_SumXmean, 0, sizeof(m_SumXmean));
  memset(m_SumYmean, 0, sizeof(m_SumYmean));

  // fill_n from <algorithm> can initialize arrays with any type
  // but it is slower than memset (and you have to give it the address
  // of the first member
  fill_n(&m_SumXsigma[0][0][0][0],sizeof(m_SumXsigma)/sizeof(float), 1.);
  fill_n(&m_SumYsigma[0][0][0][0],sizeof(m_SumYsigma)/sizeof(float), 1.);
  fill_n(&m_SumUpdate[0][0][0][0],sizeof(m_SumUpdate)/sizeof(bool), false);

  memset(m_FlatCos, 0, sizeof(m_FlatCos));
  memset(m_FlatSin, 0, sizeof(m_FlatSin));
  fill_n(&m_FlatUpdate[0][0][0][0][0], sizeof(m_FlatUpdate)/sizeof(bool), false);
}

void ReactionPlaneCalibv1::RpCalibData::print()
{
   cout<<"  id:    "<<m_id   <<" ("<<m_name.c_str()<<")"<<endl;
   cout<<"  nkind: "<<m_nkind<<","; 
   cout<< " nhar: " <<m_nhar <<",";
   cout<< " ncent: "<<m_ncent<<",";
   cout<< " nz: "   <<m_nz   <<",";
   cout<< " nord: " <<m_nord <<endl;
   cout<<"  isEnable: " <<(m_enable ? "Enabled" : "Disabled") <<endl;
   cout<<"  used: ";
   for(uint ikind=0; ikind<m_nkind; ikind++){
     if(isUsed(ikind)) cout<<ikind<<", ";
   }
   cout<<endl<<endl;
}

void  ReactionPlaneCalibv1::
RpCalibData::setUsedAll(const bool flag)
{
  for(uint i=0; i<m_nkind; i++){
    setUsed(i, flag);
  }
}

void  ReactionPlaneCalibv1::
RpCalibData::setUsedByArray(const uint size, const int *array, const bool flag)
{
  for(uint i=0; i<size; i++){
    setUsed(array[i], flag);
  }
}


float ReactionPlaneCalibv1::
RpCalibData::getSumXYmeansigma(const uint idx, 
                               const uint ikind, 
                               const uint ihar, 
                               const uint icent, 
                               const uint iz)
{
  if(isValidRange(ikind, ihar, icent, iz)){
    if     (idx==0) return m_SumXmean [ikind][ihar][icent][iz];
    else if(idx==1) return m_SumXsigma[ikind][ihar][icent][iz];
    else if(idx==2) return m_SumYmean [ikind][ihar][icent][iz];
    else            return m_SumYsigma[ikind][ihar][icent][iz];
  }

  return (idx%2)==0 ? 0.0 : 1.0;
}

void ReactionPlaneCalibv1::
RpCalibData::setSumXYmeansigma(const uint idx, 
                               const uint ikind, 
                               const uint ihar, 
                               const uint icent, 
                               const uint iz,
                               const float val)
{
  if(isValidRange(ikind, ihar, icent, iz)){
    if     (idx==0) m_SumXmean [ikind][ihar][icent][iz] = val;
    else if(idx==1) m_SumXsigma[ikind][ihar][icent][iz] = val;
    else if(idx==2) m_SumYmean [ikind][ihar][icent][iz] = val;
    else            m_SumYsigma[ikind][ihar][icent][iz] = val;

    m_SumUpdate[ikind][ihar][icent][iz] = true;
  }
}

float ReactionPlaneCalibv1::
RpCalibData::getFlatCosSin(const uint idx, 
                           const uint ikind, 
                           const uint ihar, 
                           const uint icent, 
                           const uint iz,
                           const uint iord)
{
  if(isValidRange(ikind, ihar, icent, iz, iord)){
    if(idx==0) return m_FlatCos[ikind][ihar][icent][iz][iord];
    else       return m_FlatSin[ikind][ihar][icent][iz][iord];
  }

  return 0;
}

void ReactionPlaneCalibv1::
RpCalibData::setFlatCosSin(const uint idx, 
                           const uint ikind, 
                           const uint ihar, 
                           const uint icent, 
                           const uint iz,
                           const uint iord,
                           const float val)
{
  if(isValidRange(ikind, ihar, icent, iz, iord)){
    if(idx==0) m_FlatCos[ikind][ihar][icent][iz][iord] = val;
    else       m_FlatSin[ikind][ihar][icent][iz][iord] = val;

    m_FlatUpdate[ikind][ihar][icent][iz][iord] = true;
  }
}

bool ReactionPlaneCalibv1::
RpCalibData::isSumUpdated(const uint ikind,  
                          const uint ihar, 
                          const uint icent, 
                          const uint iz)
{
  if(isValidRange(ikind, ihar, icent, iz)){
    return m_SumUpdate[ikind][ihar][icent][iz];
  }
  return false;
}

bool ReactionPlaneCalibv1::
RpCalibData::isFlatUpdated(const uint ikind,  
                          const uint ihar, 
                          const uint icent, 
                          const uint iz,
                          const uint iord)
{
  if(isValidRange(ikind, ihar, icent, iz, iord)){
    return m_FlatUpdate[ikind][ihar][icent][iz][iord];
  }
  return false;
}

bool ReactionPlaneCalibv1::
RpCalibData::isValidRange(const uint ikind,  
                          const uint ihar, 
                          const uint icent, 
                          const uint iz)
{
  return (ikind<m_nkind && ihar<m_nhar && icent<m_ncent && iz<m_nz);
}

bool ReactionPlaneCalibv1::
RpCalibData::isValidRange(const uint ikind,  
                          const uint ihar, 
                          const uint icent, 
                          const uint iz,
                          const uint iord)
{
  return (isValidRange(ikind, ihar, icent, iz) && iord<m_nord);
}


///////////////////////////////////////////
//
ReactionPlaneCalibv1::ReactionPlaneCalibv1() :
  m_verbosity(0),
  m_databasename("calib.reactionplane"), 
  m_isCalibOK(false)
{

  // register RP detector
  {  
    m_rpCalData[0] = new RpCalibData(RP::ID_SVX, 65, RP::NHAR4, RP::NMUL3, RP::NZPS3, RP::NORD3, "SVX", false); 
    m_rpCalData[1] = new RpCalibData(RP::ID_SEG,  9, RP::NHAR4, RP::NMUL3, RP::NZPS3, RP::NORD3, "SEG", false);
    m_rpCalData[2] = new RpCalibData(RP::ID_MPC,  3, RP::NHAR4, RP::NMUL3, RP::NZPS2, RP::NORD3, "MPC", false);
    m_rpCalData[3] = new RpCalibData(RP::ID_BBC,  3, RP::NHAR4, RP::NMUL3, RP::NZPS2, RP::NORD3, "BBC", false);
    m_rpCalData[4] = new RpCalibData(RP::ID_SMD,  3,         1, RP::NMUL3, RP::NZPS2, RP::NORD3, "SMD", false);
    m_rpCalData[5] = new RpCalibData(RP::ID_CNT,  5, RP::NHAR4, RP::NMUL3, RP::NZPS2, RP::NORD3, "CNT", false);
    m_rpCalData[6] = new RpCalibData(RP::ID_FVT, 70, RP::NHAR4, RP::NMUL3, RP::NZPS2, RP::NORD3, "FVT", false);

    {
      //SVX setting
      int svxidx = 0;
      m_rpCalData[svxidx]->setUsedAll(false);
      const int nidx=25;
      int usedIdx[nidx] = {36,37,38,39,40,41,42,43,44,45,46,47,
                         52,
                         53,54,55,56,57,58,59,60,61,62,63,64};
      m_rpCalData[svxidx]->setUsedByArray(nidx, usedIdx, true);

      //FVTX setting
      int fvtxidx=6;
      m_rpCalData[fvtxidx]->setUsedAll(false);
      const int nidxf=6;
      int usedIdxFvt[nidxf] = {40,41,42,43,44,45};
      m_rpCalData[fvtxidx]->setUsedByArray(nidxf, usedIdxFvt, true);

    }

    //createIdIdxMap
    for(uint idx=0; idx<NDET; idx++){
      m_vIDtoIDX.insert(pair<int, int>(m_rpCalData[idx]->m_id, idx));
      m_vIDXtoID.push_back(m_rpCalData[idx]->m_id);
    }
  }

  Reset();

  
  // show
  //printIDtoIDXMap();
  //printIDXtoIDMap();
  //cout<<endl;
  //PrintDet(false);
}

ReactionPlaneCalibv1::~ReactionPlaneCalibv1() 
{
  for(uint idet=0; idet<NDET; idet++)
  { 
    delete m_rpCalData[idet];
  }
}

void ReactionPlaneCalibv1::Reset()
{
// reset all
  for(uint idet=0; idet<NDET; idet++){
    m_rpCalData[idet]->reset();
  }
}

void ReactionPlaneCalibv1::test()
{
  char name[20];
  calcCalibConfig(0, RP::ID_SMD, 0, 1, 2, 5, 0, name);

  cout<<" name : ";
  for(int i=0; i<8; i++){ cout<<hex<<(int)name[i]<<dec<<" ";}
  cout<<endl;

  int icorr, detid, ikind, ihar, icent, iz, ipar;
  recoverCalibConfig(name, icorr, detid, ikind, ihar, icent, iz, ipar);

/*
  Update(NULL);
*/

}


float ReactionPlaneCalibv1::Recentering(
  const int detid, 
  const uint ikind, const uint ihar, const uint icent, const uint iz, 
  const float qxy, const float qw, const int xy)
{
  if(qw<=0.) return -9999.;
  
  if (qxy<-9000) {
    if(m_verbosity>1) cout << PHWHERE << " qxy invalid : " << qxy << " " <<GetDetName(detid)<<" "<<ikind<<" "<<ihar<<endl;
    return qxy;
  }

  float mean=0.0, sigma=1.0;
  if(xy==0){
    mean  = GetSumXmean (detid, ikind, ihar, icent, iz);
    sigma = GetSumXsigma(detid, ikind, ihar, icent, iz);
  }
  else if(xy==1){
    mean  = GetSumYmean (detid, ikind, ihar, icent, iz);
    sigma = GetSumYsigma(detid, ikind, ihar, icent, iz);
  }

  if(isnan(sigma)){ // error
    if(m_verbosity>1){ 
      cerr << PHWHERE << " sigma > 0. (detid, ikind, ihar)=(" << detid << "," << ikind<<","<<ihar << ")" << endl; 
    }
    return -9999.0;
  }

  float qxyw = qxy/qw;
  
  return ( qxyw - mean ) / sigma;
}

float ReactionPlaneCalibv1::Flattening(
  const int detid, 
  const uint ikind, const uint ihar, const uint icent, const uint iz, 
  const float psi)
{
  float deltaPsi = 0.0;
  float psi0 = psi * (ihar + 1.0); // -pi < psi0 < pi
  
  for(uint iord=0; iord<GetDetNorder(detid); iord++) {
    float flatCos = GetFlatCos(detid, ikind, ihar, icent, iz, iord);
    float flatSin = GetFlatSin(detid, ikind, ihar, icent, iz, iord);

    float cosPsi = cos( (iord + 1.0) * psi0 );
    float sinPsi = sin( (iord + 1.0) * psi0 );
    deltaPsi += ( -flatSin * cosPsi + flatCos * sinPsi) * 2.0 / (iord + 1.0);
  }

  return atan2(sin(psi0 + deltaPsi), cos(psi0 + deltaPsi)) / (ihar + 1.0); // -pi/n < psi < pi/n
}

bool ReactionPlaneCalibv1::Fetch(const int runNumber)
{
  bool status=true;
  for(uint idet=0; idet<GetNDet(); idet++){
    int detid = getDetId(idet);
    status &= Fetch(detid, runNumber);
  }
 
  return status;
}


bool ReactionPlaneCalibv1::Fetch(const int detid, const int runNumber)
{
//  cout << "ReactionPlaneCalibv1::Fetch from DB for run " << runNumber << endl;
  
//-- // DLW version: check env var for filename template.  This template
//-- // is expected to have one %d field in it, and it will be replaced with
//-- // the current run number.
//-- if ( char* fnameTemplate = getenv("RPCALIB_FILENAME_TEMPLATE") )
//--   {
//--     const int LEN = 4096;
//--     char fname[LEN];
//--     int n = snprintf(fname,LEN,fnameTemplate,runNumber);
//--     if ( n >= LEN ) 
//--       {
//--         std::cout << "ReactionPlaneCalibv1::Fetch: WARNING: snprintf truncated output string" 
//--       	    << std::endl;
//--       }
//--     std::cout << "ReactionPlaneCalibv1::Fetch: overriding DB fetch with file " << fname << std::endl;
//--     // Attempt to use the provided file.  If successful, return.  If not,
//--     // proceed with DB fetching and see if that works.
//--     if ( Fetch(fname) == 0 ) return 0;
//--     else
//--       std::cout << "ReactionPlaneCalibv1::Fetch: Failed to fetch calib from file, "
//--                 << "trying DB instead... " << std::endl;
//--   }
  
  PHTimeStamp *tstart = RunToTime::instance()->getBeginTime(runNumber);
  if(tstart==NULL) {
    cerr<<PHWHERE<<" Failed to fetch parameter from DB : No time available for run "<<runNumber<<endl;
    return false;
  }

  Fetch(detid, *tstart);
  delete tstart;

  return true;
}


union uIdcodeConv{
  char carray[4];
  int  val;
};

void ReactionPlaneCalibv1::recoverCalibConfig(
  const char *name, 
  int& icorrect, int& detid, int& ikind, int& ihar, int& icent, int& iz, int& ipar)
{
  char theName[20];
  strncpy(theName, name, sizeof(theName));
  //uIdcodeConv uIdConv;
  //strncpy(uIdConv.carray, &name[1], 4);
  //RP::reverseIdCode(uIdConv.val, detid, ikind, ihar);

  static const int offset=1;
  for(int i=0; i<8; i++){
    theName[i]-=offset;
  }

  icorrect = (int)theName[0];
  ihar     = (int)theName[1];
  ikind    = (int)theName[2];
  detid    = (int)((theName[4]<<4) | theName[3]);
  icent    = (int)theName[5];
  iz       = (int)theName[6];
  ipar     = (int)theName[7];

//  cout<<PHWHERE<<sizeof(name)<<" : "<<icorrect<<" "<<detid<<" "
//               <<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<" "<<ipar<<endl;
}

void ReactionPlaneCalibv1::calcCalibConfig(
  const int icorrect, 
  const int detid, const int ikind, const int ihar,
  const int icent, const int iz, const int ipar, 
  char* name)
{
  //int idcode = RP::calcIdCode(detid, ikind, ihar);

  //uIdcodeConv uIdConv;
  //uIdConv.val = idcode;
  //strncpy(&name[1], uIdConv.carray, 4);

  
  name[0] = (char)( icorrect&0xFF);
  name[1] = (char)( ihar&0xFF);
  name[2] = (char)( ikind&0xFF);
  name[3] = (char)( detid&0xFF);
  name[4] = (char)((detid>>4)&0xFF);
  name[5] = (char)( icent   &0xFF);
  name[6] = (char)( iz      &0xFF);
  name[7] = (char)( ipar    &0xFF);
  name[8] = 0; // termination

  static const int offset=1;
  for(int i=0; i<8; i++){
    name[i]+=offset;
  }
  //--for(int i=0; i<8; i++){
  //--  cout<<hex<<(int)name[i]<<dec<<" ";
  //--}
  //--cout<<endl;
  //cout<<icorrect<<" "<<hex<<detid<<hex<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<" "<<ipar<<endl;
  //-- test
  //--int Corr, Det, Ki, Ha, Ce, Z, Pa;
  //--recoverCalibConfig(name, Corr, Det, Ki, Ha, Ce, Z, Pa);
}

bool ReactionPlaneCalibv1::Fetch(PdbCalBank* rpBank)
{
  //////////////////////////////////////
  // Nov. 14. 2013 Takashi Hachiya
  // PdbParameter has 2 internal variables;
  //    float thePar,
  //    char  theName[20]
  // here, some of the theName is used to record idcode, icent, iz bins 
  // for corresponding calibration parameters
  // We assign the theName as follow;
  //   theName[0]   = parameter id :0=recentering, 1=flattening
  //   theName[4-1] = idcode       :0xDDDDKKHH => [4,3]DDDD=detid, [2]KK=kind, [1]HH=harmonics
  //   theName[5]   = icent        :
  //   theName[6]   = iz           :
  //   theName[7]   = par-kind     : recent: 0:xmean, 1:xsigma, 2:ymean, 3:ysigma
  //                               : flat  : 2n:cos, 2n+1:sin, n=n-th order flattening parameter
  //   theName[8]   = 0            : termination

  if(rpBank==NULL){ return false; }

  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  two checks, scheme and no. entries...
  //
  //int bankid = rpBank->getBankID().getInternalValue() ;
  //int detid  = (bankid>>16)&0xFFFF;
  //int schema = (bankid    )&0xFFFF;
  
  int length = rpBank->getLength();
  //cout<<"DB length : "<<length<<" "<<hex<<bankid<<dec<<endl;
  
  int index = 0;
  PdbParameter* parameter = (PdbParameter*)(& rpBank->getEntry(index++));
  int scheme_val = (int)parameter->getParameter();
  if (scheme_val != RP::SCHEMA_V3 ) {
    cout << PHWHERE << " FATAL...wrong scheme DB read for R.P. : " << scheme_val <<endl;
    return false;
  }

  parameter = (PdbParameter *)(& rpBank->getEntry(index++));
  int entries = (int)parameter->getParameter();
  if (entries != length - 2) {
    cout << PHWHERE << " FATAL...wrong entries DB read for R.P" << endl;
    return false;
  }
  
  //----------------------------------------------------
  //  Checks passed...get the parameters...
  {
    //char name[20];
    while( index < length ){
      parameter = (PdbParameter*) & rpBank->getEntry(index++);
      float value    = parameter->getParameter();
      const char *na = parameter->getName();

      int icorrect, detid, ikind, ihar, icent, iz, ipar;
      recoverCalibConfig(na,
                         icorrect, detid, ikind, ihar, icent, iz, ipar);

      //-int iname0 = (na[3]<<24)|(na[2]<<16)|(na[1]<<8)|(na[0]);
      //-int iname1 = (na[7]<<24)|(na[6]<<16)|(na[5]<<8)|(na[4]);
      //-cout<<index<<" : "<<icorrect<<" "<<hex<<detid<<dec<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<" "<<ipar
      //-    <<" : "<<hex<<iname1<<" "<<iname0<<dec<<" "<<value<<endl;

      // set parameter
      if(icorrect==0){ // recentering
        if(     ipar==0) SetSumXmean( detid, ikind, ihar, icent, iz, value);
        else if(ipar==1) SetSumXsigma(detid, ikind, ihar, icent, iz, value);
        else if(ipar==2) SetSumYmean( detid, ikind, ihar, icent, iz, value);
        else if(ipar==3) SetSumYsigma(detid, ikind, ihar, icent, iz, value);
        else { cerr<<PHWHERE<<"unknown recenter id : " <<ipar<<" : "
                   <<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<endl;};
      }
      else if(icorrect==1){ // flattening
        int  iord  = (ipar/2);
        bool isCos = ((ipar%2)==0); //cos:2n, sin:2n+1
        if(isCos){ SetFlatCos(detid, ikind, ihar, icent, iz, iord, value); }
        else     { SetFlatSin(detid, ikind, ihar, icent, iz, iord, value); }
      }
      else {
         cerr<<PHWHERE<<"unknown parameter id : " <<icorrect<<" : "
             <<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<endl;
      }
    }
  }
  
//  delete rpBank;
  m_isCalibOK = true;

  return true;
}

bool ReactionPlaneCalibv1::Fetch(const int detid, const PHTimeStamp& tstart)
{
//  cout << "ReactionPlaneCalibv1::Fetch from DB." << endl;

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startRead())
    {
      cout << PHWHERE << " Error : Aborting ... Database not readable" << endl;
      application->abort();
      return false;
    }

  // Set run number from time stamp
  int runNumber = RunToTime::instance()->getRunNumber(tstart);
  
  // make bank ID
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID(runNumber, detid));
  
  // Grap a pointer to the bank ...
  PHTimeStamp Tstart = tstart;
  PdbCalBank* rpBank = bankManager->fetchBank("PdbParameterBank", bankID, m_databasename.c_str(), Tstart);
  
  if (!rpBank)
  {
    cout<<"Read failed : "<<GetDetName(detid) <<endl;
    return false;
  }

  cout << "  Reaction Plane : READ from Database: " << GetDetName(detid)<<" for run "<<runNumber<<endl;

  bool status = Fetch(rpBank);
  if(!status){
    cout<<"Read failed : "<<GetDetName(detid) <<endl;
  }

  if(rpBank!=NULL) delete rpBank;

  return status;
}

bool ReactionPlaneCalibv1::Fetch(const char* filename)
{
  // Need to set run number before calling this function
  ifstream file(filename);
  if (!file) {
    cout << PHWHERE << " Could not open input file " << filename << endl;
    return false;
  }
  
  if (m_verbosity > 0) {
    cout << "ReactionPlaneCalibv1:: R.P. calibration parameter from " << filename << endl;
  }

  // file format
  // 0 idcode icent iz sumXmean  sumXsigma  sumYmean  sumYsigma for recentering
  // 1 idcode icent iz iorder flatcos  flatsin for recentering
  
  // SumX/Y correction
  cout << " read SumX/Y correction for BBC/SMD/FCL/CNT" << endl;
  cout << " filename " << filename << endl;

  int nrecent=0, nflat=0;
  int   icorrect, idcode, icent, iz;
  while(file>>icorrect>> hex>>idcode>>dec >>icent>>iz)
    {
      int  detid, ikind, ihar;
      RP::reverseIdCode(idcode, detid, ikind, ihar);
      if(icorrect==0){ // recentering parameter
        float sumXmean, sumXsigma, sumYmean, sumYsigma;
        file>>sumXmean>>sumXsigma>>sumYmean>>sumYsigma;

        SetSumXmean (detid, ikind, ihar, icent, iz, sumXmean);
        SetSumXsigma(detid, ikind, ihar, icent, iz, sumXsigma);
        SetSumYmean (detid, ikind, ihar, icent, iz, sumYmean);
        SetSumYsigma(detid, ikind, ihar, icent, iz, sumYsigma);

        nrecent++;

  //      cout<<"recent : "<<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<" "<<sumXmean<<" "<<sumXsigma<<" "<<sumYmean<<" "<<sumYsigma<<endl;
      }
      else { // flattening parameter
        int iord;
        float flatCos, flatSin;
        file>>iord>>flatCos>>flatSin;

        SetFlatCos(detid, ikind, ihar, icent, iz, iord, flatCos);
        SetFlatSin(detid, ikind, ihar, icent, iz, iord, flatSin);

        nflat++;

  //      cout<<"flat : "<<detid<<" "<<ikind<<" "<<ihar<<" "<<iord<<" "<<icent<<" "<<iz<<" "<<flatCos<<" "<<flatSin<<endl;
      }
    }

  cout<<"   Fetch parameters: Nrecent="<<nrecent<<", Nflat="<<nflat<<endl;
  
  file.close();

  m_isCalibOK = true;

  return true;
}

void ReactionPlaneCalibv1::Update(const int beginrun, const int endrun)
{
  for(uint idet=0; idet<GetNDet(); idet++){
    int detid = getDetId(idet);
    Update(detid, beginrun, endrun);
  }
}

void ReactionPlaneCalibv1::Update(const int detid, const int beginrun, const int endrun)
{
  //RunNumber = beginrun;

  RunToTime* rt = RunToTime::instance();
  PHTimeStamp *Tstart = rt->getBeginTime(beginrun);
  PHTimeStamp *Tstop  = NULL;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0) {
    Tstop = rt->getEndTime(endrun);
  }
  else {
    Tstop = new PHTimeStamp();
    Tstop->setToFarFuture();
  }

  if(Tstart==NULL || Tstop==NULL){
    cerr<<PHWHERE<<" Time is null. runnum:begin-end = "<<beginrun<<"-"<<endrun<<endl;
    return;
  }
  
  Update(detid, *Tstart, *Tstop);

  delete Tstart;
  delete Tstop;
}

void ReactionPlaneCalibv1::Update(const int detid, const PHTimeStamp& tstart, const PHTimeStamp& tstop)
{
  //  Make the managers...
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if (!application->startUpdate()) {
    cout << PHWHERE << " Error : Aborting ... Database not writable" << endl;
    application->abort();
  }
  
  // Get run number from time stamp
  int runNumber = RunToTime::instance()->getRunNumber(tstart);
  
  //  Make a bank ID...
  PdbBankID bankID;
  bankID.setInternalValue(GetBankID(runNumber, detid));
  ostringstream descrip;
  descrip << "R.P. parameters for RUN " << runNumber;

  PHTimeStamp Tstart = tstart;
  PHTimeStamp Tstop  = tstop;

  //  Grap a pointer to the bank...
  PdbCalBank* rpBank = bankManager->createBank("PdbParameterBank", 
                                               bankID, 
                                               descrip.str().c_str(), 
                                               Tstart, 
                                               Tstop, 
                                               m_databasename.c_str());

  cout << "  Reaction Plane : WRITE to Database: " << GetDetName(detid)<<endl;

  if( Update(rpBank) ) {
    application->commit(rpBank);
  }
  else {
    cerr<<PHWHERE<<" Failed to write to DB : no Bank : "<<GetDetName(detid)<<endl;
    application->abort();
  }

  if(rpBank!=NULL) delete rpBank;
  
}

bool ReactionPlaneCalibv1::Update(PdbCalBank* rpBank)
{
  if(rpBank==NULL) return false;

  int bankid = rpBank->getBankID().getInternalValue();
  int detid  = (bankid>>16)&0xFFFF;
  int schema = bankid&0xFFFF;

  // check if detis exist
  int detidx = getDetIdx(detid);
  if(detidx==-1) {
    cout<<PHWHERE<<" unknown detid : "<<detid<<endl;
    return false;
  }
  
  // check if schema exist
  if(schema!=RP::SCHEMA_V3){
    cout<<PHWHERE<<" unknown schema value : "<<schema<<endl;
    return false;
  }

  int nUpdateRecent=0, nUpdateFlat=0;
  { // count Nupdated parameters
    for(uint ikind=0; ikind<GetDetNkind(detid); ikind++){
      for(uint ihar=0; ihar<GetDetNhar(detid); ihar++){
        for(uint icent=0; icent<GetDetNcent(detid); icent++){
          for(uint iz=0; iz<GetDetNz(detid); iz++){
            if(IsSumUpdated(detid, ikind, ihar, icent, iz)){ nUpdateRecent++; }

            for(uint iord=0; iord<GetDetNorder(detid); iord++) {
              if(IsFlatUpdated(detid, ikind, ihar, icent, iz, iord)){ nUpdateFlat++; }
            } // iord loop
          } // iz loop
        } // icent loop
      } // ihar loop
    } // ikind loop
  }
  
  const int length = 4*nUpdateRecent + 2*nUpdateFlat; // 4: xmean,xsigma,ymean,ysigma, 2:cos,sin
  if(length==0) {
    cout<<PHWHERE<<" length is 0; "<<length<<" : "<<nUpdateRecent<<" "<<nUpdateFlat<<endl;
    return false;
  }

  rpBank->setLength(length+2);
  
  //cout << "Length of DB = " << length <<" : "<<nUpdateRecent<<" "<<nUpdateFlat<<endl;
  
  int index = 0;
  PdbParameter *parameter;

  parameter = (PdbParameter *) & rpBank->getEntry(index++);
  parameter->setParameter(schema);
  parameter->setName("scheme");
  
  parameter = (PdbParameter *) & rpBank->getEntry(index++);
  parameter->setParameter(length);
  parameter->setName("entries");

  // recent parameter
  for(uint ikind=0; ikind<GetDetNkind(detid); ikind++){
    for(uint ihar=0; ihar<GetDetNhar(detid); ihar++){
      for(uint icent=0; icent<GetDetNcent(detid); icent++){
        for(uint iz=0; iz<GetDetNz(detid); iz++){
          if(IsSumUpdated(detid, ikind, ihar, icent, iz)){
            for(int ipar=0; ipar<4; ipar++){
              float val;
              if(     ipar==0) val = GetSumXmean( detid, ikind, ihar, icent, iz);
              else if(ipar==1) val = GetSumXsigma(detid, ikind, ihar, icent, iz);
              else if(ipar==2) val = GetSumYmean( detid, ikind, ihar, icent, iz);
              else             val = GetSumYsigma(detid, ikind, ihar, icent, iz);

              char name[20]="";
              calcCalibConfig(0, detid, ikind, ihar, icent, iz, ipar, name);

              //int iname0 = (name[3]<<24)|(name[2]<<16)|(name[1]<<8)|(name[0]);
              //int iname1 = (name[7]<<24)|(name[6]<<16)|(name[5]<<8)|(name[4]);
              //if(ipar==0)
              //cout<<index<<" : "<<0<<" "<<hex<<detid<<dec<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<" "<<ipar<<" "<<val
              //    <<" : "<<hex<<iname1<<" "<<iname0<<"  "<<iname3<<" "<<iname2<<dec<<" : "<<sizeof(name)<<endl;

              parameter = (PdbParameter*) & rpBank->getEntry(index++);
              parameter->setParameter(val);
              parameter->setName(name);
            }
          } // if
        } // iz loop
      } // icent loop
    } // ihar loop
  } // ikind loop

  for(uint ikind=0; ikind<GetDetNkind(detid); ikind++){
    for(uint ihar=0; ihar<GetDetNhar(detid); ihar++){
      for(uint icent=0; icent<GetDetNcent(detid); icent++){
        for(uint iz=0; iz<GetDetNz(detid); iz++){
          for(uint iord=0; iord<GetDetNorder(detid); iord++) {
            if(IsFlatUpdated(detid, ikind, ihar, icent, iz, iord)){
              for(int ipar=0; ipar<2; ipar++){
                float val = (ipar==0) ? GetFlatCos(detid, ikind, ihar, icent, iz, iord) 
                                      : GetFlatSin(detid, ikind, ihar, icent, iz, iord);

                char name[20]="";
                calcCalibConfig(1, detid, ikind, ihar, icent, iz, 2*iord+ipar, name);
                parameter = (PdbParameter*) & rpBank->getEntry(index++);
                parameter->setParameter(val);
                parameter->setName(name);
              }
            } // if
          } // iord loop
        } // iz loop
      } // icent loop
    } // ihar loop
  } // ikind loop

  return true;
}


void ReactionPlaneCalibv1::Write(const char* filename)
{
  cout << "Write parameter to " << filename << endl;
  
  ofstream fout(filename);

  for(uint idet=0; idet<NDET; idet++){
    uint detid = getDetId(idet);

    for(uint ikind=0; ikind<GetDetNkind(detid); ikind++){
      for(uint ihar=0; ihar<GetDetNhar(detid); ihar++){
        for(uint icent=0; icent<GetDetNcent(detid); icent++){
          for(uint iz=0; iz<GetDetNz(detid); iz++){
            if(IsSumUpdated(detid, ikind, ihar, icent, iz)){
              int idcode = RP::calcIdCode(detid, ikind, ihar);
              fout << "0 "<<hex<<idcode<<dec<<" "<<icent<<" "<<iz<<" "
                   << GetSumXmean (detid, ikind, ihar, icent, iz)<<" "
                   << GetSumXsigma(detid, ikind, ihar, icent, iz)<<" "
                   << GetSumYmean (detid, ikind, ihar, icent, iz)<<" "
                   << GetSumYsigma(detid, ikind, ihar, icent, iz)<<" "
                   << endl;
            } // if
          } // iz loop
        } // icent loop
      } // ihar loop
    } // ikind loop
  } // idet loop
  
  for(uint idet=0; idet<NDET; idet++){
    uint detid = getDetId(idet);
    
    for(uint ikind=0; ikind<GetDetNkind(detid); ikind++){
      for(uint ihar=0; ihar<GetDetNhar(detid); ihar++){
        for(uint icent=0; icent<GetDetNcent(detid); icent++){
          for(uint iz=0; iz<GetDetNz(detid); iz++){
            for(uint iord=0; iord<GetDetNorder(detid); iord++) {

              if(IsFlatUpdated(detid, ikind, ihar, icent, iz, iord)){
                int idcode = RP::calcIdCode(detid, ikind, ihar);

                fout << "1 "<<hex<<idcode<<dec<<" "<<icent<<" "<<iz<<" "<<iord<<" "
                     << GetFlatCos(detid, ikind, ihar, icent, iz, iord)<<" "
                     << GetFlatSin(detid, ikind, ihar, icent, iz, iord)<<" "
                     << endl;
              } // if
            } // iord loop
          } // iz loop
        } // icent loop
      } // ihar loop
    } // ikind loop
  } // idet loop

  fout.close();

  cout << "Write parameter to " << filename << " [DONE] " << endl;
}

float ReactionPlaneCalibv1::GetSumXmean(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz)
{
  int idx = getDetIdx(detid);
  if(idx<0) { return 0.0; }

  return m_rpCalData[idx]->getSumXmean (ikind, ihar, icent, iz);
}

float ReactionPlaneCalibv1::GetSumXsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz)
{
  int idx = getDetIdx(detid);
  if(idx<0) { return 1.0; }

  return m_rpCalData[idx]->getSumXsigma (ikind, ihar, icent, iz);
}

float ReactionPlaneCalibv1::GetSumYmean(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz)
{
  int idx = getDetIdx(detid);
  if(idx<0) { return 0.0; }

  return m_rpCalData[idx]->getSumYmean (ikind, ihar, icent, iz);
}

float ReactionPlaneCalibv1::GetSumYsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz)
{
  int idx = getDetIdx(detid);
  if(idx<0) { return 1.0; }

  return m_rpCalData[idx]->getSumYsigma (ikind, ihar, icent, iz);
}

float ReactionPlaneCalibv1::GetFlatCos(
  const int detid, const uint ikind, const uint ihar, const uint icent, 
  const uint iz, const uint iord)
{
  int idx = getDetIdx(detid);
  if(idx<0) { return 0.0; }

  return m_rpCalData[idx]->getFlatCos(ikind, ihar, icent, iz, iord);
}

float ReactionPlaneCalibv1::GetFlatSin(
  const int detid, const uint ikind, const uint ihar, const uint icent, 
  const uint iz, const uint iord)
{
  int idx = getDetIdx(detid);
  if(idx<0) { return 0.0; }

  return m_rpCalData[idx]->getFlatSin(ikind, ihar, icent, iz, iord);
}


void ReactionPlaneCalibv1::SetSumXmean(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"Xmean out of range : "<<detid<<endl; return; }

  m_rpCalData[idx]->setSumXmean(ikind, ihar, icent, iz, val);
}
void ReactionPlaneCalibv1::SetSumXsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"Xsigma out of range : "<<detid<<endl; return; }

  m_rpCalData[idx]->setSumXsigma(ikind, ihar, icent, iz, val);
}
void ReactionPlaneCalibv1::SetSumYmean(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"Ymean out of range : "<<detid<<endl; return; }

  m_rpCalData[idx]->setSumYmean(ikind, ihar, icent, iz, val);
}
void ReactionPlaneCalibv1::SetSumYsigma(const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const float val)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"Ysigma out of range : "<<detid<<endl; return; }

  m_rpCalData[idx]->setSumYsigma(ikind, ihar, icent, iz, val);
}

void ReactionPlaneCalibv1::SetFlatCos(
  const int detid, const uint ikind, const uint ihar, const uint icent, 
  const uint iz, const uint iord, const float val)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"FlatCos out of range : "<<detid<<endl; return; }

  m_rpCalData[idx]->setFlatCos(ikind, ihar, icent, iz, iord, val);
}

void ReactionPlaneCalibv1::SetFlatSin(
  const int detid, const uint ikind, const uint ihar, const uint icent, 
  const uint iz, const uint iord, const float val)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"FlatSin out of range : "<<detid<<endl; return; }

  m_rpCalData[idx]->setFlatSin(ikind, ihar, icent, iz, iord, val);
}

bool ReactionPlaneCalibv1::GetEnableDet(const int detid)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"GetEnableDet out of range : "<<detid<<endl; return false; }
  
  return m_rpCalData[idx]->m_enable;
}

void ReactionPlaneCalibv1::setEnableDet(const int detid, const bool flag)
{
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"GetEnableDet out of range : "<<detid<<endl; return; }
  
  m_rpCalData[idx]->m_enable = flag;
}

void ReactionPlaneCalibv1::SetEnableDetArray(const vector<int>& array, const bool flag)
{
  vector<int>::const_iterator itr;
  for(itr=array.begin(); itr!=array.end(); ++itr){
    int detid = *itr;
    setEnableDet(detid, flag);
  }

  createEnableDetMap();
}

void ReactionPlaneCalibv1::SetEnableDetByRunNumber(const int runnumber)
{
  vector<int> vEnable;
  vEnable.push_back(RP::ID_MPC);
  vEnable.push_back(RP::ID_BBC);
  vEnable.push_back(RP::ID_SMD);
  vEnable.push_back(RP::ID_CNT);

  if(BEGIN_OF_RUN11<=runnumber&&runnumber<BEGIN_OF_RUN12){ // run11
    vEnable.push_back(RP::ID_SVX);
    vEnable.push_back(RP::ID_SEG);
  }
  else {
    vEnable.push_back(RP::ID_SVX);
    vEnable.push_back(RP::ID_SEG);
    vEnable.push_back(RP::ID_FVT);
  }
  SetEnableDetArray(vEnable, true);
}

void ReactionPlaneCalibv1::SetEnableDetAll(const bool flag)
{
  vector<int> vArray;
  for(uint idet=0; idet<GetNDet(); idet++){
    int detid = getDetId(idet);
    vArray.push_back(detid);
  }

  SetEnableDetArray(vArray, flag);
}


void ReactionPlaneCalibv1::PrintDet(const bool onlyenable)
{
  cout<<"ReactionPlaneCalibv1 registered detector setup"<<endl;
  for(uint idet=0; idet<NDET; idet++){
    if(onlyenable&& !m_rpCalData[idet]->m_enable){
      continue;
    }

    m_rpCalData[idet]->print();
  }
}

string ReactionPlaneCalibv1::GetDetName(const int detid){
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"GetDetName out of range : "<<detid<<endl; return "NONE"; }

  return m_rpCalData[idx]->m_name;
}

uint ReactionPlaneCalibv1::GetDetConst(const int detid, const int ispecies){
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"GetDetConst out of range : "<<detid<<endl; return 0; }

  if(     ispecies==0) return m_rpCalData[idx]->m_nkind;
  else if(ispecies==1) return m_rpCalData[idx]->m_nhar;
  else if(ispecies==2) return m_rpCalData[idx]->m_ncent;
  else if(ispecies==3) return m_rpCalData[idx]->m_nz;
  else                 return m_rpCalData[idx]->m_nord;
}

bool ReactionPlaneCalibv1::IsSumUpdated (
  const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz)
{ 
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"IsSumUpdated out of range : "<<detid<<endl; return false; }

  return m_rpCalData[idx]->isSumUpdated(ikind, ihar, icent, iz);
}

bool ReactionPlaneCalibv1::IsFlatUpdated(
  const int detid, const uint ikind, const uint ihar, const uint icent, const uint iz, const uint iord)
{ 
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"IsFlatUpdated out of range : "<<detid<<endl; return false; }

  return m_rpCalData[idx]->isFlatUpdated(ikind, ihar, icent, iz, iord);
}


int ReactionPlaneCalibv1::GetCentrality(PHCompositeNode *topNode)
{
  //RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  //  PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  // int runNumber = run->get_RunNumber();
  
  return -1;
}


int ReactionPlaneCalibv1::GetCentralityBin(const float cent)
{
  int icent = (int) ( RP::NMUL3 * ((cent - 0.001) / 100.) );
  
  if(icent<0 || RP::NMUL3<=icent) icent = -1;
  
  return icent;
}

int ReactionPlaneCalibv1::GetZVertexBin(const float vertex)
{
  if( vertex<-30.0 || 30.0<=vertex ) return -1;

  int iz = (int)(RP::NZPS2 * ((vertex + 30.0) / 60.0)); // -30<=vertex<30

  if(iz<0 || RP::NZPS2<=iz) iz = -1;

  return iz;
}

int ReactionPlaneCalibv1::GetZVertexBinSvx(const float vertex)
{

  if( vertex<-10.0 || 10.0<=vertex ) return -1;

  int iz = (int)(RP::NZPS3 * ((vertex + 10.0) / 20.0)); // -10<=vertex<10

  if(iz<0 || RP::NZPS3<=iz) iz = -1;

  return iz;
}


int ReactionPlaneCalibv1::GetBankID(int runNumber, const int detid) const
{
  // return bank id 

  if( BEGIN_OF_RUN4 <= runNumber && runNumber < BEGIN_OF_RUN7 ){
    return 1;
  }
  else if( BEGIN_OF_RUN7 <= runNumber && runNumber < BEGIN_OF_RUN11 ){
    return 2;
  }
  else if( BEGIN_OF_RUN11 <= runNumber ){
    return (((detid&0xFFFF)<<16) | RP::SCHEMA_V3);
  }
  else{
    cout << PHWHERE << " Invalid run number for reaction plane, run = " << runNumber << endl;
    cout << " return default value (1)" << endl;
    return 1;
  }
}

bool ReactionPlaneCalibv1::isUsedQvectorForAnalysis( int detid, unsigned int ikind )
{
  // only use all-layer-eta-slided Q vector
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"isUsedQvectorForAnalysis out of range : "<<detid<<endl; return false; }

  return m_rpCalData[idx]->isUsed(ikind);
}

void ReactionPlaneCalibv1::setUsedQvectorForAnalysis( int detid, vector<int> vKind )
{
  // only use all-layer-eta-slided Q vector
  int idx = getDetIdx(detid);
  if(idx<0) { cerr<<"setUsedQvectorForAnalysis out of range : "<<detid<<endl; }

  uint nAry = vKind.size();
  int* kindAry = new int[nAry];
  for(uint i=0; i<nAry; i++){
    kindAry[i] = vKind[i];
  }

  // reset first
  m_rpCalData[idx]->setUsedAll(false);
  m_rpCalData[idx]->setUsedByArray(nAry, kindAry, true);
  m_rpCalData[idx]->print();

  delete [] kindAry;
}


int ReactionPlaneCalibv1::GetCentralityBin(PHCompositeNode *topNode, float& bbcchargesum)
{
  PHGlobal *global = getClass<PHGlobal>(topNode, "PHGlobal");
  BbcOut   *bbcout = getClass<BbcOut>  (topNode, "BbcOut");

  int imul = RP::NMUL3-1;
  if(global==NULL && bbcout==NULL){
    cerr << PHWHERE << "No both PHGlobal and BbcOut!" << endl;
    return imul;
  }

  //BBCcharge for centrality
  float bbccha_s = -9999;
  float bbccha_n = -9999;
  float bbccha_t = -9999;
  if(global){
    bbccha_s = global->getBbcChargeS();
    bbccha_n = global->getBbcChargeN();
  }
  else if(bbcout){
    bbccha_s = bbcout->get_ChargeSum(0);
    bbccha_n = bbcout->get_ChargeSum(1);
  }

  if(bbccha_s>-9000 && bbccha_n>-9000){
    bbccha_t = bbccha_s + bbccha_n;
  }
  else 
    bbccha_t = -9999;

  float bbc_cut[RP::NMUL3-1] = { 167, 233, 270, 307, 337,
                                 366, 389, 411, 428, 445,
                                 457, 469, 477, 484, 488,
                                 493, 495, 498, 499 }; //500bin
  for(int jcent=RP::NMUL3-2;jcent>=0;jcent--){
   if(bbccha_t>2500.0*(500.0-bbc_cut[jcent])/500.0) imul=jcent;
  }

  bbcchargesum = bbccha_t;
 
  return imul;
}

int ReactionPlaneCalibv1::GetZVertexBin(PHCompositeNode *topNode, float& zvertex)
{

  PHGlobal *global = getClass<PHGlobal>(topNode, "PHGlobal");
  BbcOut   *bbcout = getClass<BbcOut>(topNode, "BbcOut");
  VtxOut   *vtxout = getClass<VtxOut>(topNode, "VtxOut");

  //////////////////
  // Z_Vertex Cut //
  float vertex=-9999;

  if(vtxout) vertex = vtxout->get_ZVertex();
  else {
    float bbcz = -9999;
    if(global)      bbcz = global->getBbcZVertex();
    else if(bbcout) bbcz = bbcout->get_VertexPoint();
    
    vertex = bbcz;
  }

  int izps = GetZVertexBin(vertex);

  zvertex  = vertex;

  return izps;
}

void ReactionPlaneCalibv1::createEnableDetMap()
{
  for(uint idx=0; idx<NDET; idx++){
    if(m_rpCalData[idx]->m_enable) m_enableArray.push_back(m_rpCalData[idx]->m_id);
  }
}

void ReactionPlaneCalibv1::printIDtoIDXMap()
{
  cout<<"Id to Idx Map"<<endl;
  map<int, int>::iterator itr;
  for(itr=m_vIDtoIDX.begin(); itr!=m_vIDtoIDX.end(); ++itr){
    int detid  = itr->first;
    int detidx = itr->second;
    cout<<"   detid:detidx = "<<detid<<" : "<<detidx<<endl;
  }
}
void ReactionPlaneCalibv1::printIDXtoIDMap()
{
  cout<<"Idx to Id Map"<<endl;
  for(unsigned int idx=0; idx<m_vIDXtoID.size(); idx++){
    int detid  = m_vIDXtoID[idx];
    cout<<"   detid:detidx = "<<detid<<" : "<<idx<<endl;
  }
}

int ReactionPlaneCalibv1::getDetIdx(const int detid)
{
  map<int, int>::iterator itr = m_vIDtoIDX.find(detid);
  if(itr==m_vIDtoIDX.end()){
    return -1;
  }
  return itr->second;
}

int ReactionPlaneCalibv1::getDetId(const int idx)
{
  if(idx<0||(int)m_vIDXtoID.size()<=idx){
    return -1;
  }
  return m_vIDXtoID[idx];
}
