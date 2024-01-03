#include "PHGlobalv2.h"

#include <iostream>

ClassImp(PHGlobalv2)

using namespace std;

PHGlobalv2::PHGlobalv2() 
{
  Reset();
}

void PHGlobalv2::Reset() 
{
  run=0;
  seq=0;
  evt=0;
  centclock=-999;
  centperp=-999;
  zdce0=0.;
  zdce1=0.;
  zdcz=-9999.;
  zdct0=-9999.;
  bbcn=0.;
  bbcs=0.;
  bbcqn=0.;
  bbcqs=0.;
  bbcz=-9999.;
  bbct0=-9999.;
  ndc=0;
  npc1=0;
  npc2=0;
  npc3=0;
  ntec=0;
  nemc=0;
  ntof=0;
  ncrk=0;
  etotw=0.;
  etote=0.;
  zvertex=-9999.; 
  ntcTimeZero	=-9999;
  ntcZVertex	=-9999;
  return;
}

void PHGlobalv2::identify(std::ostream& os) const 
{
  os << "identify yourself: PHGlobalv2 Object, Global Event Information." 
     << std::endl;
  return;
}

int PHGlobalv2::isValid() const 
{
  return ((run>0) ? 1 : 0);
}

float PHGlobalv2::getEmcEnergy() const 
{
    return (etote+etotw);
}

void PHGlobalv2::setZVertex(const float vtx) 
{
    zvertex=vtx;
}

float PHGlobalv2::getZVertex() const 
{
    return zvertex;
}

void PHGlobalv2::setRunNumber(const int crun) 
{
    run = crun;
}

int PHGlobalv2::getRunNumber() const 
{
    return run;
}

void PHGlobalv2::setRunSequence(const int cseq) 
{
  seq =cseq;
}

int PHGlobalv2::getRunSequence() const 
{
  return seq;
}

void PHGlobalv2::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv2::getEventNumber() const 
{
  return evt;
}

void PHGlobalv2::setZdcZVertex(const float czdcz){
  zdcz = czdcz;
}
float PHGlobalv2::getZdcZVertex() const {
  return zdcz;
}

void PHGlobalv2::setZdcEnergyNS(const float zdceNorth, const float zdceSouth){
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv2::getZdcEnergyN() const {
  return zdce0;
}
float PHGlobalv2::getZdcEnergyS() const {
  return zdce1;
}

void PHGlobalv2::setZdcTimeZero(const float czdct0) {
  zdct0 = czdct0;
}
float PHGlobalv2::getZdcTimeZero()  const {
  return zdct0;
}

void PHGlobalv2::setBbcMultNS(const short int  bbcNorth, const short int  bbcSouth) {
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv2::getBbcMultN()  const {
  return (short int) bbcn;
}

short int PHGlobalv2::getBbcMultS()  const {
  return (short int) bbcs;
}

void PHGlobalv2::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth) {
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv2::getBbcChargeN()  const {
  return bbcqn;
}
float PHGlobalv2::getBbcChargeS()  const {
  return bbcqs;
}

void PHGlobalv2::setBbcZVertex(const float cbbcz){
  bbcz = cbbcz;
}
float PHGlobalv2::getBbcZVertex() const {
  return bbcz;
}

void PHGlobalv2::setBbcTimeZero(const float cbbct0){
  bbct0 = cbbct0;
}
float PHGlobalv2::getBbcTimeZero() const {
  return bbct0;
}


void PHGlobalv2::setNumberDchTracks(const short int num){
  ndc = num;
}
short int PHGlobalv2::getNumberDchTracks() const {
  return ndc;
}

void PHGlobalv2::setNumberPC1Hits(const short int num) {
  npc1 = num;
}
short int PHGlobalv2::getNumberPC1Hits()  const {
  return npc1;
}

void PHGlobalv2::setNumberPC2Hits(const short int num) {
  npc2 = num;
}
short int PHGlobalv2::getNumberPC2Hits()  const {
  return npc2;
}

void PHGlobalv2::setNumberPC3Hits(const short int num) {
  npc3 = num;
}
short int PHGlobalv2::getNumberPC3Hits()  const {
  return npc3;
}

void PHGlobalv2::setNumberTecTracks(const short int num) {
  ntec = num;
}
short int PHGlobalv2::getNumberTecTracks()  const {
  return ntec;
}

void PHGlobalv2::setNumberEmcClusters(const short int num) {
  nemc = num;
}
short int PHGlobalv2::getNumberEmcClusters()  const {
  return nemc;
}

void PHGlobalv2::setNumberTofHits(const short int cntof){
  ntof = cntof;
}
short int PHGlobalv2::getNumberTofHits() const {
  return ntof;
}

void PHGlobalv2::setNumberCerenkovHits(const short int cncrk){
  ncrk = cncrk;
}
short int PHGlobalv2::getNumberCerenkovHits() const {
  return ncrk;
}

void PHGlobalv2::setEmcEnergyEW(const float east, const float west){
  etote = east;
  etotw = west;
}
float PHGlobalv2::getEmcEnergyW() const {
  return etotw;
}
float PHGlobalv2::getEmcEnergyE() const {
  return etote;
}

void PHGlobalv2::setCentralitybyClock(const int centvalue) {
  centclock=centvalue;
}

int PHGlobalv2::getCentralitybyClock() const {
  return centclock;
}

void PHGlobalv2::setCentralitybyPerp(const int centvalue) {
  centperp=centvalue;
}

int PHGlobalv2::getCentralitybyPerp() const {
  return centperp;
}

