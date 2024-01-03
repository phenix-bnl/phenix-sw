#include "PHGlobalv1.h"

#include <iostream>

ClassImp(PHGlobalv1)

using namespace std;

PHGlobalv1::PHGlobalv1() 
{
  Reset();
}


void PHGlobalv1::Reset() 
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
  return;
}

void PHGlobalv1::identify(ostream& os) const 
{
  os << "identify yourself: PHGlobalv1 Object, Global Event Information." << std::endl;
  return;
}

int PHGlobalv1::isValid() const 
{
  return ((run>0) ? 1 : 0);
}

float PHGlobalv1::getEmcEnergy() const 
{
    return (etote+etotw);
}

void PHGlobalv1::setZVertex(const float vtx) 
{
    zvertex=vtx;
}

float PHGlobalv1::getZVertex() const 
{
    return zvertex;
}

void PHGlobalv1::setRunNumber(const int crun) 
{
    run = crun;
}

int PHGlobalv1::getRunNumber() const 
{
    return run;
}

void PHGlobalv1::setRunSequence(const int cseq) 
{
  seq =cseq;
}

int PHGlobalv1::getRunSequence() const 
{
  return seq;
}

void PHGlobalv1::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv1::getEventNumber() const 
{
  return evt;
}

void PHGlobalv1::setZdcZVertex(const float czdcz){
  zdcz = czdcz;
}

float PHGlobalv1::getZdcZVertex() const {
  return zdcz;
}

void PHGlobalv1::setZdcEnergyNS(const float zdceNorth, const float zdceSouth){
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv1::getZdcEnergyN() const {
  return zdce0;
}
float PHGlobalv1::getZdcEnergyS() const {
  return zdce1;
}

void PHGlobalv1::setZdcTimeZero(const float czdct0) {
  zdct0 = czdct0;
}
float PHGlobalv1::getZdcTimeZero()  const {
  return zdct0;
}

void PHGlobalv1::setBbcMultNS(const short int  bbcNorth, const short int  bbcSouth) {
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv1::getBbcMultN()  const {
  return (short int) bbcn;
}

short int PHGlobalv1::getBbcMultS()  const {
  return (short int) bbcs;
}

void PHGlobalv1::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth) {
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv1::getBbcChargeN()  const {
  return bbcqn;
}
float PHGlobalv1::getBbcChargeS()  const {
  return bbcqs;
}

void PHGlobalv1::setBbcZVertex(const float cbbcz){
  bbcz = cbbcz;
}
float PHGlobalv1::getBbcZVertex() const {
  return bbcz;
}

void PHGlobalv1::setBbcTimeZero(const float cbbct0){
  bbct0 = cbbct0;
}
float PHGlobalv1::getBbcTimeZero() const {
  return bbct0;
}


void PHGlobalv1::setNumberDchTracks(const short int num){
  ndc = num;
}
short int PHGlobalv1::getNumberDchTracks() const {
  return ndc;
}

void PHGlobalv1::setNumberPC1Hits(const short int num) {
  npc1 = num;
}
short int PHGlobalv1::getNumberPC1Hits()  const {
  return npc1;
}

void PHGlobalv1::setNumberPC2Hits(const short int num) {
  npc2 = num;
}
short int PHGlobalv1::getNumberPC2Hits()  const {
  return npc2;
}

void PHGlobalv1::setNumberPC3Hits(const short int num) {
  npc3 = num;
}
short int PHGlobalv1::getNumberPC3Hits()  const {
  return npc3;
}

void PHGlobalv1::setNumberTecTracks(const short int num) {
  ntec = num;
}
short int PHGlobalv1::getNumberTecTracks()  const {
  return ntec;
}

void PHGlobalv1::setNumberEmcClusters(const short int num) {
  nemc = num;
}
short int PHGlobalv1::getNumberEmcClusters()  const {
  return nemc;
}

void PHGlobalv1::setNumberTofHits(const short int cntof){
  ntof = cntof;
}
short int PHGlobalv1::getNumberTofHits() const {
  return ntof;
}

void PHGlobalv1::setNumberCerenkovHits(const short int cncrk){
  ncrk = cncrk;
}
short int PHGlobalv1::getNumberCerenkovHits() const {
  return ncrk;
}

void PHGlobalv1::setEmcEnergyEW(const float east, const float west){
  etote = east;
  etotw = west;
}
float PHGlobalv1::getEmcEnergyW() const {
  return etotw;
}
float PHGlobalv1::getEmcEnergyE() const {
  return etote;
}

void PHGlobalv1::setCentralitybyClock(const int centvalue) {
  centclock=centvalue;
}

int PHGlobalv1::getCentralitybyClock() const {
  return centclock;
}

void PHGlobalv1::setCentralitybyPerp(const int centvalue) {
  centperp=centvalue;
}

int PHGlobalv1::getCentralitybyPerp() const {
  return centperp;
}
