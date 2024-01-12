#include <DumpTecOut.h>
#include <TecOut.hh>
#include <TecTrackTR.hh>

#include <string>

using namespace std;

typedef PHIODataNode<TecOut> MyNode_t;

DumpTecOut::DumpTecOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpTecOut::process_Node(PHNode *myNode)
{
  TecOut *tecout = 0;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      tecout = thisNode->getData();
    }
  if (tecout && tecout->isValid())
    {
      TClonesArray *tc1 = tecout->GetTecTracks();
      TecTrackTR *trk = 0;
      if (tc1)
        {
          trk = (TecTrackTR *) tc1->UncheckedAt(1);
        }
      if (trk)
        {
          trk->ShutUp();
        }
      *fout << "getRunNumber(): " << tecout->getRunNumber() << endl;
      *fout << "getNHits(): " << tecout->getNHits() << endl;
      *fout << "getNTracks(): " << tecout->getNTracks() << endl;
      for (int i = 0; i < tecout->getNHits(); i++)
        {
          *fout << "getHitIndex(" << i << "): " << tecout->getHitIndex(i) << endl;
          *fout << "getHitGlobalIndex(" << i << "): " << tecout->getHitGlobalIndex(i) << endl;
          *fout << "getHitSector(" << i << "): " << tecout->getHitSector(i) << endl;
          *fout << "getHitPlane(" << i << "): " << tecout->getHitPlane(i) << endl;
          *fout << "getHitSide(" << i << "): " << tecout->getHitSide(i) << endl;
          *fout << "getHitWire(" << i << "): " << tecout->getHitWire(i) << endl;
          *fout << "getHitTimeBin(" << i << "): " << tecout->getHitTimeBin(i) << endl;
          *fout << "getHitADC(" << i << "): " << tecout->getHitADC(i) << endl;
          *fout << "getHitCharge(" << i << "): " << tecout->getHitCharge(i) << endl;
          *fout << "getHitX(" << i << "): " << tecout->getHitX(i) << endl;
          *fout << "getHitY(" << i << "): " << tecout->getHitY(i) << endl;
          *fout << "getHitTrackID(" << i << "): " << tecout->getHitTrackID(i) << endl;
          for (int j = 0; j < 3;j++)
            {
              *fout << "getHitTrackID(" << i << "," << j << "): " << tecout->getHitTrackID(i, j) << endl;
            }
        }
      for (int i = 0; i < tecout->getNTracks(); i++)
        {
          *fout << "getTrackIndex(" << i << "): " << tecout->getTrackIndex(i) << endl;
          *fout << "getTrackSector(" << i << "): " << tecout->getTrackSector(i) << endl;
          *fout << "getTrackSide(" << i << "): " << tecout->getTrackSide(i) << endl;
          *fout << "getTrackXin(" << i << "): " << tecout->getTrackXin(i) << endl;
          *fout << "getTrackXout(" << i << "): " << tecout->getTrackXout(i) << endl;
          *fout << "getTrackYin(" << i << "): " << tecout->getTrackYin(i) << endl;
          *fout << "getTrackYout(" << i << "): " << tecout->getTrackYout(i) << endl;
          *fout << "getTrackXinError(" << i << "): " << tecout->getTrackXinError(i) << endl;
          *fout << "getTrackXoutError(" << i << "): " << tecout->getTrackXoutError(i) << endl;
          *fout << "getTrackYinError(" << i << "): " << tecout->getTrackYinError(i) << endl;
          *fout << "getTrackYoutError(" << i << "): " << tecout->getTrackYoutError(i) << endl;
          *fout << "getTrackdEdX(" << i << "): " << tecout->getTrackdEdX(i) << endl;
          *fout << "getTrackLength(" << i << "): " << tecout->getTrackLength(i) << endl;
          *fout << "getTrackNdEdXbins(" << i << "): " << tecout->getTrackNdEdXbins(i) << endl;
          *fout << "getTrackNhits(" << i << "): " << tecout->getTrackNhits(i) << endl;

          *fout << "getTrackAlpha(" << i << "): " << tecout->getTrackAlpha(i) << endl;
          *fout << "getTrackPhi(" << i << "): " << tecout->getTrackPhi(i) << endl;
          *fout << "getTrackPt(" << i << "): " << tecout->getTrackPt(i) << endl;
          *fout << "getTrackSlope(" << i << "): " << tecout->getTrackSlope(i) << endl;
          *fout << "getTrackIntercept(" << i << "): " << tecout->getTrackIntercept(i) << endl;
          for (int j = 0; j < 6;j++)
            {
              *fout << "getTrackNhits(" << i << "," << j << "): " << tecout->getTrackNhits(i, j) << endl;
              *fout << "getTrackNwires(" << i << "," << j << "): " << tecout->getTrackNwires(i, j) << endl;
            }

        }
      if (trk)
        {
          trk->ShutUp(1);
        }
    }
  return 0;
}

