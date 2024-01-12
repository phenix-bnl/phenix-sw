#include <DumpPHTrackOut.h>
#include <PHTrackOut.h>
#include <PHIODataNode.h>
#include <string>

using namespace std;

typedef PHIODataNode<PHTrackOut> MyNode_t;

DumpPHTrackOut::DumpPHTrackOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPHTrackOut::process_Node(PHNode *myNode)
{
  PHTrackOut *phtrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      phtrack = thisNode->getData();
    }
  if (phtrack && phtrack->isValid())
    {
      phtrack->ShutUp();
      *fout << "PHNTrack: " << phtrack->get_PHNTrack() << endl;
      for (unsigned int i = 0; i < phtrack->get_PHNTrack(); i++)
        {
          *fout << "PHTrackOut->ifIntersectVtx(" << i << "): " << phtrack->ifIntersectVtx(i) << endl;
          if (phtrack->ifIntersectVtx(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionVtx(" << i << "," << j << "): " << phtrack->get_projectionVtx(i, j) << endl;
                  *fout << "PHTrackOut->get_directionVtx(" << i << "," << j << "): " << phtrack->get_directionVtx(i, j) << endl;
                }
            }
          *fout << "PHTrackOut->ifIntersectDch(" << i << "): " << phtrack->ifIntersectDch(i) << endl;
          if (phtrack->ifIntersectDch(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionDch(" << i << "," << j << "): " << phtrack->get_projectionDch(i, j) << endl;
                  *fout << "PHTrackOut->get_directionDch(" << i << "," << j << "): " << phtrack->get_directionDch(i, j) << endl;
                }
            }

          *fout << "PHTrackOut->ifIntersectHbd(" << i << "): " << phtrack->ifIntersectHbd(i) << endl;
          if (phtrack->ifIntersectHbd(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionHbd(" << i << "," << j << "): " << phtrack->get_projectionHbd(i, j) << endl;
                  *fout << "PHTrackOut->get_directionHbd(" << i << "," << j << "): " << phtrack->get_directionHbd(i, j) << endl;
                }
            }
          *fout << "PHTrackOut->ifIntersectPc1(" << i << "): " << phtrack->ifIntersectPc1(i) << endl;
          if (phtrack->ifIntersectPc1(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionPc1(" << i << "," << j << "): " << phtrack->get_projectionPc1(i, j) << endl;
                  *fout << "PHTrackOut->get_directionPc1(" << i << "," << j << "): " << phtrack->get_directionPc1(i, j) << endl;
                }
            }
          *fout << "PHTrackOut->ifIntersectPc2(" << i << "): " << phtrack->ifIntersectPc2(i) << endl;
          if (phtrack->ifIntersectPc2(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionPc2(" << i << "," << j << "): " << phtrack->get_projectionPc2(i, j) << endl;
                  *fout << "PHTrackOut->get_directionPc2(" << i << "," << j << "): " << phtrack->get_directionPc2(i, j) << endl;
                }
            }
          *fout << "PHTrackOut->ifIntersectPc3(" << i << "): " << phtrack->ifIntersectPc3(i) << endl;
          if (phtrack->ifIntersectPc3(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionPc3(" << i << "," << j << "): " << phtrack->get_projectionPc3(i, j) << endl;
                  *fout << "PHTrackOut->get_directionPc3(" << i << "," << j << "): " << phtrack->get_directionPc3(i, j) << endl;
                }
            }
          *fout << "PHTrackOut->ifIntersectCrk(" << i << "): " << phtrack->ifIntersectCrk(i) << endl;
          if (phtrack->ifIntersectCrk(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionCrk(" << i << "," << j << "): " << phtrack->get_projectionCrk(i, j) << endl;
                  *fout << "PHTrackOut->get_directionCrk(" << i << "," << j << "): " << phtrack->get_directionCrk(i, j) << endl;
                }
              *fout << "PHTrackOut->get_crkPathLength(" << i << "): " << phtrack->get_crkPathLength(i) << endl;
            }
          *fout << "PHTrackOut->ifIntersectTec(" << i << "): " << phtrack->ifIntersectTec(i) << endl;
          if (phtrack->ifIntersectTec(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionTec(" << i << "," << j << "): " << phtrack->get_projectionTec(i, j) << endl;
                  *fout << "PHTrackOut->get_directionTec(" << i << "," << j << "): " << phtrack->get_directionTec(i, j) << endl;
                }
            }
          *fout << "PHTrackOut->ifIntersectTof(" << i << "): " << phtrack->ifIntersectTof(i) << endl;
          if (phtrack->ifIntersectTof(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionTof(" << i << "," << j << "): " << phtrack->get_projectionTof(i, j) << endl;
                  *fout << "PHTrackOut->get_directionTof(" << i << "," << j << "): " << phtrack->get_directionTof(i, j) << endl;
                }
              *fout << "PHTrackOut->get_tofPathLength(" << i << "): " << phtrack->get_tofPathLength(i) << endl;
            }
          *fout << "PHTrackOut->ifIntersectTofw(" << i << "): " << phtrack->ifIntersectTofw(i) << endl;
          if (phtrack->ifIntersectTofw(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionTofw(" << i << "," << j << "): " << phtrack->get_projectionTofw(i, j) << endl;
                  *fout << "PHTrackOut->get_directionTofw(" << i << "," << j << "): " << phtrack->get_directionTofw(i, j) << endl;
                }
              *fout << "PHTrackOut->get_tofwPathLength(" << i << "): " << phtrack->get_tofwPathLength(i) << endl;
            }
          *fout << "PHTrackOut->ifIntersectPbgl(" << i << "): " << phtrack->ifIntersectPbgl(i) << endl;
          if (phtrack->ifIntersectPbgl(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionPbGl(" << i << "," << j << "): " << phtrack->get_projectionPbGl(i, j) << endl;
                  *fout << "PHTrackOut->get_directionPbGl(" << i << "," << j << "): " << phtrack->get_directionPbGl(i, j) << endl;
                }
              *fout << "PHTrackOut->get_emcPathLength(" << i << "): " << phtrack->get_emcPathLength(i) << endl;
            }
          *fout << "PHTrackOut->ifIntersectPbsc(" << i << "): " << phtrack->ifIntersectPbsc(i) << endl;
          if (phtrack->ifIntersectPbsc(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionPbSc(" << i << "," << j << "): " << phtrack->get_projectionPbSc(i, j) << endl;
                  *fout << "PHTrackOut->get_directionPbSc(" << i << "," << j << "): " << phtrack->get_directionPbSc(i, j) << endl;
                }
              *fout << "PHTrackOut->get_emcPathLength(" << i << "): " << phtrack->get_emcPathLength(i) << endl;
            }
          if (phtrack->ifIntersectMrpc(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionMrpc(" << i << "," << j << "): " << phtrack->get_projectionMrpc(i, j) << endl;
                  *fout << "PHTrackOut->get_directionMrpc(" << i << "," << j << "): " << phtrack->get_directionMrpc(i, j) << endl;
                }
              *fout << "PHTrackOut->get_mrpcPathLength(" << i << "): " << phtrack->get_mrpcPathLength(i) << endl;
            }
          if (phtrack->ifIntersectAcc(i))
            {
              for (short j = 0;j < 3;j++)
                {
                  *fout << "PHTrackOut->get_projectionAcc(" << i << "," << j << "): " << phtrack->get_projectionAcc(i, j) << endl;
                  *fout << "PHTrackOut->get_directionAcc(" << i << "," << j << "): " << phtrack->get_directionAcc(i, j) << endl;
                }
            }
          for (int k = 0; k < 4;k++)
            {
              if (phtrack->ifIntersectSvx(i, k))
                {
                  for (short j = 0;j < 3;j++)
                    {
                      *fout << "PHTrackOut->get_projectionSvx(" << i << "," << k << "," << j << "): " << phtrack->get_projectionSvx(i, k, j) << endl;
                      *fout << "PHTrackOut->get_directionSvx(" << i << "," << k << "," << j << "): " << phtrack->get_directionSvx(i, k, j) << endl;
                    }
                }
            }
        }

      phtrack->ShutUp(0);
    }
  return 0;
}


