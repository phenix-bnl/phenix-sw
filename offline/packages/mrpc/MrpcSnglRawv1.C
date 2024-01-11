#include <MrpcSnglRawv1.h>
#include <MrpcPar.h>

ClassImp(MrpcSnglRawv1)

MrpcSnglRawv1::MrpcSnglRawv1()
{
  slatid = -9999;
  for(int i=0;i<MRPC_NTYPE_READ;i++)
    {
      t3[i] = -9999;
      t4[i] = -9999;
      q1[i] = -9999;
      q3[i] = -9999;
      qvc[i] = -9999;
      t3_dig[i] = -9999;
      t4_dig[i] = -9999;
    }
  
  return;
}

MrpcSnglRawv1::MrpcSnglRawv1(MrpcSnglRawv1* track)
{
  
  if(!track) return;
  
  slatid = track->get_slatid();
  
  for(int i=0;i<MRPC_NTYPE_READ;i++)
    {
      t3[i] = track->get_t3(i);
      t4[i] = track->get_t4(i);
      q1[i] = track->get_q1(i);
      q3[i] = track->get_q3(i);
      qvc[i] = track->get_qvc(i);
      t3_dig[i] = track->get_t3_dig(i);
      t4_dig[i] = track->get_t4_dig(i);
    }
  
  return;
}
