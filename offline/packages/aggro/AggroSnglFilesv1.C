
#include "AggroSnglFilesv1.h"

ClassImp(AggroSnglFilesv1)

AggroSnglFilesv1::AggroSnglFilesv1()
{

  filename="none";
  filesegm=-9999;

  return;
}

AggroSnglFilesv1::AggroSnglFilesv1(AggroSnglFilesv1 *track)
{

  if (!track) return;

  filename = track->get_filename();
  filesegm = track->get_filesegm();

  return;
}










