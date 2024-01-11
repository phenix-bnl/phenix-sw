#include <SvxPrimVertexInfov1.h>

#include <PHPoint.h>

#include <vector>
#include <iostream>

using namespace std;

ClassImp(SvxPrimVertexInfov1);

//----------------------------------------------------------------------------//
void SvxPrimVertexInfov1::Reset()
{
  for (int i = 0; i < 3; i++)
  {
    m_vertex[i] = -9999.;
    m_vertexResolution[i] = -9999.;
  }
  m_segmentID.clear();
}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
int SvxPrimVertexInfov1::isValid() const
{
  if (m_vertex[0] == -9999. || m_vertex[1] == -9999. || m_segmentID.size() == 0)
    return 0;
  else
    return 1;
}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
void SvxPrimVertexInfov1::Copy(SvxPrimVertexInfo *info)
{
  // Reset this instance
  Reset();

  // Copy the vertex position
  set_vertex(info->get_vertexX(),
             info->get_vertexY(),
             info->get_vertexZ());

  // Copy the vertex resolution
  set_vertexResolution(info->get_vertexResolutionX(),
                       info->get_vertexResolutionY(),
                       info->get_vertexResolutionZ());

  // Copy the segment ID's
  for (unsigned int i = 0; i < info->get_nUsedSegments(); i++)
  {
    add_segmentID( info->get_segmentID(i) );
  }

}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
SvxPrimVertexInfo* SvxPrimVertexInfov1::Clone()
{
  SvxPrimVertexInfov1 *cloned = new SvxPrimVertexInfov1();
  cloned->set_vertex(m_vertex[0], m_vertex[1], m_vertex[2]);
  cloned->set_vertexResolution(m_vertexResolution[0],
                               m_vertexResolution[1],
                               m_vertexResolution[2]);
  for (unsigned int i = 0; i < m_segmentID.size(); i++)
    add_segmentID( m_segmentID.at(i) );

  return cloned;
}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
void SvxPrimVertexInfov1::set_vertex(double x,
                                     double y,
                                     double z)
{
  m_vertex[0] = x;
  m_vertex[1] = y;
  m_vertex[2] = z;
}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
void SvxPrimVertexInfov1::set_vertexResolution(
  double sig_x,
  double sig_y,
  double sig_z)
{
  m_vertexResolution[0] = sig_x;
  m_vertexResolution[1] = sig_y;
  m_vertexResolution[2] = sig_z;
}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
PHPoint SvxPrimVertexInfov1::get_vertex()
{
  return PHPoint(m_vertex[0], m_vertex[1], m_vertex[2]);
}
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
int SvxPrimVertexInfov1::get_segmentID(unsigned int i)
{
  // Make sure the input index is valid
  if ( i >= m_segmentID.size() )
  {
    cout << PHWHERE << ": get_segmentID(): Invalid input. "
         << i << " >= " << m_segmentID.size()
         << endl;
    return -1;
  }
  else
  {
    return m_segmentID.at(i);
  }
}
//----------------------------------------------------------------------------//

