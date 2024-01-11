#ifndef __SvxPrimVertexFinderv1_H__
#define __SvxPrimVertexFinderv1_H__

/*!
 * \file    SvxPrimVertexInfov1.h
 * \brief   Object to hold information on the Svx Precise vertex
 * \author  D. McGlinchey
 * \version $Revision: 1.2 $
 * \date    $Date: 2016/08/24 14:50:38 $
 */

#include <SvxPrimVertexInfo.h>

#include <vector>

class PHPoint;

class SvxPrimVertexInfov1 : public SvxPrimVertexInfo
{

public:

  /*! Constructor */
  SvxPrimVertexInfov1() { Reset(); }

  /*! Destructor */
  virtual ~SvxPrimVertexInfov1() {}

  /*! Identify object */
  void identify(std::ostream &os = std::cout) const {
    os << "Identify yourself: SvxPrimVertexInfov1 object" << std::endl;
  }

  /*! Clear Event */
  void Reset();

  /*! 
   * \brief isValid returns non zero if object contains valid data 
   *
   * Check that the object contains valid data. Require that the vertex
   * positions are != -9999 (default values) and that the segment ID list 
   * has a nonzero size.
   *
   * \return 0 if object contains invalid data, else 1
   */
  int isValid() const;

  /*! Clone object */
  SvxPrimVertexInfo* Clone();

  /*! Copy object */
  void Copy(SvxPrimVertexInfo* info);


  //! @name Setters
  //@{

  /*!
   * \brief Set the vertex position 
   *
   * \param[in] x Vertex position in X. Units should be cm.
   * \param[in] y Vertex position in Y. Units should be cm.
   * \param[in] z Vertex position in Z. Units should be cm.
   * \return None
   */
  void set_vertex(double x, double y, double z);

  /*!
   * \brief Set the vertex resolution 
   *
   * \param[in] sig_x Vertex resolution in X. Units should be cm.
   * \param[in] sig_y Vertex resolution in Y. Units should be cm.
   * \param[in] sig_z Vertex resolution in Z. Units should be cm.
   * \return None
   */
  void set_vertexResolution(double sig_x, double sig_y, double sig_z);

  /*!
   * \brief Add the ID of SvxSegment used in vertex calculation 
   *
   * \param[in] id ID of SvxSegment in SvxSegmentList
   * \return None
   */
  void add_segmentID(int id) { m_segmentID.push_back(id); }

  //@}



  //! @name Getters
  //@{

  /*! 
   * \brief Get the vertex position (PHPoint) 
   *
   * \return PHPoint containing (X, Y, Z) vertex position
   */
  PHPoint get_vertex();

  /*! 
   * \brief Get the vertex position in X 
   *
   * \return The vertex position in X
   */
  double get_vertexX() { return m_vertex[0]; }

  /*! 
   * \brief Get the vertex position in Y 
   *
   * \return The vertex position in Y
   */
  double get_vertexY() { return m_vertex[1]; }

  /*!
   * \brief Get the vertex position in Z 
   *
   * \return The vertex position in Z
   */
  double get_vertexZ() { return m_vertex[2]; }

  /*! 
   * \brief Get the vertex resolution in X 
   *
   * \return The vertex resolution in X
   */
  double get_vertexResolutionX() { return m_vertexResolution[0]; }

  /*! 
   * \brief Get the vertex resolution in Y 
   *
   * \return The vertex resolution in Y
   */
  double get_vertexResolutionY() { return m_vertexResolution[1]; }

  /*! 
   * \brief Get the vertex resolution in Z 
   *
   * \return The vertex resolution in Z
   */
  double get_vertexResolutionZ() { return m_vertexResolution[2]; }

  /*! 
   * \brief Get the number of segments used in the vertex calculation 
   *
   * \return The number of SvxSegments used in the vertex calculation
   */
  unsigned int get_nUsedSegments() { return m_segmentID.size(); }

  /*! 
   * \brief Get the ID of the ith segment used in the vertex calculation 
   *
   * \param[in] i The index ranging [0, get_nUsedSegments()]
   * \return The SvxSegment ID for index i
   */
  int get_segmentID(unsigned int i);

  //@}

private:

  //-- Class variables
  double m_vertex[3]; ///< Vertex position {X, Y, Z}
  double m_vertexResolution[3]; ///< Vertex resolution {X, Y, Z}
  std::vector<int> m_segmentID; ///< Vector of SvxSegmentID's used in vtx calc


  ClassDef(SvxPrimVertexInfov1, 1);

};

#endif /* __SvxPrimVertexFinderv1_H__ */
