#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE offline_packages_geo
#include <boost/test/unit_test.hpp>
#include "PHVector.h"
#include "PHGeometry.h"

BOOST_AUTO_TEST_CASE(vector_dot_product) {
  PHVector p1(1, 0, 0);
  PHVector p2(0, 1, 0);

  BOOST_CHECK_EQUAL(p1.dot(p2), 0);
  BOOST_CHECK_EQUAL(PHGeometry::dot(p1, p2), 0);
}

BOOST_AUTO_TEST_SUITE(a_PHVector);
BOOST_AUTO_TEST_CASE(constructors) {
  PHVector p1;
  BOOST_CHECK_EQUAL(p1.getX(), 0);
  BOOST_CHECK_EQUAL(p1.getY(), 0);
  BOOST_CHECK_EQUAL(p1.getZ(), 0);

  PHVector p2(1, 2, 3);
  BOOST_CHECK_EQUAL(p2.getX(), 1);
  BOOST_CHECK_EQUAL(p2.getY(), 2);
  BOOST_CHECK_EQUAL(p2.getZ(), 3);
}

BOOST_AUTO_TEST_CASE(standard_operators) {
  PHVector p1;
  PHVector p2(1, 2, 3);
  p1 = p2;
  BOOST_CHECK_EQUAL(p1.getX(), 1);
  BOOST_CHECK_EQUAL(p1.getY(), 2);
  BOOST_CHECK_EQUAL(p1.getZ(), 3);

  PHVector p3(-1, -2, -3);
  BOOST_CHECK_EQUAL((-p3).getX(), 1);
  BOOST_CHECK_EQUAL((-p3).getY(), 2);
  BOOST_CHECK_EQUAL((-p3).getZ(), 3);

  PHVector p4;
  PHVector p5(1, 2, 3);
  PHVector p6(p4-p5);
  BOOST_CHECK_EQUAL(p6.getX(), -1);
  BOOST_CHECK_EQUAL(p6.getY(), -2);
  BOOST_CHECK_EQUAL(p6.getZ(), -3);

  PHVector p7(1, 2, 3);
  PHVector p8(1, 2, 3);
  PHVector p9(p7+p8);
  BOOST_CHECK_EQUAL(p9.getX(), 2);
  BOOST_CHECK_EQUAL(p9.getY(), 4);
  BOOST_CHECK_EQUAL(p9.getZ(), 6);

  PHVector p10(1, 2, 3);
  PHVector p11(p10*2);
  BOOST_CHECK_EQUAL(p11.getX(), 2);
  BOOST_CHECK_EQUAL(p11.getY(), 4);
  BOOST_CHECK_EQUAL(p11.getZ(), 6);
}

BOOST_AUTO_TEST_CASE(construct_from_point) {
  PHPoint p(1, 2, 3);
  PHVector v(p);
  BOOST_CHECK_EQUAL(v.getX(), 1);
  BOOST_CHECK_EQUAL(v.getY(), 2);
  BOOST_CHECK_EQUAL(v.getZ(), 3);
}
BOOST_AUTO_TEST_SUITE_END();

BOOST_AUTO_TEST_SUITE(a_PHGeometry);
BOOST_AUTO_TEST_CASE(frames2MatrixAndVector) {
  PHVector x(1, 0, 0);
  PHVector y(0, 1, 0);
  PHVector z(0, 0, 1);
  PHFrame f1(x, y, z);
  PHFrame f2(y, x, z);

  PHMatrix rot;
  PHVector trans;
  PHGeometry::frames2MatrixAndVector(f1, f2, rot, trans);
  BOOST_CHECK_EQUAL(trans.getX(), 1);
  BOOST_CHECK_EQUAL(trans.getY(), 0);
  BOOST_CHECK_EQUAL(trans.getZ(), 1);

  BOOST_CHECK_EQUAL(rot.getA00(), 0);
  BOOST_CHECK_EQUAL(rot.getA01(), 0);
  BOOST_CHECK_EQUAL(rot.getA02(), 1);

  BOOST_CHECK_EQUAL(rot.getA10(), 0);
  BOOST_CHECK_EQUAL(rot.getA11(), 1);
  BOOST_CHECK_EQUAL(rot.getA12(), 0);

  BOOST_CHECK_EQUAL(rot.getA20(), -1);
  BOOST_CHECK_EQUAL(rot.getA21(), 0);
  BOOST_CHECK_EQUAL(rot.getA22(), 0);
}

BOOST_AUTO_TEST_SUITE_END();
