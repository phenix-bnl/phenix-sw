#define BOOST_TEST_MODULE tests
#include <boost/test/included/unit_test.hpp>
#include <Momentum.h>

BOOST_AUTO_TEST_SUITE(Mom3_tests)

  BOOST_AUTO_TEST_CASE(is_copy_constructable) {
    Mom3 a(1, 2, 3);
    Mom3 b(a);
    BOOST_CHECK_EQUAL(b.Getpx(), 1);
    BOOST_CHECK_EQUAL(b.Getpy(), 2);
    BOOST_CHECK_EQUAL(b.Getpz(), 3);
  }

  BOOST_AUTO_TEST_CASE(is_addable) {
    Mom3 a(1, 2, 3);
    Mom3 b(0, 0, 0);
    Mom3 c = a + b;
    BOOST_CHECK_EQUAL(c.Getpx(), 1);
    BOOST_CHECK_EQUAL(c.Getpy(), 2);
    BOOST_CHECK_EQUAL(c.Getpz(), 3);

    Mom3 d(1, 2, 3);
    Mom3 e(0, 0, 0);
    e += d;
    BOOST_CHECK_EQUAL(e.Getpx(), 1);
    BOOST_CHECK_EQUAL(e.Getpy(), 2);
    BOOST_CHECK_EQUAL(e.Getpz(), 3);
  }

  BOOST_AUTO_TEST_CASE(is_subtractable) {
    Mom3 a(1, 2, 3);
    Mom3 b(0, 0, 0);
    Mom3 c = b - a;
    BOOST_CHECK_EQUAL(c.Getpx(), -1);
    BOOST_CHECK_EQUAL(c.Getpy(), -2);
    BOOST_CHECK_EQUAL(c.Getpz(), -3);

    Mom3 d(1, 2, 3);
    Mom3 e(0, 0, 0);
    e -= d;
    BOOST_CHECK_EQUAL(e.Getpx(), -1);
    BOOST_CHECK_EQUAL(e.Getpy(), -2);
    BOOST_CHECK_EQUAL(e.Getpz(), -3);
  }

  BOOST_AUTO_TEST_CASE(is_multiplicable_with_Mom3) {
    Mom3 a(1, 0, 0);
    Mom3 b(0, 1, 0);
    BOOST_CHECK_EQUAL(a*b, 0);

    Mom3 c(0, 1, 0);
    Mom3 d(0, 1, 0);
    BOOST_CHECK_EQUAL(c*d, 1);
  }

  BOOST_AUTO_TEST_CASE(is_multiplicable_with_double) {
    Mom3 a(1, 2, 3);
    const double f = 5;
    Mom3 b = a*f;
    BOOST_CHECK_EQUAL(b.Getpx(), 5);
    BOOST_CHECK_EQUAL(b.Getpy(), 10);
    BOOST_CHECK_EQUAL(b.Getpz(), 15);

    a *= f;
    BOOST_CHECK_EQUAL(a.Getpx(), 5);
    BOOST_CHECK_EQUAL(a.Getpy(), 10);
    BOOST_CHECK_EQUAL(a.Getpz(), 15);
  }

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(Mom4_test)
  BOOST_AUTO_TEST_CASE(properly_decays_to_Mom3) {
    Mom4 a(0, 1, 2, 3);
    Mom3 b(a);
    BOOST_CHECK_EQUAL(b.Getpx(), 1);
    BOOST_CHECK_EQUAL(b.Getpy(), 2);
    BOOST_CHECK_EQUAL(b.Getpz(), 3);
  }

  BOOST_AUTO_TEST_CASE(is_copy_constructable) {
    Mom4 a(0, 1, 2, 3);
    Mom4 b(a);
    BOOST_CHECK_EQUAL(b.GetE(), 0);
    BOOST_CHECK_EQUAL(b.Getpx(), 1);
    BOOST_CHECK_EQUAL(b.Getpy(), 2);
    BOOST_CHECK_EQUAL(b.Getpz(), 3);
  }

  BOOST_AUTO_TEST_CASE(is_addable) {
    Mom4 a(-1, 1, 2, 3);
    Mom4 b(0, 0, 0, 0);
    Mom4 c = a + b;
    BOOST_CHECK_EQUAL(c.GetE(), -1);
    BOOST_CHECK_EQUAL(c.Getpx(), 1);
    BOOST_CHECK_EQUAL(c.Getpy(), 2);
    BOOST_CHECK_EQUAL(c.Getpz(), 3);

    Mom4 d(-1, 1, 2, 3);
    Mom4 e( 0, 0, 0, 0);
    e += d;
    BOOST_CHECK_EQUAL(e.GetE(), -1);
    BOOST_CHECK_EQUAL(e.Getpx(), 1);
    BOOST_CHECK_EQUAL(e.Getpy(), 2);
    BOOST_CHECK_EQUAL(e.Getpz(), 3);
  }

  BOOST_AUTO_TEST_CASE(is_subtractable) {
    Mom4 a(-1, 1, 2, 3);
    Mom4 b( 0, 0, 0, 0);
    Mom4 c = b - a;
    BOOST_CHECK_EQUAL(c.GetE(),   1);
    BOOST_CHECK_EQUAL(c.Getpx(), -1);
    BOOST_CHECK_EQUAL(c.Getpy(), -2);
    BOOST_CHECK_EQUAL(c.Getpz(), -3);

    Mom4 d(-1, 1, 2, 3);
    Mom4 e( 0, 0, 0, 0);
    e -= d;
    BOOST_CHECK_EQUAL(e.GetE(),   1);
    BOOST_CHECK_EQUAL(e.Getpx(), -1);
    BOOST_CHECK_EQUAL(e.Getpy(), -2);
    BOOST_CHECK_EQUAL(e.Getpz(), -3);
  }

  BOOST_AUTO_TEST_CASE(is_multiplicable_with_Mom4) { }

  BOOST_AUTO_TEST_CASE(is_multiplicable_with_double) {
    Mom4 a(-1, 1, 2, 3);
    const double f = 5;
    Mom4 b = a*f;
    BOOST_CHECK_EQUAL(b.GetE(), -5);
    BOOST_CHECK_EQUAL(b.Getpx(), 5);
    BOOST_CHECK_EQUAL(b.Getpy(), 10);
    BOOST_CHECK_EQUAL(b.Getpz(), 15);

    a *= f;
    BOOST_CHECK_EQUAL(a.Getpx(), 5);
    BOOST_CHECK_EQUAL(a.Getpy(), 10);
    BOOST_CHECK_EQUAL(a.Getpz(), 15);
  }

BOOST_AUTO_TEST_SUITE_END()
