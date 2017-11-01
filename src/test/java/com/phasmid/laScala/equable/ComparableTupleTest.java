/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.equable;
//
//
//import org.junit.Test;
//
//import java.util.ArrayList;
//import java.util.Collection;
//
//import static junit.framework.TestCase.assertEquals;
//
//
//public class ComparableTupleTest {
//
//    /**
//     * Test methods for MockComparableTuple
//     */
//    @Test
//    public void testComparableTuple1() {
//        MockComparableTuple tuple1 = new MockComparableTuple(1, Math.PI);
//        MockComparableTuple tuple2 = new MockComparableTuple(2, Math.E);
//        assertEquals(Integer.compare(1, 2), tuple1.compareTo(tuple2));
//        MockComparableTuple tuple3 = new MockComparableTuple(1, Math.E);
//        assertEquals(Double.compare(Math.PI, Math.E), tuple1.compareTo(tuple3));
//    }
//
//    @Test(expected = ComparableEquable.ComparableEquableException.class)
//    public void testComparableTuple2() {
//        MockIncomparableTuple tuple1 = new MockIncomparableTuple(new Incomparable());
//        MockIncomparableTuple tuple2 = new MockIncomparableTuple(new Incomparable());
//        tuple1.compareTo(tuple2);
//    }
//
//    public class MockComparableTuple extends BaseComparableEquable implements Comparable<MockComparableTuple> {
//
//        private final int x;
//        private final double y;
//
//        public MockComparableTuple(int x, double y) {
//            this.x = x;
//            this.y = y;
//        }
//
//        @Override
//        public String toString() {
//            return "Tuple(" + x + ", " + y + ")";
//        }
//
//        public Equable getEquable() {
//            @SuppressWarnings("unchecked") Collection<Object> elements = new ArrayList();
//            elements.add(x);
//            elements.add(y);
//            return new ComparableEquable(elements);
//        }
//
//        @Override
//        public int compareTo(MockComparableTuple o) {
//            return super.compareTo(o);
//        }
//    }
//
//    class Incomparable {
//        public Incomparable() {
//            super();
//        }
//    }
//
//    class MockIncomparableTuple extends BaseComparableEquable implements Comparable<MockIncomparableTuple> {
//
//        private final Incomparable y;
//
//        public MockIncomparableTuple(Incomparable y) {
//            this.y = y;
//        }
//
//        @Override
//        public String toString() {
//            return "MockIncomparableTuple(" + y + ")";
//        }
//
//        @SuppressWarnings("unchecked")
//        public Equable getEquable() {
//            Collection<Object> elements = new ArrayList();
//            elements.add(y);
//            return new ComparableEquable(elements);
//        }
//
//        @Override
//        public int compareTo(MockIncomparableTuple o) {
//            return super.compareTo(o);
//        }
//    }
//
//
//}
