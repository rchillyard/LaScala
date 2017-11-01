/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.equable;
//
//import org.junit.Test;
//
//import java.util.ArrayList;
//import java.util.Collection;
//
//import static org.junit.Assert.assertTrue;
//
//public class TupleTest {
//
//    /**
//     * Test method for MockTuple
//     */
//    @SuppressWarnings("EqualsWithItself")
//    @Test
//    public void testTuple() {
//        MockTuple mockTuple1 = new MockTuple(1, Math.PI);
//        MockTuple mockTuple2 = new MockTuple(2, Math.E);
//        assertTrue("mockTuple1.hashCode()==340593922", mockTuple1.hashCode() == 340593922);
//        assertTrue("mockTuple2.hashCode()==-888018783", mockTuple2.hashCode() == -888018783);
//        assertTrue("mockTuple1.equals(mockTuple1)", mockTuple1.equals(mockTuple1));
//        assertTrue("mockTuple2.equals(mockTuple2)", mockTuple2.equals(mockTuple2));
//        assertTrue("!mockTuple1.equals(mockTuple2)", !mockTuple1.equals(mockTuple2));
//        assertTrue("mockTuple1.toString()==\"MockTuple(1, 3.141592653589793)\"", mockTuple1.toString().equals("MockTuple(1, 3.141592653589793)"));
//
//    }
//
//    public class MockTuple extends BaseEquable {
//
//        public MockTuple(int x, double y) {
//            this.x = x;
//            this.y = y;
//        }
//
//        @Override
//        public String toString() {
//            return "MockTuple(" + x + ", " + y + ")";
//        }
//
//
//        @Override
//        public Equable getEquable() {
//            Collection<Object> elements = new ArrayList<>();
//            elements.add(x);
//            elements.add(y);
//            return new Equable(elements);
//        }
//
//        private final int x;
//        private final double y;
//
//    }
//
//
//}
