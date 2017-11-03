/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.equable;

import java.util.Iterator;

/**
 * This package, borrowed from my INFO6205 class, implements the concept of a "primary key" to be used by the equals,
 * hashCode and, perhaps, compareTo methods.
 * Thus, defining a "primary key" for a class, guarantees that equals and hashCode (and perhaps compareTo) are consistent.
 */

public class Equable {

    public Equable(Iterable<?> elements) {
        this.elements = elements;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Equable equable = (Equable) o;
        Iterator<?> thisIterator = elements.iterator();
        Iterator<?> thatIterator = equable.elements.iterator();
        while (thisIterator.hasNext())
            if (thatIterator.hasNext())
                if (thisIterator.next().equals(thatIterator.next()))
                    continue;
                else
                    return false;
            else
                return false;
        return true;
    }

    @Override
    public int hashCode() {
        int result = 0;
        for (Object element : elements) result = 31 * result + element.hashCode();
        return result;
    }

    protected final Iterable<?> elements;

}
