/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.equable;

public abstract class BaseEquable {

    protected abstract Equable getEquable();

    @Override
    public int hashCode() {
        return getEquable().hashCode();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BaseEquable equable = (BaseEquable) o;
        return getEquable().equals(equable.getEquable());
    }
}
