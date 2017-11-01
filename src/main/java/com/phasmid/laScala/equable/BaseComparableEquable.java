/*
 * LaScala
 * Copyright (c) 2017. Phasmid Software
 */

package com.phasmid.laScala.equable;

/*
 * Copyright (c) 2017. Phasmid Software
 */
public abstract class BaseComparableEquable extends BaseEquable {

    protected int compareTo(BaseComparableEquable o) {
        return ((ComparableEquable) getEquable()).compareTo((ComparableEquable) o.getEquable());
    }
}
