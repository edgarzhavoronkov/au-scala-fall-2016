package ru.spbau.mit.hw2;

/*
 * Decompiled with CFR 0_118.
 * 
 * Could not load the following classes:
 *  scala.reflect.ScalaSignature
 *  scala.runtime.TraitSetter
 */
import scala.runtime.TraitSetter;

public interface Base {
    int getX();

    @TraitSetter
    void setX(int var1);

    int getY();

    int foo();

    int goo();
}
