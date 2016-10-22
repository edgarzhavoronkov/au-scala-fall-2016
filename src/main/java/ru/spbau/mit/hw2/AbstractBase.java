package ru.spbau.mit.hw2;

/*
 * Decompiled with CFR 0_118.
 */
public abstract class AbstractBase {
    public static int y(Base instance) {
        return instance.goo();
    }

    public static int goo(Base instance) {
        return instance.getX() + 2;
    }

    public static void init(Base instance) {
        instance.setX(1);
    }
}
