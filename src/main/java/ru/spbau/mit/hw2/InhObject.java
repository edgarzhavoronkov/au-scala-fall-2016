package ru.spbau.mit.hw2;

/*
 * Decompiled with CFR 0_118.
 */
public final class InhObject implements Runnable {
    public static final InhObject instance;

    static {
        instance = new InhObject();
    }

    @Override
    public void run() {
    }

    private int foo() {
        return 4;
    }

    private InhObject() {

    }
}
