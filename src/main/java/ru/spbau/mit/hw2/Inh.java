package ru.spbau.mit.hw2;

/*
 * Decompiled with CFR 0_118.
 * 
 * Could not load the following classes:
 *  scala.reflect.ScalaSignature
 *  scala.runtime.TraitSetter
 */
import scala.runtime.TraitSetter;

public class Inh implements Base {
    private int x;
    //after decompilation became final, but i removed modifier
    private int y;
    private volatile boolean flag;

    public static void run() {
        InhObject.instance.run();
    }

    @Override
    public int getX() {
        return this.x;
    }

    @TraitSetter
    @Override
    public void setX(int x1) {
        this.x = x1;
    }

    @Override
    public int getY() {
        return this.flag ? this.y : this.computeY();
    }

    private synchronized int computeY() {
        if (!this.flag) {
            this.y = AbstractBase.y(this);
            this.flag = true;
        }
        return this.y;
    }

    @Override
    public int goo() {
        return AbstractBase.goo(this);
    }

    @Override
    public int foo() {
        return 3;
    }

    public Inh() {
        AbstractBase.init(this);
    }
}
