package com.reallifedeveloper.uu_1dl251.fitnesse;

public class AddInformation {

	private int foo;
	private int bar;

	public void setFoo(int foo) {
		this.foo = foo;
	}

	public int bar() {
		return bar;
	}

	public void reset() {
		this.foo = 0;
		this.bar = 0;
	}

	public void execute() {
		if (this.foo == 42) {
			this.bar = 4711;
		}
	}
}
