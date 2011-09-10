package javarun;

import java.util.ArrayList;

import scala.tools.nsc.MainGenericRunner;

public class JavaMain {

	public static void main(String[] args) {
		ArrayList<String> argList = new ArrayList<String>();
		argList.add("plan.Main");
		for (String s : args) {
			argList.add(s);
		}
		MainGenericRunner.main(argList.toArray(new String[0]));
	}
}
