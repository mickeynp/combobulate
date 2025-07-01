// -*- combobulate-test-point-overlays: ((1 outline 243) (2 outline 251) (3 outline 264) (4 outline 296) (5 outline 315) (6 outline 331) (7 outline 353)); eval: (combobulate-test-fixture-mode t); -*-
package si.lnet;

public class Example {
	@First
	@Second(xo)
	@Third
	public static void main(String[] args) {
		final var fn = (int a, int b) -> {
			return a + b;
		};
		if (true) {
			System.out.println("if true");
		} else {
			System.out.println("if false");
		}
		System.out.println("Testing");
		System.out.println("Testing");
		System.out.println("Testing");
		System.out.println("First line", 1, 2, 3);
		System.out.println("Testing");
	}
}
