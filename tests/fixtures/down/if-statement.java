// -*- combobulate-test-point-overlays: ((1 outline 268) (2 outline 292) (3 outline 319) (4 outline 339) (5 outline 343) (6 outline 367) (7 outline 372) (8 outline 388) (9 outline 405) (10 outline 411)); eval: (combobulate-test-fixture-mode t); -*-
package example;

public class Example {
	@First
	public static void main(String[] args) {
		if (Math.random() < 10) {
			final var fn = (int a, int b) -> {
				return a + b;
			};
		}
	}
}
