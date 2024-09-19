// -*- combobulate-test-point-overlays: ((1 outline 180) (2 outline 221) (3 outline 264)); eval: (combobulate-test-fixture-mode t); -*-
func foo () {
	switch value := x.(type) {
	case string:
		fmt.Println("String type")
	case int:
		fmt.Println("Integer type")
	default:
		fmt.Printf("Unexpected type %T\n", value)
	}

}
