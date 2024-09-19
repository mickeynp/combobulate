// -*- combobulate-test-point-overlays: ((1 outline 120)); eval: (combobulate-test-fixture-mode t); -*-
func foo () {
	switch value := x.(type) {
	case int:
		fmt.Println("Integer type")
	case string:
		fmt.Println("String type")
	default:
		fmt.Printf("Unexpected type %T\n", value)
	}

}
