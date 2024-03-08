// -*- combobulate-test-point-overlays: ((1 outline 168) (2 outline 209) (3 outline 252)); eval: (combobulate-test-fixture-mode t); -*-
func foo () {
	switch value {
	case int:
		fmt.Println("Integer type")
	case string:
		fmt.Println("String type")
	default:
		fmt.Printf("Unexpected type %T\n", value)
	}

}
